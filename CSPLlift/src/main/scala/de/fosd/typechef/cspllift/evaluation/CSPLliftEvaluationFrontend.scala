package de.fosd.typechef.cspllift.evaluation

import java.io._

import de.fosd.typechef.cspllift._
import de.fosd.typechef.cspllift.analysis.{InformationFlow, InformationFlowGraphWriter, SuperCallGraph}
import de.fosd.typechef.cspllift.cifdsproblem.informationflow.InformationFlowProblem
import de.fosd.typechef.cspllift.cifdsproblem.informationflow.flowfact.InformationFlowFact
import de.fosd.typechef.cspllift.cifdsproblem.informationflow.flowfact.sinkorsource.Sink
import de.fosd.typechef.cspllift.cifdsproblem.{CFlowFact, CIFDSProblem}
import de.fosd.typechef.cspllift.commons.{CInterCFGCommons, ConditionTools}
import de.fosd.typechef.cspllift.options.CSPLliftOptions
import de.fosd.typechef.customization.StopWatch
import de.fosd.typechef.featureexpr.bdd.BDDFeatureModel
import de.fosd.typechef.featureexpr.{FeatureExpr, FeatureModel}
import de.fosd.typechef.parser.c.{PrettyPrinter, TranslationUnit}

import scala.collection.concurrent.TrieMap

class CSPLliftEvaluationFrontend(ast: TranslationUnit, fm: FeatureModel = BDDFeatureModel.empty) extends ConditionTools with CInterCFGCommons {

    /**
      * For performance reasons we do not compare in our setup for mbedTLS the intrafile results for the following hash function implementations
      * at every file. We observed that these files generate a huge amount of costy facts for comparision, causing out-of-memory errors.
      */
    private val ignoredFiles: List[String] = List()

    def evaluate(opt: CSPLliftOptions): Boolean = {
        var successful = true

        if (opt.isLiftSamplingEvaluationEnabled)
            successful = checkAgainstSampling(opt) && successful

        if (opt.isLiftSingleEvaluationEnabled)
            successful = checkAgainstErrorConfiguration(opt) && successful

        successful
    }


    def checkAgainstSampling(opt: CSPLliftOptions): Boolean = {
        var successful = true

        if (opt.liftTaintAnalysis)
            successful = runSampling[InformationFlowFact, InformationFlowProblem](classOf[InformationFlowProblem], opt: CSPLliftOptions) && successful

        successful
    }

    def checkAgainstErrorConfiguration(opt: CSPLliftOptions): Boolean = {
        var successful = true

        if (opt.liftTaintAnalysis)
            successful = runErrorConfiguration[InformationFlowFact, InformationFlowProblem](classOf[InformationFlowProblem], opt: CSPLliftOptions) && successful

        successful
    }

    private def runAndCompare[D <: CFlowFact, T <: CIFDSProblem[D]](cSPLlift: (java.lang.Class[T], CSPLliftOptions), configs: List[SimpleConfiguration], liftedFacts: List[LiftedCFlowFact[D]], strategy: String = "") = {
        def isIgnoredFile(f: LiftedCFlowFact[D]): Boolean =
            f match {
                case (s: Sink, _) =>
                    val fName = getPlainFileNameS(s.cICFGStmt.getStmt.entry.getPositionTo.getFile)
                    !ignoredFiles.contains(fName)
                case _ => false
            }

        val (ifdsProblem, opt) = cSPLlift
        var matchedLiftedFacts = scala.collection.concurrent.TrieMap[LiftedCFlowFact[D], Int]()
        val liftedEvalFacts = liftedFacts.par.filter(_._1.isEvaluationFact).filter(isIgnoredFile).toList

        liftedEvalFacts.foreach(fact => matchedLiftedFacts += (fact -> 0))

        val res = configs.zipWithIndex.flatMap {
            case (config, i) => singleCompareRun(configs, strategy, isIgnoredFile _, ifdsProblem, opt, matchedLiftedFacts, liftedEvalFacts, config, i)
        }

        val unmatchedLiftedFacts = matchedLiftedFacts.par.collect { case ((x, 0)) => x }.toList
        (unmatchedLiftedFacts.distinct, res)
    }

    private def singleCompareRun[T <: CIFDSProblem[D], D <: CFlowFact](configs: List[SimpleConfiguration], strategy: String, isIgnoredFile: ((D, FeatureExpr)) => Boolean, ifdsProblem: Class[T], opt: CSPLliftOptions, matchedLiftedFacts: TrieMap[(D, FeatureExpr), Int], liftedEvalFacts: List[(D, FeatureExpr)], config: SimpleConfiguration, i: Int): Iterable[(List[(D, FeatureExpr)], SimpleConfiguration)] = {
        val run: String = if (strategy.equalsIgnoreCase("")) strategy + "_" + i else i.toString

        println("### starting run:\t" + (i + 1) + " of " + configs.size)
        println(config)

        val cInterCFGOptions = new ConfigurationBasedCInterCFGConfiguration(config, opt.getCLinkingInterfacePath, run)
        val (wallTime, (solution, icfg)) = runSPLLift[D, T](ifdsProblem, cInterCFGOptions, run + "_")

        if (opt.writeVariants) writeVariants(icfg, opt, strategy, Some(i), Some(config))
        if (opt.isLiftPrintExplodedSuperCallGraphEnabled) writeExplodedSuperCallGraph(opt, strategy, Some(run))

        val sampleEvalFacts = solution.par.filter(_._1.isEvaluationFact).filter(isIgnoredFile)
        val satLiftedEvalFacts = liftedEvalFacts.par.filter(fact => isSatisfiableInConfiguration(fact._2, config)).toList

        val unmatchedSampleEvalFacts = sampleEvalFacts.filterNot(fact => satLiftedEvalFacts.foldLeft(false)((found, oFact) =>
            if (oFact._1.isEquivalentTo(fact._1, config)) {
                // is match, increase vaa match counter
                matchedLiftedFacts += (oFact -> (matchedLiftedFacts.getOrElse(oFact, 0) + 1))
                true
            } else found
        )).toList

        // icfg.cInterCFGElementsCacheEnv.cFunctionPointerEQRelation.showPointerEquivalenceClasses()

        if (unmatchedSampleEvalFacts.nonEmpty) Some((unmatchedSampleEvalFacts, config))
        else None
    }

    private def runSampling[D <: CFlowFact, T <: CIFDSProblem[D]](ifdsProblem: java.lang.Class[T], opt: CSPLliftOptions): Boolean = {
        val method: String = "codeCoverage"

        // 1. Step -> Run VAA first in order to detect all linked files for codecoverageconfiguration generation
        val cInterCFGOptions = new DefaultCInterCFGConfiguration(opt.getCLinkingInterfacePath)
        val (vaaWallTime, (liftedFacts, icfg)) = runSPLLift[D, T](ifdsProblem, cInterCFGOptions, "vaa")

        if (opt.writeVariants) writeVariants(icfg, opt, method)
        if (opt.isLiftPrintExplodedSuperCallGraphEnabled) writeExplodedSuperCallGraph(opt, method)

        // 2. Generate Code Coverage Configurations for all referenced files
        val sampling = new Sampling(icfg.cInterCFGElementsCacheEnv.getAllKnownTUnitsAsSingleTUnit, fm)
        val configs = sampling.codeConfigurationCoverage()

        // 3. Run Analysis for every generated config
        // 4. Compare
        val (unmatchedLiftedFacts, unmatchedCoverageFacts) = runAndCompare((ifdsProblem, opt), configs, liftedFacts, method)

        println("\n### Tested " + configs.size + " unique variants for code coverage.")
        configs.foreach(println)
        println

        if (unmatchedLiftedFacts.nonEmpty) {
            println("\n### Following results were not covered by the coverage approach: ")
            println("Size:\t" + unmatchedLiftedFacts.size)
        }

        if (unmatchedCoverageFacts.nonEmpty) {
            println("\n### Following results were not covered by the lifted approach: ")
            println("Size:\t" + unmatchedCoverageFacts.foldLeft(0)((i, x) => x._1.size + i))

            unmatchedCoverageFacts.foreach(uc => {
                println("Configuration:\t" + uc._2)
                uc._1.foreach(uc2 => {
                    println("Error:\n" + uc2._1)
                    println
                    println(uc2._1.toText)
                })

                println("###\n")
            })
        } else println("\n### All results were covered by the lifted approach!")

        unmatchedCoverageFacts.isEmpty
    }

    private def runErrorConfiguration[D <: CFlowFact, T <: CIFDSProblem[D]](ifdsProblem: Class[T], opt: CSPLliftOptions): Boolean = {
        val method: String = "conditionCoverage"

        // 1. Step -> Run VAA first in order to detect all affected features
        val cInterCFGOptions = new DefaultCInterCFGConfiguration(opt.getCLinkingInterfacePath)
        val (vaaUserTime, (liftedFacts, icfg)) = runSPLLift[D, T](ifdsProblem, cInterCFGOptions, "vaa")

        if (opt.writeVariants) writeVariants(icfg, opt, method)
        if (opt.isLiftPrintExplodedSuperCallGraphEnabled) writeExplodedSuperCallGraph(opt, method)

        println("### results for lifiting ")
        val allLiftSinks = InformationFlow.allSinks(liftedFacts.asInstanceOf[List[(InformationFlowFact, FeatureExpr)]])

        println(InformationFlow.prettyPrintSinks(allLiftSinks))

        // icfg.cInterCFGElementsCacheEnv.cFunctionPointerEQRelation.showPointerEquivalenceClasses()

        // 2. Collect distinct conditions
        val cfgConditions = liftedFacts.foldLeft(Set[FeatureExpr]())((cfgConds, fact) => {
            val cfgCond = fact._2
            cfgConds + cfgCond
        })

        // 3. Generate Condition Coverage Configurations for all distinct warning conditions
        val sampling = new Sampling(icfg.cInterCFGElementsCacheEnv.getAllKnownTUnitsAsSingleTUnit, fm)
        val configs = sampling.conditionConfigurationCoverage(cfgConditions.asInstanceOf[Set[FeatureExpr]])

        /*// 4. Run Analysis for every generated config
        val coverageFacts = configs.zipWithIndex.map(x => {
            val (config, i) = x
            val run = "singleConf_" + i

            println("### starting run:\t" + (i + 1) + " of " + configs.size)
            println(config)

            val cInterCFGOptions = new ConfigurationBasedCInterCFGConfiguration(config, opt.getCLinkingInterfacePath, run)
            val (wallTime, (solution, icfg)) = runSPLLift[D, T](ifdsProblem, cInterCFGOptions, run + "_")

            if (opt.writeVariants) writeVariants(icfg, opt, method, Some(i), Some(config))
            if (opt.isLiftPrintExplodedSuperCallGraphEnabled) writeExplodedSuperCallGraph(opt, method, Some(run))

           /* println("### results for " + config)
            val interestingSamplingFacts = solution.filter(_._1.isEvaluationFact)

            val allSinks = InformationFlow.allSinks(interestingSamplingFacts.asInstanceOf[List[(InformationFlowFact, FeatureExpr)]])

            println(InformationFlow.prettyPrintSinks(allSinks)) */

            (solution, config, wallTime)
        })

        // 5. Compare
        println("### Started Result Compare...")
        val (unmatchedLiftedFacts, unmatchedCoverageFacts) = compareLiftedWithSampling(liftedFacts, coverageFacts.map(x => (x._1, x._2))) */
        val (unmatchedLiftedFacts, unmatchedCoverageFacts) = runAndCompare((ifdsProblem, opt), configs, liftedFacts, method)

        println("\n### Tested " + configs.size + " unique variants for condition coverage.")

        if (unmatchedLiftedFacts.nonEmpty) {
            println("\n### Following results were not covered by the condition coverage approach: ")
            println("Size:\t" + unmatchedLiftedFacts.size)
            println(liftedFacts.count(_._1.isEvaluationFact))

            var conditions: List[FeatureExpr] = List()

            unmatchedLiftedFacts.foreach(uc => {
                conditions = uc._2 :: conditions
                println("Error:\n" + "\tCondition:" + uc._2 + "\n\t" + uc._1.toText)
            })

            println("#unique conditions")

            conditions.distinct.foreach(cond => {
                println(cond)
            })
        } else println("\n### All results were covered by the condition coverage approach!")

        if (unmatchedCoverageFacts.nonEmpty) {
            println("\n### Following results were not covered by the lifted approach: ")
            println("Size:\t" + unmatchedCoverageFacts.foldLeft(0)((i, x) => x._1.size + i))

            unmatchedCoverageFacts.foreach(uc => {
                println("Configuration:\t" + uc._2)
                val all = InformationFlow.allSinks(uc._1.asInstanceOf[List[(InformationFlowFact, FeatureExpr)]])
                println(InformationFlow.prettyPrintSinks(all))
                println("###\n")
            })
        } else println("\n### All condition coverage results were covered by the lifted approach!")

        unmatchedLiftedFacts.isEmpty && unmatchedCoverageFacts.isEmpty
    }

    private def compareLiftedWithSampling[D <: CFlowFact](liftedFacts: List[LiftedCFlowFact[D]], samplingResults: List[(List[LiftedCFlowFact[D]], SimpleConfiguration)]): (List[LiftedCFlowFact[D]], List[(List[LiftedCFlowFact[D]], SimpleConfiguration)]) = {
        def isHashingFile(f: LiftedCFlowFact[D]): Boolean =
            f match {
                case (s: Sink, _) =>
                    val fName = getPlainFileNameS(s.cICFGStmt.getStmt.entry.getPositionTo.getFile)
                    !ignoredFiles.exists(_.equalsIgnoreCase(fName))
                case _ => false
            }

        val interestingLiftedFacts = liftedFacts.par.filter(_._1.isEvaluationFact).filter(isHashingFile).toList
        var matchedLiftedFacts = scala.collection.concurrent.TrieMap[LiftedCFlowFact[D], Int]()
        interestingLiftedFacts.foreach(fact => matchedLiftedFacts += (fact -> 0))

        def unmatchedFacts(samplingFacts: List[(D, FeatureExpr)], liftedFacts: List[(D, FeatureExpr)], config: SimpleConfiguration): List[(D, FeatureExpr)] = {
            samplingFacts.par.filterNot(fact => liftedFacts.foldLeft(false)((found, oFact) =>
                if (oFact._1.isEquivalentTo(fact._1, config)) {
                    // is match, increase vaa match counter
                    matchedLiftedFacts += (oFact -> (matchedLiftedFacts.getOrElse(oFact, 0) + 1))
                    true
                } else found
            )).toList
        }

        val unmatchedSamplingFacts = samplingResults.flatMap(samplingResult => {
            val (samplingFacts, config) = samplingResult
            val interestingSamplingFacts = samplingFacts.par.filter(_._1.isEvaluationFact).filter(isHashingFile).toList
            val satisfiableLiftedFacts = interestingLiftedFacts.par.filter(fact => isSatisfiableInConfiguration(fact._2, config)).toList

            val unmatched = unmatchedFacts(interestingSamplingFacts, satisfiableLiftedFacts, config)

            if (unmatched.isEmpty) None
            else Some((unmatched, config))
        })

        val unmatchedLiftedFacts = matchedLiftedFacts.par.collect { case ((x, 0)) => x }.toList

        (unmatchedLiftedFacts.distinct, unmatchedSamplingFacts)
    }


    private def runSPLLift[D <: CFlowFact, T <: CIFDSProblem[D]](ifdsProblem: Class[T], cInterCFGOptions: CInterCFGConfiguration, stopWatchMark: String = "None"): (Long, (List[LiftedCFlowFact[D]], CInterCFG)) =
        StopWatch.measureUserTime(stopWatchMark, {
            val cInterCFG = new CInterCFG(ast, fm, cInterCFGOptions)
            val problem = getCIFDSProblemInstance[D, T](ifdsProblem)(cInterCFG)

            (CSPLlift.solve[D](problem), cInterCFG)
        })

    private def writeVariants[T <: CIFDSProblem[D], D <: CFlowFact](icfg: CInterCFG, opt: CSPLliftOptions, method: String = "", index: Option[Int] = None, config: Option[SimpleConfiguration] = None) = {
        val printDir = opt.getVariantsOutputDir + "/" + method + "/"
        val outputDir = if (index.isDefined) printDir + "/" + index.get else printDir

        val currDir = new File(outputDir)
        if (!(currDir.exists() && currDir.isDirectory)) currDir.mkdirs()

        if (config.isDefined) writeStringToGZipFile(config.get.toString, outputDir + "/config")

        icfg.cInterCFGElementsCacheEnv.getAllFiles.foreach {
            case (file, tunit) =>
                val variant = outputDir + "/" + getPlainFileNameS(file) + ".c"
                writeStringToGZipFile(PrettyPrinter.print(tunit), variant)
        }
    }

    private def writeExplodedSuperCallGraph(opt: CSPLliftOptions, method: String, variant: Option[String] = None): Unit = {
        val graphDir = opt.getInformationFlowGraphsOutputDir + "/" + method + "/"
        val dir = new File(graphDir)

        if (!(dir.exists() && dir.isDirectory)) dir.mkdirs()

        if (variant.isEmpty) SuperCallGraph.write(new InformationFlowGraphWriter(new FileWriter(graphDir + "/callGraph.dot")))
        else SuperCallGraph.write(new InformationFlowGraphWriter(new FileWriter(graphDir + "/" + variant.get + "_callGraph.dot")))

        SuperCallGraph.clear()
    }
}