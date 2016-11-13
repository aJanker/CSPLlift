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

class CSPLliftEvaluationFrontend(ast: TranslationUnit, fm: FeatureModel = BDDFeatureModel.empty) extends ConditionTools with CInterCFGCommons {

    /**
      * For performance reasons we do not compare in our setup for mbedTLS the results for the following hash function implementations.
      * Their implementation is correct as well our evaluation of these functions was true,
      * however they generate over 250000 single facts which causes to slow down all other evaluation tasks.
      */
    private val ignoredFiles: List[String] = List("md5", "md4", "md2", "sha1", "sha256", "sha512", "bignum")

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
        val coverageFacts = configs.zipWithIndex.map(x => {
            val (config, i) = x
            val run = "coverage_" + i

            println("### starting run:\t" + (i + 1) + " of " + configs.size)
            println(config)

            val cInterCFGOptions = new ConfigurationBasedCInterCFGConfiguration(config, opt.getCLinkingInterfacePath, run)
            val (wallTime, (solution, icfg)) = runSPLLift[D, T](ifdsProblem, cInterCFGOptions, run + "_")

            if (opt.writeVariants) writeVariants(icfg, opt, method, Some(i), Some(config))
            if (opt.isLiftPrintExplodedSuperCallGraphEnabled) writeExplodedSuperCallGraph(opt, method, Some(run))

            (solution, config, wallTime)
        })

        // 4. Compare
        println("### Started Result Compare...")
        val (unmatchedLiftedFacts, unmatchedCoverageFacts) = compareLiftedWithSampling(liftedFacts, coverageFacts.map(x => (x._1, x._2)))

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

        /*println("### results for lifiting ")
        val allLiftSinks = InformationFlow.allSinks(liftedFacts.asInstanceOf[List[(InformationFlowFact, FeatureExpr)]])

        println(InformationFlow.prettyPrintSinks(allLiftSinks)) */

        // 2. Collect distinct conditions
        val cfgConditions = liftedFacts.foldLeft(Set[FeatureExpr]())((cfgConds, fact) => {
            val cfgCond = fact._2
            cfgConds + cfgCond
        })

        // 3. Generate Condition Coverage Configurations for all distinct warning conditions
        val sampling = new Sampling(icfg.cInterCFGElementsCacheEnv.getAllKnownTUnitsAsSingleTUnit, fm)
        val configs = sampling.conditionConfigurationCoverage(cfgConditions.asInstanceOf[Set[FeatureExpr]])

        // 4. Run Analysis for every generated config
        val coverageFacts = configs.zipWithIndex.map(x => {
            val (config, i) = x
            val run = "singleConf_" + i

            println("### starting run:\t" + (i + 1) + " of " + configs.size)
            println(config)

            val cInterCFGOptions = new ConfigurationBasedCInterCFGConfiguration(config, opt.getCLinkingInterfacePath, run)
            val (wallTime, (solution, icfg)) = runSPLLift[D, T](ifdsProblem, cInterCFGOptions, run + "_")

            if (opt.writeVariants) writeVariants(icfg, opt, method, Some(i), Some(config))
            if (opt.isLiftPrintExplodedSuperCallGraphEnabled) writeExplodedSuperCallGraph(opt, method, Some(run))

            /*println("### results for " + config)
            val interestingSamplingFacts = solution.filter(_._1.isInterestingFact)

            val allSinks = InformationFlow.allSinks(interestingSamplingFacts.asInstanceOf[List[(InformationFlowFact, FeatureExpr)]])

            println(InformationFlow.prettyPrintSinks(allSinks)) */

            (solution, config, wallTime)
        })

        // 5. Compare
        println("### Started Result Compare...")
        val (unmatchedLiftedFacts, unmatchedCoverageFacts) = compareLiftedWithSampling(liftedFacts, coverageFacts.map(x => (x._1, x._2)))

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

    private def compareResults[D <: CFlowFact](liftedFacts: List[LiftedCFlowFact[D]], samplingFacts: List[LiftedCFlowFact[D]], config : SimpleConfiguration) = {
        def isIgnoredFile(f: LiftedCFlowFact[D]): Boolean =
            f match {
                case (s: Sink, _) =>
                    val fName = getPlainFileNameS(s.cICFGStmt.getStmt.entry.getPositionTo.getFile)
                    !ignoredFiles.contains(fName)
                case _ => false
            }

        val interestingLiftedFacts = liftedFacts.par.filter(_._1.isEvaluationFact).filter(isIgnoredFile).filter(_._2.evaluate(config.getTrueFeatures)).toList
        val interestingSamplingFacts = samplingFacts.par.filter(_._1.isEvaluationFact).filter(isIgnoredFile).toList

        var matchedLiftedFacts = scala.collection.concurrent.TrieMap[LiftedCFlowFact[D], Int]()
        interestingLiftedFacts.foreach(fact => matchedLiftedFacts += (fact -> 0))

        interestingSamplingFacts.par.filterNot(sampleFact => interestingLiftedFacts.foldLeft(false)((found, liftedFact) =>
            if (liftedFact._1.isEquivalentTo(sampleFact._1, config)) {
                // is match, increase vaa match counter
                matchedLiftedFacts += (liftedFact -> (matchedLiftedFacts.getOrElse(liftedFact, 0) + 1))
                true
            } else found
        )).toList
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
        /* val interestingSamplingFacts = samplingResults.par.flatMap(res => res._1.filter(_._1.isEvaluationFact)).filter(isHashingFile).toList

        println("### Comparing " + interestingLiftedFacts.size + " lifted facts with a total amount of product facts: " + interestingSamplingFacts.size) */

        var matchedLiftedFacts = scala.collection.concurrent.TrieMap[LiftedCFlowFact[D], Int]()
        interestingLiftedFacts.foreach(fact => matchedLiftedFacts += (fact -> 0))

        def unmatchedFacts(samplingFacts: List[(D, FeatureExpr)], liftedFacts: List[(D, FeatureExpr)], config: SimpleConfiguration): List[(D, FeatureExpr)] = {
            val matchingLiftedFacts = liftedFacts.par.filter(_._2.evaluate(config.getTrueFeatures)).toList
            samplingFacts.par.filterNot(fact => matchingLiftedFacts.foldLeft(false)((found, oFact) =>
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
            // val satisfiableLiftedFacts = interestingLiftedFacts.par.filter(fact => isSatisfiableInConfiguration(fact._2, config)).toList

            val unmatched = unmatchedFacts(interestingSamplingFacts, interestingLiftedFacts, config)

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