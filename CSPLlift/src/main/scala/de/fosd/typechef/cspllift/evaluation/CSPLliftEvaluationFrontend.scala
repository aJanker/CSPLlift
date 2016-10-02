package de.fosd.typechef.cspllift.evaluation

import java.io._

import de.fosd.typechef.cspllift.analysis.{InformationFlowGraphWriter, SuperCallGraph, Taint}
import de.fosd.typechef.cspllift.cifdsproblem.informationflow.{FlowFact, InformationFlowProblem}
import de.fosd.typechef.cspllift.cifdsproblem.{CFlowFact, CIFDSProblem}
import de.fosd.typechef.cspllift.commons.{CInterCFGCommons, ConditionTools}
import de.fosd.typechef.cspllift.options.CSPLliftOptions
import de.fosd.typechef.cspllift.{CInterCFG, CSPLlift, DefaultCInterCFGConfiguration, _}
import de.fosd.typechef.customization.StopWatch
import de.fosd.typechef.featureexpr.bdd.{BDDFeatureExpr, BDDFeatureModel}
import de.fosd.typechef.featureexpr.{FeatureExpr, FeatureModel}
import de.fosd.typechef.parser.c.{PrettyPrinter, TranslationUnit}
import spllift.Constraint

class CSPLliftEvaluationFrontend(ast: TranslationUnit, fm: FeatureModel = BDDFeatureModel.empty) extends ConditionTools with CInterCFGCommons {

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
            successful = runSampling[FlowFact, InformationFlowProblem](classOf[InformationFlowProblem], opt: CSPLliftOptions) && successful

        successful
    }

    def checkAgainstErrorConfiguration(opt: CSPLliftOptions): Boolean = {
        var successful = true

        if (opt.liftTaintAnalysis)
            successful = runErrorConfiguration[FlowFact, InformationFlowProblem](classOf[InformationFlowProblem], opt: CSPLliftOptions) && successful

        successful
    }

    private def runSampling[D <: CFlowFact, T <: CIFDSProblem[D]](ifdsProblem: java.lang.Class[T], opt: CSPLliftOptions): Boolean = {
        val method: String = "codeCoverage"

        // 1. Step -> Run VAA first in order to detect all linked files for codecoverageconfiguration generation
        val cInterCFGOptions = new DefaultCInterCFGConfiguration(opt.getCLinkingInterfacePath)
        val (vaaWallTime, (liftedFacts, icfg)) = runSPLLift[D, T](ifdsProblem, cInterCFGOptions, "vaa")

        if (opt.isLiftPrintExplodedSuperCallGraphEnabled) writeExplodedSuperCallGraph(opt, method)

        // 2. Generate Code Coverage Configurations for all referenced files
        val sampling = new Sampling(icfg.cInterCFGElementsCacheEnv.getAllKnownTUnitsAsSingleTUnit, fm)
        val configs = sampling.codeConfigurationCoverage()

        // 3. Run Analysis for every generated config
        val coverageFacts = configs.zipWithIndex.map(x => {
            val (config, i) = x
            val run = "coverage_" + i
            val cInterCFGOptions = new ConfigurationBasedCInterCFGConfiguration(config, opt.getCLinkingInterfacePath, run)
            val (wallTime, (solution, icfg)) = runSPLLift[D, T](ifdsProblem, cInterCFGOptions, run + "_")
            if (opt.isLiftPrintExplodedSuperCallGraphEnabled) writeExplodedSuperCallGraph(opt, method, Some(run))
            (solution, config, icfg, wallTime)
        })

        // Print Variants
        if (opt.writeVariants) printVariants(icfg, coverageFacts, opt, method)

        // 4. Compare
        val (unmatchedLiftedFacts, unmatchedCoverageFacts) = compareLiftedWithSampling(liftedFacts, coverageFacts.map(x => (x._1, x._2)))

        println("\n### Tested " + configs.size + " unique variants for code coverage.")
        configs.foreach(println)
        println

        if (unmatchedLiftedFacts.nonEmpty) {
            println("\n### Following results were not covered by the coverage approach: ")
            println("Size:\t" + unmatchedLiftedFacts.size)

            // unmatchedLiftedFacts.foreach(uc => println("Error:\n" + uc))
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
        val method : String = "conditionCoverage"

        // 1. Step -> Run VAA first in order to detect all affected features
        val cInterCFGOptions = new DefaultCInterCFGConfiguration(opt.getCLinkingInterfacePath)
        val (vaaUserTime, (liftedFacts, icfg)) = runSPLLift[D, T](ifdsProblem, cInterCFGOptions, "vaa")

        // 2. Collect distinct conditions
        val cfgConditions = liftedFacts.foldLeft(Set[BDDFeatureExpr]())((cfgConds, fact) => {
            val cfgCond = fact._2.getFeatureExpr
            cfgConds + cfgCond
        })

        // 3. Generate Condition Coverage Configurations for all distinct warning conditions
        val sampling = new Sampling(icfg.cInterCFGElementsCacheEnv.getAllKnownTUnitsAsSingleTUnit, fm)
        val configs = sampling.conditionConfigurationCoverage(cfgConditions.asInstanceOf[Set[FeatureExpr]])

        // 4. Run Analysis for every generated config
        val coverageFacts = configs.zipWithIndex.map(x => {
            val (config, i) = x
            val run = "singleConf_" + i
            val cInterCFGOptions = new ConfigurationBasedCInterCFGConfiguration(config, opt.getCLinkingInterfacePath, run)
            val (wallTime, (solution, icfg)) = runSPLLift[D, T](ifdsProblem, cInterCFGOptions, run + "_")
            if (opt.isLiftPrintExplodedSuperCallGraphEnabled) writeExplodedSuperCallGraph(opt, method, Some(run))
            (solution, config, icfg, wallTime)
        })

        // Print Variants
        if (opt.writeVariants) printVariants(icfg, coverageFacts, opt, method)

        // 5. Compare
        val (unmatchedLiftedFacts, unmatchedCoverageFacts) = compareLiftedWithSampling(liftedFacts, coverageFacts.map(x => (x._1, x._2)))

        println("\n### Tested " + configs.size + " unique variants for condition coverage.")
        configs.foreach(config => {
            println("### Current Config:\t" + config + "\n")
            println(PrettyPrinter.print(deriveProductWithCondition(ast, config.getTrueFeatures)))
        })
        println

        if (unmatchedLiftedFacts.nonEmpty) {
            println("\n### Following results were not covered by the condition coverage approach: ")
            println("Size:\t" + unmatchedLiftedFacts.size)

            unmatchedLiftedFacts.foreach(uc => println("Error:\n" + "\tCondition:" + uc._2 + "\n\t" + uc._1.toText))
        } else println("\n### All results were covered by the condition coverage approach!")


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
        } else println("\n### All condition coverage results were covered by the lifted approach!")

        if (!(unmatchedLiftedFacts.isEmpty && unmatchedCoverageFacts.isEmpty)) {
            val interestingCoverageFacts = coverageFacts.map {
                fact => (Taint.allSinks(fact._1.asInstanceOf[List[LiftedCFlowFact[FlowFact]]]), fact._2)
            }
            // debug purpose only
            println(interestingCoverageFacts.size)
        }

        unmatchedLiftedFacts.isEmpty && unmatchedCoverageFacts.isEmpty
    }

    private def compareLiftedWithSampling[D <: CFlowFact](liftedFacts: List[LiftedCFlowFact[D]], samplingResults: List[(List[LiftedCFlowFact[D]], SimpleConfiguration)]): (List[LiftedCFlowFact[D]], List[(List[LiftedCFlowFact[D]], SimpleConfiguration)]) = {
        val interestingLiftedFacts = liftedFacts.filter(_._1.isInterestingFact)
        val interestingSamplingFacts = samplingResults.map(res => (res._1.filter(_._1.isInterestingFact), res._2))

        var matchedLiftedFacts = scala.collection.mutable.Map[LiftedCFlowFact[D], Int]()
        interestingLiftedFacts.foreach(fact => matchedLiftedFacts += (fact -> 0))

        def unmatchedFacts(samplingFacts: List[(D, Constraint)], liftedFacts: List[(D, Constraint)], config: SimpleConfiguration): List[(D, Constraint)] =
            samplingFacts.filterNot(fact => liftedFacts.foldLeft(false)((found, oFact) =>
                if (oFact._1.isEquivalentTo(fact._1, config)) {
                    // is match, increase vaa match counter
                    matchedLiftedFacts += (oFact -> (matchedLiftedFacts.getOrElse(oFact, 0) + 1))
                    true
                } else found
            ))

        val unmatchedSamplingFacts = samplingResults.flatMap(samplingResult => {
            val (samplingFacts, config) = samplingResult
            val interestingSamplingFacts = samplingFacts.filter(_._1.isInterestingFact)
            val satisfiableLiftedFacts = interestingLiftedFacts.filter(fact => isSatisfiableInConfiguration(fact._2, config))

            val unmatched = unmatchedFacts(interestingSamplingFacts, satisfiableLiftedFacts, config)

            if (unmatched.isEmpty) None
            else Some((unmatched, config))
        })

        val unmatchedLiftedFacts = matchedLiftedFacts.toList.collect { case ((x, 0)) => x }

        /* interestingSamplingFacts.foreach(s => {
            println(s._2)
            s._1.foreach(x => println(x._1.toText))
        }) */

        (unmatchedLiftedFacts.distinct, unmatchedSamplingFacts)
    }


    private def runSPLLift[D <: CFlowFact, T <: CIFDSProblem[D]](ifdsProblem: Class[T], cInterCFGOptions: CInterCFGConfiguration, stopWatchMark: String = "None"): (Long, (List[LiftedCFlowFact[D]], CInterCFG)) =
        StopWatch.measureUserTime(stopWatchMark, {
            val cInterCFG = new CInterCFG(ast, fm, cInterCFGOptions)
            val problem = getCIFDSProblemInstance[D, T](ifdsProblem)(cInterCFG)

            (CSPLlift.solve[D](problem), cInterCFG)
        })

    private def printVariants[T <: CIFDSProblem[D], D <: CFlowFact](vaaCIFG: CInterCFG, coverageFacts: List[(List[(D, Constraint)], SimpleConfiguration, CInterCFG, Long)], opt: CSPLliftOptions, method: String): Unit = {
        val printDir = opt.getVariantsOutputDir + "/" + method + "/"
        val dir = new File(printDir)

        if (!(dir.exists() && dir.isDirectory)) dir.mkdirs()

        vaaCIFG.cInterCFGElementsCacheEnv.getAllFiles.foreach {
            case (file, tunit) =>
                val variant = printDir + "/" + getPlainFileNameS(file) + ".c"
                writeStringToGZipFile(PrettyPrinter.print(tunit), variant)
        }

        coverageFacts.zipWithIndex.foreach {
            case ((_, config, icfg, _), index) =>
                val outputDir = printDir + "/" + index
                val currDir = new File(outputDir)

                if (!(currDir.exists() && currDir.isDirectory)) currDir.mkdirs()

                writeStringToGZipFile(config.toString, outputDir + "/config")

                icfg.cInterCFGElementsCacheEnv.getAllFiles.foreach {
                    case (file, tunit) =>
                        val variant = outputDir + "/" + getPlainFileNameS(file) + ".c"
                        writeStringToGZipFile(PrettyPrinter.print(tunit), variant)
                }
        }
    }

    private def writeExplodedSuperCallGraph(opt: CSPLliftOptions, method: String, variant : Option[String] = None) : Unit = {
        val graphDir = opt.getInformationFlowGraphsOutputDir + "/" + method + "/"
        val dir = new File(graphDir)

        if (!(dir.exists() && dir.isDirectory)) dir.mkdirs()

        if (variant.isEmpty) SuperCallGraph.write(new InformationFlowGraphWriter(new FileWriter(graphDir + "/callGraph.dot")))
        else SuperCallGraph.write(new InformationFlowGraphWriter(new FileWriter(graphDir + "/" + variant.get + "_callGraph.dot")))

        SuperCallGraph.clear()
    }
}
