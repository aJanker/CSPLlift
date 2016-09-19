package de.fosd.typechef.spllift.evaluation

import de.fosd.typechef.commons.StopWatch
import de.fosd.typechef.crewrite.ProductDerivation
import de.fosd.typechef.featureexpr.FeatureModel
import de.fosd.typechef.featureexpr.bdd.{BDDFeatureExpr, BDDFeatureModel}
import de.fosd.typechef.parser.c.{PrettyPrinter, TranslationUnit}
import de.fosd.typechef.spllift.cifdsproblem.{CFlowFact, CIFDSProblem, InformationFlow, InformationFlowProblem}
import de.fosd.typechef.spllift.commons.ConditionTools
import de.fosd.typechef.spllift.options.CSPLliftOptions
import de.fosd.typechef.spllift.{CInterCFG, CSPLlift, DefaultCInterCFGConfiguration, _}
import soot.spl.ifds.Constraint

class CSPLliftEvaluationFrontend(ast: TranslationUnit, fm: FeatureModel = BDDFeatureModel.empty) extends ConditionTools {

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
            successful = runSampling[InformationFlow, InformationFlowProblem](classOf[InformationFlowProblem], opt: CSPLliftOptions) && successful

        successful
    }

    def checkAgainstErrorConfiguration(opt: CSPLliftOptions): Boolean = {
        var successful = true

        if (opt.liftTaintAnalysis)
            successful = runErrorConfiguration[InformationFlow, InformationFlowProblem](classOf[InformationFlowProblem], opt: CSPLliftOptions) && successful

        successful
    }

    private def runSampling[D <: CFlowFact, T <: CIFDSProblem[D]](ifdsProblem: java.lang.Class[T], opt: CSPLliftOptions): Boolean = {

        // 1. Step -> Run VAA first in order to detect all linked files for codecoverageconfiguration generation
        val cInterCFGOptions = new DefaultCInterCFGConfiguration(opt.getCLinkingInterfacePath)
        val (vaaWallTime, (liftedFacts, icfg)) = runSPLLift[D, T](ifdsProblem, cInterCFGOptions, "vaa")

        // 2. Generate Code Coverage Configurations for all referenced files
        val sampling = new Sampling(icfg.cInterCFGElementsCacheEnv.getAllKnownTUnitsAsSingleTUnit, fm)
        val configs = sampling.codeConfigurationCoverage()

        // 3. Run Analysis for every generated config
        val coverageFacts = configs.zipWithIndex.map(x => {
            val (config, i) = x
            val run = "coverage_" + i
            val cInterCFGOptions = new ConfigurationBasedCInterCFGConfiguration(config, opt.getCLinkingInterfacePath, run)
            val (wallTime, (solution, icfg)) = runSPLLift[D, T](ifdsProblem, cInterCFGOptions, run + "_")
            (solution, config, wallTime)
        })

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
        // 1. Step -> Run VAA first in order to detect all affected features
        val cInterCFGOptions = new DefaultCInterCFGConfiguration(opt.getCLinkingInterfacePath)
        val (vaaUserTime, (liftedFacts, icfg)) = runSPLLift[D, T](ifdsProblem, cInterCFGOptions, "vaa")

        // 2. Collect distinct conditions
        val (cfgConditions, factConditions) = liftedFacts.foldLeft((Set[BDDFeatureExpr](), Set[BDDFeatureExpr]()))((x, fact) => {
            val (cfgConds, factConds) = x

            val cfgCond = fact._2.getFeatureExpr
            val factCond = fact._1.getConditions

            (cfgConds + cfgCond, factConds ++ factCond)
        })

        // 3. Generate Condition Coverage Configurations for all distinct warning conditions
        val sampling = new Sampling(icfg.cInterCFGElementsCacheEnv.getAllKnownTUnitsAsSingleTUnit, fm)
        val configs = sampling.conditionConfigurationCoverage(cfgConditions)

        // 4. Run Analysis for every generated config
        val coverageFacts = configs.zipWithIndex.map(x => {
            val (config, i) = x
            val run = "singleConf_" + i
            val cInterCFGOptions = new ConfigurationBasedCInterCFGConfiguration(config, opt.getCLinkingInterfacePath, run)
            val (wallTime, (solution, icfg)) = runSPLLift[D, T](ifdsProblem, cInterCFGOptions, run + "_")
            (solution, config, wallTime)
        })

        // 5. Compare
        val (unmatchedLiftedFacts, unmatchedCoverageFacts) = compareLiftedWithSampling(liftedFacts, coverageFacts.map(x => (x._1, x._2)))

        println("\n### Tested " + configs.size + " unique variants for condition coverage.")
        configs.foreach(config => {
            println("### Current Config:\t" + config + "\n")
            println(PrettyPrinter.print(ProductDerivation.deriveProduct(ast, config.getTrueFeatures)))
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

        unmatchedLiftedFacts.isEmpty && unmatchedCoverageFacts.isEmpty
    }

    private def compareLiftedWithSampling[D <: CFlowFact](liftedFacts: List[LiftedCFlowFact[D]], samplingResults: List[(List[LiftedCFlowFact[D]], SimpleConfiguration)]): (List[LiftedCFlowFact[D]], List[(List[LiftedCFlowFact[D]], SimpleConfiguration)]) = {
        val interestingLiftedFacts = liftedFacts.filter(_._1.isInterestingFact)
        val interestingSamplingFacts = samplingResults.map(res => (res._1.filter(_._1.isInterestingFact) ,res._2))

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

       interestingSamplingFacts.foreach(s => {
           println(s._2)
           s._1.foreach(x => println(x._1.toText))
       })

        (unmatchedLiftedFacts.distinct, unmatchedSamplingFacts)
    }


    private def runSPLLift[D <: CFlowFact, T <: CIFDSProblem[D]](ifdsProblem: Class[T], cInterCFGOptions: CInterCFGConfiguration, stopWatchMark: String = "None"): (Long, (List[LiftedCFlowFact[D]], CInterCFG)) =
        StopWatch.measureUserTime(stopWatchMark, {
            val cInterCFG = new CInterCFG(ast, fm, cInterCFGOptions)
            val problem = getCIFDSProblemInstance[D, T](ifdsProblem)(cInterCFG)

            (CSPLlift.solve[D](problem), cInterCFG)
        })
}
