package de.fosd.typechef.spllift.evaluation

import de.fosd.typechef.commons.StopWatch
import de.fosd.typechef.featureexpr.FeatureModel
import de.fosd.typechef.featureexpr.bdd.BDDFeatureModel
import de.fosd.typechef.parser.c.TranslationUnit
import de.fosd.typechef.spllift.cifdsproblem.{CIFDSProblem, FlowFact, InformationFlow, InformationFlowProblem}
import de.fosd.typechef.spllift.commons.ConditionTools
import de.fosd.typechef.spllift.options.CSPLliftOptions
import de.fosd.typechef.spllift.{CInterCFG, CSPLlift, DefaultCInterCFGOptions, _}
import soot.spl.ifds.Constraint

class CSPLliftEvalFrontend(ast: TranslationUnit, fm: FeatureModel = BDDFeatureModel.empty) extends ConditionTools {

    def checkAgainstSampling(opt: CSPLliftOptions) : Boolean = {
        var successful = true

        if (opt.liftTaintAnalysis)
            successful = runSampling[InformationFlow, InformationFlowProblem](classOf[InformationFlowProblem], opt: CSPLliftOptions) && successful

        successful
    }

    def checkAgainstErrorConfiguration(opt: CSPLliftOptions) = {

    }


    private def runSampling[D <: FlowFact, T <: CIFDSProblem[D]](ifdsProblem: java.lang.Class[T], opt: CSPLliftOptions) : Boolean = {

        // 1. Step -> Run VAA first in order to detect all linked files for codecoverageconfiguration generation
        val cInterCFGOptions = new DefaultCInterCFGOptions(opt.getCLinkingInterfacePath)
        val (vaaWallTime, (vaaSolution, icfg)) = runSPLLift[D, T](ifdsProblem, cInterCFGOptions, "vaa")
        val vaaFacts = vaaSolution.flatMap(_.toList)

        // 2. Generate Code Coverage Configurations for all referenced files
        val sampling = new Sampling(icfg.cInterCFGElementsCacheEnv.getAllKnownTUnitsAsSingleTUnit, fm)
        val configs = sampling.codeConfigurationCoverage()

        // 3. Run Code Coverage
        val coverageResults = configs.map(config => {
            val cInterCFGOptions = new ConfigurationBasedCInterCFGOptions(config, opt.getCLinkingInterfacePath)
            val (wallTime, (solution, icfg)) = runSPLLift[D, T](ifdsProblem, cInterCFGOptions, "coverage")
            (solution, config, wallTime)
        })

        // 4. Compare
        var matchedVaaFacts = scala.collection.mutable.Map[(D, Constraint[_]), Int]()
        vaaFacts.foreach(fact => matchedVaaFacts += (fact -> 0))

        val unmatchedCoverageFacts = coverageResults.flatMap(c => {
            val covFacts = c._1.flatMap(_.toList)
            val configuration = c._2

            // Check if for every coverage based result a corresponding vaa based result was found.
            val unmatched = covFacts.filterNot(cr => {
                vaaFacts.exists(vr => {
                    val condition = vr._2

                    if (isSatisfiableInConfiguration(condition, configuration) && vr._1.isEquivalent(cr._1, configuration)  ) {
                        // is match, increase vaa match counter
                        matchedVaaFacts += (vr -> (matchedVaaFacts.getOrElse(vr, 0) + 1))
                        true
                    } else false

                })
            })

            unmatched.map(um => (um, configuration))
        })

        val unmatchedVAAFacts = matchedVaaFacts.toList.collect { case ((x, 0)) => x }

        if (unmatchedVAAFacts.nonEmpty) {
            println("\n### Following results where not covered by the coverage approach: ")
            println("Size:\t" +  unmatchedVAAFacts.size)

            unmatchedVAAFacts.foreach(uc => println("Error:\n" + uc))
        }

        if (unmatchedCoverageFacts.nonEmpty) {
            println("\n### Following results where not covered by the lifted approach: ")
            println("Size:\t" + unmatchedCoverageFacts.size)

            unmatchedCoverageFacts.foreach(uc => {
                println("Configuration:\t" + uc._2)
                println("Error:\n" + uc._1)
            })
        }

        unmatchedCoverageFacts.isEmpty
    }

    private def runSPLLift[D <: FlowFact, T <: CIFDSProblem[D]](ifdsProblem: Class[T], cInterCFGOptions: CInterCFGOptions, stopWatchMark: String = "None"): (Long, (List[Map[D, Constraint[String]]], CInterCFG)) =
        StopWatch.measureUserTime(stopWatchMark, {
            val cInterCFG = new CInterCFG(ast, fm, cInterCFGOptions)
            val problem = getCIFDSProblemInstance[D, T](ifdsProblem)(cInterCFG)

            (CSPLlift.solve[D](problem), cInterCFG)
        })
}
