package de.fosd.typechef.spllift.evaluation

import de.fosd.typechef.commons.StopWatch
import de.fosd.typechef.featureexpr.FeatureModel
import de.fosd.typechef.featureexpr.bdd.BDDFeatureModel
import de.fosd.typechef.parser.c.TranslationUnit
import de.fosd.typechef.spllift.cifdsproblem.{CIFDSProblem, FlowFact, InformationFlow, InformationFlowProblem}
import de.fosd.typechef.spllift.options.CSPLliftOptions
import de.fosd.typechef.spllift.{CInterCFG, CSPLlift, DefaultCInterCFGOptions, _}
import soot.spl.ifds.Constraint

class CSPLliftEvalFrontend(ast: TranslationUnit, fm: FeatureModel = BDDFeatureModel.empty) {

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
        val unCoveredCoverageWarnings = coverageResults.flatMap(result => {
            val uncovered = result._1.filterNot(cSolution => {
                vaaSolution.exists(vSolution => {
                     vSolution

                    true
                })     // TODO Compare Method
            })

            uncovered.map(uc => (uc, result._2))
        })

        if (unCoveredCoverageWarnings.nonEmpty) {
            println("### Following results where not covered by the lifted approach: ")
            unCoveredCoverageWarnings.foreach(uc => {
                println("Configuration:\t" + uc._2)
                println("Error:\n" + uc._1)
            })
        }

        unCoveredCoverageWarnings.isEmpty
    }

    private def runSPLLift[D <: FlowFact, T <: CIFDSProblem[D]](ifdsProblem: Class[T], cInterCFGOptions: CInterCFGOptions, stopWatchMark: String = "None"): (Long, (List[Map[D, Constraint[String]]], CInterCFG)) =
        StopWatch.measureUserTime(stopWatchMark, {
            val cInterCFG = new CInterCFG(ast, fm, cInterCFGOptions)
            val problem = getCIFDSProblemInstance[D, T](ifdsProblem)(cInterCFG)

            (CSPLlift.solve[D](problem), cInterCFG)
        })
}
