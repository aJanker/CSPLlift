package de.fosd.typechef.spllift.evaluation

import java.util

import de.fosd.typechef.commons.StopWatch
import de.fosd.typechef.featureexpr.FeatureModel
import de.fosd.typechef.featureexpr.bdd.BDDFeatureModel
import de.fosd.typechef.parser.c.TranslationUnit
import de.fosd.typechef.spllift.cifdsproblem.{CIFDSProblem, InformationFlow, InformationFlowProblem}
import de.fosd.typechef.spllift.options.CSPLliftOptions
import de.fosd.typechef.spllift.{CInterCFG, CSPLlift, DefaultCInterCFGOptions, _}
import soot.spl.ifds.Constraint

class CSPLliftEvalFrontend(ast: TranslationUnit, fm: FeatureModel = BDDFeatureModel.empty) {

    def checkAgainstSampling(opt: CSPLliftOptions) = {
        if (opt.liftTaintAnalysis)
            runSampling[InformationFlow, InformationFlowProblem](classOf[InformationFlowProblem], opt: CSPLliftOptions)
    }

    def checkAgainstErrorConfiguration(opt: CSPLliftOptions) = {

    }


    private def runSampling[D, T <: CIFDSProblem[D]](ifdsProblem: java.lang.Class[T], opt: CSPLliftOptions) = {
        // 1. Step -> Run VAA first in order to detect all linked files for codecoverageconfiguration generation

        val cInterCFGOptions = new DefaultCInterCFGOptions(opt.getCLinkingInterfacePath)
        val (vaaWallTime, (vaaSolution, icfg)) = runSPLLift[D, T](ifdsProblem, cInterCFGOptions, "vaa")

        val sampling = new Sampling(icfg.cInterCFGElementsCacheEnv.getAllKnownTUnitsAsSingleTUnit, fm)
        val configs = sampling.codeConfigurationCoverage()

        val coverageResults = configs.map(config => {
            val cInterCFGOptions = new ConfigurationBasedCInterCFGOptions(config.getTrueSet, opt.getCLinkingInterfacePath)
            val (wallTime, (solution, icfg)) = runSPLLift[D, T](ifdsProblem, cInterCFGOptions, "coverage")
            (solution, config.getTrueSet, wallTime)
        })

        // TODO Compare Results


    }

    private def runSPLLift[D, T <: CIFDSProblem[D]](ifdsProblem: Class[T], cInterCFGOptions: CInterCFGOptions, stopWatch: String = "None"): (Long, (List[util.Map[D, Constraint[String]]], CInterCFG)) =
        StopWatch.measureUserTime(stopWatch, {
            val cInterCFG = new CInterCFG(ast, fm, cInterCFGOptions)
            val problem = getCIFDSProblemInstance[D, T](ifdsProblem)(cInterCFG)

            (CSPLlift.solve[D](problem), cInterCFG)
        })
}
