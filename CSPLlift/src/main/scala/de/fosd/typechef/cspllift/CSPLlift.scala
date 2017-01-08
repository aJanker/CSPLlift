package de.fosd.typechef.cspllift

import de.fosd.typechef.cmodule.CModule
import de.fosd.typechef.cspllift.analysis.{SuperCallGraph, TaintCheck}
import de.fosd.typechef.cspllift.cifdsproblem.CFlowFact
import de.fosd.typechef.cspllift.cintercfg.DefaultCInterCFGConfiguration
import de.fosd.typechef.cspllift.commons.RewriteEngine
import de.fosd.typechef.cspllift.options.CSPLliftOptions
import de.fosd.typechef.customization.StopWatch
import de.fosd.typechef.featureexpr.FeatureModel
import de.fosd.typechef.featureexpr.bdd.BDDFeatureModel
import de.fosd.typechef.parser.c._
import spllift.LiftedCIFDSSolver

/*
 * Frontend interface for selecting different analysis strategies.
 */
class CSPLliftFrontend(env: CModule) {
    def analyze(opt: CSPLliftOptions): Unit = {

        lazy val cInterCFGConfiguration = new DefaultCInterCFGConfiguration(opt.getCLinkingInterfacePath, opt.resolveFunctionPointer)

        if (opt.IFDSTaintAnalysis)
            TaintCheck.checkAES(env, opt, cInterCFGConfiguration)
    }
}

/**
  * Connector the Java Frontend of the lifted IFDS solver.
  */
object CSPLlift {

    private lazy val REWRITE_ENGINE = new RewriteEngine {}

    def getIFDSRewriteRules: List[(TranslationUnit, FeatureModel) => TranslationUnit] = REWRITE_ENGINE.getIFDSRewriteRules

    def solve[D <: CFlowFact](problem: IFDSProblem[D], fm: FeatureModel = BDDFeatureModel.empty, benchmarkTag: Option[String] = None): LiftedCIFDSSolver[D] = {
        SuperCallGraph.clear()

        val solver = new LiftedCIFDSSolver(problem, fm, true)

        StopWatch.measureWallTime(benchmarkTag.getOrElse("") + "WALL_SOLVER_RUN", {solver.solve()})

        solver
    }

    def solveAndCollectResults[D <: CFlowFact](problem: IFDSProblem[D], fm: FeatureModel = BDDFeatureModel.empty, benchmarkTag : Option[String] = None): List[LiftedCFlowFact[D]] = {
        SuperCallGraph.clear()

        val solver = new LiftedCIFDSSolver(problem, fm, false)

        StopWatch.measureWallTime(benchmarkTag.getOrElse("") + "WALL_SOLVER_RUN", {solver.solve()})

        liftedFlowFactsAsScala(solver.getAllResults)
    }
}
