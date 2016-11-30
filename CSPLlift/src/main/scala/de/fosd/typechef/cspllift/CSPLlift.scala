package de.fosd.typechef.cspllift

import de.fosd.typechef.cspllift.analysis.{SuperCallGraph, TaintCheck}
import de.fosd.typechef.cspllift.cifdsproblem.CFlowFact
import de.fosd.typechef.cspllift.cintercfg.DefaultCInterCFGConfiguration
import de.fosd.typechef.cspllift.options.CSPLliftOptions
import de.fosd.typechef.customization.StopWatch
import de.fosd.typechef.featureexpr.FeatureModel
import de.fosd.typechef.featureexpr.bdd.BDDFeatureModel
import de.fosd.typechef.parser.c._
import spllift.LiftedIFDSSolver

/*
 * Frontend interface for selecting different analysis strategies.
 */
class CSPLliftFrontend(ast: TranslationUnit, fm: FeatureModel = BDDFeatureModel.empty) {
    def analyze(opt: CSPLliftOptions) = {

        lazy val cInterCFGConfiguration = new DefaultCInterCFGConfiguration(opt.getCLinkingInterfacePath, opt.resolveFunctionPointer)

        if (opt.IFDSTaintAnalysis)
            TaintCheck.checkAES(ast, fm, opt, cInterCFGConfiguration)
    }
}

/**
  * Connector the Java Frontend of the lifted IFDS solver.
  */
object CSPLlift {

    def solve[D <: CFlowFact](problem: IFDSProblem[D], fm: FeatureModel = BDDFeatureModel.empty, benchmarkTag : Option[String] = None) : LiftedIFDSSolver[D] = {
        SuperCallGraph.clear()

        val solver = new LiftedIFDSSolver(problem, fm, true)

        StopWatch.measureWallTime(benchmarkTag.getOrElse("") + "WALL_SOLVER_RUN", {solver.solve()})

        solver
    }

    def solveAndCollectResults[D <: CFlowFact](problem: IFDSProblem[D], fm: FeatureModel = BDDFeatureModel.empty, benchmarkTag : Option[String] = None): List[LiftedCFlowFact[D]] = {
        SuperCallGraph.clear()

        val solver = new LiftedIFDSSolver(problem, fm, false)

        StopWatch.measureWallTime(benchmarkTag.getOrElse("") + "WALL_SOLVER_RUN", {solver.solve()})

        liftedFlowFactsAsScala(solver.getAllResults)
    }
}
