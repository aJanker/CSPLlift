package de.fosd.typechef.cspllift

import de.fosd.typechef.cspllift.analysis.{SuperCallGraph, TaintCheck}
import de.fosd.typechef.cspllift.cifdsproblem.{CFlowFact, CIFDSProblem}
import de.fosd.typechef.cspllift.commons.WarningsCache
import de.fosd.typechef.cspllift.options.CSPLliftOptions
import de.fosd.typechef.customization.StopWatch
import de.fosd.typechef.featureexpr.FeatureModel
import de.fosd.typechef.featureexpr.bdd.BDDFeatureModel
import de.fosd.typechef.parser.c._
import spllift.SPLIFDSSolver

class CSPLliftFrontend(ast: TranslationUnit, fm: FeatureModel = BDDFeatureModel.empty) {

    def analyze(opt: CSPLliftOptions) = {

        val cInterCFGConfiguration = new DefaultCInterCFGConfiguration(opt.getCLinkingInterfacePath)

        if (opt.liftTaintAnalysis)
            TaintCheck.check(ast, fm, opt, cInterCFGConfiguration)
    }


}

object CSPLlift {

    def solveCIFDSProblem[D <: CFlowFact, T <: CIFDSProblem[D]](ifdsProblem: java.lang.Class[T], cifg: CInterCFG, fm: FeatureModel = BDDFeatureModel.empty, printWarnings: Boolean = false): List[LiftedCFlowFact[D]] =
        CSPLlift.solve[D](getCIFDSProblemInstance[D, T](ifdsProblem)(cifg), fm, printWarnings)

    def solve[D <: CFlowFact](problem: IFDSProblem[D], fm: FeatureModel = BDDFeatureModel.empty, printWarnings: Boolean = false): List[LiftedCFlowFact[D]] = {
        SuperCallGraph.clear()

        val (_, solver) = StopWatch.measureWallTime("wall_lift_init", {new SPLIFDSSolver(problem, fm, false)})
        StopWatch.measureWallTime("wall_lift_solve", {solver.solve()})

        if (printWarnings && WarningsCache.size() != 0) {
            println("#ISSUED Warnings:")
            println(WarningsCache)
            println("#TOTAL Warnings:\t" +  WarningsCache.issuedWarnings())
        }

        liftedFlowFactsAsScala(solver.getAllResults)
    }
}
