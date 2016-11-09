package de.fosd.typechef.cspllift

import java.io.{File, FileWriter}

import de.fosd.typechef.cspllift.analysis.{InformationFlow, InformationFlowGraphWriter, SuperCallGraph}
import de.fosd.typechef.cspllift.cifdsproblem.informationflow._
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
            taintCheck(opt, cInterCFGConfiguration)
    }

    private def taintCheck(opt: CSPLliftOptions, cInterCFGConfiguration: DefaultCInterCFGConfiguration): Unit = {

        val cInterCFG = new CInterCFG(ast, fm, cInterCFGConfiguration)

        val (_, (solution)) = StopWatch.measureUserTime("taint_lift", {
            val problem = new InformationFlowProblem(cInterCFG)
            CSPLlift.solve(problem, printWarnings = true)
        })

        if (opt.isLiftPrintExplodedSuperCallGraphEnabled)
            writeExplodedSuperCallGraph(opt)

        val allSinks = InformationFlow.allSinks(solution)

        println("#static taint analysis with spllift - result")

        println("\n#sinks")
        println(InformationFlow.prettyPrintSinks(allSinks))

        println("\n#used tunits number:")
        println(cInterCFG.cInterCFGElementsCacheEnv.getAllKnownTUnits.size + "\n")
        println("#static taint analysis with spllift - finished")
    }

    private def writeExplodedSuperCallGraph(opt: CSPLliftOptions) : Unit = {
        val graphDir = opt.getInformationFlowGraphsOutputDir
        val dir = new File(graphDir)

        if (!(dir.exists() && dir.isDirectory)) dir.mkdirs()

        SuperCallGraph.write(new InformationFlowGraphWriter(new FileWriter(graphDir + "/callGraph.dot")))

        SuperCallGraph.clear()
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
