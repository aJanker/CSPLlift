package de.fosd.typechef.cspllift

import de.fosd.typechef.commons.StopWatch
import de.fosd.typechef.cspllift.analysis.Taint2
import de.fosd.typechef.cspllift.cifdsproblem.informationflow._
import de.fosd.typechef.cspllift.cifdsproblem.{CFlowFact, CIFDSProblem}
import de.fosd.typechef.cspllift.commons.WarningsCache
import de.fosd.typechef.cspllift.options.CSPLliftOptions
import de.fosd.typechef.featureexpr.FeatureModel
import de.fosd.typechef.featureexpr.bdd.BDDFeatureModel
import de.fosd.typechef.parser.c._
import soot.spl.ifds.SPLIFDSSolver

class CSPLliftFrontend(ast: TranslationUnit, fm: FeatureModel = BDDFeatureModel.empty) {

    def analyze(opt: CSPLliftOptions) = {

        val cInterCFGConfiguration = new DefaultCInterCFGConfiguration(opt.getCLinkingInterfacePath)

        if (opt.liftTaintAnalysis)
            taintCheck(opt, cInterCFGConfiguration)
    }

    private def taintCheck(opt: CSPLliftOptions, cInterCFGConfiguration: DefaultCInterCFGConfiguration): Unit = {

        val cInterCFG = new CInterCFG(ast, fm, cInterCFGConfiguration)

        val (_, (solution)) = StopWatch.measureUserTime("taint_lift", {
            val problem = new InformationFlow2Problem(cInterCFG)
            CSPLlift.solve(problem)
        })

        val allSinks = Taint2.allSinks(solution)

        println("#static taint analysis with spllift - result")

        println("\n#used tunits\n")
        cInterCFG.cInterCFGElementsCacheEnv.getAllKnownTUnits.foreach(x => println(PrettyPrinter.print(x)))

        println("\n#sinks\n")
        println(Taint2.prettyPrintSinks(allSinks))
        println("#static taint analysis with spllift - finished")
    }

    /* private def taintCheck(opt: CSPLliftOptions, cInterCFGConfiguration: DefaultCInterCFGConfiguration): Unit = {

        val cInterCFG = new CInterCFG(ast, fm, cInterCFGConfiguration)

        val (_, (solution)) = StopWatch.measureUserTime("taint_lift", {
            val problem = new InformationFlowProblem(cInterCFG)
            CSPLlift.solve(problem)
        })

        val allReaches = Taint.allSinks(solution)

        def hasName(name: String, source: Source): Boolean = {
            source.name.entry.name.equalsIgnoreCase(name) || source.reachingSources.exists(rs => hasName(name, rs))
        }

        val allKeyReaches = allReaches.filter(x => x._2.exists(y => y._1.sources.exists(s => hasName("key", s))))

        println("#static analysis with spllift - result")

        Taint.writeGraphToSink(cInterCFG, allKeyReaches, opt.getInformationFlowGraphsOutputDir, opt.getInformationFlowGraphExtension)
        Taint.writeGraphFromSource(cInterCFG, allKeyReaches, opt.getInformationFlowGraphsOutputDir, "_fromSource" + opt.getInformationFlowGraphExtension)
        SuperCallGraph.write(new InformationFlowGraphWriter(new FileWriter(opt.getInformationFlowGraphsOutputDir + "/callGraph.dot")))

        println("\n#used tunits\n")
        cInterCFG.cInterCFGElementsCacheEnv.getAllKnownTUnits.foreach(x => println(PrettyPrinter.print(x)))

        println("\n#sinks\n")
        println(Taint.prettyPrintSinks(allReaches))
    } */
}

object CSPLlift {

    def solveCIFDSProblem[D <: CFlowFact, T <: CIFDSProblem[D]](ifdsProblem: java.lang.Class[T], cifg: CInterCFG, fm: FeatureModel = BDDFeatureModel.empty, printWarnings: Boolean = false): List[LiftedCFlowFact[D]] =
        CSPLlift.solve[D](getCIFDSProblemInstance[D, T](ifdsProblem)(cifg), fm, printWarnings)

    def solve[D <: CFlowFact](problem: IFDSProblem[D], fm: FeatureModel = BDDFeatureModel.empty, printWarnings: Boolean = false): List[LiftedCFlowFact[D]] = {

        val (_, solver) = StopWatch.measureWallTime("wall_lift_init", {new SPLIFDSSolver(problem, fm, true)})
        StopWatch.measureWallTime("wall_lift_solve", {solver.solve()})

        if (printWarnings && WarningsCache.size() != 0) {
            println("#ISSUED Warnings:")
            println(WarningsCache)
            println("#TOTAL Warnings:\t" +  WarningsCache.issuedWarnings())
        }

        // Looks messy, but requiered for a clean conversion from java collections to scala collections...
        liftedFlowFactsAsScala(solver.getAllResults)
    }
}
