package de.fosd.typechef.spllift

import java.io.FileWriter

import de.fosd.typechef.commons.StopWatch
import de.fosd.typechef.featureexpr.FeatureModel
import de.fosd.typechef.featureexpr.bdd.BDDFeatureModel
import de.fosd.typechef.parser.c._
import de.fosd.typechef.spllift.analysis.{InformationFlowGraphWriter, SuperCallGraph, Taint}
import de.fosd.typechef.spllift.cifdsproblem.{CFlowFact, CIFDSProblem, InformationFlowProblem, Source}
import de.fosd.typechef.spllift.commons.WarningsCache
import de.fosd.typechef.spllift.options.CSPLliftOptions
import soot.spl.ifds.{Constraint, SPLIFDSSolver}

import scala.collection.JavaConverters._

class CSPLliftFrontend(ast: TranslationUnit, fm: FeatureModel = BDDFeatureModel.empty) {

    def analyze(opt: CSPLliftOptions) = {

        val cInterCFGOptions = new DefaultCInterCFGConfiguration(opt.getCLinkingInterfacePath)

        if (opt.liftTaintAnalysis) {
            val cInterCFG = new CInterCFG(ast, fm, cInterCFGOptions)

            val (_, (solution)) = StopWatch.measureUserTime("taint_lift", {
                val problem = new InformationFlowProblem(cInterCFG)
                CSPLlift.solve(problem)
            })

            val allReaches = Taint.allReaches(solution)

            def hasName(name: String, source: Source): Boolean = {
                source.name.entry.name.equalsIgnoreCase(name) || source.reachingSources.exists(rs => hasName(name, rs))
            }

            val allKeyReaches = allReaches.filter(x => x._2.exists(y => y._2.sources.exists(s => hasName("key", s))))

            println("#static analysis with spllift - result")

            Taint.writeGraphToSink(cInterCFG, allKeyReaches, opt.getInformationFlowGraphsOutputDir, opt.getInformationFlowGraphExtension)
            Taint.writeGraphFromSource(cInterCFG, allKeyReaches, opt.getInformationFlowGraphsOutputDir, "_fromSource" + opt.getInformationFlowGraphExtension)
            SuperCallGraph.write(new InformationFlowGraphWriter(new FileWriter(opt.getInformationFlowGraphsOutputDir + "/callGraph.dot")))
            println(Taint.prettyPrintSinks(allReaches))
        }
    }

}

object CSPLlift {

    def solveCIFDSProblem[D <: CFlowFact, T <: CIFDSProblem[D]](ifdsProblem: java.lang.Class[T], cifg: CInterCFG, fm: FeatureModel = BDDFeatureModel.empty, printWarnings: Boolean = false): List[Map[D, Constraint]] =
        CSPLlift.solve[D](getCIFDSProblemInstance[D, T](ifdsProblem)(cifg), fm, printWarnings)

    def solve[D <: CFlowFact](problem: IFDSProblem[D], fm: FeatureModel = BDDFeatureModel.empty, printWarnings: Boolean = false): List[Map[D, Constraint]] = {

        val (_, solver) = StopWatch.measureWallTime("wall_lift_init", {new SPLIFDSSolver(problem, fm, true)})
        StopWatch.measureWallTime("wall_lift_solve", {solver.solve()})

        if (printWarnings && WarningsCache.size() != 0) {
            println("#ISSUED Warnings:")
            println(WarningsCache)
            println("#TOTAL Warnings:\t" +  WarningsCache.issuedWarnings())
        }

        // Looks messy, but requiered for a clean conversion from java collections to scala collections...
        solver.getAllResults.asScala.map(_.asScala.toMap).toList

    }
}
