package de.fosd.typechef.spllift

import java.io.FileWriter
import java.util

import de.fosd.typechef.commons.StopWatch
import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.featureexpr.FeatureModel
import de.fosd.typechef.featureexpr.bdd.BDDFeatureModel
import de.fosd.typechef.parser.c._
import de.fosd.typechef.spllift.analysis.{InformationFlowGraphWriter, SuperCallGraph, Taint}
import de.fosd.typechef.spllift.commons.WarningsCache
import de.fosd.typechef.spllift.ifdsproblem.{InformationFlow, InformationFlowProblem, Source}
import de.fosd.typechef.spllift.options.CSPLliftOptions
import heros.IFDSTabulationProblem
import soot.spl.ifds.{Constraint, FeatureModelContext, SPLIFDSSolver}

import scala.collection.JavaConverters._

class CSPLliftFrontend(ast: TranslationUnit, fm: FeatureModel = BDDFeatureModel.empty) {

    def analyze(opt: CSPLliftOptions) = {
        val cInterCFGOptions = new DefaultCInterCFGOptions(opt.getCLinkingInterfacePath)
        val cInterCFG = new CInterCFG(ast, fm, cInterCFGOptions)

        if (opt.liftTaintAnalysis) {
            val (_, (solution, cifg)) = StopWatch.measureUserTime("spllift", {
                val problem = new InformationFlowProblem(cInterCFG)
                (CSPLlift.solve[InformationFlow](problem), cInterCFG)

            })

            val allReaches = Taint.allReaches[String](solution)

            def hasName(name: String, source: Source): Boolean = {
                source.name.entry.name.equalsIgnoreCase(name) || source.reachingSources.exists(rs => hasName(name, rs))
            }

            val allKeyReaches = allReaches.filter(x => x._2.exists(y => y._2.sources.exists(s => hasName("key", s))))

            println("#static analysis with spllift - result")

            Taint.writeGraphToSink(cifg, allKeyReaches, opt.getInformationFlowGraphsOutputDir, opt.getInformationFlowGraphExtension)
            Taint.writeGraphFromSource(cifg, allKeyReaches, opt.getInformationFlowGraphsOutputDir, "_fromSource" + opt.getInformationFlowGraphExtension)
            SuperCallGraph.write(new InformationFlowGraphWriter(new FileWriter(opt.getInformationFlowGraphsOutputDir + "/callGraph.dot")))
            println(Taint.prettyPrintSinks(allKeyReaches))
        }
    }

}

object CSPLlift {

    def solve[D](problem: IFDSTabulationProblem[Opt[AST], D, Opt[FunctionDef], CInterCFG], fmContext: FeatureModelContext = new FeatureModelContext(), printWarnings: Boolean = false): List[util.Map[D, Constraint[String]]] = {

        val (_, solver) = StopWatch.measureWallTime("spllift_init", {new SPLIFDSSolver(problem, fmContext, false)})
        StopWatch.measureWallTime("spllift_solve", {solver.solve()})

        if (printWarnings && WarningsCache.size() != 0) {
            println("#ISSUED Warnings:")
            println(WarningsCache)
            println("#TOTAL Warnings:\t" +  WarningsCache.issuedWarnings())
        }

        solver.getAllResults.asScala.toList

    }
}
