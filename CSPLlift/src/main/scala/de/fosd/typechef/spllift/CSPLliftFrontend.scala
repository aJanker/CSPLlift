package de.fosd.typechef.spllift

import java.util

import de.fosd.typechef.commons.StopWatch
import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.parser.c._
import de.fosd.typechef.spllift.commons.WarningsCache
import heros.IFDSTabulationProblem
import soot.spl.ifds.{Constraint, FeatureModelContext, SPLIFDSSolver}

import scala.collection.JavaConverters._

object CSPLliftFrontend {

    def solve[D](problem: IFDSTabulationProblem[Opt[AST], D, Opt[FunctionDef], CInterCFG], fmContext: FeatureModelContext = new FeatureModelContext()): List[util.Map[D, Constraint[String]]] = {

        val (_, solver) = StopWatch.measureWallTime("spllift_init", {new SPLIFDSSolver(problem, fmContext, false)})
        StopWatch.measureWallTime("spllift_solve", {solver.solve()})

        if (WarningsCache.size() != 0) {
            println("#ISSUED Warnings:")
            println(WarningsCache)
            println("#TOTAL Warnings:\t" +  WarningsCache.issuedWarnings())
        }

        println("eq-relations")

        println(problem.interproceduralCFG().cInterCFGElementsCacheEnv.cFunctionPointerEQRelation.pointerEquivalenceClassesToString())

        println()
        solver.getAllResults.asScala.toList

    }
}
