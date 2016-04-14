package de.fosd.typechef.spllift

import java.util

import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.parser.c._
import heros.IFDSTabulationProblem
import soot.spl.ifds.{Constraint, FeatureModelContext, SPLIFDSSolver}

import scala.collection.JavaConverters._

object CSPLliftFrontend {

    def solve[D](problem: IFDSTabulationProblem[AST, D, Opt[FunctionDef], CInterCFG], fmContext: FeatureModelContext = new FeatureModelContext()): List[util.Map[D, Constraint[String]]] = {

        val solver = new SPLIFDSSolver(problem, fmContext, false)
        solver.solve()

        solver.getAllResults.asScala.toList

    }
}
