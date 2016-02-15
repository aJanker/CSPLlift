package de.fosd.typechef.spllift

import de.fosd.typechef.parser.c.{AST, FunctionDef, TranslationUnit}
import heros.IFDSTabulationProblem
import soot.spl.ifds.{FeatureModelContext, SPLIFDSSolver}

class CSPLliftFrontend(startTunit: TranslationUnit, options: CSPLliftOptions = DefaultCSPLliftOptions) {

  private val cintercfg = new CInterCFG(startTunit, options)

  def solve[D](problem: IFDSTabulationProblem[AST, D, FunctionDef, CInterCFG]) = {
    val fmContext = new FeatureModelContext()
    val solver = new SPLIFDSSolver(problem, fmContext, false)
    solver.solve()
    //solver.resultsAt()
  }

  def getCInterCFG = cintercfg

}
