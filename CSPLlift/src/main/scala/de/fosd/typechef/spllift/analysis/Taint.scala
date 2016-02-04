package de.fosd.typechef.spllift.analysis

import java.util

import de.fosd.typechef.crewrite.UsedDefinedDeclaredVariables
import de.fosd.typechef.parser.c.{AST, FunctionDef, Id}
import de.fosd.typechef.spllift.CInterCFG
import heros.solver.Pair
import heros.{FlowFunctions, IFDSTabulationProblem}

class Taint(icfg: CInterCFG) extends IFDSTabulationProblem[AST, Pair[Id, AST], FunctionDef, CInterCFG] with UsedDefinedDeclaredVariables {
  override def initialSeeds(): util.Map[AST, util.Set[Pair[Id, AST]]] = ???

  override def interproceduralCFG(): CInterCFG = ???

  override def zeroValue(): Pair[Id, AST] = ???

  override def flowFunctions(): FlowFunctions[AST, Pair[Id, AST], FunctionDef] = ???

  override def computeValues(): Boolean = ???

  override def followReturnsPastSeeds(): Boolean = ???

  override def numThreads(): Int = ???

  override def autoAddZero(): Boolean = ???
}
