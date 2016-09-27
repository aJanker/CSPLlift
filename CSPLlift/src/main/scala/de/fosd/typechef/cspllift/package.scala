package de.fosd.typechef

import java.util

import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.cspllift.cifdsproblem.{CFlowFact, CIFDSProblem}
import de.fosd.typechef.parser.c.{AST, FunctionDef}
import heros.IFDSTabulationProblem
import soot.spl.ifds.Constraint

import scala.collection.JavaConverters._


package object cspllift {

    type IFDSProblem[D <: CFlowFact] = IFDSTabulationProblem[Opt[AST], D, Opt[FunctionDef], CInterCFG]

    def getCIFDSProblemInstance[D <: CFlowFact, T <: CIFDSProblem[D]](clazz: java.lang.Class[T])(args: AnyRef*): T = clazz.getConstructors()(0).newInstance(args: _*).asInstanceOf[T]

    type LiftedCFlowFact[D <: CFlowFact] = (D, Constraint)

    def liftedFlowFactsAsScala[D <: CFlowFact](javaFacts: util.List[util.Map[D, Constraint]]): List[LiftedCFlowFact[D]] = javaFacts.asScala.flatMap(_.asScala).map {case (fact, constraint) => (fact.get, constraint)}.toList.distinct.asInstanceOf[List[LiftedCFlowFact[D]]]

}
