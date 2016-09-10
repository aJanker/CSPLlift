package de.fosd.typechef

import java.util

import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.parser.c.{AST, FunctionDef}
import de.fosd.typechef.spllift.cifdsproblem.{CFlowFact, CIFDSProblem}
import heros.IFDSTabulationProblem
import soot.spl.ifds.Constraint

import scala.collection.JavaConverters._



package object spllift {

    type IFDSProblem[D <: CFlowFact] = IFDSTabulationProblem[Opt[AST], D, Opt[FunctionDef], CInterCFG]

    def getCIFDSProblemInstance[D <: CFlowFact, T <: CIFDSProblem[D]](clazz: java.lang.Class[T])(args:AnyRef*): T = clazz.getConstructors()(0).newInstance(args:_*).asInstanceOf[T]

    type LiftedCFlowFact[D] = (D, Constraint)

    def asScalaLiftedFlowFact[D](javaFacts: util.List[util.Map[D, Constraint]]): List[LiftedCFlowFact[D]] = javaFacts.asScala.toList.flatMap(_.asScala.toList)

}
