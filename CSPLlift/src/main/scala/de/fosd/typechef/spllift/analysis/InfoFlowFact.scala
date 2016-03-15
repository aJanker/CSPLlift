package de.fosd.typechef.spllift.analysis

import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.parser.c.{AST, Id}

import scala.collection.mutable.ListBuffer


sealed trait InfoFlowFact

case object Zero extends InfoFlowFact

case class Source (name: Opt[Id], stmt: Opt[_], reachingSources: ListBuffer[Source] = ListBuffer(), globalFile : Option[String] = None) extends InfoFlowFact {
    override def hashCode() = name.hashCode + stmt.hashCode() + globalFile.hashCode()
}

case class Reach(to: Opt[AST], from: List[Opt[Id]], sources: List[Source]) extends InfoFlowFact

