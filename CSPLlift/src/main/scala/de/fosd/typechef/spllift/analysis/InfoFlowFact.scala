package de.fosd.typechef.spllift.analysis

import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.parser.c.Id


sealed trait InfoFlowFact

case object Zero extends InfoFlowFact

case class Source(name: Opt[Id], stmt: Opt[_], reachingSources: List[Source] = List(), globalFile : Option[String] = None) extends InfoFlowFact

case class Reach(to: Opt[_], from: List[Opt[Id]], sources: List[Source]) extends InfoFlowFact
