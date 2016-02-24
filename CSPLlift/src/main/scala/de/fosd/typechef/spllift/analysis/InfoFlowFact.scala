package de.fosd.typechef.spllift.analysis

import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.parser.c.{AST, Id}


sealed trait InfoFlowFact

case object Zero extends InfoFlowFact

case class Source(name: Opt[Id], stmt: Opt[_], linkedSources: List[Source] = List()) extends InfoFlowFact

case class Sink(id: Opt[Id], stmt: AST, sources: List[Source]) extends InfoFlowFact
