package de.fosd.typechef.spllift.analysis

import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.parser.c.Id


sealed trait InfoFlowFact

case object Zero extends InfoFlowFact

case class Source(id: List[Opt[Id]] /*, value : String  type */) extends InfoFlowFact

case class Sink(sources: List[Opt[Id]]) extends InfoFlowFact
