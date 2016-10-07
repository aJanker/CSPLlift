package de.fosd.typechef.cspllift.analysis

import java.io.{StringWriter, Writer}

import de.fosd.typechef.conditional.{One, Opt}
import de.fosd.typechef.cspllift.LiftedCFlowFact
import de.fosd.typechef.cspllift.cifdsproblem.informationflow.flowfact.InformationFlowFact
import de.fosd.typechef.cspllift.cifdsproblem.informationflow.flowfact.sinkorsource.{Sink, SinkOrSource, Source}
import de.fosd.typechef.parser.c.{AST, EmptyStatement, ForStatement, PrettyPrinter}
import spllift.Constraint

object Taint {

    def allSinks(solverResult: List[LiftedCFlowFact[InformationFlowFact]]) = filter[Sink](solverResult, r => true)

    def findSinks(solverResult: List[LiftedCFlowFact[InformationFlowFact]], isSink: Sink => Boolean) = filter[Sink](solverResult, isSink)

    def allSources(solverResult: List[LiftedCFlowFact[InformationFlowFact]]) = filter[Source](solverResult, s => true)

    def prettyPrintSinks(sinks: Traversable[(Opt[AST], List[LiftedCFlowFact[Sink]])]): String = prettyPrintSinks(sinks, new StringWriter).toString
    def prettyPrintSinks(sinks: Traversable[(Opt[AST], List[LiftedCFlowFact[Sink]])], writer: Writer): Writer =
        sinks.foldLeft(writer) {
            (writer, sink) => {
                sink._1.entry match {
                    case f: ForStatement => writer.append("Sink at:\t" + PrettyPrinter.print(f.copy(s = One(EmptyStatement()))) + "\tin:\t" + sink._1.entry.getPositionFrom + "\n")
                    case x => writer.append("Sink at:\t" + PrettyPrinter.print(x) + "\tin:\t" + sink._1.entry.getPositionFrom + "\n")
                }

                sink._2.foreach { ssink => writer.append("\tCFGcondition: " + ssink._2 + "\n" +
                  "\tCFGcondition (simplified): " + ssink._2.simplify() + "\n" +
                  ssink._1.toText + "\n") }
                writer.append("\n")
            }
        }

    private def filter[F <: SinkOrSource](solverResult: List[(InformationFlowFact, Constraint)], isMatch: (F) => Boolean)(implicit m: Manifest[F]): Traversable[(Opt[AST], List[LiftedCFlowFact[F]])] =
        solverResult.foldLeft(Map[Opt[AST], List[LiftedCFlowFact[F]]]()) {
            case (map, solution@(s: F, c: Constraint)) if isMatch(s) =>
                val key = s.stmt
                // remove duplicate source sinks
                val value = solution.asInstanceOf[LiftedCFlowFact[F]]
                map + (key -> (value :: map.getOrElse(key, List())).distinct)
            case (map, _ ) => map
        }
}
