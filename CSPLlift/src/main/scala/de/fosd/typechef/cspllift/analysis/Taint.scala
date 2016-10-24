package de.fosd.typechef.cspllift.analysis

import java.io.{StringWriter, Writer}

import de.fosd.typechef.conditional.One
import de.fosd.typechef.cspllift.cifdsproblem.informationflow.flowfact.InformationFlowFact
import de.fosd.typechef.cspllift.cifdsproblem.informationflow.flowfact.sinkorsource.{Sink, SinkOrSource, Source}
import de.fosd.typechef.cspllift.{CICFGStmt, LiftedCFlowFact}
import de.fosd.typechef.featureexpr.FeatureExpr
import de.fosd.typechef.parser.c.{EmptyStatement, ForStatement, PrettyPrinter}

object Taint {

    def allSinks(solverResult: List[LiftedCFlowFact[InformationFlowFact]]) = filter[Sink](solverResult, r => true)

    def findSinks(solverResult: List[LiftedCFlowFact[InformationFlowFact]], isSink: Sink => Boolean) = filter[Sink](solverResult, isSink)

    def allSources(solverResult: List[LiftedCFlowFact[InformationFlowFact]]) = filter[Source](solverResult, s => true)

    def prettyPrintSinks(sinks: Traversable[(CICFGStmt, List[LiftedCFlowFact[Sink]])]): String = prettyPrintSinks(sinks, new StringWriter).toString
    def prettyPrintSinks(sinks: Traversable[(CICFGStmt, List[LiftedCFlowFact[Sink]])], writer: Writer): Writer =
        sinks.foldLeft(writer) {
            (writer, sink) => {
                sink._1.getStmt.entry match {
                    case f: ForStatement => writer.append("Sink at:\t" + PrettyPrinter.print(f.copy(s = One(EmptyStatement()))) + "\tin:\t" + sink._1.getStmt.entry.getPositionFrom + "\n")
                    case x => writer.append("Sink at:\t" + PrettyPrinter.print(x) + "\tin:\t" + sink._1.getStmt.entry.getPositionFrom + "\n")
                }

                sink._2.foreach { ssink => writer.append("\tCFGcondition: " + ssink._2.toTextExpr + "\n" +
                  /*"\tCFGcondition (simplified): " + ssink._2.simplify() + "\n" + */
                  ssink._1.toText + "\n") }
                writer.append("\n")
            }
        }

    private def filter[F <: SinkOrSource](solverResult: List[(InformationFlowFact, FeatureExpr)], isMatch: (F) => Boolean)(implicit m: Manifest[F]): Traversable[(CICFGStmt, List[LiftedCFlowFact[F]])] =
        solverResult.foldLeft(Map[CICFGStmt, List[LiftedCFlowFact[F]]]()) {
            case (map, solution@(s: F, c: FeatureExpr)) if isMatch(s) =>
                val key = s.cICFGStmt
                // remove duplicate source sinks
                val value = solution.asInstanceOf[LiftedCFlowFact[F]]
                map + (key -> (value :: map.getOrElse(key, List())).distinct)
            case (map, _ ) => map
        }
}
