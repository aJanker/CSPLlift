package de.fosd.typechef.cspllift.analysis

import java.io.{StringWriter, Writer}

import de.fosd.typechef.conditional.One
import de.fosd.typechef.cspllift.cifdsproblem.informationflow.flowfact.InformationFlowFact
import de.fosd.typechef.cspllift.cifdsproblem.informationflow.flowfact.sinkorsource.{Sink, SinkOrSource, Source}
import de.fosd.typechef.cspllift.cintercfg.CInterCFGNode
import de.fosd.typechef.cspllift.{LiftedCFlowFact, StmtFlowFacts}
import de.fosd.typechef.featureexpr.FeatureExpr
import de.fosd.typechef.parser.c.{EmptyStatement, ForStatement, PrettyPrinter}

import scala.collection.GenTraversable

/**
  * Information-Flow Analysis Filter: retrieves interesting Information-Facts from the solver result set.
  */
object InformationFlow {

    /**
      * Retrieves all found sinks from the solver result
      */
    def allSinks(solverResult: List[LiftedCFlowFact[InformationFlowFact]]): Traversable[(CInterCFGNode, List[(Sink, FeatureExpr)])] = filter[Sink](solverResult, r => true)

    /**
      * Retrieves all found sinks from the solver result which fulfill a defined property.
      */
    def findSinks(solverResult: List[LiftedCFlowFact[InformationFlowFact]], isSink: Sink => Boolean): Traversable[(CInterCFGNode, List[(Sink, FeatureExpr)])] = filter[Sink](solverResult, isSink)

    /**
      * Retrieves all found source facts from the solver result
      */
    def allSources(solverResult: List[LiftedCFlowFact[InformationFlowFact]]): Traversable[(CInterCFGNode, List[(Source, FeatureExpr)])] = filter[Source](solverResult, s => true)

    /**
      * Pretty Printing of Sink Facts.
      */
    def prettyPrintSinks(sinks: GenTraversable[StmtFlowFacts[Sink]]): String = prettyPrintSinks(sinks, new StringWriter).toString
    def prettyPrintSinks(sinks: GenTraversable[StmtFlowFacts[Sink]], writer: Writer): Writer =
        sinks.foldLeft(writer) {
            (writer, sink) => {
                sink._1.get match {
                    case f: ForStatement => writer.append("Sink at:\t" + PrettyPrinter.print(f.copy(s = One(EmptyStatement()))) + "\tin:\t" + sink._1.get.getPositionFrom + "\n")
                    case x => writer.append("Sink at:\t" + PrettyPrinter.print(x) + "\tin:\t" + sink._1.get.getPositionFrom + "\n")
                }

                sink._2.foreach { ssink => writer.append("\tCFGcondition: " + ssink._2.toTextExpr + "\n" + ssink._1.toText + "\n") }
                writer.append("\n")
            }
        }

    private def filter[F <: SinkOrSource](solverResult: List[(InformationFlowFact, FeatureExpr)], isMatch: (F) => Boolean)(implicit m: Manifest[F]): Traversable[StmtFlowFacts[F]] =
        solverResult.foldLeft(Map[CInterCFGNode, List[LiftedCFlowFact[F]]]()) {
            case (map, solution@(s: F, c: FeatureExpr)) if isMatch(s) =>
                val key = s.cfgNode
                // remove duplicate source sinks
                val value = solution.asInstanceOf[LiftedCFlowFact[F]]
                map + (key -> (value :: map.getOrElse(key, List())).distinct)
            case (map, _ ) => map
        }
}
