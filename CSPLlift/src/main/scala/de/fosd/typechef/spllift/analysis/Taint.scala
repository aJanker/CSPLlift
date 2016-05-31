package de.fosd.typechef.spllift.analysis

import java.io.{File, FileWriter, StringWriter, Writer}
import java.util

import de.fosd.typechef.conditional.{One, Opt}
import de.fosd.typechef.featureexpr.FeatureExpr
import de.fosd.typechef.parser.c._
import de.fosd.typechef.spllift.CInterCFG
import de.fosd.typechef.spllift.ifdsproblem.{InformationFlow, Reach, Source}
import soot.spl.ifds.Constraint

import scala.collection.JavaConverters._

object Taint {

    def allReaches[T](solverResult: List[util.Map[InformationFlow, Constraint[T]]]) = matchReaches[T](solverResult, r => true)

    def findSinks[T](solverResult: List[util.Map[InformationFlow, Constraint[T]]], isSink: Reach => Boolean) = matchReaches[T](solverResult, isSink)

    def allSources[T](solverResult: List[util.Map[InformationFlow, Constraint[T]]]) = matchSources[T, Source](solverResult, s => true)

    def prettyPrintSinks(sinks: List[(AST, List[(Constraint[_], Reach)])]): String = prettyPrintSinks(sinks, new StringWriter).toString

    def prettyPrintSinks(sinks: List[(AST, List[(Constraint[_], Reach)])], writer: Writer): Writer =
        sinks.foldLeft(writer) {
            (writer, sink) => {
                sink._1 match {
                    case f: ForStatement => writer.append("Sink at:\t" + PrettyPrinter.print(f.copy(s = One(EmptyStatement()))) + "\tin:\t" + sink._1.getPositionFrom + "\n")
                    case x => writer.append("Sink at:\t" + PrettyPrinter.print(x) + "\tin:\t" + sink._1.getPositionFrom + "\n")
                }

                sink._2.foreach { ssink => writer.append("CFGcondition " + ssink._1 + ":\t" + ssink._2.toText + "\n") }
                writer.append("\n")
            }
        }

    def writeGraphs(icfg: CInterCFG, sinks: List[(AST, List[(Constraint[_], Reach)])], outputDir: String, extension: String) = {
        val dir = new File(outputDir)
        if (!dir.exists()) dir.mkdirs()

        /**
          * Retrieves the original presence condition in the ast, not the dataflow condition
          */
        def getNodeWithASTFeature(x: Opt[AST]): Node =
            icfg.findEnv(x.entry) match  {
                case Some(env) => Node(Opt(env.featureExpr(x.entry), x.entry))
                case _=> Node(x)
            }

        def getEdge(f: Opt[AST], t: Opt[AST]): Edge =
            Edge(getNodeWithASTFeature(f), getNodeWithASTFeature(t), f.condition.and(t.condition))

        sinks.par.zipWithIndex.foreach {
            case (sink, i) =>
                val writer = new InformationFlowGraphWriter(new FileWriter(new File(outputDir + "/" + i + extension)))

                val header = sink._1 match {
                    case f: ForStatement => "Sink at:\t" + PrettyPrinter.print(f.copy(s = One(EmptyStatement()))) + "\tin:\t" + sink._1.getPositionFrom
                    case x => "Sink at:\t" + PrettyPrinter.print(x) + "\tin:\t" + sink._1.getPositionFrom
                }

                writer.writeHeader("")

                // write nodes first
                val allUniqueSourceNodes = sink._2.flatMap(reach => (reach._2.to :: reach._2.from)).map(getNodeWithASTFeature).distinct

                allUniqueSourceNodes.foreach(writer.writeNode)

                val allUniqueDataFlowEdges = sink._2.flatMap(reach => {
                    val from = reach._2.from.reverse

                    from.foldLeft((from, List[Edge]()))((x, curr) => {
                        val (r, edges) = x
                        val remaining = r.tail
                        val edge = getEdge(curr, remaining.headOption.getOrElse(reach._2.to))
                        (remaining, edge :: edges)
                    })._2
                }).distinct

                allUniqueDataFlowEdges.foreach(e => writer.writeEdge(e.from, e.to, e.condition))

                writer.writeFooter()
                writer.close()
        }
    }


    private def matchSources[T, S <: Source](solverResult: List[util.Map[InformationFlow, Constraint[T]]], isMatch: S => Boolean)(implicit m: Manifest[S]): List[(Opt[Id], List[(Constraint[T], S)])] =
        solverResult.foldLeft(Map[Opt[Id], List[(Constraint[T], S)]]()) {
            (m, result) => result.asScala.foldLeft(m) {
                case (lm, x@(s: S, c: Constraint[T])) if isMatch(s) =>
                    val key = s.name
                    val swap = x.asInstanceOf[(S, Constraint[T])].swap
                    if (lm.contains(key)) lm + (key -> (swap :: lm.get(key).get)) else lm + (key -> List(swap))
                case (lm, _) => lm
            }
        }.toList


    private def matchReaches[T](solverResult: List[util.Map[InformationFlow, Constraint[T]]], isMatch: Reach => Boolean): List[(AST, List[(Constraint[T], Reach)])] =
        solverResult.foldLeft(Map[AST, List[(Constraint[T], Reach)]]()) {
            (m, result) => result.asScala.foldLeft(m) {
                case (lm, x@(r: Reach, c: Constraint[T])) if isMatch(r) =>
                    val key = r.to.entry
                    val swap = x.asInstanceOf[(Reach, Constraint[T])].swap
                    if (lm.contains(key)) lm + (key -> (swap :: lm.get(key).get).distinct) else lm + (key -> List(swap).distinct)
                case (lm, _) => lm
            }
        }.toList

}

case class Node(value : Opt[AST])  {
    override def equals(that: Any) = that match {
        case that: Node => (that canEqual this) && (value equals that.value) && value.entry.getPositionFrom.equals(that.value.entry.getPositionFrom)
        case _ => false
    }
    override def canEqual(that: Any) = that.isInstanceOf[Node]
    override def hashCode = value.hashCode + value.entry.getPositionFrom.hashCode
}

case class Edge(from: Node, to: Node, condition: FeatureExpr) {
    override def equals(that: Any) = that match {
        case that: Edge => (that canEqual this) && ((this.from equals  that.from) && (this.to equals  that.to) && this.condition == that.condition)
        case _ => false
    }
    override def canEqual(that: Any) = that.isInstanceOf[Edge]
    override def hashCode = from.hashCode + to.hashCode + condition.hashCode
}
