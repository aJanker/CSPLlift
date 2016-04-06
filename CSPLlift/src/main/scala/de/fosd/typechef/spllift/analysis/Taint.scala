package de.fosd.typechef.spllift.analysis

import java.io.{StringWriter, Writer}
import java.util

import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.parser.c.{AST, Id, PrettyPrinter}
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
                writer.append("Sink at:\t" + PrettyPrinter.print(sink._1) + "\tin:\t" + sink._1.getPositionFrom + "\n")
                sink._2.foreach { ssink => writer.append("CFGcondition " + ssink._1 + ":\t" + ssink._2.toText + "\n") }
                writer.append("\n")
            }
        }

    private def matchSources[T, S <: Source](solverResult: List[util.Map[InformationFlow, Constraint[T]]], isMatch: S => Boolean)(implicit m : Manifest[S]): List[(Opt[Id], List[(Constraint[T], S)])] =
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
                    if (lm.contains(key)) lm + (key -> (swap :: lm.get(key).get)) else lm + (key -> List(swap))
                case (lm, _) => lm
            }
        }.toList

}
