package de.fosd.typechef.spllift.analysis

import java.util

import de.fosd.typechef.parser.c.AST
import de.fosd.typechef.spllift.ifdsproblem.{InformationFlow, Reach}
import soot.spl.ifds.Constraint

import scala.collection.JavaConverters._

object Taint {

    def allReaches[T](solverResult: List[util.Map[InformationFlow, Constraint[T]]]): List[(AST, List[(Constraint[T], Reach)])] = matchReaches[T](solverResult, r => true)
    def allSources() = ???

    def findSinks[T](solverResult: List[util.Map[InformationFlow, Constraint[T]]], isSink: Reach => Boolean): List[(AST, List[(Constraint[_], Reach)])] = matchReaches[T](solverResult, isSink)

    private def matchReaches[T](solverResult: List[util.Map[InformationFlow, Constraint[T]]], isMatch: Reach => Boolean): List[(AST, List[(Constraint[T], Reach)])] =
        solverResult.foldLeft(Map[AST, List[(Constraint[T], Reach)]]()) {
            case (m, result) => result.asScala.foldLeft(m) {
                case (lm, x@(r : Reach, c: Constraint[T])) if isMatch(r) =>
                    val key = r.to.entry
                    val swap = x.asInstanceOf[(Reach, Constraint[T])].swap
                    if (lm.contains(key)) lm + (key -> (swap :: lm.get(key).get)) else lm + (key -> List(swap))
                case (lm, _) => lm
            }
        }.toList

}
