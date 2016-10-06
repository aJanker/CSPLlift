package de.fosd.typechef.cspllift.cifdsproblem.informationflow

import de.fosd.typechef.cspllift.cifdsproblem.informationflow.flowfact.{InformationFlowFact, Source, Struct}
import de.fosd.typechef.cspllift.cifdsproblem.{CFlowConstants, CFlowOperations}
import de.fosd.typechef.parser.c.Id

trait InformationFlowProblemOperations extends CFlowConstants with CFlowOperations[InformationFlowFact] {
    def isFullFieldMatch(s: Source, fieldAssignment: (Id, List[Id])): Boolean = {
        def matches(s: Source, parents: List[Id]): Boolean =
            if (parents.isEmpty) s match {
                case s: Source => s.getType match {
                    case Struct(name, Some(_)) => false
                    case _ => fieldAssignment._1.equals(s.getType.getName)
                }
                case _ => false
            }
            else s match {
                case s: Source => s.getType match {
                    case Struct(name, Some(field)) if parents.head.equals(name) => matches(field.get, parents.tail)
                    case _ => false
                }
                case _ => false
            }

        matches(s, fieldAssignment._2.reverse)
    }

    /**
      * Returns true for sources of x.y.z and assignments to x.y
      */
    def isPartFieldMatch(s: Source, fieldAssignment: (Id, List[Id])): Boolean = {
        def matches(s: Source, parents: List[Id]): Boolean =
            if (parents.isEmpty) fieldAssignment._1.equals(s.getType.getName)
            else s match {
                case s: Source => s.getType match {
                    case Struct(name, Some(field)) if parents.head.equals(name) => matches(field.get, parents.tail)
                    case _ => false
                }
                case _ => false
            }
        matches(s, fieldAssignment._2.reverse)
    }
}
