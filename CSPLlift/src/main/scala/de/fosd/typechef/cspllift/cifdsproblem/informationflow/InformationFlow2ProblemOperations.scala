package de.fosd.typechef.cspllift.cifdsproblem.informationflow

import de.fosd.typechef.cspllift.cifdsproblem.{CFlowConstants, CFlowOperations}
import de.fosd.typechef.parser.c.Id

/**
  * Created by andi on 29/09/16.
  */
// Global TODO: Documentation
/*
trait InformationFlow2ProblemOperations extends CFlowConstants with CFlowOperations[InformationFlow2] {

    def isFullFieldMatch(s: Source, fieldAssignment: (Id, List[Id])): Boolean = {
        def matches(s: Source, parents: List[Id]): Boolean =
            if (parents.isEmpty)
                s match {
                    case StructSource(_, Some(_), _, _, _) | StructSourceOf(_, Some(_), _, _, _, _) => false
                    case _ => fieldAssignment._1.equals(s.getId)

                }
            else
                s match {
                    case StructSource(_, Some(field), _, _, _) if parents.head.equals(s.getId) => matches(field.get, parents.tail)
                    case StructSourceOf(_, Some(field), _, _, _, _) if parents.head.equals(s.getId) => matches(field.get, parents.tail)
                    case _ => false
                }

        matches(s, fieldAssignment._2.reverse)
    }

    /**
      * Returns true for sources of x.y.z and assignments to x.y
      */
    def isPartFieldMatch(s: Source, fieldAssignment: (Id, List[Id])): Boolean = {
        def matches(s: Source, parents: List[Id]): Boolean =
            if (parents.isEmpty) fieldAssignment._1.equals(s.getId)
            else s match {
                case StructSource(_, Some(field), _, _, _) if parents.head.equals(s.getId) => matches(field.get, parents.tail)
                case StructSourceOf(_, Some(field), _, _, _, _) if parents.head.equals(s.getId) => matches(field.get, parents.tail)
                case _ => false
            }

        matches(s, fieldAssignment._2.reverse)
    }

}
*/