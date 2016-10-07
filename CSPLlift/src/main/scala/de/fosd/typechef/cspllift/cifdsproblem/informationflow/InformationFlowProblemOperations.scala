package de.fosd.typechef.cspllift.cifdsproblem.informationflow

import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.cspllift.cifdsproblem.informationflow.flowfact._
import de.fosd.typechef.cspllift.cifdsproblem.informationflow.flowfact.sinkorsource.{Source, SourceDefinition, SourceDefinitionOf, Struct}
import de.fosd.typechef.cspllift.cifdsproblem.{CFlowConstants, CFlowOperations}
import de.fosd.typechef.parser.c.{AST, Id}

trait InformationFlowProblemOperations extends CFlowConstants with CFlowOperations[InformationFlowFact] {

    def copySource(s: Source, previousStmt: Opt[AST]): Source =
        s match {
            case s: SourceDefinition => s.copy(previousStmt = Some(previousStmt.entry))
            case sOf: SourceDefinitionOf => sOf.copy(previousStmt = Some(previousStmt.entry))
            case _ => s
        }

    def getSourceDefinition(s: Source): SourceDefinition =
        s match {
            case sd: SourceDefinition => sd
            case so: SourceDefinitionOf => so.getDefinition
        }


    def isFullFieldMatch(s: Source, fieldAssignment: (Id, List[Id])): Boolean = {
        def matches(s: Source, parents: List[Id]): Boolean =
            if (parents.isEmpty) s match {
                case s: Source => s.getType match {
                    case Struct(name, Some(_)) => false
                    case _ => fieldAssignment._1.equals(s.getType.getName)
                }
                case _ => false
            } else s match {
                case s: Source => s.getType match {
                    case Struct(name, Some(field)) if parents.head.equals(name) => matches(field.get, parents.tail)
                    case _ => false
                }
                case _ => false
            }

        matches(s, fieldAssignment._2)
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
        matches(s, fieldAssignment._2)
    }
}
