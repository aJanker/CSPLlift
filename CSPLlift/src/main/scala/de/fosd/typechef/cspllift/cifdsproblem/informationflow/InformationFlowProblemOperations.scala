package de.fosd.typechef.cspllift.cifdsproblem.informationflow

import de.fosd.typechef.cspllift.cifdsproblem.informationflow.flowfact._
import de.fosd.typechef.cspllift.cifdsproblem.informationflow.flowfact.sinkorsource.{Source, SourceDefinition, SourceDefinitionOf, Struct}
import de.fosd.typechef.cspllift.cifdsproblem.{CFlowConstants, CFlowOperations}
import de.fosd.typechef.cspllift.commons.CInterCFGCommons
import de.fosd.typechef.parser.c.{ASTEnv, ArrayAccess, Id}
import de.fosd.typechef.typesystem._

trait InformationFlowProblemOperations extends CFlowConstants with CFlowOperations[InformationFlowFact] with InformationFlowHelper

trait InformationFlowHelper extends CInterCFGCommons {
    def getDefinition(s: Source): SourceDefinition =
        s match {
            case sd: SourceDefinition => sd
            case so: SourceDefinitionOf => so.getDefinition
        }

    def isOnlyUsedAsArrayAccess(id: Id, uses: List[Id], env: ASTEnv): Boolean = {
        val matchingUses = uses.filter(id.equals)
        if (matchingUses.isEmpty) false
        else !matchingUses.exists(findPriorASTElem[ArrayAccess](_, env).isEmpty)
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
                    case Struct(name, Some(field)) if parents.head.equals(name) => matches(field, parents.tail)
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
                    case Struct(name, Some(field)) if parents.head.equals(name) => matches(field, parents.tail)
                    case _ => false
                }
                case _ => false
            }
        matches(s, fieldAssignment._2)
    }

    def isUnknownType(cType: CType): Boolean =
        cType.atype match {
            case _: CUnknown => true
            case _ => false
        }

    def isStructOrUnion(cType: CType): Boolean =
        cType.atype match {
            case CPointer(t) => isStructOrUnion(t) // simple pointer detection - really, really cheap coding
            case _: CStruct | _: CAnonymousStruct => true
            case _ => false
        }
}
