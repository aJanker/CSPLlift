package de.fosd.typechef.cspllift.cifdsproblem.informationflow

import java.util

import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.cspllift.CInterCFG
import de.fosd.typechef.cspllift.commons.CInterCFGCommons
import de.fosd.typechef.parser.c._
import heros.FlowFunction

trait InformationFlowPseudoVistingSystemLibFunctions extends InformationFlowProblemOperations with CInterCFGCommons {

    def pseudoSystemFunctionCallCallFlowFunction(callStmt: Opt[AST], callEnv: ASTEnv, interproceduralCFG: CInterCFG): FlowFunction[FlowFact] with Object {def computeTargets(flowFact: FlowFact): util.Set[FlowFact]} = {
        val fCallNode = parentOpt(callStmt.entry, callEnv).asInstanceOf[Opt[AST]]
        val fCall = filterAllASTElems[FunctionCall](callStmt, callEnv).head
        val callExprs = fCall.params
        val callUses = uses(callExprs)
        val callStructUses = usesField(callExprs)

        new FlowFunction[FlowFact] {
            override def computeTargets(flowFact: FlowFact): util.Set[FlowFact] =
                flowFact match {
                    case s: Source => s match {
                        case _: VarSource | StructSource(_, None, _, _, _) | StructSourceOf(_, None, _, _, _, _) if callUses.contains(s.getId) =>
                            val sink = SinkToUse(fCallNode, s)
                            if (s.getScope == SCOPE_GLOBAL) GEN(List(s, sink)) else GEN(sink)
                        case StructSource(_, Some(field), _, _, _) if callStructUses.exists(use => isFullFieldMatch(s, use)) =>
                            val sink = SinkToUse(fCallNode, s)
                            if (s.getScope == SCOPE_GLOBAL) GEN(List(s, sink)) else GEN(sink)
                        case _ :  VarSourceOf | StructSourceOf(_, None, _, _, _, _) if callUses.contains(s.getId) =>
                            val sink = SinkToUse(fCallNode, s.asInstanceOf[SourceOf].getSource)
                            if (s.getScope == SCOPE_GLOBAL) GEN(List(s, sink)) else GEN(sink)
                        case s@StructSourceOf(_, Some(field), _, _, _, _) if callStructUses.exists(use => isFullFieldMatch(s, use)) =>
                            val sink = SinkToUse(fCallNode, s.source)
                            if (s.getScope == SCOPE_GLOBAL) GEN(List(s, sink)) else GEN(sink)
                        case s: Source if s.getScope == SCOPE_GLOBAL => GEN(s)
                        case _ => KILL
                    }
                    case _ => KILL
                }
        }
    }

    def pseudoSystemFunctionCallReturnFlow: FlowFunction[FlowFact] with Object {def computeTargets(flowFact: FlowFact): util.Set[FlowFact]} =
        new FlowFunction[FlowFact] {
            override def computeTargets(flowFact: FlowFact): util.Set[FlowFact] =
                flowFact match {
                    case s: Sink => GEN(s)
                    case s: Source if s.getScope == SCOPE_GLOBAL => GEN(s) // TODO Copy
                    case _ => KILL
                }
        }
}