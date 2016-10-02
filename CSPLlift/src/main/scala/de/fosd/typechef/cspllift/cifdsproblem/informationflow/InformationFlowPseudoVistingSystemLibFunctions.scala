package de.fosd.typechef.cspllift.cifdsproblem.informationflow

import java.util

import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.cspllift.CInterCFG
import de.fosd.typechef.cspllift.cifdsproblem.CFlowOperations
import de.fosd.typechef.cspllift.commons.CInterCFGCommons
import de.fosd.typechef.parser.c._
import heros.FlowFunction

trait InformationFlowPseudoVistingSystemLibFunctions extends CFlowOperations[FlowFact] with CInterCFGCommons {

    def pseudoSystemFunctionCallCallFlowFunction(callStmt: Opt[AST], callEnv: ASTEnv, interproceduralCFG: CInterCFG): FlowFunction[FlowFact] with Object {def computeTargets(flowFact: FlowFact): util.Set[FlowFact]} = {
        val fCallNode = parentOpt(callStmt.entry, callEnv).asInstanceOf[Opt[AST]]
        val fCall = filterAllASTElems[FunctionCall](callStmt, callEnv).head
        val callExprs = fCall.params
        val callUses = uses(callExprs)

        new FlowFunction[FlowFact] {
            override def computeTargets(flowFact: FlowFact): util.Set[FlowFact] =
                flowFact match {
                    case v@VarSource(source, _, scope, _) if callUses.contains(source) =>
                        val sink = SinkToUse(fCallNode, v)
                        if (scope == SCOPE_GLOBAL) GEN(List(v.copy(last = Some(fCallNode.entry)), sink)) else GEN(sink)
                    case vo@VarSourceOf(id, _, source, scope, _) if callUses.contains(id) =>
                        val sink = SinkToUse(fCallNode, source)
                        if (scope == SCOPE_GLOBAL) GEN(List(vo.copy(last = Some(fCallNode.entry)), sink)) else GEN(sink)
                    case s: Source if s.getScope == SCOPE_GLOBAL => GEN(s) // TODO Copy
                    case z: Zero => GEN(z)
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