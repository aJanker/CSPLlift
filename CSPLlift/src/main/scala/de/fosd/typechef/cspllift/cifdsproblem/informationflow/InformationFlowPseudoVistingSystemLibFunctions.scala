package de.fosd.typechef.cspllift.cifdsproblem.informationflow

import java.util

import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.cspllift.CInterCFG
import de.fosd.typechef.cspllift.cifdsproblem.CFlowOperations
import de.fosd.typechef.cspllift.commons.CInterCFGCommons
import de.fosd.typechef.parser.c._
import heros.FlowFunction

trait InformationFlowPseudoVistingSystemLibFunctions extends CFlowOperations[InformationFlow2] with CInterCFGCommons {

    def pseudoSystemFunctionCallCallFlowFunction(callStmt: Opt[AST], callEnv: ASTEnv, interproceduralCFG: CInterCFG): FlowFunction[InformationFlow2] with Object {def computeTargets(flowFact: InformationFlow2): util.Set[InformationFlow2]} = {
        val fCallNode = parentOpt(callStmt.entry, callEnv).asInstanceOf[Opt[AST]]
        val fCall = filterAllASTElems[FunctionCall](callStmt, callEnv).head
        val callExprs = fCall.params
        val callUses = uses(callExprs)

        new FlowFunction[InformationFlow2] {
            override def computeTargets(flowFact: InformationFlow2): util.Set[InformationFlow2] =
                flowFact match {
                    case v@VarSource(source, _, scope, _) if callUses.contains(source) =>
                        val sink = SinkToUse(fCallNode, v)
                        if (scope == SCOPE_GLOBAL) GEN(List(v.copy(last = Some(fCallNode.entry)), sink)) else GEN(sink)
                    case vo@VarSourceOf(id, _, source, scope) if callUses.contains(id) =>
                        val sink = SinkToUse(fCallNode, source)
                        if (scope == SCOPE_GLOBAL) GEN(List(vo, sink)) else GEN(sink)
                    case s: Source if s.getScope == SCOPE_GLOBAL => GEN(s)
                    case _ => KILL
                }
        }
    }

    def pseudoSystemFunctionCallReturnFlow: FlowFunction[InformationFlow2] with Object {def computeTargets(flowFact: InformationFlow2): util.Set[InformationFlow2]} =
        new FlowFunction[InformationFlow2] {
            override def computeTargets(flowFact: InformationFlow2): util.Set[InformationFlow2] =
                flowFact match {
                    case s: Sink => GEN(s)
                    case s: Source if s.getScope == SCOPE_GLOBAL => GEN(s)
                    case _ => KILL
                }
        }
}