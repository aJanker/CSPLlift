package de.fosd.typechef.cspllift.cifdsproblem.informationflow

import java.util

import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.cspllift.cifdsproblem.informationflow.flowfact._
import de.fosd.typechef.cspllift.cifdsproblem.informationflow.flowfact.sinkorsource._
import de.fosd.typechef.cspllift.cintercfg.{CICFGNode, CInterCFG}
import de.fosd.typechef.cspllift.commons.CInterCFGCommons
import de.fosd.typechef.parser.c._
import heros.FlowFunction

trait InformationFlowPseudoVistingSystemLibFunctions extends InformationFlowProblemOperationsInformation with CInterCFGCommons {

    def pseudoSystemFunctionCallCallFlowFunction(callStmt: CICFGNode, callEnv: ASTEnv, interproceduralCFG: CInterCFG): FlowFunction[InformationFlowFact] with Object {def computeTargets(flowFact: InformationFlowFact): util.Set[InformationFlowFact]} = {
        val fCallNode = parentOpt(callStmt.getStmt.entry, callEnv).asInstanceOf[Opt[AST]]
        val fCall = filterAllASTElems[FunctionCall](callStmt, callEnv).head
        val callExprs = fCall.params
        val callUses = uses(callExprs)
        val callStructUses = usesField(callExprs)

        new FlowFunction[InformationFlowFact] {
            override def computeTargets(flowFact: InformationFlowFact): util.Set[InformationFlowFact] = {
                val use = flowFact match {
                    case s: Source if s.getType.isInstanceOf[Variable] && callUses.contains(s.getType.getName) =>
                        GEN(SinkToUse(callStmt, getDefinition(s)))
                    case s: Source if s.getType.isInstanceOf[Struct] && callStructUses.exists(field => isFullFieldMatch(s, field)) =>
                        GEN(SinkToUse(callStmt, getDefinition(s)))
                    case s: Source if s.getType.isInstanceOf[Struct] && callStructUses.isEmpty && s.getType.asInstanceOf[Struct].field.isEmpty && callUses.contains(s.getType.getName) =>
                        GEN(SinkToUse(callStmt, getDefinition(s)))
                    case _ => KILL
                }

                val global = flowFact match {
                    case s: Source if s.getScope == SCOPE_GLOBAL => GEN(s)
                    case s: Sink => KILL
                    case _ => KILL
                }
                GEN(global, use)
            }
        }
    }

    def pseudoSystemFunctionCallReturnFlow: FlowFunction[InformationFlowFact] with Object {def computeTargets(flowFact: InformationFlowFact): util.Set[InformationFlowFact]} =
        new FlowFunction[InformationFlowFact] {
            override def computeTargets(flowFact: InformationFlowFact): util.Set[InformationFlowFact] =
                flowFact match {
                    case s: Sink => GEN(s)
                    case s: Source if s.getScope == SCOPE_GLOBAL => GEN(s)
                    case _ => KILL
                }
        }
}