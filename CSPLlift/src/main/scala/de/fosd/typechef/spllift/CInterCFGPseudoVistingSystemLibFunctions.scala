package de.fosd.typechef.spllift

import java.util

import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.featureexpr.bdd.True
import de.fosd.typechef.parser.c._
import de.fosd.typechef.spllift.ifdsproblem._
import heros.FlowFunction

trait CInterCFGPseudoVistingSystemLibFunctions extends InformationFlowProblemOperations with ASTHelper {

    lazy val PSEUDO_SYSTEM_FUNCTION_CALL = Opt(True, FunctionDef(List(Opt(True, VoidSpecifier())), AtomicNamedDeclarator(List(), Id(PSEUDO_SYSTEM_FUNCTION_CALL_NAME), List(Opt(True, DeclIdentifierList(List())))), List(), CompoundStatement(List(Opt(True, ReturnStatement(None))))))

    val PSEUDO_SYSTEM_FUNCTION_CALL_NAME = "PSEUDO_SYSTEM_FUNCTION_CALL"

    def pseudoSystemFunctionCallCallFlowFunction(callStmt: AST, callEnv: ASTEnv, interproceduralCFG: CInterCFG): FlowFunction[InformationFlow] with Object {def computeTargets(flowFact: InformationFlow): util.Set[InformationFlow]} = {
        val fCallOpt = parentOpt(callStmt, callEnv).asInstanceOf[Opt[AST]]
        val fCall = filterAllASTElems[FunctionCall](callStmt, callEnv).head // TODO Check if != 1
        val callExprs = fCall.params
        val callUses = uses(callExprs)

        def isSatisfiable_(inner: Opt[Id], outer: Opt[Id]): Boolean = inner.entry.name.equalsIgnoreCase(outer.entry.name) && inner.condition.and(outer.condition).isSatisfiable(interproceduralCFG.getFeatureModel)
        def isSatisfiable(inner: Id, outer: Opt[Id]): Boolean = isSatisfiable_(Opt(callEnv.featureExpr(inner), inner), outer)

        def useIsSatisfiable(x: Opt[Id]): Boolean = callUses.exists(isSatisfiable(_, x))

        new FlowFunction[InformationFlow] {
            override def computeTargets(flowFact: InformationFlow): util.Set[InformationFlow] =
                flowFact match {
                    case s@VarSource(sId, _, _, global) if useIsSatisfiable(sId) =>
                        val r = Reach(fCallOpt, s.name :: s.reachingSources.toList.map(_.name), List(s))
                        if (global.isDefined) GEN(List(r, s)) else GEN(r)
                    case s : Source if s.globalFile.isDefined => GEN(s)
                    case _ => KILL
                }
        }
    }

    def pseudoSystemFunctionCallReturnFlow: FlowFunction[InformationFlow] with Object {def computeTargets(flowFact: InformationFlow): util.Set[InformationFlow]} =
        new FlowFunction[InformationFlow] {
            override def computeTargets(flowFact: InformationFlow): util.Set[InformationFlow] =
                flowFact match {
                    case r: Reach => GEN(r)
                    case s : Source if s.globalFile.isDefined => GEN(s)
                    case _ => KILL
                }
        }

}
