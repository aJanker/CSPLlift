package de.fosd.typechef.cspllift.cifdsproblem.informationflow.flowfunction

import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.cspllift.cintercfg._
import de.fosd.typechef.parser.c._
import de.fosd.typechef.typesystem.{CDeclUse, CTypeCache, CTypeSystemFrontend}
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.JavaConverters._

/**
  * Adds additional methods for call and callee parameter matching requiered at call-flow edges.
  */
abstract class IFCallFlowFunction(interproceduralCFG: CInterCFG, callStmt: CInterCFGNode, destinationMethod: CInterCFGFDef) extends IFDefaultFlowFunction(interproceduralCFG, callStmt, destinationMethod) {
    private lazy val logger: Logger = LoggerFactory.getLogger(getClass)

    private[informationflow] lazy val destinationStmt: CInterCFGNode = interproceduralCFG.getSuccsOf(destinationMethod).asScala.headOption.getOrElse(destinationMethod)
    private[informationflow] lazy val destinationTS: CTypeSystemFrontend with CTypeCache with CDeclUse = interproceduralCFG.getTS(destinationStmt)
    private[informationflow] lazy val destinationVarEnv = destinationTS.lookupEnv(destinationStmt.get)
    private[informationflow] lazy val fCallParamsToFDefParams: List[(List[Opt[Expr]], List[Opt[ParameterDeclarationD]])] = matchCallParamsToDefParams(fCallExprs, calleeParams)
    private lazy val fCall = filterASTElems[FunctionCall](callStmt.get).head
    private lazy val fCallExprs = fCall.params.exprs
    private lazy val calleeParams = getCalleeParameters(destinationMethod.method)

    /**
      * Map corresponding call parameters to their equivalent callee parameter.
      */
    def mapCallParamToCalleeParam(callParams: List[Opt[Expr]], calleeParams: List[Opt[ParameterDeclarationD]], res: List[Opt[(Id, Id)]] = List()): List[Opt[(Id, Id)]] = {
        if (callParams.isEmpty && calleeParams.isEmpty) return res

        val currentCallParameter = callParams.head
        val currentCalleeParameter = calleeParams.head
        val currentParameterMatchCondition = currentCallParameter.condition.and(currentCalleeParameter.condition)

        val currRes =
            if (currentParameterMatchCondition.isSatisfiable(interproceduralCFG.getFeatureModel))
                currentCallParameter.entry match {
                    case i: Id => Option(Opt(currentParameterMatchCondition, (i, currentCalleeParameter.entry.decl.getId)))
                    case PointerCreationExpr(i: Id) => Option(Opt(currentParameterMatchCondition, (i, currentCalleeParameter.entry.decl.getId)))
                    case PointerDerefExpr(i: Id) => Option(Opt(currentParameterMatchCondition, (i, currentCalleeParameter.entry.decl.getId)))
                    case c: Constant => Option(Opt(currentParameterMatchCondition, (Id("constant"), currentCalleeParameter.entry.decl.getId)))
                    case s: SizeOfExprU => Option(Opt(currentParameterMatchCondition, (Id("sizeU"), currentCalleeParameter.entry.decl.getId)))
                    case missed => throw new IllegalArgumentException("No rule defined for converting expression to parameter mapping: " + missed + "\n" + callStmt)
                }
            else None

        if (currRes.isDefined)
            mapCallParamToCalleeParam(callParams.tail, calleeParams.tail, currRes.get :: res)
        else if (callParams.size < calleeParams.size)
            mapCallParamToCalleeParam(callParams, calleeParams.tail, res)
        else
            mapCallParamToCalleeParam(callParams.tail, calleeParams, res)
    }

    /**
      * Matcher strategy to determin call and callee parameter pairs.
      */
    def matchCallParamsToDefParams[T, U](callParams: List[Opt[T]], calleeParams: List[Opt[U]]): List[(List[Opt[T]], List[Opt[U]])] = {
        val callPs = groupOptListVAware(callParams, interproceduralCFG.getFeatureModel)
        val calleePs = groupOptListVAware(calleeParams, interproceduralCFG.getFeatureModel)

        // deal with variadic functions
        def calleeParamHasVarArgs: Boolean = calleePs.lastOption match {
            case Some(l) if l.exists {
                case Opt(_, v: VarArgs) => true
                case _ => false
            } => true
            case _ => false
        }

        def calleeParamIsVoidSpecifier: Boolean =
            (calleePs.size == 1) && calleePs.head.exists {
                case Opt(_, _: VoidSpecifier) | Opt(_, PlainParameterDeclaration(List(Opt(_, _: VoidSpecifier)), _)) => true
                case _ => false
            }

        if (calleeParamHasVarArgs) callPs.map((_, calleePs.head))
        else if ((callPs.size != calleePs.size) && calleeParamIsVoidSpecifier) List()
        else {
            if (callPs.size != calleePs.size)
                if (logger.isDebugEnabled) logger.debug("Call and function parameter sizes does not match for: " + currOpt
                  + "\n" + callPs.toString + "\n" + calleePs.toString)

            callPs zip calleePs
        }
    }
}
