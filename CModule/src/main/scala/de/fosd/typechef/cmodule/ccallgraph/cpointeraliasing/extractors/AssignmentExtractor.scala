package de.fosd.typechef.cmodule.ccallgraph.cpointeraliasing.extractors

import de.fosd.typechef.cmodule.ccallgraph.PointsToFDef
import de.fosd.typechef.cmodule.ccallgraph.cpointeraliasing.types._
import de.fosd.typechef.cmodule.{CModule, CModuleCommons}
import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.customization.StopWatch
import de.fosd.typechef.parser.c._
import de.fosd.typechef.typesystem._
import org.slf4j.{Logger, LoggerFactory}

/**
  * Factory for extracting all (pointer related) assignments as objectnames for the used aliasing algorithm.
  */
object AssignmentExtractor extends CModuleCommons with ExtractorCommons {

    private lazy val logger: Logger = LoggerFactory.getLogger(getClass)

    private lazy val EMPTY = Set[Opt[Assignment]]()

    def get(node: Any, env: CModule): Set[Opt[Assignment]] =
        extractClassicAssignment(node, env)

    def getCallCalleeAssignments(translationUnit: TranslationUnit, env: CModule): Set[Opt[Assignment]] =
        extractCallCalleeAssignments(translationUnit, env)

    /**
      * Extracts all function call and return assignments such as x = y;
      */
    private def extractCallCalleeAssignments(node: TranslationUnit, env: CModule): Set[Opt[Assignment]] = {
        val callGraph = env.getCallGraph
        val allCalls = env.filterAllASTElems[FunctionCall](node).flatMap(call => env.findPriorASTElem[PostfixExpr](call, env.getASTEnv(call)))

        val (t0, callCallee) = StopWatch.measureThreadUserTime({
            allCalls.flatMap(call => {
                val callees = callGraph.getCalleesOfCallAt(call, aliasing = false)
                if (callees.isEmpty) None else Some((call, callees))
            })
        })

        val (t1, callToCalleeAssignments) = StopWatch.measureThreadUserTime({
            callCallee.flatMap { case (call, callees) => callees flatMap (callee => {
                val callToCalleeAssignements = matchCallParamsWithCalleeParams(call, callee, env)
                val returnToCall = matchReturnToCall(call, callee, env)

                callToCalleeAssignements ++ returnToCall
            })
            }
        })

        val dur: Long = t1 + t0
        logger.debug("Call-to-Callee Mapping in " + dur + "ms.")

        callToCalleeAssignments.toSet
    }

    /**
      * Extracts all classic assignments such as x = y;
      */
    private def extractClassicAssignment(node: Any, env: CModule): Set[Opt[Assignment]] = {
        node match {
            case ae: AssignExpr if !env.getCallGraph.isFunctionCall(ae.source) => // classic "=" assignment
                if (isPointerRelatedExpr(ae.source, env) || isPointerRelatedExpr(ae.target, env)) // Compute only pointer related assignments.
                    pairLeftAndRightHandSideOfAssignment(ae.source, ae.target, env)
                else EMPTY
            case i@InitDeclaratorI(declarator, _, Some(init@Initializer(_, l: LcurlyInitializer))) =>
                val astEnv = env.getASTEnv(l)
                val priorDeclaration = env.findPriorASTElem[Declaration](l, astEnv).get
                val baseObjectNames = ObjectNameExtractor.getTopObjectNames(priorDeclaration.init.map(_.entry.declarator.getId), env)
                val ts = env.getTypeSystem(l)

                val tsEnv =
                    try {
                        ts.lookupEnv(l)
                    } catch {
                        case e: Exception =>
                            // No typechecked env available - silently ignore this special corner case
                            logger.debug("No typechecked env available for: " + priorDeclaration)
                            return EMPTY
                    }

                lazy val typeDefTypeSpecifiers = env.filterASTElems[TypeDefTypeSpecifier](priorDeclaration)
                lazy val structOrUnionSpecifiers = env.filterASTElems[StructOrUnionSpecifier](priorDeclaration)

                if (tsEnv == null) {
                    logger.debug("No typechecked env available for: " + priorDeclaration)
                    EMPTY
                } else if (typeDefTypeSpecifiers.nonEmpty) {
                    typeDefTypeSpecifiers.flatMap(spec => {
                        val cTypes = tsEnv.typedefEnv.apply(spec.name.name)
                        cTypes.toOptList.flatMap {
                            _.entry match {
                                case CType(s: CAnonymousStruct, _, _, _) =>
                                    logger.debug("Missed CAnonymousStruct: " + s)
                                    EMPTY
                                case CType(s: CStruct, _, _, _) =>
                                    val fields = tsEnv.structEnv.getFields(s.s, s.isUnion).toOptList
                                    matchStructFieldWithCurlyInits(baseObjectNames, l, fields, env)
                                case x =>
                                    logger.debug("Missed curly type of: " + x)
                                    EMPTY
                            }
                        }
                    }).toSet
                } else if (structOrUnionSpecifiers.nonEmpty) {
                    structOrUnionSpecifiers.flatMap {
                        case spec if spec.id.isDefined =>
                            val fields = tsEnv.structEnv.getFields(spec.id.get.name, spec.isUnion).toOptList
                            matchStructFieldWithCurlyInits(baseObjectNames, l, fields, env)
                        case _ => None
                    }.toSet
                } else EMPTY

            case i@InitDeclaratorI(declarator, _, Some(init@Initializer(_, l))) if !l.isInstanceOf[LcurlyInitializer] && !env.getCallGraph.isFunctionCall(init.expr) =>
                if (!(isPointerRelatedExpr(declarator.getId, env) || isPointerRelatedExpr(l, env))) return EMPTY

                val orgObjectNamesLeft = ObjectNameExtractor.getTopObjectNames(declarator, env)
                val orgObjectNamesRight = ObjectNameExtractor.getTopObjectNames(init, env)

                //  assignments such as int* x = y are treated like int* x; x = y;
                val filteredObjectNamesLeft = orgObjectNamesLeft.map {
                    case Opt(condition, PointerObjectName(baseObjectName, PointerDerefOperator())) => Opt(condition, baseObjectName)
                    case x => x
                }

                filteredObjectNamesLeft.flatMap(leftName =>
                    orgObjectNamesRight.map(rightName =>
                        Opt(leftName.condition.and(rightName.condition), Assignment(leftName.entry, rightName.entry))))
            case p: Product => p.productIterator.flatMap(get(_, env)).toSet
            case _ => Set[Opt[Assignment]]()
        }
    }

    private def matchStructFieldWithCurlyInits(baseObjectNames: Iterable[Opt[ObjectName]], lcurlyInitializer: LcurlyInitializer, fields: List[Opt[ConditionalTypeMap]], env: CModule): Set[Opt[Assignment]] = {
        baseObjectNames.flatMap(baseObjectName =>
            fields.flatMap(cField => {
                // group inits and fields together
                groupInitializerWithFields(lcurlyInitializer, cField).flatMap {
                    case (field, init) =>
                        field.flatMap(f => {
                            val left = FieldObjectName(baseObjectName.entry, PlainObjectName(f.entry, None, None, None))
                            init.flatMap(i => ObjectNameExtractor.getTopObjectNames(i, env).map(right =>
                                Opt(baseObjectName.condition.and(f.condition).and(right.condition), Assignment(left, right.entry))))
                        })
                }
            })).toSet
    }

    private def matchCallParamsWithCalleeParams(call: PostfixExpr, callee: PointsToFDef, env: CModule): Set[Opt[Assignment]] = {
        if (!call.s.isInstanceOf[FunctionCall]) return EMPTY
        if (call.s.asInstanceOf[FunctionCall].params.exprs.isEmpty) return EMPTY

        val callParams = call.s.asInstanceOf[FunctionCall].params.exprs
        val calleeParams = getCalleeParameters(callee.getFDef)

        val paired = pairCallParamsToDefParams(callParams, calleeParams)

        val assignments = paired.flatMap {
            case (callP, calleeP) =>
                val callPHasPointer = callP.exists(x => isPointerRelatedExpr(x.entry, env))
                val calleePHasPointer = env.filterAllASTElems[Expr](calleeP).exists(isPointerRelatedExpr(_, env))

                // Compute only pointer related assignments.
                if (!(callPHasPointer || calleePHasPointer)) None
                else {
                    val orgObjectNamesCallP = ObjectNameExtractor.getTopObjectNames(callP, env)
                    val orgObjectNamesCalleeP = ObjectNameExtractor.getTopObjectNames(calleeP, env)

                    // call(&x) -> callee(*y) are treated like y = &x;
                    val filteredObjectNamesCalleeP = orgObjectNamesCalleeP.map {
                        case Opt(condition, PointerObjectName(baseObjectName, PointerDerefOperator())) => Opt(condition, baseObjectName)
                        case x => x
                    }

                    orgObjectNamesCallP.flatMap(callPName =>
                        filteredObjectNamesCalleeP.flatMap(calleePName => {
                            val condition = callPName.condition.and(calleePName.condition)
                            if (condition.isSatisfiable()) Some(Opt(condition, Assignment(callPName.entry, calleePName.entry)))
                            else None
                        }))
                }
        }

        assignments.toSet
    }

    /**
      * Match assignments at call site together with values from the exit site like assignments.
      */
    private def matchReturnToCall(call: PostfixExpr, callee: PointsToFDef, env: CModule): Set[Opt[Assignment]] = {
        val assignExpr = env.findPriorASTElem[AssignExpr](call, env.getASTEnv(call))
        val initExpr = env.findPriorASTElem[InitDeclaratorI](call, env.getASTEnv(call))
        val returnExprs = env.filterAllASTElems[ReturnStatement](callee).flatMap { case ReturnStatement(x) => x }

        if ((assignExpr.isEmpty && initExpr.isEmpty) || returnExprs.isEmpty) return EMPTY

        val expr = if (assignExpr.isDefined) assignExpr.get.target else initExpr.get.declarator.getId

        // Compute only pointer related assignments.
        val targetHasPointer = isPointerRelatedExpr(expr, env)
        val returnSiteHasPointer = returnExprs.exists(isPointerRelatedExpr(_, env))

        if (targetHasPointer || returnSiteHasPointer) pairLeftAndRightHandSideOfAssignment(expr, returnExprs, env)
        else EMPTY
    }

    private def pairLeftAndRightHandSideOfAssignment(left: Product, right: Product, env: CModule) = {
        val leftAssignmentNames = ObjectNameExtractor.getTopObjectNames(left, env)
        val rightAssignmentNames = ObjectNameExtractor.getTopObjectNames(right, env)

        leftAssignmentNames.flatMap(leftName =>
            rightAssignmentNames.flatMap(rightName => {
                val condition = leftName.condition.and(rightName.condition)
                if (condition.isSatisfiable()) Some(Opt(condition, Assignment(leftName.entry, rightName.entry)))
                else None
            }))
    }

    /**
      * Matcher strategy to determine call and callee parameter pairs.
      */
    private def pairCallParamsToDefParams[T, U](callParams: List[Opt[T]], calleeParams: List[Opt[U]]): List[(List[Opt[T]], List[Opt[U]])] = {
        val callPs = groupOptListVAware(callParams)
        val calleePs = groupOptListVAware(calleeParams)

        // deal with variadic functions
        def calleeParamHasVarArgs: Boolean = calleePs.lastOption match {
            case Some(l) =>
                l.exists {
                    case Opt(_, v: VarArgs) => true
                    case _ => false
                }
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
            if (callPs.size != calleePs.size) if (logger.isDebugEnabled) logger.debug("Call and function parameter sizes does not match for: " + "\n" + callPs.toString + "\n" + calleePs.toString)
            callPs zip calleePs
        }
    }

    private def getCalleeParameters(fDef: FunctionDef): List[Opt[ParameterDeclarationD]] =
        fDef.declarator.extensions.flatMap {
            case Opt(_, DeclParameterDeclList(l)) =>
                l.headOption match {
                    case Some(Opt(_, p: ParameterDeclarationD)) => l.asInstanceOf[List[Opt[ParameterDeclarationD]]]
                    case _ => None
                }
            case _ => None
        }

}
