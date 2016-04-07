package de.fosd.typechef.spllift.ifdsproblem

import java.util
import java.util.Collections

import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.featureexpr.{FeatureExpr, FeatureExprFactory}
import de.fosd.typechef.parser.c._
import de.fosd.typechef.spllift.{CInterCFG, CInterCFGCommons, CInterCFGPseudoVistingSystemLibFunctions}
import de.fosd.typechef.typesystem.{CAnonymousStruct, CStruct, CType}
import heros.{FlowFunction, FlowFunctions, IFDSTabulationProblem}

import scala.collection.JavaConverters._
import scala.collection.mutable.ListBuffer

trait InformationFlowProblemOperations extends CInterCFGCommons {
    def GEN(fact: InformationFlow): util.Set[InformationFlow] = Collections.singleton(fact)

    def GEN(res: List[InformationFlow]): util.Set[InformationFlow] = res.toSet.asJava

    def KILL: util.Set[InformationFlow] = Collections.emptySet()
}

class InformationFlowProblem(cICFG: CInterCFG) extends IFDSTabulationProblem[AST, InformationFlow, FunctionDef, CInterCFG] with InformationFlowConfiguration with InformationFlowProblemOperations with CInterCFGCommons with CInterCFGPseudoVistingSystemLibFunctions {

    private var initialGlobalsFile: String = ""

    /**
      * Returns initial seeds to be used for the analysis. This is a mapping of statements to initial analysis facts.
      * We consider global variables as initial sources.
      */
    override def initialSeeds(): util.Map[AST, util.Set[InformationFlow]] =
        interproceduralCFG.entryFunctions.foldLeft(new util.HashMap[AST, util.Set[InformationFlow]])((res, entry) => {
            interproceduralCFG.getStartPointsOf(entry).asScala.foreach(res.put(_, globalsAsInitialSeeds(entry)))
            initialGlobalsFile = entry.getFile.getOrElse("")
            res
        })

    /**
      * Returns the interprocedural control-flow graph which this problem is computed over.
      *
      * <b>NOTE:</b> this method could be called many times. Implementations of this
      * interface should therefore cache the return value!
      */
    override def interproceduralCFG: CInterCFG = cICFG

    /**
      * This must be a data-flow fact of type {@link D}, but must <i>not</i>
      * be part of the domain of data-flow facts. Typically this will be a
      * singleton object of type {@link D} that is used for nothing else.
      * It must holds that this object does not equals any object
      * within the domain.
      *
      * <b>NOTE:</b> this method could be called many times. Implementations of this
      * interface should therefore cache the return value!
      */
    override def zeroValue(): InformationFlow = Zero

    /**
      * Returns a set of flow functions. Those functions are used to compute data-flow facts
      * along the various kinds of control flows.
      *
      * <b>NOTE:</b> this method could be called many times. Implementations of this
      * interface should therefore cache the return value!
      */
    override def flowFunctions(): FlowFunctions[AST, InformationFlow, FunctionDef] = cachedFlowFunctions

    private val cachedFlowFunctions: FlowFunctions[AST, InformationFlow, FunctionDef] =
        new FlowFunctions[AST, InformationFlow, FunctionDef] {

            private var filesWithSeeds: List[String] = List() // check for double calls of initial seeds!

            private def initialSeedsExists(destinationMethod: FunctionDef): Boolean = {
                val destinationMethodFile = destinationMethod.getFile.getOrElse("")
                initialGlobalsFile.equalsIgnoreCase(destinationMethodFile) || filesWithSeeds.exists {
                    destinationMethodFile.equalsIgnoreCase
                }
            }

            /**
              * Returns the flow function that computes the flow for a call statement.
              *
              * @param callStmt
              * The statement containing the invoke expression giving rise to
              * this call.
              * @param destinationMethod
              * The concrete target method for which the flow is computed.
              */
            override def getCallFlowFunction(callStmt: AST, destinationMethod: FunctionDef): FlowFunction[InformationFlow] = {
                val callEnv = interproceduralCFG.nodeToEnv(callStmt)

                if (interproceduralCFG.getOptions.pseudoVisitingSystemLibFunctions
                    && destinationMethod.getName.equalsIgnoreCase(PSEUDO_SYSTEM_FUNCTION_CALL_NAME))
                    return pseudoSystemFunctionCallCallFlowFunction(callStmt, callEnv, interproceduralCFG)


                def mapCallParamToFDefParam(callParams: List[Opt[Expr]], fDefParams: List[Opt[DeclaratorExtension]], res: List[Opt[(Id, Id)]] = List()): List[Opt[(Id, Id)]] = {
                    if (callParams.isEmpty && fDefParams.isEmpty) return res

                    val currentCallParameter = callParams.head
                    val currentFDefParameter = fDefParams.head.entry match {
                        case DeclParameterDeclList(paramDecls) => paramDecls.flatMap {
                            case Opt(ft, p: ParameterDeclarationD) => Some(Opt(ft, p.decl.getId))
                            case _ => None
                        }
                        case missed =>
                            throw new IllegalArgumentException("No rule defined for converting parameter to parameter mapping: " + missed)
                    }

                    // TODO Variability Check
                    // TODO Expr other than id
                    // TODO outercall(innercall(variable))
                    val currRes = currentFDefParameter.map(currFDefParam => {
                        currentCallParameter.entry match {
                            case i: Id => Opt(currentCallParameter.condition.and(currFDefParam.condition), (i, currFDefParam.entry))
                            case missed =>
                                throw new IllegalArgumentException("No rule defined for converting expression to parameter mapping: " + missed)
                        }
                    })

                    mapCallParamToFDefParam(callParams.tail, fDefParams.tail, currRes ::: res)
                }

                val destinationEnv = interproceduralCFG().nodeToEnv(destinationMethod)

                val fCallOpt = parentOpt(callStmt, callEnv)
                val fCall = filterAllASTElems[FunctionCall](callStmt, callEnv).head // TODO Check if != 1
                val callExprs = fCall.params.exprs
                val fDefParams = destinationMethod.declarator.extensions
                val callParamToFDefParams = mapCallParamToFDefParam(callExprs, fDefParams)

                new FlowFunction[InformationFlow] {
                    override def computeTargets(flowFact: InformationFlow): util.Set[InformationFlow] = {
                        var ret = KILL

                        flowFact match {
                            // TODO Struct and Pointer Sources
                            case s@VarSource(x, _, _, global) =>
                                callParamToFDefParams.find(callParamToFDefParam => x.entry.name.equalsIgnoreCase(callParamToFDefParam.entry._1.name)) match {
                                    case Some(hit) =>
                                        val condition = hit.condition.and(x.condition)
                                        val source = VarSource(Opt(condition, hit.entry._2), fCallOpt, ListBuffer(s) ++ s.reachingSources)
                                        val reach = Reach(Opt(condition, hit.entry._1), s.name :: s.reachingSources.toList.map(_.name), List(s))
                                        ret = if (global.isDefined) GEN(List(source, s, reach)) else GEN(List(source, reach)) // New local Parameter
                                    case None if global.isDefined => ret = GEN(s) // Global Variable
                                    case None => ret = KILL // Local Variable from previous function
                                }

                            case s@StructSource(x, _, _, _, global) if global.isDefined => GEN(s)
                            case Zero if !initialSeedsExists(destinationMethod) =>
                                // Introduce Global Variables from linked file
                                filesWithSeeds ::= destinationMethod.getFile.getOrElse("")
                                ret = globalsAsInitialSeeds(destinationMethod)
                            case r: Reach => GEN(r)
                            case _ => KILL
                        }

                        ret
                    }
                }
            }

            /**
              * Returns the flow function that computes the flow for a an exit from a
              * method. An exit can be a return or an exceptional exit.
              *
              * @param callSite
              * One of all the call sites in the program that called the
              * method from which the exitStmt is actually returning. This
              * information can be exploited to compute a value that depends on
              * information from before the call.
              * <b>Note:</b> This value might be <code>null</code> if
              * using a tabulation problem with { @link IFDSTabulationProblem#followReturnsPastSeeds()}
              * returning <code>true</code> in a situation where the call graph
              * does not contain a caller for the method that is returned from.
              * @param calleeMethod
              * The method from which exitStmt returns.
              * @param exitStmt
              * The statement exiting the method, typically a return or throw
              * statement.
              * @param returnSite
              * One of the successor statements of the callSite. There may be
              * multiple successors in case of possible exceptional flow. This
              * method will be called for each such successor.
              * <b>Note:</b> This value might be <code>null</code> if
              * using a tabulation problem with { @link IFDSTabulationProblem#followReturnsPastSeeds()}
              * returning <code>true</code> in a situation where the call graph
              * does not contain a caller for the method that is returned from.
              * @return
              */
            override def getReturnFlowFunction(callSite: AST, calleeMethod: FunctionDef, exitStmt: AST, returnSite: AST): FlowFunction[InformationFlow] = {

                def assignsReturnVariablesTo(callStmt: AST, returnStatement: AST): List[(Id, List[Id])] = {
                    val fCall = filterASTElems[FunctionCall](callSite)
                    assignsVariables(callStmt).flatMap(assign => if (assign._2.exists(isPartOf(_, fCall))) Some((assign._1, uses(returnStatement))) else None)
                }

                // Default Return Flow: Kill all variables except globally defined
                def default(flowFact: InformationFlow) = flowFact match {
                    case s: Source if s.globalFile.isDefined => GEN(s)
                    case _ => KILL
                }

                if (interproceduralCFG.getOptions.pseudoVisitingSystemLibFunctions
                    && calleeMethod.getName.equalsIgnoreCase(PSEUDO_SYSTEM_FUNCTION_CALL_NAME))
                    return pseudoSystemFunctionCallReturnFlow

                val exitOpt = parentOpt(exitStmt, interproceduralCFG.nodeToEnv(exitStmt))

                new InfoFlowFunction(callSite, returnSite, Some(defines(callSite)), Some(uses(exitStmt)), Some(assignsReturnVariablesTo(callSite, exitStmt))) {
                    override def computeTargets(flowFact: InformationFlow): util.Set[InformationFlow] = {
                        var res = KILL

                        flowFact match {
                            case r: Reach => res = GEN(r)

                            case s@VarSource(sId, _, _, global) if useIsSatisfiable(sId) =>
                                val reachCondition = sId.condition.and(currOpt.condition).and(exitOpt.condition)
                                val reach = Reach(currOpt.copy(condition = reachCondition), s.name :: s.reachingSources.toList.map(_.name), List(s)) // Announce the fact, that this source reaches a new source generation

                                val sources = currAssignments.flatMap {
                                    case (target, assignments) =>
                                        assignments.flatMap {
                                            case assignment if isSatisfiable(assignment, sId) => genVarSource(target, Some(s), reachCondition)
                                            case _ => None
                                        }
                                }

                                res = if (global.isDefined) GEN(sources ::: List(s, reach)) else GEN(sources ::: List(reach)) // Pass global variables back to original flow

                            case Zero if currUses.isEmpty => // Return is something like return 0; -> we generate a new source
                                val sources = currAssignments.flatMap {
                                    case (target, assignment) if assignment.isEmpty => genVarSource(target, reachCondition = currOpt.condition.and(exitOpt.condition)) // Apply only when assignment is really empty
                                    case _ => None
                                }
                                res = GEN(sources)

                            case _ => res = default(flowFact) // Do default flow action

                        }

                        res
                    }
                }


            }

            /**
              * Returns the flow function that computes the flow for a normal statement,
              * i.e., a statement that is neither a call nor an exit statement.
              *
              * @param curr
              * The current statement.
              * @param succ
              * The successor for which the flow is computed. This value can
              * be used to compute a branched analysis that propagates
              * different values depending on where control0flow branches.
              */
            override def getNormalFlowFunction(curr: AST, succ: AST): FlowFunction[InformationFlow] = {
                def default(flowFact: InformationFlow) = GEN(flowFact)
                new InfoFlowFunction(curr, succ) {

                    override def computeTargets(flowFact: InformationFlow): util.Set[InformationFlow] = {
                        var res = KILL

                        flowFact match {
                            case s@VarSource(sId, _, _, global) if useIsSatisfiable(sId) =>

                                val reachCondition = sId.condition.and(currOpt.condition)
                                val reach = Reach(currOpt.copy(condition = reachCondition), s.name :: s.reachingSources.toList.map(_.name), List(s)) // Announce the fact, that this source reaches a new source

                                val sources = currAssignments.flatMap {
                                    // Find the correct assignee matching the current source
                                    case (target, assignments) =>

                                        def assignToVar(i: Id) = assignments.flatMap {
                                            case assignment if !isSatisfiable(assignment, sId) => None // Does not match - featurewise or namewise -> just do nothing. Note: this behaviour is correct as at least one match will be satisfiable, as the previous useIsSatisfiable returned true
                                            case assignment if isImplication(target, sId) => genVarSource(target, Some(s), reachCondition) // Kill source, as the old source gets replaced by the new one (eg in case of x = x + 1;)
                                            case assignment => s :: genVarSource(target, Some(s), reachCondition) // Pass the original source through, because of the fact that it will be not become invalid
                                        }

                                        def assignToStruct(i: Id) =
                                            assignments.flatMap {
                                                case assignment if !isSatisfiable(assignment, sId) => Some(s) // Does not match - featurewise or namewise -> just do nothing. Note: this behaviour is correct as at least one match will be satisfiable, as the previous useIsSatisfiable returned true
                                                case assignment =>

                                                    val field = currStructFieldDefines.find(_._2.headOption match {
                                                        case None => false
                                                        case Some(filedHit) => target.eq(filedHit)
                                                    })

                                                    if (field.isDefined) s :: genStructSource(target, field.get, Some(s), reachCondition) else List(s) // return reaching source again as a variable gets assigned to a struct source
                                            }

                                        singleVisitOnSourceTypes(target, assignToStruct, assignToVar)
                                }
                                res = GEN(reach :: sources)

                            case s@VarSource(id, _, _, None) if defineIsImplication(id) => res = KILL // Kill previously known local source as it is now no longer valid
                            case s@VarSource(id, _, _, global) if global.isDefined && defineIsImplication(id) && globalNameScopeIsSatisfiable(id, global) => res = KILL // Kill previously known global source as it is now no longer valid , only kill this source in case the name scope is globally visible

                            case s@StructSource(_, _, _, _, None) if structFieldDefineIsImplication(s) => res = KILL // Kill previously known local source as it is now no longer valid
                            case s: StructSource =>
                                val currRes = if (structUseIsSatisfiable(s)) {
                                    val reachCondition = s.name.condition.and(currOpt.condition)
                                    List(Reach(currOpt.copy(condition = reachCondition), s.name :: s.reachingSources.toList.map(_.name), List(s)))
                                } else List()

                                res = GEN(s :: currRes)

                            case Zero if currDefines.nonEmpty =>

                                val facts: List[InformationFlow] = currDefines.flatMap(id =>
                                    succVarEnv.varEnv.lookupScope(id.name).toOptList.flatMap(scope =>
                                        if (currAssignments.exists(_._1.name.equalsIgnoreCase(id.name)))
                                            None // Do not generate a source for assignments from other variables (e.g x = y;)
                                        else {
                                            // is completly new introduced declaration without usage (e.g int x = 50;)
                                            val file = if ((scope.entry == 0) && scope.condition.isSatisfiable(interproceduralCFG.getFeatureModel)) getFileName(id.getFile) else None

                                            def zeroStructSource(i: Id) = List(StructSource(Opt(currOpt.condition.and(scope.condition), id), None, currOpt, ListBuffer(), file))
                                            def zeroVarSource(i: Id) = List(VarSource(Opt(currOpt.condition.and(scope.condition), id), currOpt, ListBuffer(), file))

                                            singleVisitOnSourceTypes(id, zeroStructSource, zeroVarSource)
                                        }))
                                res = GEN(facts)

                            case Zero if currStructFieldDefines.nonEmpty && currUses.isEmpty =>
                                // TODO Scoping
                                val facts: List[InformationFlow] = currStructFieldDefines.flatMap(field => genStructSource(field._1, field, None, currOpt.condition))
                                res = GEN(facts)
                            case r: Reach => res = KILL
                            case _ => res = default(flowFact)

                        }
                        res
                    }
                }
            }

            /**
              * Returns the flow function that computes the flow from a call site to a
              * successor statement just after the call. There may be multiple successors
              * in case of exceptional control flow. In this case this method will be
              * called for every such successor. Typically, one will propagate into a
              * method call, using {@link #getCallFlowFunction(Object, Object)}, only
              * such information that actually concerns the callee method. All other
              * information, e.g. information that cannot be modified by the call, is
              * passed along this call-return edge.
              *
              * @param callSite
              * The statement containing the invoke expression giving rise to
              * this call.
              * @param returnSite
              * The return site to which the information is propagated. For
              * exceptional flow, this may actually be the start of an
              * exception handler.
              */
            override def getCallToReturnFlowFunction(callSite: AST, returnSite: AST): FlowFunction[InformationFlow] = {
                def default(flowFact: InformationFlow) = GEN(flowFact)

                new FlowFunction[InformationFlow] {
                    override def computeTargets(infoFlowFact: InformationFlow): util.Set[InformationFlow] =
                        infoFlowFact match {
                            case s: Source if s.globalFile.isDefined => KILL
                            case _ => default(infoFlowFact) // TODO Pointer passed to function! // TODO LAST Statement
                        }
                }
            }
        }

    private def globalsAsInitialSeeds(f: FunctionDef): util.Set[InformationFlow] = {
        val globalVariables = interproceduralCFG.nodeToTUnit(f).defs.filterNot {
            // Ignore function and typedef definitions
            case Opt(_, f: FunctionDef) => true //
            case Opt(_, d: Declaration) => d.declSpecs.exists {
                case Opt(_, t: TypedefSpecifier) => true
                case _ => false
            }
            case _ => false
        }

        val globalInfoFlowFacts = globalVariables.flatMap(x => {
            val decls = declares(x)

            // Note: we ignore the actual file of the declaration as it may be declared in a header file.
            // As variables declared in header files may be included across several files, this way prevents matching errors.
            if (decls.nonEmpty) decls.map(decl => VarSource(Opt(x.condition, decl), x, ListBuffer(), getFileName(f.getFile)))
            else None
        })

        GEN(globalInfoFlowFacts :+ zeroValue())
    }

    private abstract class InfoFlowFunction(curr: AST, succ: AST, definedIds: Option[List[Id]] = None, usedIds: Option[List[Id]] = None, assignments: Option[List[(Id, List[Id])]] = None) extends FlowFunction[InformationFlow] {
        // Lazy cache some repeatedly used variables
        lazy val currASTEnv = interproceduralCFG.nodeToEnv(curr)
        lazy val currOpt: Opt[AST] = parentOpt(curr, currASTEnv).asInstanceOf[Opt[AST]]
        lazy val currTS = interproceduralCFG.nodeToTS(curr)
        lazy val succVarEnv = currTS.lookupEnv(succ)

        lazy val currDefines = definedIds match {
            case None => defines(curr)
            case Some(x) => x
        }

        lazy val currUses = usedIds match {
            case None => uses(curr)
            case Some(x) => x
        }

        lazy val currAssignments = assignments match {
            case None => assignsVariables(curr)
            case Some(x) => x
        }

        lazy val currStructFieldDefines = definesField(curr)

        lazy val currStructFieldUses = usesField(curr)

        private var varSourceCache = Map[Opt[Id], List[Source]]()

        private var structSourceCache = Map[Opt[Id], List[StructSource]]()

        def isStructOrUnion(cType: CType): Boolean =
            cType.atype match {
                case _: CStruct | _: CAnonymousStruct => true
                case _ => false
            }

        def isSatisfiable(inner: Id, outer: Opt[Id]): Boolean = isSatisfiable(Opt(currASTEnv.featureExpr(inner), inner), outer)
        def isSatisfiable(inner: Opt[Id], outer: Opt[Id]): Boolean = inner.entry.name.equalsIgnoreCase(outer.entry.name) && inner.condition.and(outer.condition).isSatisfiable(interproceduralCFG.getFeatureModel)

        def isImplication(inner: Id, outer: Opt[Id]): Boolean = isImplication(Opt(currASTEnv.featureExpr(inner), inner), outer)
        def isImplication(inner: Opt[Id], outer: Opt[Id]): Boolean = outer.entry.name.equalsIgnoreCase(inner.entry.name) && outer.condition.implies(inner.condition).isTautology(interproceduralCFG.getFeatureModel)

        def defineIsSatisfiable(x: Opt[Id]): Boolean = occurrenceFulfills(x, currDefines, isSatisfiable)

        def structFieldDefineIsSatisfiable(x: Opt[Id]): Boolean = occurrenceFulfills(x, currStructFieldDefines.flatMap(_._2.headOption), isSatisfiable)

        def defineIsImplication(x: Opt[Id]): Boolean = occurrenceFulfills(x, currDefines, isImplication)

        def structFieldDefineIsImplication(s: StructSource): Boolean = currStructFieldDefines.exists(structFieldMatch(s, _, _.equals(_)))

        def useIsSatisfiable(x: Opt[Id]): Boolean = occurrenceFulfills(x, currUses, isSatisfiable)

        def structUseIsSatisfiable(s: StructSource) = currStructFieldUses.exists(structFieldMatch(s, _, _.equals(_)))

        private def occurrenceFulfills(x: Opt[Id], occurrences: List[Id], fulfills: (Id, Opt[Id]) => Boolean) = occurrences.exists(fulfills(_, x))

        def globalNameScopeIsSatisfiable(id: Opt[Id], declarationFile: Option[String] = None): Boolean = {
            val eqDeclFile = declarationFile match {
                case None => false
                case Some(declFile) => getFileName(id.entry.getFile).getOrElse("").equalsIgnoreCase(declFile)
            }

            eqDeclFile && currTS.lookupEnv(succ).varEnv.lookupScope(id.entry.name).toOptList.exists {
                case o@Opt(cond, 0) => id.condition.and(cond).isSatisfiable(interproceduralCFG.getFeatureModel)
                case _ => false
            }
        }

        def genVarSource(target: Id, reachingSource: Option[Source] = None, reachCondition: FeatureExpr = FeatureExprFactory.True): List[Source] = {
            val targetCondition = currASTEnv.featureExpr(target).and(reachCondition)
            val targetOpt = Opt(targetCondition, target)

            varSourceCache.get(targetOpt) match {
                case None => // New source -> add to cache map
                    val buffer = if (reachingSource.isDefined) ListBuffer[Source](reachingSource.get) ++ reachingSource.get.reachingSources else ListBuffer[Source]()
                    val genSources = currTS.lookupEnv(succ).varEnv.lookupScope(target.name).toOptList.flatMap(scope => {

                        val scopeCondition = targetCondition.and(scope.condition)
                        val currCondition = if (reachingSource.isDefined) scopeCondition.and(reachingSource.get.name.condition) else scopeCondition

                        // gen with the correct  and satisfiable scope
                        if (currCondition.isSatisfiable(interproceduralCFG.getFeatureModel))
                            if (scope.entry == 0)
                                Some(VarSource(targetOpt.copy(condition = currCondition), currOpt, buffer, getFileName(target.getFile))) // add file when global definition exists
                            else
                                Some(VarSource(targetOpt.copy(condition = currCondition), currOpt, buffer))
                        else
                            None
                    })

                    varSourceCache += (targetOpt -> genSources)
                    genSources

                case Some(genSource) => // Update already existing targets
                    if (reachingSource.isDefined) genSource.foreach(genS => genS.reachingSources.+=(reachingSource.get) ++ reachingSource.get.reachingSources) // Update new reaching sources
                    List()
            }
        }

        def genStructSource(target: Id, field: (Id, List[Id]), reachingSource: Option[Source] = None, reachCondition: FeatureExpr = FeatureExprFactory.True): List[StructSource] = {
            val targetCondition = currASTEnv.featureExpr(target).and(reachCondition)
            val targetOpt = Opt(targetCondition, target)

            lazy val fieldSources: List[Source] = {
                def structSource(i: Id): List[StructSource] = List(StructSource(Opt(currASTEnv.featureExpr(i), i), None, currOpt))
                def varSource(i: Id): List[VarSource] = List(VarSource(Opt(currASTEnv.featureExpr(i), i), currOpt))

                def genFieldSource(target: Id, field: (Id, List[Id])): List[Source] = {
                    field._2.headOption match {
                        case None => singleVisitOnSourceTypes[Source](field._1, structSource, varSource)
                        case Some(id) if id.eq(target) => genFieldSource(target, (field._1, List()))
                        case Some(id) =>
                            singleVisitOnSourceTypes[Source](id, structSource, varSource).flatMap(currParent => genFieldSource(target, (field._1, field._2.tail)).map(src => currParent match {
                                case s: StructSource => s.copy(field = Some(src))
                                case _ => src
                            }))
                    }
                }

                genFieldSource(target, field)
            }

            val matches = structSourceCache.getOrElse(targetOpt, List()).filter(structFieldMatch(_, field))

            // TODO Global scoping

            val res: List[StructSource] = {
                if (matches.isEmpty) {
                    val buffer = if (reachingSource.isDefined) ListBuffer[Source](reachingSource.get) ++ reachingSource.get.reachingSources else ListBuffer[Source]()
                    val sources = fieldSources.map(src => StructSource(targetOpt, Some(src), currOpt, buffer))
                    structSourceCache += (targetOpt -> (sources ::: structSourceCache.getOrElse(targetOpt, List())))
                    sources
                } else {
                    matches.foreach(genS => genS.reachingSources.+=(reachingSource.get) ++ reachingSource.get.reachingSources) // Update new reaching sources
                    List()
                }
            }

            res
        }

        /*
         * We are using the variability-aware typesystem of TypeChef. However, variability encoded in the type definition of an variable or struct does not matter for us.
         * As a consequence we only visit one type-definition as we do assume correct type assignments.
         */
        def singleVisitOnSourceTypes[T <: InformationFlow](typedId: Id, structFun: (Id => List[T]), varFun: (Id => List[T])): List[T] = {
            val cTypes = succVarEnv.varEnv.lookupType(typedId.name)
            var cFacts: List[T] = List()

            // Do not generate sources for every possible type condition; only once for either struct or variable
            if (cTypes.exists(ct => isStructOrUnion(ct)))
                cFacts :::= structFun(typedId)
            if (cTypes.exists(ct => !isStructOrUnion(ct)))
                cFacts :::= varFun(typedId)

            cFacts
        }

        /*
         * Retrieves if a given struct field access (e.g. parent.innerStruct.actualField) is matched by a certain InfoFlowFact source.
         * For example: the given struct above would match with the following StructSource: StructSource(parent, StructSource(innerStruct, VarSource(actualField)))
         */
        def structFieldMatch(s: Source, fieldMapping: (Id, List[Id]), isEqual: (Id, Id) => Boolean = _.eq(_)): Boolean = {
            def isMatch(field: Opt[Id], otherField: Opt[Id]): Boolean = isEqual(field.entry, otherField.entry) && field.condition.and(otherField.condition).isSatisfiable(interproceduralCFG.getFeatureModel)

            val (cField, cParents) = fieldMapping
            lazy val fieldOpt = Opt(currASTEnv.featureExpr(cField), cField)

            s match {
                case VarSource(fieldVar, _, _, _) if cParents.isEmpty => isMatch(fieldVar, fieldOpt) // matches if field source was found
                case StructSource(sParent, sField, _, _, _) if sField.isEmpty && cParents.isEmpty => isMatch(sParent, fieldOpt) // matches with outerStruct.innerStruct
                case StructSource(sParent, sField, _, _, _) if sField.isDefined => cParents.headOption match {
                    case Some(head) if isMatch(sParent, Opt(currASTEnv.featureExpr(head), head)) => structFieldMatch(sField.get, (cField, cParents.tail), isEqual) // Remove matched parent and continue search
                    case _ => false
                }
                case _ => false
            }
        }
    }

}