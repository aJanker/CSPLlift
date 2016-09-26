package de.fosd.typechef.cspllift.cifdsproblem.informationflow

import java.util

import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.cspllift.CInterCFG
import de.fosd.typechef.cspllift.analysis.{Edge, Node, SuperCallGraph}
import de.fosd.typechef.cspllift.cifdsproblem.{CFlowConstants, CFlowOperations, CIFDSProblem, CZeroFact}
import de.fosd.typechef.cspllift.commons.WarningsCache
import de.fosd.typechef.parser.c._
import de.fosd.typechef.typesystem.{CAnonymousStruct, CPointer, CStruct, CType}
import heros.{FlowFunction, FlowFunctions}

import scala.collection.JavaConverters._


trait InformationFlow2ProblemOperations extends CFlowConstants with CFlowOperations[InformationFlow2]

class InformationFlow2Problem(cICFG: CInterCFG) extends CIFDSProblem[InformationFlow2](cICFG) with InformationFlow2Configuration with InformationFlow2ProblemOperations {

    private var initialGlobalsFile: String = ""

    private var filesWithSeeds: Set[String] = Set()

    /**
      * Returns initial seeds to be used for the analysis. This is a mapping of statements to initial analysis facts.
      * We consider global variables as initial sources.
      */
    override def initialSeeds(): util.Map[Opt[AST], util.Set[InformationFlow2]] = {
        val initialSeeds = new util.HashMap[Opt[AST], util.Set[InformationFlow2]]()
        interproceduralCFG.getEntryFunctions.foldLeft(initialSeeds)((seeds, entryFunction) => {
            interproceduralCFG.getStartPointsOf(entryFunction).asScala.foreach(seeds.put(_, globalsAsInitialSeeds(entryFunction)))
            initialGlobalsFile = entryFunction.entry.getFile.getOrElse("")
            filesWithSeeds = filesWithSeeds + initialGlobalsFile
            seeds
        })
    }

    private def initialSeedsExists(destinationMethod: FunctionDef): Boolean = {
        val destinationMethodFile = destinationMethod.getFile.getOrElse("")
        initialGlobalsFile.equalsIgnoreCase(destinationMethodFile) || filesWithSeeds.exists(destinationMethodFile.equalsIgnoreCase)
    }


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
    private val zeroVal = Zero()

    override def zeroValue(): InformationFlow2 with CZeroFact = zeroVal

    /**
      * Returns a set of flow functions. Those functions are used to compute data-flow facts
      * along the various kinds of control flows.
      *
      * <b>NOTE:</b> this method could be called many times. Implementations of this
      * interface should therefore cache the return value!
      */
    override def flowFunctions(): FlowFunctions[Opt[AST], InformationFlow2, Opt[FunctionDef]] = cachedFlowFunctions

    private val cachedFlowFunctions: FlowFunctions[Opt[AST], InformationFlow2, Opt[FunctionDef]] =
        new FlowFunctions[Opt[AST], InformationFlow2, Opt[FunctionDef]] {

            /**
              * Returns the flow function that computes the flow for a normal statement,
              * i.e., a statement that is neither a call nor an exit statement.
              *
              * @param curr
              * The current statement.
              * @param succ
              * The successor for which the flow is computed. This value can
              * be used to compute a branched analysis that propagates
              * different values depending on where controlflow branches.
              */
            override def getNormalFlowFunction(curr: Opt[AST], succ: Opt[AST]): FlowFunction[InformationFlow2] = {
                def default(flowFact: InformationFlow2) = GEN(flowFact)

                new InfoFlowFunction(curr, succ, default) {
                    override def computeTargets(flowFact: InformationFlow2): util.Set[InformationFlow2] = {
                        val result = flowFact match {
                            case v@VarSource(source, _, isSourceOf, usedIn, scope) =>
                                // Self Assignment -> new VarSource + SourceOf + Kill old source
                                if (currAssignments.exists { case (assignee, assignors) => hasSelfAssignement(source, assignee, assignors) }) {
                                    val assignments = currAssignments.filter { case (assignee, assignors) => hasSelfAssignement(source, assignee, assignors) }

                                    val genSet = assignments.flatMap {
                                        case (assignee, assignors) =>
                                            val sink = SinkToAssignment(currOpt, v, assignee)
                                            val sourceOf = VarSourceOf(assignee, currOpt, v, List(), scope)
                                            List(sink, sourceOf)
                                    }
                                    GEN(genSet)
                                    // New Assignment -> KILL
                                } else if (currAssignments.exists { case (assignee, assignor) => assignee.equals(source) }) KILL
                                // update current varsource, new varsource + sourceof + sink
                                else if (currAssignments.nonEmpty && currUses.contains(source)) {
                                    val assignees = currAssignments.filter { case (assignee, assignor) => assignor.contains(source) }

                                    val sources = assignees.map { case (assignee, assignor) => VarSource(assignee, currOpt, List(), List(), SCOPE_UNKNOWN) }
                                    val sourcesOf = sources.map(newSource => {
                                        val scope = currTS.lookupEnv(curr.entry).varEnv.lookupScope(newSource.name.name).select(currOpt.condition.collectDistinctFeatures)
                                        VarSourceOf(newSource.name, currOpt, v, List(), scope)
                                    })
                                    val sinks = assignees.map(assignment => SinkToAssignment(currOpt, v, assignment._1))
                                    val updatedVarSource = v.copy(usedIn = currOpt :: usedIn, isSourceOf = sources ::: isSourceOf)

                                    GEN(updatedVarSource :: sourcesOf ::: sinks)
                                } else super.computeTargets(v)

                            case vo@VarSourceOf(id, _, source, usedIn, scope) =>
                                // Self Assignment -> new VarSource + SourceOf + Kill old source
                                if (currAssignments.exists { case (assignee, assignors) => hasSelfAssignement(id, assignee, assignors) }) {
                                    val assignments = currAssignments.filter { case (assignee, assignors) => hasSelfAssignement(id, assignee, assignors) }

                                    val genSet = assignments.flatMap {
                                        case (assignee, assignors) =>
                                            val sink = SinkToAssignment(currOpt, source, assignee)
                                            val sourceOf = VarSourceOf(assignee, currOpt, source, List(), scope)
                                            List(sink, sourceOf)
                                    }
                                    GEN(genSet)
                                    // New Assignment -> KILL
                                } else if (currAssignments.exists { case (assignee, assignor) => assignee.equals(id) }) KILL
                                // update current varsource, new varsource + sourceof + sink
                                else if (currAssignments.nonEmpty && currUses.contains(id)) {
                                    val assignees = currAssignments.filter { case (assignee, assignor) => assignor.contains(id) }

                                    val sources = assignees.map { case (assignee, assignor) => VarSource(assignee, currOpt, List(), List(), SCOPE_UNKNOWN) }
                                    val sourcesOf = sources.map(newSource => {
                                        val scope = currTS.lookupEnv(curr.entry).varEnv.lookupScope(newSource.name.name).select(currOpt.condition.collectDistinctFeatures)
                                        VarSourceOf(newSource.name, currOpt, source, List(), scope)
                                    })
                                    val sinks = assignees.map(assignment => SinkToAssignment(currOpt, source, assignment._1))
                                    val updatedVarSource = vo.copy(usedIn = currOpt :: usedIn)

                                    GEN(updatedVarSource :: sourcesOf ::: sinks)
                                } else super.computeTargets(vo)

                            case z: Zero if currAssignments.nonEmpty =>
                                // new assignment => gen new source
                                def varFun(assignee: Id): List[VarSource] = {
                                    val scope = currTS.lookupEnv(curr.entry).varEnv.lookupScope(assignee.name).select(currOpt.condition.collectDistinctFeatures)
                                    List(VarSource(assignee, currOpt, List(), List(), scope))
                                }

                                def structFun(define: Id): List[StructSource] = List()

                                val sources = currAssignments.flatMap { case (assignee, _) =>
                                    singleVisitOnSourceTypes(assignee, structFun, varFun)
                                }
                                GEN(z :: sources)

                            case z: Zero if currDefines.nonEmpty =>
                                // newly introduced variable or struct
                                def varFun(define: Id): List[VarSource] = List(VarSource(define, currOpt, List(), List(), SCOPE_LOCAL))
                                def structFun(define: Id): List[StructSource] = List(StructSource(define, None, currOpt, List(), List(), SCOPE_LOCAL))

                                val sources = currDefines.flatMap(define => singleVisitOnSourceTypes(define, structFun, varFun))

                                GEN(z :: sources)
                            case x => super.computeTargets(x)
                        }
                        result
                    }
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
            override def getCallFlowFunction(callStmt: Opt[AST], destinationMethod: Opt[FunctionDef]): FlowFunction[InformationFlow2] = {
                val flowCondition = destinationMethod.condition.and(callStmt.condition)
                val destinationEnv = interproceduralCFG().getASTEnv(destinationMethod)
                val destinationOpt = parentOpt(destinationMethod.entry, destinationEnv).asInstanceOf[Opt[FunctionDef]]
                SuperCallGraph.addEge(Edge(Node(interproceduralCFG.getMethodOf(callStmt)), Node(destinationOpt), flowCondition))

                if (interproceduralCFG.getOptions.pseudoVisitingSystemLibFunctions && destinationMethod.entry.getName.equalsIgnoreCase(SPLLIFT_PSEUDO_SYSTEM_FUNCTION_CALL_NAME))
                    return pseudoSystemFunctionCallCallFlowFunction(callStmt, interproceduralCFG.getASTEnv(callStmt), interproceduralCFG)

                def default(flowFact: InformationFlow2) = KILL
                def getZeroFactWithFlow(zero: Zero): Zero = zero.copy(flowCondition = flowCondition.and(zero.flowCondition))

                new CallFlowFunction(callStmt, destinationMethod, default) {
                    override def computeTargets(flowFact: InformationFlow2): util.Set[InformationFlow2] = {
                        val result = flowFact match {
                            case v@VarSource(source, _, isSourceOf, usedIn, vScope) => {
                                val callParamMatches = fCallParamsToFDefParams.filter(callParamToDefParam => uses(callParamToDefParam._1).exists(source.equals))
                                val genSet = callParamMatches.flatMap(callParamMatch =>
                                    callParamMatch._1.foldLeft(List[InformationFlow2]())((genSrc, expr) =>
                                        callParamMatch._2.foldLeft(genSrc)((genSrc, pDef) => {
                                            val genSource = VarSource(pDef.entry.decl.getId, currOpt, List(), List(), SCOPE_LOCAL)
                                            val sourceOf = VarSourceOf(pDef.entry.decl.getId, currOpt, v, List(), SCOPE_LOCAL)
                                            genSource :: sourceOf :: genSrc
                                        })))
                                if (vScope == SCOPE_GLOBAL) GEN(v :: genSet) else GEN(genSet)
                            }
                            case vo@VarSourceOf(id, _, source, usedIn, voScope) => {
                                val callParamMatches = fCallParamsToFDefParams.filter(callParamToDefParam => uses(callParamToDefParam._1).exists(id.equals))
                                val genSet = callParamMatches.flatMap(callParamMatch =>
                                    callParamMatch._1.foldLeft(List[InformationFlow2]())((genSrc, expr) =>
                                        callParamMatch._2.foldLeft(genSrc)((genSrc, pDef) => {
                                            val genSource = VarSource(pDef.entry.decl.getId, currOpt, List(), List(), SCOPE_LOCAL)
                                            val sourceOf = VarSourceOf(pDef.entry.decl.getId, currOpt, source, List(), SCOPE_LOCAL)
                                            genSource :: sourceOf :: genSrc
                                        })))
                                if (voScope == SCOPE_GLOBAL) GEN(vo :: genSet) else GEN(genSet)
                            }
                            case z: Zero if !initialSeedsExists(destinationMethod.entry) =>
                                // Introduce Global Variables from linked file
                                filesWithSeeds = filesWithSeeds + destinationMethod.entry.getFile.getOrElse("")
                                GEN(getZeroFactWithFlow(z) :: globalsAsInitialSeedsL(destinationMethod))
                            case s: Source if s.getScope == SCOPE_GLOBAL => GEN(s)
                            case z: Zero => GEN(getZeroFactWithFlow(z))
                            case x => super.computeTargets(x)
                        }
                        result
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
            override def getReturnFlowFunction(callSite: Opt[AST], calleeMethod: Opt[FunctionDef], exitStmt: Opt[AST], returnSite: Opt[AST]): FlowFunction[InformationFlow2] = {
                def default(flowFact: InformationFlow2) = KILL
                lazy val fCall = filterASTElems[FunctionCall](callSite)
                lazy val fCallOpt = parentOpt(callSite.entry, interproceduralCFG.getASTEnv(callSite)).asInstanceOf[Opt[AST]]
                lazy val exitOpt = parentOpt(exitStmt.entry, interproceduralCFG.getASTEnv(exitStmt)).asInstanceOf[Opt[AST]]
                lazy val pointerParamNames = getPointerFDefParamNames(calleeMethod)


                if (interproceduralCFG.getOptions.pseudoVisitingSystemLibFunctions && calleeMethod.entry.getName.equalsIgnoreCase(SPLLIFT_PSEUDO_SYSTEM_FUNCTION_CALL_NAME))
                    return pseudoSystemFunctionCallReturnFlow

                exitStmt.entry match {
                    case ReturnStatement(_) =>
                    case _ => WarningsCache.add("Exiting " + calleeMethod.entry.getName + " without return statement.")
                }

                def assignsReturnVariablesTo(callStmt: AST, returnStatement: AST): List[(Id, List[Id])] = assignsVariables(callStmt).flatMap(assign => if (assign._2.exists(isPartOfTerm(_, fCall))) Some((assign._1, uses(returnStatement))) else None)
                val assignments = assignsReturnVariablesTo(callSite.entry, exitStmt.entry)

                new InfoFlowFunction(exitStmt, callSite, default) {
                    override def computeTargets(flowFact: InformationFlow2): util.Set[InformationFlow2] = {
                        val result = flowFact match {
                            case v@VarSource(source, _, isSourceOf, usedIn, vscope) =>
                                val genSet = assignments.flatMap(assignment => assignment._2.flatMap {
                                    case x if source.equals(x) =>
                                        val assignee = assignment._1
                                        val scope = interproceduralCFG.getTS(callSite).lookupEnv(callSite.entry).varEnv.lookupScope(assignee.name).select(currOpt.condition.collectDistinctFeatures)
                                        val newSource = VarSource(assignee, fCallOpt, List(), List(), scope)
                                        val sourceOf = VarSourceOf(assignee, fCallOpt, v, List(), scope)
                                        val sink = SinkToAssignment(fCallOpt, v, assignee)
                                        List(newSource, sourceOf, sink)
                                    case _ => None
                                })

                                if (vscope == SCOPE_GLOBAL) GEN(v :: genSet) else GEN(genSet)

                            case vo@VarSourceOf(id, _, source, usedIn, vo_scope) =>
                                val genSet = assignments.flatMap(assignment => assignment._2.flatMap {
                                    case x if id.equals(x) =>
                                        val assignee = assignment._1
                                        val scope = interproceduralCFG.getTS(callSite).lookupEnv(assignee).varEnv.lookupScope(assignee.name).select(currOpt.condition.collectDistinctFeatures)
                                        val newSource = VarSource(assignee, fCallOpt, List(), List(), scope)
                                        val sourceOf = VarSourceOf(assignee, fCallOpt, source, List(), scope)
                                        val sink = SinkToAssignment(fCallOpt, source, assignee)
                                        List(newSource, sourceOf, sink)
                                    case _ => None
                                })

                                if (vo_scope == SCOPE_GLOBAL) GEN(vo :: genSet) else GEN(genSet)

                            case s: Sink => GEN(s)
                            case s: Source if s.getScope == SCOPE_GLOBAL => GEN(s)
                            case z: Zero => KILL
                            case x => super.computeTargets(flowFact)
                        }
                        result
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
            override def getCallToReturnFlowFunction(callSite: Opt[AST], returnSite: Opt[AST]): FlowFunction[InformationFlow2] = {
                def default(flowFact: InformationFlow2) = GEN(flowFact)

                new InfoFlowFunction(callSite, returnSite, default) {
                    override def computeTargets(flowFact: InformationFlow2): util.Set[InformationFlow2] =
                        flowFact match {
                            case s: Source if s.getScope == SCOPE_GLOBAL => KILL // isGlobal -> Kill
                            case s: Source if currDefines.exists(s.getId.equals) => KILL // Kill this fact, as it is handled at return flow
                            case x => super.computeTargets(x)
                        }
                }
            }
        }

    private abstract class InfoFlowFunction(curr: Opt[AST], succ: Opt[AST], default: InformationFlow2 => util.Set[InformationFlow2]) extends FlowFunction[InformationFlow2] {
        // Lazy cache some repeatedly used variables
        lazy val currASTEnv = interproceduralCFG.getASTEnv(curr)
        lazy val currOpt: Opt[AST] = parentOpt(curr.entry, currASTEnv).asInstanceOf[Opt[AST]]
        lazy val currTS = interproceduralCFG.getTS(curr)

        lazy val succTS = interproceduralCFG.getTS(succ)
        lazy val succVarEnv = succ.entry match {
            case f: FunctionDef => succTS.lookupEnv(f.stmt.innerStatements.headOption.getOrElse(currOpt).entry)
            case _ => currTS.lookupEnv(succ.entry)
        }

        lazy val currDefines = defines(curr)
        lazy val currUses = uses(curr)
        lazy val currAssignments = assignsVariables(curr)
        lazy val currStructFieldAssigns = assignsField(curr)
        lazy val currStructFieldUses = usesField(curr)

        def hasSelfAssignement(source: Id, assignee: Id, assignors: List[Id]): Boolean = assignee.equals(source) && assignors.exists(source.equals)

        /*
         * We are using the variability-aware typesystem of TypeChef. However, variability encoded within the type definition of an variable or struct does not matter for us.
         * As a consequence we only visit one type-definition as we do assume correct type assignments.
         */
        def singleVisitOnSourceTypes[T <: InformationFlow2](currId: Id, structFun: (Id => List[T]), varFun: (Id => List[T])): List[T] = {
            val cTypes = succVarEnv.varEnv.lookupType(currId.name)
            var cFacts: List[T] = List()

            // Do not generate sources for every possible type condition; only once for either struct or variable
            if (cTypes.exists(ct => isStructOrUnion(ct)))
                cFacts :::= structFun(currId)

            if (cTypes.exists(ct => !isStructOrUnion(ct)))
                cFacts :::= varFun(currId)

            cFacts
        }

        private def isStructOrUnion(cType: CType): Boolean =
            cType.atype match {
                case CPointer(t) => isStructOrUnion(t) // simple pointer detection - really, really cheap coding
                case _: CStruct | _: CAnonymousStruct => true
                case _ => false
            }

        override def computeTargets(flowFact: InformationFlow2): util.Set[InformationFlow2] =
            flowFact match {
                case z: Zero => GEN(z)
                case x => default(x)
            }
    }

    private abstract class CallFlowFunction(callStmt: Opt[AST], destinationMethod: Opt[FunctionDef], default: InformationFlow2 => util.Set[InformationFlow2]) extends InfoFlowFunction(callStmt, destinationMethod, default) {
        def mapCallParamToFDefParam(callParams: List[Opt[Expr]], fDefParams: List[Opt[ParameterDeclarationD]], res: List[Opt[(Id, Id)]] = List()): List[Opt[(Id, Id)]] = {
            if (callParams.isEmpty && fDefParams.isEmpty) return res

            val currentCallParameter = callParams.head
            val currentFDefParameter = fDefParams.head
            val currentParameterMatchCondition = currentCallParameter.condition.and(currentFDefParameter.condition)

            val currRes =
                if (currentParameterMatchCondition.isSatisfiable(interproceduralCFG.getFeatureModel))
                    currentCallParameter.entry match {
                        case i: Id => Option(Opt(currentParameterMatchCondition, (i, currentFDefParameter.entry.decl.getId)))
                        case PointerCreationExpr(i: Id) => Option(Opt(currentParameterMatchCondition, (i, currentFDefParameter.entry.decl.getId)))
                        case PointerDerefExpr(i: Id) => Option(Opt(currentParameterMatchCondition, (i, currentFDefParameter.entry.decl.getId)))
                        case c: Constant => Option(Opt(currentParameterMatchCondition, (Id("constant"), currentFDefParameter.entry.decl.getId)))
                        case s: SizeOfExprU => Option(Opt(currentParameterMatchCondition, (Id("sizeU"), currentFDefParameter.entry.decl.getId)))
                        case missed => throw new IllegalArgumentException("No rule defined for converting expression to parameter mapping: " + missed + "\n" + callStmt)
                    }
                else None

            if (currRes.isDefined)
                mapCallParamToFDefParam(callParams.tail, fDefParams.tail, currRes.get :: res)
            else if (callParams.size < fDefParams.size)
                mapCallParamToFDefParam(callParams, fDefParams.tail, res)
            else
                mapCallParamToFDefParam(callParams.tail, fDefParams, res)
        }

        def matchCallParamsToDefParams[T, U](callParams: List[Opt[T]], defParams: List[Opt[U]]): List[(List[Opt[T]], List[Opt[U]])] = {
            val callPs = groupOptListVAware(callParams, interproceduralCFG.getFeatureModel)
            val defPs = groupOptListVAware(defParams, interproceduralCFG.getFeatureModel)

            // deal with variadic functions
            def defParamHasVarArgs: Boolean =
            defPs.lastOption match {
                case Some(l) if l.exists {
                    case Opt(_, v: VarArgs) => true
                    case _ => false
                } => true
                case _ => false
            }

            def defParamIsVoidSpecifier: Boolean =
                (defPs.size == 1) && defPs.head.exists {
                    case Opt(_, _: VoidSpecifier) | Opt(_, PlainParameterDeclaration(List(Opt(_, _: VoidSpecifier)), _)) => true
                    case _ => false
                }

            if ((callPs.size != defPs.size) && defParamHasVarArgs) callPs.map((_, defPs.head))
            else if ((callPs.size != defPs.size) && defParamIsVoidSpecifier) List()
            else {
                if (callPs.size != defPs.size)
                    WarningsCache.add("Call and function parameter sizes does not match for: " + currOpt
                      + "\n" + callPs.toString + "\n" + defPs.toString)

                callPs zip defPs
            }
        }

        private val fCall = filterASTElems[FunctionCall](callStmt.entry).head
        private val fCallExprs = fCall.params.exprs
        private val fDefParams = getFDefParameters(destinationMethod)
        val fCallParamsToFDefParams = matchCallParamsToDefParams(fCallExprs, fDefParams)
    }

    private def globalsAsInitialSeeds(fDef: Opt[FunctionDef]): util.Set[InformationFlow2] = GEN(globalsAsInitialSeedsL(fDef) :+ zeroValue())

    private def globalsAsInitialSeedsL(fDef: Opt[FunctionDef]): List[InformationFlow2] = {
        val globalVariables = interproceduralCFG.getTUnit(fDef).defs.filterNot {
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
            if (decls.nonEmpty) decls.map(decl => VarSource(decl, x, List(), List(), SCOPE_GLOBAL))
            else None
        })

        globalInfoFlowFacts
    }
}