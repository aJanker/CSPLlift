package de.fosd.typechef.cspllift.cifdsproblem.informationflow

import java.util

import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.cspllift.analysis.{Edge, Node, SuperCallGraph}
import de.fosd.typechef.cspllift.cifdsproblem.informationflow.flowfact.{Source, _}
import de.fosd.typechef.cspllift.cifdsproblem.{CIFDSProblem, CZeroFact}
import de.fosd.typechef.cspllift.commons.WarningsCache
import de.fosd.typechef.cspllift.{CICFGFDef, CICFGStmt, CInterCFG}
import de.fosd.typechef.featureexpr.bdd.BDDFeatureExprFactory
import de.fosd.typechef.parser.c._
import de.fosd.typechef.typesystem._
import heros.{FlowFunction, FlowFunctions}

import scala.collection.JavaConverters._

class InformationFlowProblem(cICFG: CInterCFG) extends CIFDSProblem[InformationFlowFact](cICFG) with InformationFlowConfiguration with InformationFlowProblemOperations {

    private var initialGlobalsFile: String = ""

    private var filesWithSeeds: Set[String] = Set()

    /**
      * Returns initial seeds to be used for the analysis. This is a mapping of statements to initial analysis facts.
      * We consider global variables as initial sources.
      */
    override def initialSeeds(): util.Map[CICFGStmt[AST], util.Set[InformationFlowFact]] = {
        val initialSeeds = new util.HashMap[CICFGStmt[AST], util.Set[InformationFlowFact]]()
        interproceduralCFG.getEntryFunctions.foldLeft(initialSeeds)((seeds, entryFunction) => {
            interproceduralCFG.getStartPointsOf(entryFunction).asScala.foreach(seeds.put(_, globalsAsInitialSeeds(entryFunction)))
            initialGlobalsFile = entryFunction.method.entry.getFile.getOrElse("")
            filesWithSeeds = filesWithSeeds + initialGlobalsFile
            seeds
        })
    }

    private def initialSeedsExists(destinationMethod: FunctionDef): Boolean = {
        val destinationMethodFile = destinationMethod.getFile.getOrElse("")
        initialGlobalsFile.equalsIgnoreCase(destinationMethodFile) || filesWithSeeds.exists(destinationMethodFile.equalsIgnoreCase)
    }

    private def globalsAsInitialSeeds(fDef: CICFGFDef): util.Set[InformationFlowFact] = GEN(globalsAsInitialSeedsL(fDef) :+ zeroValue())

    private def globalsAsInitialSeedsL(fDef: CICFGFDef): List[InformationFlowFact] = {
        val globalVariables = interproceduralCFG.getTUnit(fDef).defs.filterNot {
            // Ignore function and typedef definitions
            case Opt(_, f: FunctionDef) => true //
            case Opt(_, d: Declaration) =>
                d.declSpecs.exists {
                    case Opt(_, t: TypedefSpecifier) => true
                    case _ => false
                }
            case _ => false
        }

        val globalInfoFlowFacts = globalVariables.flatMap(x => {
            val decls = declares(x)

            // Note: we ignore the actual file of the declaration as it may be declared in a header file.
            // As variables declared in header files may be included across several files, this way prevents matching errors.
            if (decls.nonEmpty) decls.map(decl => SourceDefinition(Variable(decl), x, SCOPE_GLOBAL, Some(decl)))
            else None
        })

        globalInfoFlowFacts
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

    override def zeroValue(): InformationFlowFact with CZeroFact = zeroVal

    /**
      * Returns a set of flow functions. Those functions are used to compute data-flow facts
      * along the various kinds of control flows.
      *
      * <b>NOTE:</b> this method could be called many times. Implementations of this
      * interface should therefore cache the return value!
      */
    override def flowFunctions(): FlowFunctions[CICFGStmt[AST], InformationFlowFact, CICFGFDef] = cachedFlowFunctions

    private val cachedFlowFunctions: FlowFunctions[CICFGStmt[AST], InformationFlowFact, CICFGFDef] =
        new FlowFunctions[CICFGStmt[AST], InformationFlowFact, CICFGFDef] {

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
            override def getNormalFlowFunction(curr: CICFGStmt[AST], succ: CICFGStmt[AST]): FlowFunction[InformationFlowFact] = {
                def default(flowFact: InformationFlowFact) = GEN(flowFact)

                new InfoFlowFunction(curr, succ, default) {
                    override def computeTargets(flowFact: InformationFlowFact): util.Set[InformationFlowFact] = {
                        val result = flowFact match {
                            case s: Source => s.getType match {
                                case _: Variable => computeVariable(s)
                                case _: Struct => computeStruct(s)
                                case _ => super.computeTargets(s)
                            }
                            case z: Zero if currAssignments.nonEmpty || currStructFieldAssigns.nonEmpty =>
                                // gen source for all new assignments
                                GEN(z :: assignmentSources)
                            case z: Zero if currDefines.nonEmpty =>
                                // newly introduced variable or struct
                                GEN(z :: defineSources)
                            case x => super.computeTargets(x)
                        }

                        result
                    }

                    private def computeStruct(source: Source): util.Set[InformationFlowFact] = {
                        assert(source.getType.isInstanceOf[Struct], "Computation source must be a struct.")
                        lazy val copy = copySource(source, currOpt)
                        lazy val currSourceDefinition = getSourceDefinition(source)
                        lazy val varName = source.getType.getName
                        lazy val field = source.getType.asInstanceOf[Struct].field

                        if (!currStatementIsAssignment) {
                            if (field.isDefined && currStructFieldUses.exists(use => isFullFieldMatch(source, use))) GEN(copy :: SinkToUse(currOpt, currSourceDefinition) :: Nil) // use of struct.field
                            else if (currUses.exists(id.equals)) GEN(copy :: SinkToUse(currOpt, currSourceDefinition) :: Nil) // plain struct use
                            else KILL
                        } else {
                            // TODO Document each case
                            val usages = {
                                if (currStructFieldUses.exists(use => isFullFieldMatch(source, use))) {
                                    val sources = currAssignments.flatMap {
                                        case (assignee, assignor) if assignor.contains(varName) => getDefineSourcesFromAssignment(assignee)
                                        case _ => None
                                    }

                                    getSinksAndSourcesOf(currSourceDefinition, sources)
                                } else if (currStructFieldAssigns.isEmpty && currStructFieldUses.isEmpty && currAssignments.exists { case (assignee, assignor) => assignor.contains(varName) }) {
                                    val sources = currAssignments.flatMap {
                                        case (assignee, assignor) if assignor.contains(varName) => getDefineSourcesFromAssignment(assignee)
                                        case _ => None
                                    }

                                    getSinksAndSourcesOf(currSourceDefinition, sources)
                                } else KILL
                            }

                            val assignments =
                                if (currFactIsAssignee(source)) KILL
                                else super.computeTargets(copy)

                            GEN(assignments, usages)
                        }
                    }

                    private def computeVariable(source: Source): util.Set[InformationFlowFact] = {
                        assert(source.getType.isInstanceOf[Variable], "Computation source must be a variable.")
                        lazy val copy = copySource(source, currOpt)
                        lazy val currSourceDefinition = getSourceDefinition(source)
                        lazy val varName = source.getType.getName

                        // TODO Document

                        if (!currStatementIsAssignment) {
                            if (currUses.contains(varName)) {
                                val sink = SinkToUse(currOpt, currSourceDefinition)
                                GEN(List(sink, copy))
                            } else super.computeTargets(copy)
                        } else {
                            val usage = {
                                val assignees = currAssignments.filter { case (assignee, assignor) => assignor.contains(varName) }
                                val sources = assignees.flatMap { case (assignee, assignor) => getDefineSourcesFromAssignment(assignee) }

                                getSinksAndSourcesOf(currSourceDefinition, sources)
                            }

                            val assignment =
                                if (currAssignments.exists { case (assignee, assignor) => assignee.equals(varName) } || currDefines.exists(varName.equals)) KILL
                                else super.computeTargets(copy)

                            GEN(assignment, usage)
                        }
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
            override def getCallFlowFunction(callStmt: CICFGStmt[AST], destinationMethod: CICFGFDef): FlowFunction[InformationFlowFact] = {
                val flowCondition = destinationMethod.getStmt.condition.and(callStmt.getStmt.condition)
                val destinationEnv = interproceduralCFG().getASTEnv(destinationMethod)
                val destinationOpt = parentOpt(destinationMethod.getStmt.entry, destinationEnv).asInstanceOf[Opt[FunctionDef]]
                SuperCallGraph.addEge(Edge(Node(interproceduralCFG.getMethodOf(callStmt).getStmt.asInstanceOf[Opt[FunctionDef]]), Node(destinationOpt), flowCondition))

                /*if (interproceduralCFG.getOptions.pseudoVisitingSystemLibFunctions && destinationMethod.entry.getName.equalsIgnoreCase(SPLLIFT_PSEUDO_SYSTEM_FUNCTION_CALL_NAME))
                    return pseudoSystemFunctionCallCallFlowFunction(callStmt, interproceduralCFG.getASTEnv(callStmt), interproceduralCFG) */

                def default(flowFact: InformationFlowFact) = KILL
                def getZeroFactWithFlow(zero: Zero): Zero = zero.copy(flowCondition = flowCondition.and(zero.flowCondition))

                new CallFlowFunction(callStmt, destinationMethod, default) {
                    override def computeTargets(flowFact: InformationFlowFact): util.Set[InformationFlowFact] = {
                        val result = flowFact match {
                            /* case v@VarSource(source, _, vScope, _) => {
                                val callParamMatches = fCallParamsToFDefParams.filter(callParamToDefParam => uses(callParamToDefParam._1).exists(source.equals))
                                val genSet = callParamMatches.flatMap(callParamMatch =>
                                    callParamMatch._1.foldLeft(List[InformationFlowFact]())((genSrc, expr) =>
                                        callParamMatch._2.foldLeft(genSrc)((genSrc, pDef) => {
                                            val genSource = VarSource(pDef.entry.decl.getId, currOpt, SCOPE_LOCAL, Some(currOpt.entry))
                                            val sourceOf = VarSourceOf(pDef.entry.decl.getId, currOpt, v, SCOPE_LOCAL, Some(currOpt.entry))
                                            genSource :: sourceOf :: genSrc
                                        })))
                                if (vScope == SCOPE_GLOBAL) GEN(v.copy(last = Some(currOpt.entry)) :: genSet) else GEN(genSet)
                            }
                            case vo@VarSourceOf(id, _, source, voScope, _) => {
                                val callParamMatches = fCallParamsToFDefParams.filter(callParamToDefParam => uses(callParamToDefParam._1).exists(id.equals))
                                val genSet = callParamMatches.flatMap(callParamMatch =>
                                    callParamMatch._1.foldLeft(List[InformationFlowFact]())((genSrc, expr) =>
                                        callParamMatch._2.foldLeft(genSrc)((genSrc, pDef) => {
                                            val genSource = VarSource(pDef.entry.decl.getId, currOpt, SCOPE_LOCAL, Some(currOpt.entry))
                                            val sourceOf = VarSourceOf(pDef.entry.decl.getId, currOpt, source, SCOPE_LOCAL, Some(currOpt.entry))
                                            genSource :: sourceOf :: genSrc
                                        })))
                                if (voScope == SCOPE_GLOBAL) GEN(vo.copy(last = Some(currOpt.entry)) :: genSet) else GEN(genSet)
                            } */
                            case s: Source if s.getScope == SCOPE_GLOBAL => GEN(s)
                            case z: Zero if !initialSeedsExists(destinationMethod.method.entry) =>
                                // Introduce Global Variables from linked file
                                filesWithSeeds = filesWithSeeds + destinationMethod.method.entry.getFile.getOrElse("")
                                GEN(getZeroFactWithFlow(z) :: globalsAsInitialSeedsL(destinationMethod))
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
            override def getReturnFlowFunction(callSite: CICFGStmt[AST], calleeMethod: CICFGFDef, exitStmt: CICFGStmt[AST], returnSite: CICFGStmt[AST]): FlowFunction[InformationFlowFact] = {
                def default(flowFact: InformationFlowFact) = KILL
                lazy val flowCondition = callSite.getStmt.condition

                lazy val fCall = filterASTElems[FunctionCall](callSite)
                lazy val fCallOpt = parentOpt(callSite.getStmt.entry, interproceduralCFG.getASTEnv(callSite)).asInstanceOf[Opt[AST]]
                lazy val exitOpt = parentOpt(exitStmt.getStmt.entry, interproceduralCFG.getASTEnv(exitStmt)).asInstanceOf[Opt[AST]]
                lazy val pointerParamNames = getPointerFDefParamNames(calleeMethod.method)


                if (interproceduralCFG.getOptions.pseudoVisitingSystemLibFunctions && calleeMethod.method.entry.getName.equalsIgnoreCase(SPLLIFT_PSEUDO_SYSTEM_FUNCTION_CALL_NAME))
                    return pseudoSystemFunctionCallReturnFlow

                exitStmt.getStmt.entry match {
                    case ReturnStatement(_) =>
                    case _ => WarningsCache.add("Exiting " + calleeMethod.method.entry.getName + " without return statement.")
                }

                def assignsReturnVariablesTo(callStmt: AST, returnStatement: AST): List[(Id, List[Id])] = assignsVariables(callStmt).flatMap(assign => if (assign._2.exists(isPartOfTerm(_, fCall))) Some((assign._1, uses(returnStatement))) else None)
                val assignments = assignsReturnVariablesTo(callSite.getStmt.entry, exitStmt.getStmt.entry)

                new InfoFlowFunction(exitStmt, callSite, default) {
                    override def computeTargets(flowFact: InformationFlowFact): util.Set[InformationFlowFact] = {
                        val result = flowFact match {
                            /* case v@VarSource(source, _, vscope, _) =>
                                val genSet = assignments.flatMap(assignment => assignment._2.flatMap {
                                    case x if source.equals(x) =>
                                        val assignee = assignment._1
                                        val scope = interproceduralCFG.getTS(callSite).lookupEnv(callSite.getStmt.entry).varEnv.lookupScope(assignee.name).select(currSatisfiableCondition)
                                        val newSource = VarSource(assignee, fCallOpt, scope, Some(currOpt.entry))
                                        val sourceOf = VarSourceOf(assignee, fCallOpt, v, scope, Some(currOpt.entry))
                                        val sink = SinkToAssignment(fCallOpt, v, assignee)
                                        List(newSource, sourceOf, sink)
                                    case _ => None
                                })

                                if (vscope == SCOPE_GLOBAL) GEN(v.copy(last = Some(currOpt.entry)) :: genSet) else GEN(genSet)

                            case vo@VarSourceOf(id, _, source, vo_scope, _) =>
                                val genSet = assignments.flatMap(assignment => assignment._2.flatMap {
                                    case x if id.equals(x) =>
                                        val assignee = assignment._1
                                        val scope = interproceduralCFG.getTS(callSite).lookupEnv(assignee).varEnv.lookupScope(assignee.name).select(currSatisfiableCondition)
                                        val newSource = VarSource(assignee, fCallOpt, scope, Some(currOpt.entry))
                                        val sourceOf = VarSourceOf(assignee, fCallOpt, source, scope, Some(currOpt.entry))
                                        val sink = SinkToAssignment(fCallOpt, source, assignee)
                                        List(newSource, sourceOf, sink)
                                    case _ => None
                                })

                                if (vo_scope == SCOPE_GLOBAL) GEN(vo.copy(last = Some(currOpt.entry)) :: genSet) else GEN(genSet) */

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
            override def getCallToReturnFlowFunction(callSite: CICFGStmt[AST], returnSite: CICFGStmt[AST]): FlowFunction[InformationFlowFact] = {
                def default(flowFact: InformationFlowFact) = GEN(flowFact)

                new InfoFlowFunction(callSite, returnSite, default) {
                    override def computeTargets(flowFact: InformationFlowFact): util.Set[InformationFlowFact] =
                        flowFact match {
                            case s: Source => s match {
                                case cs: Source if s.getScope == SCOPE_GLOBAL => KILL // Kill this fact, as it is handled at return flow
                                case s: Source => s.getType match {
                                    case Struct(_, Some(_)) if currStructFieldAssigns.exists(isPartFieldMatch(s, _)) => KILL // Kill this fact, as it is handled at return flow
                                    case _: Variable | Struct(_, None) if currDefines.exists(s.getType.getName.equals) => KILL // Kill this fact, as it is handled at return flow
                                    case _ => super.computeTargets(flowFact)
                                }
                                case _ => super.computeTargets(flowFact)
                            }
                            case _ => super.computeTargets(flowFact)
                        }
                }
            }
        }

    private abstract class InfoFlowFunction(curr: CICFGStmt[AST], succ: CICFGStmt[AST], default: InformationFlowFact => util.Set[InformationFlowFact]) extends FlowFunction[InformationFlowFact] {
        val currStmt = curr.getStmt
        val succStmt = succ.getStmt

        // Lazy caching of some repeatedly used variables
        lazy val currASTEnv = interproceduralCFG.getASTEnv(curr)
        lazy val currOpt: Opt[AST] = parentOpt(curr.getStmt.entry, currASTEnv).asInstanceOf[Opt[AST]]
        lazy val currTS = interproceduralCFG.getTS(curr)

        lazy val succTS = interproceduralCFG.getTS(succ)
        lazy val succVarEnv = succ.getStmt.entry match {
            case f: FunctionDef => succTS.lookupEnv(f.stmt.innerStatements.headOption.getOrElse(currOpt).entry)
            case _ => currTS.lookupEnv(succ.getStmt.entry)
        }

        lazy val currDefines = defines(currStmt)
        lazy val currUses = uses(currStmt)
        lazy val currAssignments = assignsVariables(currStmt)
        lazy val currStructFieldAssigns = assignsField(currStmt)
        lazy val currStructFieldUses = usesField(currStmt)
        lazy val currStatementIsAssignment: Boolean = currDefines.nonEmpty || currAssignments.nonEmpty || currStructFieldAssigns.nonEmpty

        private lazy val defaultCondition = (BDDFeatureExprFactory.TrueB.collectDistinctFeatureObjects, BDDFeatureExprFactory.FalseB.collectDistinctFeatureObjects)
        private lazy val currSatisfiableCondition_O = currOpt.condition.getSatisfiableAssignment(interproceduralCFG.getFeatureModel, currOpt.condition.collectDistinctFeatureObjects, preferDisabledFeatures = false)
        lazy val currSatisfiableCondition = currSatisfiableCondition_O.getOrElse(defaultCondition)._1.map(_.feature).toSet

        lazy val defineSources = currDefines.flatMap(genSource)
        lazy val assignmentSources =
            if (currStructFieldAssigns.isEmpty) currAssignments.flatMap { case (assignee, _) => genSource(assignee) }
            else currStructFieldAssigns.flatMap { case (field, parents) => genSourceForField(field, parents) }

        def getDefineSourcesFromAssignment(define: Id): List[SourceDefinition] = assignmentSources.filter(_.getType.getName.equals(define))

        def currFactIsAssignee(fact: Source): Boolean =
            fact match {
                case s: Source if (s.getType match {
                    case Struct(_, Some(_)) => true
                    case _ => false
                }) => currStructFieldAssigns.exists(assignment => isFullFieldMatch(fact, assignment) || isPartFieldMatch(fact, assignment))
                case s: Source => currDefines.exists(s.getType.getName.equals) || currAssignments.exists { case (assignee, _) => s.getType.getName.equals(assignee) }
                case _ => false
            }

        def getCurrentScope(id: Id): Int = 1 //currTS.lookupEnv(curr.getStmt.entry).varEnv.lookupScope(id.name).select(currSatisfiableCondition)

        def getSinksAndSourcesOf(currSourceDefinition: SourceDefinition, sources: List[SourceDefinition]): util.Set[InformationFlowFact] = {
            val sourcesOf = sources.flatMap {
                case s: Source => Some(SourceDefinitionOf(s.getType, currOpt, currSourceDefinition, s.getScope, Some(currOpt.entry)))
                case _ => None
            }

            val sinks = sources.map(genSource => SinkToAssignment(currOpt, currSourceDefinition, genSource.getType.getName))

            GEN(sourcesOf ::: sinks)
        }

        private def genVarSource(scope: Int)(define: Id): List[SourceDefinition] = List(SourceDefinition(Variable(define), currOpt, scope, Some(currOpt.entry)))

        private def genStructSource(scope: Int)(define: Id): List[SourceDefinition] = List(SourceDefinition(Struct(define, None), currOpt, scope, Some(currOpt.entry)))

        private def genSource(define: Id): List[SourceDefinition] = {
            val scope = getCurrentScope(define)
            singleVisitOnSourceTypes(define, genStructSource(scope), genVarSource(scope))
        }

        private def genSourceForField(field: Id, parents: List[Id]): List[SourceDefinition] = {
            val fieldSources = genSource(field)
            val parentSources = parents.reverse.flatMap(genSource)

            fieldSources.map(cFieldSource =>
                parentSources.foldLeft(cFieldSource) {
                    case (fieldSource, s: Source) => SourceDefinition(Struct(s.getType.getName, Some(fieldSource)), s.getStmt, s.getScope, s.getPreviousStmt)
                    // case (fieldSource, parentSource: Source(_ : Struct, _, _ , _)) => parentSource.copy(field = Some(fieldSource))
                    //case (fieldSource, parentSource: Source(_ : Variable, _, _ , _))) => StructSource(parentSource.getId, Some(fieldSource), parentSource.getStmt, parentSource.getScope, Some(currOpt.entry))
                    case (fieldSource, _) => fieldSource
                })
        }

        /*
         * We are using the variability-aware typesystem of TypeChef. However, variability encoded within the type definition of an variable or struct does not matter for us.
         * As a consequence we only visit one type-definition as we do assume correct type assignments.
         */
        private def singleVisitOnSourceTypes[T <: InformationFlowFact](currId: Id, structFun: (Id => List[T]), varFun: (Id => List[T])): List[T] = {
            val cTypes = succVarEnv.varEnv.lookupType(currId.name)
            var cFacts: List[T] = List()

            // Do not generate sources for every possible type condition; only once for either struct or variable
            if (cTypes.exists(ct => isStructOrUnion(ct)))
                cFacts :::= structFun(currId)

            if (cTypes.exists(ct => !isStructOrUnion(ct) && !isUnknownType(ct)))
                cFacts :::= varFun(currId)

            cFacts
        }

        private def isUnknownType(cType: CType): Boolean =
            cType.atype match {
                case _: CUnknown => true
                case _ => false
            }

        private def isStructOrUnion(cType: CType): Boolean =
            cType.atype match {
                case CPointer(t) => isStructOrUnion(t) // simple pointer detection - really, really cheap coding
                case _: CStruct | _: CAnonymousStruct => true
                case _ => false
            }

        override def computeTargets(flowFact: InformationFlowFact): util.Set[InformationFlowFact] =
            flowFact match {
                case z: Zero => GEN(z)
                case x => default(x)
            }
    }

    private abstract class CallFlowFunction(callStmt: CICFGStmt[AST], destinationMethod: CICFGFDef, default: InformationFlowFact => util.Set[InformationFlowFact]) extends InfoFlowFunction(callStmt, destinationMethod, default) {
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

        private val fCall = filterASTElems[FunctionCall](callStmt.getStmt.entry).head
        private val fCallExprs = fCall.params.exprs
        private val fDefParams = getFDefParameters(destinationMethod.method)

        val fCallParamsToFDefParams = matchCallParamsToDefParams(fCallExprs, fDefParams)
    }

}