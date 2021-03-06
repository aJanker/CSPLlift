package de.fosd.typechef.cspllift.cifdsproblem.informationflow

import java.util

import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.cspllift.analysis.{Edge, Node, SuperCallGraph}
import de.fosd.typechef.cspllift.cifdsproblem.CIFDSProblem
import de.fosd.typechef.cspllift.cifdsproblem.informationflow.flowfact._
import de.fosd.typechef.cspllift.cifdsproblem.informationflow.flowfact.sinkorsource._
import de.fosd.typechef.cspllift.cifdsproblem.informationflow.flowfunction.{IFCallFlowFunction, IFDefaultFlowFunction}
import de.fosd.typechef.cspllift.cintercfg._
import de.fosd.typechef.parser.c._
import heros.{FlowFunction, FlowFunctions}
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.JavaConverters._

/**
  * Assignment based information-flow analysis as example IFDS problem for the C connector to the IFDS/IDE solver Heros.
  */
class InformationFlowProblem(cICFG: CInterCFG, globalSources: List[InformationFlowFact] = List()) extends CIFDSProblem[InformationFlowFact](cICFG, globalSources) with InformationFlowConfiguration with InformationFlowProblemOperations {

    private lazy val logger: Logger = LoggerFactory.getLogger(getClass)

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
    private val zero: InformationFlowFact = Zero()

    override def zeroValue(): InformationFlowFact = zero

    /**
      * Returns initial seeds to be used for the analysis. This is a mapping of statements to initial analysis facts.
      * We consider global variables as initial sources.
      */
    override def initialSeeds(): util.Map[CInterCFGNode, util.Set[InformationFlowFact]] = {
        val initialSeeds = new util.HashMap[CInterCFGNode, util.Set[InformationFlowFact]]()

        interproceduralCFG.getEntryFunctions.foreach {
            entryFunction => interproceduralCFG.getStartPointsOf(entryFunction).asScala.foreach(initialSeeds.put(_, GEN(zeroValue() :: globalSources)))
        }

        initialSeeds
    }

    /**
      * Returns a set of flow functions. Those functions are used to compute data-flow facts
      * along the various kinds of control flows.
      *
      * <b>NOTE:</b> this method could be called many times. Implementations of this
      * interface should therefore cache the return value!
      */
    override def flowFunctions(): FlowFunctions[CInterCFGNode, InformationFlowFact, CInterCFGFDef] = flowFunctionFactory

    private lazy val flowFunctionFactory: FlowFunctions[CInterCFGNode, InformationFlowFact, CInterCFGFDef] = new FlowFunctions[CInterCFGNode, InformationFlowFact, CInterCFGFDef] {

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
        override def getNormalFlowFunction(curr: CInterCFGNode, succ: CInterCFGNode): FlowFunction[InformationFlowFact] = {
            new IFDefaultFlowFunction(interproceduralCFG, curr, succ) {
                override def computeStruct(source: Source): util.Set[InformationFlowFact] = {
                    assert(source.getType.isInstanceOf[Struct], "Computation source must be a struct.")
                    lazy val currSourceDefinition = getDefinition(source)
                    lazy val structName = source.getType.getName
                    lazy val field = source.getType.asInstanceOf[Struct].field

                    if (!currStatementIsAssignment) {
                        if (field.isDefined && currStructFieldUses.exists(use => isFullFieldMatch(source, use))) GEN(source :: SinkToUse(curr, source) :: Nil) // use of struct.field
                        else if (currUses.exists(id.equals)) GEN(source :: SinkToUse(curr, source) :: Nil) // plain struct use
                        else GEN(source)
                    } else {
                        // TODO Document each case
                        val usages = {
                            if (currStructFieldUses.exists(use => isFullFieldMatch(source, use))) {
                                val sources = currAssignments.flatMap {
                                    case (assignee, assignor) if assignor.contains(structName) => getDefineSourcesFromAssignment(assignee)
                                    case _ => None
                                }
                                GEN(getSinksAndSourcesOf(currSourceDefinition, sources))
                            } else if (currStructFieldAssigns.isEmpty && currStructFieldUses.isEmpty && currAssignments.exists { case (assignee, assignor) => assignor.contains(structName) }) {
                                // TODO Clean ConditionalEdgeFunction
                                val sources = currAssignments.flatMap {
                                    case (assignee, assignor) if assignor.contains(structName) => getDefineSourcesFromAssignment(assignee)
                                    case _ => None
                                }
                                val sinksAndSources = getSinksAndSourcesOf(currSourceDefinition, sources)

                                // TODO Documention of struct field value copy
                                val sinksAndSourcesWithFields = sinksAndSources.map {
                                    case s: SourceDefinitionOf if s.getType.isInstanceOf[Struct] =>
                                        val orgType = s.getType.asInstanceOf[Struct]
                                        val orgSource = s.getDefinition
                                        orgSource.getType match {
                                            case st: Struct => s.copy(sourceType = orgType.copy(field = st.field))
                                            case _ => s
                                        }
                                    case x => x
                                }
                                GEN(sinksAndSourcesWithFields)
                            } else KILL
                        }

                        val assignments =
                            if (currFactIsAssignee(source) && getCurrentScope(structName).exists(scope => (scope <= source.getScope) || (source.getScope == SCOPE_UNKNOWN))) KILL
                            else GEN(source)

                        GEN(assignments, usages)
                    }
                }

                override def computeVariable(source: Source): util.Set[InformationFlowFact] = {
                    assert(source.getType.isInstanceOf[Variable], "Computation source must be a variable.")
                    lazy val currSourceDefinition = getDefinition(source)
                    lazy val varName = source.getType.getName

                    if (!currStatementIsAssignment) {
                        if (currUses.contains(varName)) GEN(SinkToUse(curr, source) :: source :: Nil)
                        else GEN(source)
                    } else {
                        val rightHandSide = {
                            val sources =
                                if (currAssignments.nonEmpty) {
                                    val assignees = currAssignments.filter { case (assignee, assignor) => assignor.contains(varName) }
                                    assignees.flatMap { case (assignee, assignor) => getDefineSourcesFromAssignment(assignee) }
                                } else if (currStructFieldAssigns.nonEmpty && currUses.contains(varName)) currStructFieldAssigns.flatMap(assign => getDefineSourcesFromAssignment(assign._2.head))
                                else List()

                            val sourcesOf =
                                if (isOnlyUsedAsArrayAccess(currSourceDefinition.sourceType.getName, currUses, currASTEnv)) List()
                                else sources.flatMap {
                                    case s: Source => Some(SourceDefinitionOf(s.getType, curr, currSourceDefinition, s.getScope))
                                    case _ => None
                                }

                            val sinks = sources.map(genSource => SinkToAssignment(curr, source, genSource.getType.getName))

                            GEN(sourcesOf ::: sinks)
                        }

                        val leftHandSide =
                            if (currAssignments.exists { case (assignee, assignor) => assignee.equals(varName) } || currDefines.exists(varName.equals)
                              && getCurrentScope(varName).exists(scope => (scope <= source.getScope) || (source.getScope == SCOPE_UNKNOWN))) KILL
                            else GEN(source)

                        GEN(leftHandSide, rightHandSide)
                    }
                }

                override def computeZero(zero: Zero): util.Set[InformationFlowFact] = zero match {
                    case z: Zero if currAssignments.nonEmpty || currStructFieldAssigns.nonEmpty =>
                        // gen source for all new assignments
                        GEN(z :: assignmentSources)
                    case z: Zero if currDefines.nonEmpty =>
                        // newly introduced variable or struct
                        GEN(z :: defineSources)
                    case z => GEN(z)
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
        override def getCallFlowFunction(callStmt: CInterCFGNode, destinationMethod: CInterCFGFDef): FlowFunction[InformationFlowFact] = {
            lazy val flowCondition = interproceduralCFG.getPointsToCondition(callStmt, destinationMethod)
            lazy val destinationEnv = interproceduralCFG().getASTEnv(destinationMethod)
            lazy val destinationOpt = parentOpt(destinationMethod.get, destinationEnv).asInstanceOf[Opt[FunctionDef]]

            if (interproceduralCFG.getOptions.pseudoVisitingSystemLibFunctions && destinationMethod.method.entry.getName.equalsIgnoreCase(SPLLIFT_PSEUDO_SYSTEM_FUNCTION_CALL_NAME))
                return pseudoSystemFunctionCallCallFlowFunction(callStmt, interproceduralCFG.getASTEnv(callStmt), interproceduralCFG)

            def addCallToCallGraph() = SuperCallGraph.addEge(Edge(Node(interproceduralCFG.getMethodOf(callStmt).method), Node(destinationOpt), flowCondition))

            new IFCallFlowFunction(interproceduralCFG, callStmt, destinationMethod) {
                override def computeZero(z: Zero): util.Set[InformationFlowFact] = {
                    addCallToCallGraph()
                    GEN(z)
                }

                override def computeSink(s: Sink): util.Set[InformationFlowFact] = KILL

                override def computeStruct(source: Source): util.Set[InformationFlowFact] = {
                    assert(source.getType.isInstanceOf[Struct], "Computation source must be a variable.")
                    lazy val currSourceDefinition = getDefinition(source)
                    lazy val structName = source.getType.getName
                    lazy val structField = source.getType.asInstanceOf[Struct].field

                    val sourcesAndSinks = {
                        val sourcesAndSinks = fCallParamsToFDefParams.flatMap(callParamToDefParam => {
                            if (callParamToDefParam._1.exists(fields => usesField(fields).exists(field => isFullFieldMatch(source, field)))) {
                                // struct(.field) to variable
                                callParamToDefParam._2.flatMap(dstPDef => {
                                    val assignee = dstPDef.entry.decl.getId
                                    val sources = singleVisitOnSourceTypes(assignee, destinationVarEnv.varEnv, genStructSource(SCOPE_LOCAL), genVarSource(SCOPE_LOCAL))
                                    val sourcesOf = sources.map(cs => SourceDefinitionOf(cs.getType, cs.getCIFGStmt, currSourceDefinition, cs.getScope))
                                    val sink = SinkToAssignment(callStmt, source, assignee)

                                    sink :: sources ::: sourcesOf
                                })
                            } else if (callParamToDefParam._1.exists(fields => usesField(fields).isEmpty && uses(fields).contains(structName))) {
                                // complete struct to struct -> copy fields
                                callParamToDefParam._2.flatMap(dstPDef => {
                                    val assignee = dstPDef.entry.decl.getId
                                    val sources = singleVisitOnSourceTypes(assignee, destinationVarEnv.varEnv, genStructSource(SCOPE_LOCAL), genVarSource(SCOPE_LOCAL)) // sources of parent
                                    // copy field source
                                    val fieldSources = sources.map(parentSource => parentSource.copy(sourceType = Struct(assignee, structField)))
                                    val sourcesOf = fieldSources.map(cs => SourceDefinitionOf(cs.getType, cs.getCIFGStmt, currSourceDefinition, cs.getScope))
                                    val sink = SinkToAssignment(callStmt, source, assignee)

                                    sink :: fieldSources ::: sourcesOf
                                })
                            } else None
                        })

                        GEN(sourcesAndSinks)
                    }

                    GEN(computeGlobal(source), sourcesAndSinks)
                }

                override def computeVariable(source: Source): util.Set[InformationFlowFact] = {
                    assert(source.getType.isInstanceOf[Variable], "Computation source must be a variable.")
                    lazy val currSourceDefinition = getDefinition(source)
                    lazy val varName = source.getType.getName

                    val sourcesAndSinks = {
                        val callParamMatches = fCallParamsToFDefParams.filter(callParamToDefParam => uses(callParamToDefParam._1).exists(varName.equals))
                        val sourcesAndSinks = callParamMatches.flatMap(callParamMatch =>
                            callParamMatch._1.foldLeft(List[InformationFlowFact]())((genSrc, expr) =>
                                callParamMatch._2.foldLeft(genSrc)((genSrc, pDef) => {
                                    val assignee = pDef.entry.decl.getId
                                    val genSource = SourceDefinition(Variable(assignee), callStmt, SCOPE_LOCAL)
                                    val sourceOf = SourceDefinitionOf(Variable(assignee), callStmt, currSourceDefinition, SCOPE_LOCAL)
                                    val sink = SinkToAssignment(callStmt, source, assignee)
                                    genSource :: sourceOf :: sink :: genSrc
                                })))

                        GEN(sourcesAndSinks)
                    }

                    GEN(computeGlobal(source), sourcesAndSinks)
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
        override def getReturnFlowFunction(callSite: CInterCFGNode, calleeMethod: CInterCFGFDef, exitStmt: CInterCFGNode, returnSite: CInterCFGNode): FlowFunction[InformationFlowFact] = {
            if (interproceduralCFG.getOptions.pseudoVisitingSystemLibFunctions && calleeMethod.method.entry.getName.equalsIgnoreCase(SPLLIFT_PSEUDO_SYSTEM_FUNCTION_CALL_NAME))
                return pseudoSystemFunctionCallReturnFlow

            lazy val fCall = filterASTElems[FunctionCall](callSite)
            lazy val fCallOpt = parentOpt(callSite.get, interproceduralCFG.getASTEnv(callSite)).asInstanceOf[Opt[AST]]
            lazy val exitOpt = parentOpt(exitStmt.get, interproceduralCFG.getASTEnv(exitStmt)).asInstanceOf[Opt[AST]]
            lazy val pointerParamNames = getPointerFDefParamNames(calleeMethod.method)

            exitStmt.get match {
                case ReturnStatement(_) =>
                case _ => if (logger.isDebugEnabled) logger.debug("Exiting " + calleeMethod.method.entry.getName + " without return statement.")
            }

            def assignsReturnVariablesTo(callStmt: AST, returnStatement: AST): List[(Id, List[Id])] = {
                val call = defines(callStmt)
                val ret = uses(returnStatement)
                call.map((_, ret))
            }

            lazy val assignments = assignsReturnVariablesTo(callSite.get, exitStmt.get)

            new IFDefaultFlowFunction(interproceduralCFG, exitStmt, callSite) {
                override def computeStruct(source: Source): util.Set[InformationFlowFact] = {
                    assert(source.getType.isInstanceOf[Struct], "Computation source must be a struct.")
                    lazy val currSourceDefinition = getDefinition(source)
                    lazy val structName = source.getType.getName
                    lazy val structField = source.getType.asInstanceOf[Struct].field

                    val sourcesAndSinks = {
                        if (assignments.exists(assignment => assignment._2.contains(structName))) {
                            // struct to struct assignment
                            GEN(assignments.flatMap(assignment => assignment._2.flatMap {
                                case x if structName.equals(x) =>
                                    val assignee = assignment._1
                                    val scopes = getCurrentScope(assignee)
                                    scopes.flatMap(scope => {
                                        val newSource = SourceDefinition(Struct(assignee, structField), callSite, scope)
                                        val sourceOf = SourceDefinitionOf(Struct(assignee, structField), callSite, currSourceDefinition, scope)
                                        val sink = SinkToAssignment(callSite, source, assignee)
                                        List(newSource, sourceOf, sink)
                                    })
                                case _ => None
                            }))
                        } else KILL
                    }

                    GEN(computeGlobal(source), sourcesAndSinks)
                }

                override def computeVariable(source: Source): util.Set[InformationFlowFact] = {
                    assert(source.getType.isInstanceOf[Variable], "Computation source must be a variable.")
                    lazy val currSourceDefinition = getDefinition(source)
                    lazy val varName = source.getType.getName

                    val sourcesAndSinks = GEN(assignments.flatMap(assignment => assignment._2.flatMap {
                        case x if varName.equals(x) =>
                            val assignee = assignment._1
                            val scopes = getCurrentScope(assignee)

                            val sourceType =
                                if (assignsField(callSite.get).isEmpty) List(Variable(assignee))
                                else assignsField(callSite.get).flatMap(field => genSourceForField(field._1, field._2, scopes).map(_.getType)) // variable to struct field

                            val sources = sourceType.flatMap { st =>
                                scopes.flatMap(scope => {
                                    val newSource = SourceDefinition(st, callSite, scope)
                                    val sourceOf = SourceDefinitionOf(st, callSite, currSourceDefinition, scope)
                                    newSource :: sourceOf :: Nil
                                })

                            }

                            val sink = SinkToAssignment(callSite, source, assignee)
                            sink :: sources
                        case x => None
                    }))

                    GEN(computeGlobal(source), sourcesAndSinks)
                }

                override def computeSink(s: Sink): util.Set[InformationFlowFact] = KILL
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
        override def getCallToReturnFlowFunction(callSite: CInterCFGNode, returnSite: CInterCFGNode): FlowFunction[InformationFlowFact] = {
            new IFDefaultFlowFunction(interproceduralCFG, callSite, returnSite) {
                private def computeSource(source: Source): util.Set[InformationFlowFact] = source match {
                    case cs: Source if cs.getScope == SCOPE_GLOBAL => KILL // Kill this fact, as it is handled at return flow
                    case s: Source => s.getType match {
                        case Struct(_, Some(_)) if currStructFieldAssigns.exists(isPartFieldMatch(s, _)) => KILL // Kill this fact, as it is handled at return flow
                        case _: Variable | Struct(_, None) if currDefines.exists(s.getType.getName.equals) => KILL // Kill this fact, as it is handled at return flow
                        case _ => GEN(source)
                    }
                    case _ => GEN(source)
                }

                override def computeVariable(source: Source): util.Set[InformationFlowFact] = computeSource(source)

                override def computeStruct(source: Source): util.Set[InformationFlowFact] = computeSource(source)

                override def computeSink(s: Sink): util.Set[InformationFlowFact] = GEN(s)
            }
        }
    }

    private def computeGlobal(source: Source) = if (source.getScope == SCOPE_GLOBAL) GEN(source) else KILL
}