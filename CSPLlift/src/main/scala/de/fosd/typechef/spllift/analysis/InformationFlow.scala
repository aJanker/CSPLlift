package de.fosd.typechef.spllift.analysis

import java.util
import java.util.Collections

import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.crewrite.UsedDefinedDeclaredVariables
import de.fosd.typechef.parser.c._
import de.fosd.typechef.spllift.{ASTHelper, CInterCFG}
import heros.{FlowFunction, FlowFunctions, IFDSTabulationProblem}

import scala.collection.JavaConverters._
import scala.collection.mutable.ListBuffer

class InformationFlow(icfg: CInterCFG) extends IFDSTabulationProblem[AST, InfoFlowFact, FunctionDef, CInterCFG] with InformationFlowConfiguration with UsedDefinedDeclaredVariables with ASTHelper {

    private def GEN(fact: InfoFlowFact): util.Set[InfoFlowFact] = Collections.singleton(fact)

    private def GEN(res: List[InfoFlowFact]): util.Set[InfoFlowFact] = res.toSet.asJava

    private def KILL: util.Set[InfoFlowFact] = Collections.emptySet()

    private var initialGlobalsFile: String = ""

    /**
      * Returns initial seeds to be used for the analysis. This is a mapping of statements to initial analysis facts.
      * We consider global variables as initial sources.
      */
    override def initialSeeds(): util.Map[AST, util.Set[InfoFlowFact]] =
        interproceduralCFG.entryFunctions.foldLeft(new util.HashMap[AST, util.Set[InfoFlowFact]])((res, entry) => {
            interproceduralCFG.getStartPointsOf(entry).asScala.foreach(res.put(_, globalsAsInitialSeeds(entry)))
            initialGlobalsFile = entry.getFile.getOrElse("")
            res
        })

    private def globalsAsInitialSeeds(f: FunctionDef): util.Set[InfoFlowFact] = {
        val globalVariables = interproceduralCFG.nodeToTUnit(f).defs.filterNot {
            case Opt(_, f: FunctionDef) => true
            case _ => false
        }

        val globalInfoFlowFacts = globalVariables.flatMap(x => {
            val decls = declares(x)

            // Note: we ignore the actual file of the declaration as it may be declared in a header file.
            // As variables declared in header files may be included across several files, this way prevents matching errors.
            if (decls.nonEmpty) decls.map(decl => Source(Opt(x.condition, decl), x, ListBuffer(), getFileName(f.getFile)))
            else None
        })

        GEN(globalInfoFlowFacts :+ zeroValue())
    }

    /**
      * Returns the interprocedural control-flow graph which this problem is computed over.
      *
      * <b>NOTE:</b> this method could be called many times. Implementations of this
      * interface should therefore cache the return value!
      */
    override def interproceduralCFG: CInterCFG = icfg

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
    override def zeroValue(): InfoFlowFact = Zero

    /**
      * Returns a set of flow functions. Those functions are used to compute data-flow facts
      * along the various kinds of control flows.
      *
      * <b>NOTE:</b> this method could be called many times. Implementations of this
      * interface should therefore cache the return value!
      */
    override def flowFunctions(): FlowFunctions[AST, InfoFlowFact, FunctionDef] = cachedFlowFunctions

    private val cachedFlowFunctions: FlowFunctions[AST, InfoFlowFact, FunctionDef] =
        new FlowFunctions[AST, InfoFlowFact, FunctionDef] {

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
            override def getCallFlowFunction(callStmt: AST, destinationMethod: FunctionDef): FlowFunction[InfoFlowFact] = {
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

                val callEnv = interproceduralCFG().nodeToEnv(callStmt)
                val destinationEnv = interproceduralCFG().nodeToEnv(destinationMethod)

                val fCallOpt = parentOpt(callStmt, callEnv)
                val fCall = filterAllASTElems[FunctionCall](callStmt, callEnv).head // TODO Check if != 1
                val callExprs = fCall.params.exprs
                val fDefParams = destinationMethod.declarator.extensions
                val callParamToFDefParams = mapCallParamToFDefParam(callExprs, fDefParams)

                new FlowFunction[InfoFlowFact] {
                    override def computeTargets(source: InfoFlowFact): util.Set[InfoFlowFact] = {
                        source match {
                            case s@Source(x, _, _, global) => {
                                callParamToFDefParams.find(callParamToFDefParam => x.entry.name.equalsIgnoreCase(callParamToFDefParam.entry._1.name)) match {
                                    case Some(hit) => GEN(Source(Opt(hit.condition, hit.entry._2), fCallOpt, ListBuffer(s))) // Local Parameter
                                    case None if global.isDefined => GEN(s) // Global Variable
                                    case None => KILL // Local Variable from previous function
                                }
                            }
                            case Zero if !initialSeedsExists(destinationMethod) => {
                                filesWithSeeds ::= destinationMethod.getFile.getOrElse("")
                                globalsAsInitialSeeds(destinationMethod)
                            } // Introduce Global Variables from linked file
                            case k => KILL // Local Variable from previous function
                        }
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
            override def getReturnFlowFunction(callSite: AST, calleeMethod: FunctionDef, exitStmt: AST, returnSite: AST): FlowFunction[InfoFlowFact] =
                new FlowFunction[InfoFlowFact] {
                    override def computeTargets(source: InfoFlowFact): util.Set[InfoFlowFact] =
                        source match {
                            case s@Source(_, _, _, global) if global.isDefined => GEN(s) // Pass global variables back to return site
                            case s@Source(id, _, _, _) if exitStmt.isInstanceOf[ReturnStatement] => KILL // TODO Return Assignments
                            case r: Reach => GEN(r)
                            case k => KILL // Kill all local variables - they are no longer required in the analysis
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
            override def getNormalFlowFunction(curr: AST, succ: AST): FlowFunction[InfoFlowFact] = {
                // Cache some repeatedly used variables
                val currOpt = parentOpt(curr, interproceduralCFG.nodeToEnv(curr))
                val currASTEnv = interproceduralCFG().nodeToEnv(curr)
                val currTS = interproceduralCFG.nodoeToTS(curr)

                val currDefines = defines(curr)
                val currUses = uses(curr)
                val currAssignments = assignsVariable(curr)

                val succEnv = currTS.lookupEnv(succ)

                var sourceCache = Map[Opt[Id], List[Source]]()

                def nameMatchIsSatisfiable(i: Id, o: Opt[Id]) =
                    o.entry.name.equalsIgnoreCase(i.name) && o.condition.and(currASTEnv.featureExpr(i)).isSatisfiable(interproceduralCFG.getFeatureModel)

                def nameMatchIsTautology(i: Id, o: Opt[Id]) =
                    o.entry.name.equalsIgnoreCase(i.name) && o.condition.equivalentTo(currASTEnv.featureExpr(i))

                def occurrenceIsSatisfiable(x: Opt[Id], occurrences: List[Id]) = occurrences.exists(nameMatchIsSatisfiable(_, x))

                def occurrenceIsFullySatisfiable(x: Opt[Id], occurrences: List[Id]) = occurrences.exists(nameMatchIsTautology(_, x))

                def defineIsSatisfiable(x: Opt[Id]): Boolean = occurrenceIsSatisfiable(x, currDefines)

                def defineIsFullySatisfiable(x: Opt[Id]): Boolean = occurrenceIsFullySatisfiable(x, currDefines)

                def useIsSatisfiable(x: Opt[Id]): Boolean = occurrenceIsSatisfiable(x, currUses)

                def genSource(target: Id, reachingSource: Option[Source] = None): List[Source] = {
                    val targetOpt = Opt(currASTEnv.featureExpr(target), target)

                    sourceCache.get(targetOpt) match {
                        case None => // New source -> add to cache map
                            val buffer = if (reachingSource.isDefined) ListBuffer[Source](reachingSource.get) else ListBuffer[Source]()
                            val genSources = currTS.lookupEnv(succ).varEnv.lookupScope(target.name).toOptList.flatMap(scope => {

                                val currCondition = targetOpt.condition.and(scope.condition)

                                // gen with the correct  and satisfiable scope
                                if (currCondition.isSatisfiable(interproceduralCFG.getFeatureModel))
                                    if (scope.entry == 0)
                                        Some(Source(targetOpt.copy(condition = currCondition), currOpt, buffer, getFileName(target.getFile))) // add file when global definition exists
                                    else
                                        Some(Source(targetOpt.copy(condition = currCondition), currOpt, buffer))
                                else
                                    None
                            })

                            sourceCache += (targetOpt -> genSources)
                            genSources

                        case Some(genSource) => // Update already existing targets
                            if (reachingSource.isDefined) genSource.foreach(genS => genS.reachingSources.+=(reachingSource.get)) // Update new reaching sources
                            List()
                    }
                }

                def globalNameScopeIsSatisfiable(id: Opt[Id], declarationFile: Option[String] = None): Boolean = {
                    val eqDeclFile = declarationFile match {
                        case None => false
                        case Some(declFile) => getFileName(id.entry.getFile).getOrElse("").equalsIgnoreCase(declFile)
                    }

                    eqDeclFile && currTS.lookupEnv(succ).varEnv.lookupScope(id.entry.name).toOptList.exists {
                        case o@Opt(cond, 0) => id.condition.and(cond).isSatisfiable(icfg.getFeatureModel)
                        case _ => false
                    }
                }

                new FlowFunction[InfoFlowFact] {
                    override def computeTargets(source: InfoFlowFact): util.Set[InfoFlowFact] = {
                        var res = KILL

                        source match {
                            case Zero if currDefines.nonEmpty =>

                                val facts: List[InfoFlowFact] = currDefines.flatMap(id =>
                                    currTS.lookupEnv(succ).varEnv.lookupScope(id.name).toOptList.flatMap(scope => {
                                        if (scope.entry == 0)
                                            None // Do not generate a source for global variables
                                        else if (currAssignments.exists(_._1.name.equalsIgnoreCase(id.name)))
                                            None // Do not generate a source for assignments from other variables (e.g x = y;)
                                        else
                                            Some(Source(Opt(currOpt.condition.and(scope.condition), id), currOpt)) // is completly new introduced declaration without usage (e.g int x = 50;)
                                    }))
                                res = GEN(facts)

                            case s@Source(id, _, _, global) if useIsSatisfiable(id) =>

                                val reach = Reach(parentOpt(curr, currASTEnv), s.name :: s.reachingSources.toList.map(_.name), List(s)) // Announce the fact, that this source reaches a new source
                            val sources = currAssignments.flatMap {
                                    case (target, assignments) =>
                                        assignments.flatMap {
                                            case assignment if nameMatchIsTautology(target, id) && nameMatchIsSatisfiable(assignment, id) => genSource(target, Some(s)) // Kill source, as the old source gets replaced by the new one // TODO Full VAA
                                            case assignment if nameMatchIsSatisfiable(assignment, id) => s :: genSource(target, Some(s)) // Pass previous source
                                            case _ => None
                                        }
                                }

                                res = GEN(reach :: sources)

                            case s@Source(id, _, _, None) if defineIsFullySatisfiable(id) =>
                                res = KILL // Kill previously known local source as it is now no longer valid // TODO Full VAA

                            case s@Source(id, _, _, global) if global.isDefined && defineIsFullySatisfiable(id) && globalNameScopeIsSatisfiable(id, global) =>
                                res = KILL// Global visible source, only handle this source in case the name scope is globally visible

                            case r: Reach => res = KILL // Per default we kill the previously reached "pseudo sinks"

                            case _ => res = GEN(source) // Pass through unused infoflowfacts to succ

                        }
                        println(res + "\n")
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
            override def getCallToReturnFlowFunction(callSite: AST, returnSite: AST): FlowFunction[InfoFlowFact] = {
                new FlowFunction[InfoFlowFact] {
                    override def computeTargets(source: InfoFlowFact): util.Set[InfoFlowFact] =
                        source match {
                            case s@Source(id, _, _, global) if global.isDefined => KILL
                            case _ => GEN(source) // TODO Pointer!
                        }
                }
            }
        }
}

