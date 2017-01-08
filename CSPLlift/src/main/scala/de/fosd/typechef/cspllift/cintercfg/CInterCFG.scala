package de.fosd.typechef.cspllift.cintercfg

import java.util
import java.util.concurrent.ConcurrentHashMap

import de.fosd.typechef.cmodule.CModule
import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.crewrite.{CFG, IntraCFG}
import de.fosd.typechef.cspllift.commons.CInterCFGCommons
import de.fosd.typechef.featureexpr.{FeatureExpr, FeatureExprFactory, FeatureModel}
import de.fosd.typechef.parser.c._
import de.fosd.typechef.typesystem.CTypeSystemFrontend
import de.fosd.typechef.typesystem.modulelinking.CLinkingExtractor
import org.slf4j.{Logger, LoggerFactory}
import spllift.ConditionalEdgeFunction

import scala.collection.JavaConverters._

/**
  * Connector between the interprocedural CFG requiered by Heros/SPLLift and the TypeChef implementation of
  * the call graph and intraprocedural CFG. Note: variability lower than a CFG statement is not supported,
  * therefore the input AST must be rewritten to remove undisciplined variability.
  */
class CInterCFG(cModuleEnv: CModule, options: CInterCFGConfiguration = new DefaultCInterCFGConfiguration, benchmarkTag: Option[String] = None) extends CIFDSInterCFG[CInterCFGNode, CInterCFGFDef] with IntraCFG with CInterCFGCommons {

    private lazy val logger: Logger = LoggerFactory.getLogger(getClass)

    private val cInterCFGVisitedNodes = java.util.Collections.newSetFromMap[CInterCFGNode](new ConcurrentHashMap()) asScala

    /**
      * Returns all functions which are considered entry functions in the programm flow (such as main)
      */
    override def getEntryFunctions: List[CInterCFGFDef] = getModuleEnv.getCallGraph.getEntryFunctions.map(CInterCFGFDef)

    /**
      * Retrieves the TypeChef FeatureModel
      */
    override def getFeatureModel: FeatureModel = getModuleEnv.getFeatureModel

    /**
      * Retrieves the configuration options of the cfg.
      */
    override def getOptions: CInterCFGConfiguration = options

    /**
      * Retrieves the cfg contraint of a given node.
      */
    override def getCondition(node: CInterCFGNode): FeatureExpr = getModuleEnv.getCondition(node.get)

    /**
      * Gets the context of a given node.
      */
    override def getASTEnv(node: CInterCFGNode): ASTEnv = getModuleEnv.getASTEnv(node.get)

    /**
      * Gets the translationunit containing a node.
      */
    override def getTUnit(node: CInterCFGNode): TranslationUnit = getModuleEnv.getTranslationUnit(node.get)

    /**
      * Gets the typesystem of a given node.
      */
    override def getTS(node: CInterCFGNode): CTypeSystemFrontend with CLinkingExtractor = getModuleEnv.getTypeSystem(node.get)

    /**
      * Gets the correctly annotated (lifted) call edge condition between a call site and its corresponding target callee.
      * With the use of this method, SPLlift is able to resolve the constraint of an call and call-to-return edge.
      *
      * @param callSite the statement where the call occurred
      * @param callee   the target of the call
      * @return the correct points to flow constraint
      */
    override def getPointsToCondition(callSite: CInterCFGNode, callee: CInterCFGFDef): FeatureExpr = {
        val destinations = getModuleEnv.getCallGraph.getCalleesOfCallAt(callSite.get)
        val dest = destinations.find(d => d.getOpt.entry eq callee.get)

        if (dest.isDefined) dest.get.pointsToCondition
        else FeatureExprFactory.False
    }

    /**
      * Gets the correctly annotated (lifted) call edge function for a single statement.
      * With the use of this method, SPLlift is able to introduce initial seeds according to their presence conditions
      *
      * @param stmt the statement (generally a global variable)
      * @return the correct conditional flow edge
      */
    override def getConditionalEdgeFunction(stmt: CInterCFGNode): ConditionalEdgeFunction =
        new ConditionalEdgeFunction(getModuleEnv.getCondition(stmt.get), getModuleEnv.getFeatureModel, false)

    /**
      * Gets the correctly annotated (lifted) call edge function between a call site and its corresponding target callee.
      * With the use of this method, SPLlift is able to resolve the constraint of an call and call-to-return edge.
      *
      * @param callSite the statement where the call occurred
      * @param callee   the target of the call
      * @return the correct points-to conditional flow edge
      */
    override def getConditionalEdgeFunction(callSite: CInterCFGNode, callee: CInterCFGFDef): ConditionalEdgeFunction =
        new ConditionalEdgeFunction(getPointsToCondition(callSite, callee), getModuleEnv.getFeatureModel, false)

    /**
      * Gets the full flow condition between a source node and its successor node.
      *
      * @param srcStmt  the source statement
      * @param succStmt the successor statement
      * @return the correct flow constraint
      */
    override def getSuccFlowCondition(srcStmt: CInterCFGNode, succStmt: CInterCFGNode): FeatureExpr = getFlowCondition(srcStmt, succStmt, succ)

    /**
      * Gets the full flow condition between a source node and its successor node.
      *
      * @param srcStmt  the source statement
      * @param predStmt the predcessor statement
      * @return the correct flow constraint
      */
    override def getPredFlowCondition(srcStmt: CInterCFGNode, predStmt: CInterCFGNode): FeatureExpr = getFlowCondition(srcStmt, predStmt, pred)

    /**
      * Returns the method containing a node.
      *
      * @param node The node for which to get the parent method
      */
    override def getMethodOf(node: CInterCFGNode): CInterCFGFDef = {
        val env = getASTEnv(node)
        findPriorASTElem[FunctionDef](node.get, env) match {
            case Some(f: FunctionDef) => CInterCFGFDef(Opt(getModuleEnv.getCondition(f), f))
            case _ => throw new NoSuchElementException("No prior function found for node: " + node)
        }
    }

    /**
      * Returns the predecessor nodes.
      */
    override def getPredsOf(node: CInterCFGNode): util.List[CInterCFGNode] = getPredsOfS(node).asJava

    /**
      * Returns the successor nodes.
      */
    override def getSuccsOf(node: CInterCFGNode): util.List[CInterCFGNode] = getSuccsOfS(node).asJava

    /**
      * Returns all callee methods for a given call.
      */
    override def getCalleesOfCallAt(node: CInterCFGNode): util.Collection[CInterCFGFDef] = {
        val destinations = getModuleEnv.getCallGraph.getCalleesOfCallAt(node.get)
        if (!isRecursiveCall(node)) destinations.map(dest => CInterCFGFDef(dest.getOpt)).asJava
        else util.Collections.emptySet()
    }

    /**
      * Returns all caller statements/nodes of a given method.
      */
    override def getCallersOf(method: CInterCFGFDef) =
        throw new UnsupportedOperationException("Reverse lookup of calling origins is not supported by TypeChef.")

    /**
      * Returns all call sites within a given method.
      */
    override def getCallsFromWithin(method: CInterCFGFDef): util.Set[CInterCFGNode] = {
        val callsWithIn = filterAllASTElems[PostfixExpr](method.method.entry.stmt.innerStatements).map(call =>
            CInterCFGStmt(getCFGPresenceNode(call).entry)).filter(isCallStmt)
        asJavaIdentitySet(callsWithIn)
    }

    /**
      * Returns true is this is a method's start statement. For backward analyses
      * those may also be return or throws statements.
      */
    override def getStartPointsOf(method: CInterCFGFDef): util.Collection[CInterCFGNode] = getSuccsOfS(method).asJava

    /**
      * Returns all statements to which a call could return.
      * In the RHS paper, for every call there is just one return site.
      * We, however, use as return site the successor statements, of which
      * there can be many in case of exceptional flow.
      */
    override def getReturnSitesOfCallAt(node: CInterCFGNode): util.Collection[CInterCFGNode] = getSuccsOf(node)

    private def isRecursiveCall(node: CInterCFGNode) : Boolean = {
        val method = getMethodOf(node)
        val callees = getModuleEnv.getCallGraph.getCalleesOfCallAt(node.get)
        callees.exists(_.getFDef.getName.equalsIgnoreCase(method.getMethod.entry.getName))
    }

    /**
      * Returns <code>true</code> if the given statement is a call site.
      */
    override def isCallStmt(node: CInterCFGNode): Boolean = getModuleEnv.getCallGraph.isFunctionCall(node.get) && !isRecursiveCall(node)

    /**
      * Returns <code>true</code> if the given statement leads to a method return
      * (exceptional or not). For backward analyses may also be start statements.
      */
    override def isExitStmt(node: CInterCFGNode): Boolean = {
        val succs = succ(node.get, getASTEnv(node))
        if (succs.isEmpty) true
        else succs.exists {
            case Opt(_, f: FunctionDef) => true
            case _ => false
        }
    }

    /**
      * Returns true is this is a method's start statement. For backward analyses
      * those may also be return or throws statements.
      */
    override def isStartPoint(node: CInterCFGNode): Boolean = {
        val preds = pred(node.get, getASTEnv(node))
        if (preds.isEmpty) true
        else preds.exists {
            case Opt(_, f: FunctionDef) => true
            case _ => false
        }
    }

    /**
      * Returns the set of all nodes that are neither call nor start nodes.
      */
    override def allNonCallStartNodes(): util.Set[CInterCFGNode] = {
        def nonCallStartNode(node: CInterCFGNode) = !(isCallStmt(node) || isStartPoint(node))

        asJavaIdentitySet(cInterCFGVisitedNodes.toList filter nonCallStartNode)
    }

    /**
      * Returns whether succ is the fall-through successor of stmt,
      * i.e., the unique successor that is be reached when stmt
      * does not branch.
      */
    override def isFallThroughSuccessor(stmt: CInterCFGNode, succStmt: CInterCFGNode): Boolean =
        !isBranchTarget(stmt, succStmt)

    /**
      * Returns whether succ is a branch target of stmt.
      */
    override def isBranchTarget(stmt: CInterCFGNode, succStmt: CInterCFGNode): Boolean =
        groupOptListVAware(succ(stmt.get, getASTEnv(stmt)), getFeatureModel).exists(succs =>
            if (succs.size > 1) succs.exists(_.entry eq succStmt.get) // we consider branching a statement has at least two or more successor statements in the same config
            else false
        )

    /**
      * Retrieves the node according to its presence conditon in the source code
      * and not according to its cfg flow-condition.
      */
    private[cspllift] def getCFGPresenceNode(node: AST): Opt[AST] = Opt(getModuleEnv.getCondition(node), node)

    private[cspllift] def getModuleEnv = cModuleEnv

    private[cspllift] def getSuccsOfS(node: CInterCFGNode) = getCFGElements(succ(node.get, getASTEnv(node)))

    private[cspllift] def getPredsOfS(node: CInterCFGNode) = getCFGElements(pred(node.get, getASTEnv(node)))

    private def getCFGElements(elements: CFG): List[CInterCFGNode] = {
        val res = elements.filter {
            case Opt(_, f: FunctionDef) => false
            case x => x.condition.isSatisfiable(getModuleEnv.getFeatureModel)
        }.map(x => CInterCFGStmt(x.entry))

        res.foreach(cInterCFGVisitedNodes.add)

        res
    }

    private def getFlowCondition(start: CInterCFGNode, end: CInterCFGNode, cfg: (AST, ASTEnv) => CFG): FeatureExpr = {
        val res = end match {
            case _: CInterCFGFDef =>
                getModuleEnv.getCondition(start.get).and(getModuleEnv.getCondition(end.get))
            case _ =>
                val r = cfg(start.get, getASTEnv(start)).find {
                    _.entry eq end.get
                }

                if (r.isDefined) r.get.condition else FeatureExprFactory.False
        }
        res
    }
}
