package de.fosd.typechef.cspllift

import java.util

import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.crewrite._
import de.fosd.typechef.cspllift.commons.{CInterCFGCommons, WarningsCache}
import de.fosd.typechef.error.Position
import de.fosd.typechef.featureexpr.bdd.{BDDFeatureExpr, BDDFeatureModel}
import de.fosd.typechef.featureexpr.{FeatureExprFactory, FeatureModel}
import de.fosd.typechef.parser.c._
import de.fosd.typechef.typesystem.linker.SystemLinker
import de.fosd.typechef.typesystem.{CDeclUse, CTypeCache, CTypeSystemFrontend}
import heros.InterproceduralCFG
import spllift.Constraint

import scala.collection.JavaConverters._
import scala.collection.mutable

/**
  * Extensions to the InterproceduralCFG of the IFDS Framework used in TypeChef
  */
trait CInterproceduralCFG[N, M] extends InterproceduralCFG[N, M] {

    /**
      * Returns all functions which are considered entry functions in the programm flow (such as main)
      */
    def getEntryFunctions: List[M]

    /**
      * Retrieves the TypeChef FeatureModel
      */
    def getFeatureModel: FeatureModel

    /**
      * Retrieves the configuration options of the cfg.
      */
    def getOptions: CInterCFGConfiguration

    /**
      * Retrieves the cfg contraint of a given node.
      */
    def getConstraint(node: N): Constraint

    /**
      * Gets the context of a given node.
      */
    def getASTEnv(node: N): ASTEnv

    /**
      * Gets the translationunit containing a node.
      */
    def getTUnit(node: N): TranslationUnit

    /**
      * Gets the typesystem of a given node.
      */
    def getTS(node: N): CTypeSystemFrontend with CTypeCache with CDeclUse

    /**
      * Gets the correctly annotated (lifted) call edge between a call site and its corresponding target callee.
      * With the use of this method, SPLlift is able to resolve the constraint of an call and call-to-return edge.
      *
      * @param callSite the statment where the call occurred
      * @param callee   the target of the call
      * @return the correct points to flow constraint
      */
    def getPointsToConstraint(callSite: N, callee: M): Constraint
}

class CInterCFG(startTunit: TranslationUnit, fm: FeatureModel = BDDFeatureModel.empty, options: CInterCFGConfiguration = new DefaultCInterCFGConfiguration)
  extends CInterproceduralCFG[CICFGStmt, CICFGFDef] with IntraCFG with CInterCFGCommons with CInterCFGElementsCache {

    private val cInterCFGNodes = new mutable.HashSet[CICFGStmt]()

    override val cInterCFGElementsCacheEnv: CInterCFGElementsCacheEnv = new CInterCFGElementsCacheEnv(startTunit, fm, options)

    override def getEntryFunctions =
        filterAllASTElems[FunctionDef](cInterCFGElementsCacheEnv.startTUnit) filter {
            fdef => options.getGraphEntryFunctionNames.exists(fdef.getName.equalsIgnoreCase)
        } map {
            fdef => CICFGFDef(Opt(getASTEnv(fdef).featureExpr(fdef), fdef), fdef.getPositionFrom)
        }

    override def getFeatureModel = fm

    override def getOptions = options

    // undocumented function call to cifg from spllift -> gets current flow condition
    override def getConstraint(node: CICFGStmt): Constraint = Constraint.make(node.getStmt.condition.asInstanceOf[BDDFeatureExpr])

    override def getASTEnv(node: CICFGStmt): ASTEnv = getASTEnv(node.getStmt.entry)

    private def getASTEnv(node: AST): ASTEnv =
        getEnv(node) match {
            case Some(env) => env
            case _ => throw new NoSuchElementException("No env found for node: " + node)
        }

    override def getTUnit(node: CICFGStmt): TranslationUnit = getTUnit(node.getStmt.entry)

    private def getTUnit(node: AST): TranslationUnit =
        getTranslationUnit(node) match {
            case Some(tunit) => tunit
            case _ => throw new NoSuchElementException("No tunit found for node: " + node)
        }

    override def getTS(node: CICFGStmt): CTypeSystemFrontend with CTypeCache with CDeclUse = getTS(node.getStmt.entry)

    private def getTS(node: AST): CTypeSystemFrontend with CTypeCache with CDeclUse =
        getTypeSystem(node) match {
            case Some(ts) => ts
            case _ => throw new NoSuchElementException("No TypeSystem found for node: " + node)
        }

    override def getPointsToConstraint(callSite: CICFGStmt, callee: CICFGFDef): Constraint = {
        val pointsTo = getCalleesOfCallAtS(callSite).find(pointTo => pointTo.method.entry.equals(callee.method.entry)).getOrElse(callee)

        val callCond = getASTEnv(callSite.getStmt.entry).featureExpr(callSite.getStmt.entry)
        val calleeCond = pointsTo.getStmt.condition

        Constraint.make(calleeCond.and(callCond).asInstanceOf[BDDFeatureExpr])
    }

    /**
      * Returns the method containing a node.
      *
      * @param node The node for which to get the parent method
      */
    override def getMethodOf(node: CICFGStmt): CICFGFDef = {
        val env = getASTEnv(node)
        findPriorASTElem[FunctionDef](node.getStmt.entry, env) match {
            case Some(f: FunctionDef) => CICFGFDef(Opt(env.featureExpr(f), f), f.getPositionFrom)
            case _ => throw new NoSuchElementException("No prior function found for node: " + node)
        }
    }

    /**
      * Returns whether succ is a branch target of stmt.
      */
    override def isBranchTarget(stmt: CICFGStmt, suc: CICFGStmt): Boolean = !isFallThroughSuccessor(stmt, suc)

    /**
      * Returns all caller statements/nodes of a given method.
      */
    override def getCallersOf(m: CICFGFDef): util.Set[CICFGStmt] =
    throw new UnsupportedOperationException("HIT TODO FUNCTION!")

    // TODO

    /**
      * Returns all statements to which a call could return.
      * In the RHS paper, for every call there is just one return site.
      * We, however, use as return site the successor statements, of which
      * there can be many in case of exceptional flow.
      */
    override def getReturnSitesOfCallAt(node: CICFGStmt): util.List[CICFGStmt] =
    if (isCallStmt(node)) getSuccsOf(node)
    else java.util.Collections.emptyList[CICFGStmt]

    /**
      * Returns all callee methods for a given call.
      */
    override def getCalleesOfCallAt(call: CICFGStmt): util.Set[CICFGFDef] = asJavaIdentitySet(getCalleesOfCallAtS(call).map(getOriginalOptCalleeNode))

    /**
      * Every callee found by the function getCalleesOfCallAtS has the presence condition of its flow condition but not of its original presence condition within the ast.
      * SPLlift requieres for a sound result the callee orignal node!
      */
    private def getOriginalOptCalleeNode(callee: CICFGFDef): CICFGFDef = callee.copy(method = parentOpt(callee.method.entry, getASTEnv(callee)).asInstanceOf[Opt[FunctionDef]])

    private def getCalleesOfCallAtS(call: CICFGStmt): List[CICFGFDef] = {
        if (!isCallStmt(call)) return List[CICFGFDef]()

        val calleeNames = getCalleeNames(call)
        val tunit = getTUnit(call)

        val callees =
            if (calleeNames.nonEmpty) calleeNames.flatMap(findCallees(_, tunit))
            else filterAllASTElems[PostfixExpr](call.getStmt).flatMap {
                case PostfixExpr(pointer, FunctionCall(_)) => getFunctionPointerDestNames(pointer).flatMap(findCallees(_, tunit))
                case _ => None
            }

        if (callees.isEmpty)
            WarningsCache.add("No function destinations found for:\t" + call)

        callees
    }

    /**
      * Returns true is this is a method's start statement. For backward analyses
      * those may also be return or throws statements.
      */
    override def isStartPoint(stmt: CICFGStmt): Boolean = {
        val preds = pred(stmt.getStmt.entry, getASTEnv(stmt))
        if (preds.isEmpty) false
        else
            preds.head match {
                case Opt(_, f: FunctionDef) => true // check for variability
                case _ => false
            }
    }

    /**
      * Returns all start points of a given method. There may be
      * more than one start point in case of a backward analysis.
      */
    override def getStartPointsOf(fDef: CICFGFDef): util.Set[CICFGStmt] = asJavaIdentitySet(getSuccsOf(fDef))

    /**
      * Returns if for a given pointer expression a corresponding function definition exists
      */
    private def hasDestination(pointer: Expr): Boolean = {
        val destNames = getFunctionPointerDestNames(pointer)

        if (destNames.isEmpty)
            WarningsCache.add("No function pointer destination found for: " + pointer + " @ " + pointer.getPositionFrom + "\n" + PrettyPrinter.print(pointer))

        destNames.nonEmpty
    }

    /**
      * Returns <code>true</code> if the given statement is a call site.
      */
    override def isCallStmt(cICFGStmt: CICFGStmt): Boolean =
    filterAllASTElems[PostfixExpr](cICFGStmt.getStmt).exists {
        case PostfixExpr(Id(name), FunctionCall(_)) => !isRecursive(name, cICFGStmt)
        case PostfixExpr(pointer, FunctionCall(_)) => hasDestination(pointer)
        case _ => false
    }

    /**
      * Returns the successor nodes.
      */
    override def getSuccsOf(cICFGStmt: CICFGStmt): util.List[CICFGStmt] = {
        val succs = getSuccsOfS(cICFGStmt)
        cInterCFGNodes.++=(succs)
        succs.asJava
    }

    private def getSuccsOfS(cICFGStmt: CICFGStmt): List[CICFGStmt] = getCFGElements(succ(cICFGStmt.getStmt.entry, getASTEnv(cICFGStmt)), cICFGStmt.getPosition)


    override def getPredsOf(cICFGStmt: CICFGStmt): util.List[CICFGStmt] = {
        val preds = getPredsOfS(cICFGStmt)
        cInterCFGNodes.++=(preds)
        preds.asJava
    }

    private def getPredsOfS(cICFGStmt: CICFGStmt): List[CICFGStmt] = getCFGElements(pred(cICFGStmt.getStmt.entry, getASTEnv(cICFGStmt)), cICFGStmt.getPosition)

    private def getCFGElements(elements : CFG, fallbackPosition : Position) : List[CICFGStmt] = {
        elements.filter {
            case Opt(_, f: FunctionDef) => false
            case x => x.condition.isSatisfiable(fm)
        }.map(element => CICFGConcreteStmt(element, replaceMacroFileLocation(element.entry.getPositionFrom, fallbackPosition)))
    }

    private def replaceMacroFileLocation(position: Position, fallBack: Position): Position = if (position.getFile.endsWith(".h")) fallBack else position

    /**
      * Returns <code>true</code> if the given statement leads to a method return
      * (exceptional or not). For backward analyses may also be start statements.
      */
    override def isExitStmt(cICFGStmt: CICFGStmt): Boolean =
    succ(cICFGStmt.getStmt.entry, getASTEnv(cICFGStmt)).exists {
        case Opt(_, f: FunctionDef) => true
        case _ => false
    }

    /**
      * Returns all call sites within a given method.
      */
    override def getCallsFromWithin(method: CICFGFDef): util.Set[CICFGStmt] = {
        val callsWithIn = filterAllASTElems[PostfixExpr](method.method.entry.stmt.innerStatements).map(getPresenceNode).filter(isCallStmt)
        asJavaIdentitySet(callsWithIn)
    }

    /**
      * Returns the set of all nodes that are neither call nor start nodes.
      */
    override def allNonCallStartNodes(): util.Set[CICFGStmt] = {
        def nonCallStartOptNode(n: CICFGStmt) = nonCallStartNode(n.getStmt.entry)
        def nonCallStartNode(n: AST) = {
            val node = getPresenceNode(n)
            !(isCallStmt(node) || isStartPoint(node))
        }
        val allNonCallStartNodes = List() //cInterCFGElementsCacheEnv.getAllKnownTUnits flatMap {filterAllASTElems[Statement](_) filter nonCallStartNode} map getPresenceNode
        val allVisitedNonCallStartNodes = cInterCFGNodes.toList filter nonCallStartOptNode

        asJavaIdentitySet(allNonCallStartNodes ++ allVisitedNonCallStartNodes)
    }

    /**
      * Returns whether succ is the fall-through successor of stmt,
      * i.e., the unique successor that is be reached when stmt
      * does not branch.
      */
    override def isFallThroughSuccessor(stmt: CICFGStmt, succ: CICFGStmt): Boolean = {
        val successors = getSuccsOfS(stmt)
        successors.size > 1 && successors.last.getStmt.entry.equals(succ.getStmt.entry) && !getPresenceNode(succ.getStmt.entry).getStmt.condition.equals(succ.getStmt.condition) && successors.reverse.tail.foldLeft(FeatureExprFactory.True)((condition, cSucc) => cSucc.getStmt.condition.and(condition)).not().equivalentTo(successors.last.getStmt.condition)
    }

    private def findCallees(name: Opt[String], callTUnit: TranslationUnit): List[CICFGFDef] = {
        val dstCond = name.condition
        if (SystemLinker.allLibs.contains(name.entry) && options.pseudoVisitingSystemLibFunctions)
            return {
                val pseudoCall = cInterCFGElementsCacheEnv.getPseudoSystemFunctionCall(callTUnit)
                List(CICFGFDef(pseudoCall, pseudoCall.entry.getPositionFrom))
            }

        def findCalleeInTunit(tunit: TranslationUnit) = {
            tunit.defs.flatMap {
                case o@Opt(ft, f@FunctionDef(_, decl, _, _)) if decl.getName.equalsIgnoreCase(name.entry) && ft.and(dstCond).isSatisfiable(getFeatureModel) =>
                    Some(CICFGFDef(Opt(ft.and(dstCond), f), f.getPositionFrom))
                case _ => None
            }
        }

        def findCalleeWithBruteForce() = {
            val bruteForceResult = cInterCFGElementsCacheEnv.getAllKnownTUnits.par.flatMap(findCalleeInTunit).toList

            if (bruteForceResult.isEmpty)
                WarningsCache.add("No function definiton found for " + name + " with brute force!")

            bruteForceResult
        }

        val localDef = findCalleeInTunit(callTUnit)
        val externalDef = getExternalDefinitions(name)

        val result = externalDef ++ localDef

        if (result.nonEmpty) result else findCalleeWithBruteForce()
    }

    private def isRecursive(fCallName: String, fCallStmt: CICFGStmt): Boolean = fCallName.equalsIgnoreCase(getMethodOf(fCallStmt).method.entry.getName)

    private def getCalleeNames(call: CICFGStmt): List[Opt[String]] =
        filterAllASTElems[PostfixExpr](call.getStmt).flatMap {
            case pf@PostfixExpr(Id(calleeName), FunctionCall(_)) => Some(Opt(getASTEnv(pf).featureExpr(pf), calleeName))
            case _ => None
        }

    private def getPresenceNode(node: AST): CICFGStmt = CICFGConcreteStmt(Opt(getASTEnv(node).featureExpr(node), node), node.getPositionFrom)

    private def getFunctionPointerDestNames(pointer: Expr): List[Opt[String]] = {
        val pointerNode = getPresenceNode(pointer)
        cInterCFGElementsCacheEnv.getFPointerDestDefsNames(pointerNode.getStmt.asInstanceOf[Opt[Expr]], getMethodOf(pointerNode).method.entry.getName)
    }
}