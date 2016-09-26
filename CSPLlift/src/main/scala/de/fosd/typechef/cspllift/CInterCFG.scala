package de.fosd.typechef.cspllift

import java.util

import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.crewrite.IntraCFG
import de.fosd.typechef.cspllift.commons.{CInterCFGCommons, WarningsCache}
import de.fosd.typechef.featureexpr.FeatureModel
import de.fosd.typechef.featureexpr.bdd.{BDDFeatureExpr, BDDFeatureModel}
import de.fosd.typechef.parser.c._
import de.fosd.typechef.typesystem.linker.SystemLinker
import de.fosd.typechef.typesystem.{CDeclUse, CTypeCache, CTypeSystemFrontend}
import heros.InterproceduralCFG
import soot.spl.ifds.Constraint

import scala.collection.JavaConverters._
import scala.collection.mutable

/**
  * Extensions to the InterproceduralCFG of the IFDS Framework used in TypeChef
  */
trait CInterproceduralCFG[N, M] extends InterproceduralCFG[N, M] {

    /**
      * Returns all functions which are considered entry functions in the programm flow (such as main)
      */
    def getEntryFunctions : List[M]

    /**
      * Retrieves the TypeChef FeatureModel
      */
    def getFeatureModel : FeatureModel

    /**
      * Retrieves the configuration options of the cfg.
      */
    def getOptions : CInterCFGConfiguration

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
      * @param callee the target of the call
      * @return the correctly annotated callee target.
      */
    def getLiftedMethodOf(callSite: Opt[AST], callee: Opt[FunctionDef]) : Opt[FunctionDef]
}

class CInterCFG(startTunit: TranslationUnit, fm: FeatureModel = BDDFeatureModel.empty, options: CInterCFGConfiguration = new DefaultCInterCFGConfiguration)
    extends CInterproceduralCFG[Opt[AST], Opt[FunctionDef]] with IntraCFG with CInterCFGCommons with CInterCFGElementsCache {

    Constraint.FACTORY = de.fosd.typechef.featureexpr.bdd.FExprBuilder.bddFactory

    private val cInterCFGNodes = new mutable.HashSet[Opt[AST]]()

    override val cInterCFGElementsCacheEnv: CInterCFGElementsCacheEnv = new CInterCFGElementsCacheEnv(startTunit, fm, options)

    override def getEntryFunctions =
        filterAllASTElems[FunctionDef](cInterCFGElementsCacheEnv.startTUnit) filter {
            fdef => options.getGraphEntryFunctionNames.exists(fdef.getName.equalsIgnoreCase)
        } map {
            fdef => Opt(getASTEnv(fdef).featureExpr(fdef), fdef)
        }

    override def getFeatureModel = fm

    override def getOptions = options

    // undocumented function call to cifg from spllift -> gets current flow condition
    override def getConstraint(node: Opt[AST]): Constraint = Constraint.make(node.condition.asInstanceOf[BDDFeatureExpr])

    override def getASTEnv(node: Opt[AST]): ASTEnv = getASTEnv(node.entry)
    private def getASTEnv(node: AST): ASTEnv =
        getEnv(node) match {
            case Some(env) => env
            case _ => throw new NoSuchElementException("No env found for node: " + node)
        }

    override def getTUnit(node: Opt[AST]): TranslationUnit = getTUnit(node.entry)
    private def getTUnit(node: AST): TranslationUnit =
        getTranslationUnit(node) match {
            case Some(tunit) => tunit
            case _ => throw new NoSuchElementException("No tunit found for node: " + node)
        }

    override def getTS(node: Opt[AST]): CTypeSystemFrontend with CTypeCache with CDeclUse = getTS(node.entry)
    private def getTS(node: AST): CTypeSystemFrontend with CTypeCache with CDeclUse =
        getTypeSystem(node) match {
            case Some(ts) => ts
            case _ => throw new NoSuchElementException("No TypeSystem found for node: " + node)
        }

    override def getLiftedMethodOf(callSite: Opt[AST], callee: Opt[FunctionDef]) : Opt[FunctionDef] = {
        val pointsTo = getCalleesOfCallAtS(callSite)

        pointsTo.find(pointTo => pointTo.entry == callee.entry).getOrElse(callee)
    }
    /**
      * Returns the method containing a node.
      *
      * @param node The node for which to get the parent method
      */
    override def getMethodOf(node: Opt[AST]): Opt[FunctionDef] = {
        val env = getASTEnv(node)
        findPriorASTElem[FunctionDef](node.entry, env) match {
            case Some(f: FunctionDef) => Opt(env.featureExpr(f), f)
            case _ => throw new NoSuchElementException("No prior function found for node: " + node)
        }
    }

    /**
      * Returns whether succ is a branch target of stmt.
      */
    override def isBranchTarget(stmt: Opt[AST], suc: Opt[AST]): Boolean = !isFallThroughSuccessor(stmt, suc)

    /**
      * Returns all caller statements/nodes of a given method.
      */
    override def getCallersOf(m: Opt[FunctionDef]): util.Set[Opt[AST]] =
        throw new UnsupportedOperationException("HIT TODO FUNCTION!")
    // TODO

    /**
      * Returns all statements to which a call could return.
      * In the RHS paper, for every call there is just one return site.
      * We, however, use as return site the successor statements, of which
      * there can be many in case of exceptional flow.
      */
    override def getReturnSitesOfCallAt(node: Opt[AST]): util.List[Opt[AST]] =
        if (isCallStmt(node)) getSuccsOf(node)
        else java.util.Collections.emptyList[Opt[AST]]

    /**
      * Returns all callee methods for a given call.
      */
    override def getCalleesOfCallAt(call: Opt[AST]): util.Set[Opt[FunctionDef]] = asJavaIdentitySet(getCalleesOfCallAtS(call))

    private def getCalleesOfCallAtS(call: Opt[AST]): List[Opt[FunctionDef]] = {
        if (!isCallStmt(call)) return List[Opt[FunctionDef]]()

        val calleeNames = getCalleeNames(call)
        val tunit = getTUnit(call)

        val callees =
            if (calleeNames.nonEmpty) calleeNames.flatMap(findCallees(_, tunit))
            else filterAllASTElems[PostfixExpr](call).flatMap {
                case PostfixExpr(pointer, FunctionCall(_)) => getFunctionPointerDestNames(pointer).flatMap(findCallees(_, tunit))
                case _ => None
            }

        if (callees.isEmpty)
            WarningsCache.add("No function destinations found for:\t" +  call)

        callees
    }

    /**
      * Returns true is this is a method's start statement. For backward analyses
      * those may also be return or throws statements.
      */
    override def isStartPoint(stmt: Opt[AST]): Boolean = {
        val preds = pred(stmt.entry, getASTEnv(stmt))
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
    override def getStartPointsOf(fDef: Opt[FunctionDef]): util.Set[Opt[AST]] =
        asJavaIdentitySet(getSuccsOf(fDef))

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
    override def isCallStmt(stmt: Opt[AST]): Boolean =
        filterAllASTElems[PostfixExpr](stmt).exists {
            case PostfixExpr(Id(name), FunctionCall(_)) => !isRecursive(name, stmt)
            case PostfixExpr(pointer, FunctionCall(_)) => hasDestination(pointer)
            case _ => false
        }

    /**
      * Returns the successor nodes.
      */
    override def getSuccsOf(stmt: Opt[AST]): util.List[Opt[AST]] = {
        val succs = getSuccsOfS(stmt)

        if (stmt.entry.equals(ExprStatement(AssignExpr(Id("dbg"),"=",Id("res"))))) {
            println("dbg")
        }

        cInterCFGNodes.++=(succs)
        succs.asJava
    }

    private def getSuccsOfS(stmt: Opt[AST]): List[Opt[AST]] =
        succ(stmt.entry, getASTEnv(stmt)).filter {
            case Opt(_, f: FunctionDef) => false
            case _ => true
        }.filter(_.condition.isSatisfiable(getFeatureModel))

    /**
      * Returns <code>true</code> if the given statement leads to a method return
      * (exceptional or not). For backward analyses may also be start statements.
      */
    override def isExitStmt(stmt: Opt[AST]): Boolean =
        succ(stmt.entry, getASTEnv(stmt)).exists {
            case Opt(_, f: FunctionDef) => true
            case _ => false
        }

    /**
      * Returns all call sites within a given method.
      */
    override def getCallsFromWithin(method: Opt[FunctionDef]): util.Set[Opt[AST]] = {
        val callsWithIn = filterAllASTElems[PostfixExpr](method.entry.stmt.innerStatements).map(getPresenceNode).filter(isCallStmt)
        asJavaIdentitySet(callsWithIn)
    }

    /**
      * Returns the set of all nodes that are neither call nor start nodes.
      */
    override def allNonCallStartNodes(): util.Set[Opt[AST]] = {
        def nonCallStartOptNode(n: Opt[AST]) = nonCallStartNode(n.entry)
        def nonCallStartNode(n: AST) = {
            val node = getPresenceNode(n)
            !(isCallStmt(node) || isStartPoint(node))
        }
        val allNonCallStartNodes = cInterCFGElementsCacheEnv.getAllKnownTUnits flatMap {filterAllASTElems[Statement](_) filter nonCallStartNode} map getPresenceNode
        val allVisitedNonCallStartNodes = cInterCFGNodes.toList filter nonCallStartOptNode

        asJavaIdentitySet(allNonCallStartNodes ++ allVisitedNonCallStartNodes)
    }

    /**
      * Returns whether succ is the fall-through successor of stmt,
      * i.e., the unique successor that is be reached when stmt
      * does not branch.
      */
    override def isFallThroughSuccessor(stmt: Opt[AST], succ: Opt[AST]): Boolean = {
        val successors = getSuccsOfS(stmt)
        successors.contains(succ) && ((successors.size == 1) || !successors.exists(s => !s.equals(succ) && succ.condition.and(s.condition).isSatisfiable(getFeatureModel)))
    }

    private def findCallees(name: Opt[String], callTUnit: TranslationUnit): List[Opt[FunctionDef]] = {
        if (SystemLinker.allLibs.contains(name.entry) && options.pseudoVisitingSystemLibFunctions)
            return List(cInterCFGElementsCacheEnv.SPLLIFT_PSEUDO_SYSTEM_FUNCTION_CALL)

        def findCalleeInTunit(tunit : TranslationUnit) = {
            tunit.defs.flatMap {
                case o@Opt(ft, f@FunctionDef(_, decl, _, _)) if (decl.getName.equalsIgnoreCase(name.entry) /*&& ft.and(name.condition).isSatisfiable( TODO FM )*/) =>
                    Some(Opt(ft.and(name.condition), f))
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

    private def isRecursive(fCallName: String, fCallStmt: Opt[AST]): Boolean = fCallName.equalsIgnoreCase(getMethodOf(fCallStmt).entry.getName)

    private def getCalleeNames(call: Opt[AST]): List[Opt[String]] =
        filterAllASTElems[PostfixExpr](call).flatMap {
            case pf@PostfixExpr(Id(calleeName), FunctionCall(_)) => Some(Opt(getASTEnv(pf).featureExpr(pf), calleeName))
            case _ => None
        }

    private def getPresenceNode[T <: AST](node: T): Opt[T] = Opt(getASTEnv(node).featureExpr(node), node)

    private def getFunctionPointerDestNames(pointer: Expr) : List[Opt[String]] ={
        val pointerNode = getPresenceNode(pointer)
        cInterCFGElementsCacheEnv.getFPointerDestDefsNames(pointerNode, getMethodOf(pointerNode).entry.getName)
    }
}