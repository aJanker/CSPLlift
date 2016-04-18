package de.fosd.typechef.spllift

import java.util

import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.crewrite.IntraCFG
import de.fosd.typechef.featureexpr.FeatureModel
import de.fosd.typechef.featureexpr.bdd.{BDDFeatureExpr, BDDFeatureModel}
import de.fosd.typechef.parser.c._
import de.fosd.typechef.typesystem.linker.SystemLinker
import de.fosd.typechef.typesystem.{CDeclUse, CTypeCache, CTypeSystemFrontend}
import heros.InterproceduralCFG
import soot.spl.ifds.Constraint

import scala.collection.JavaConverters._

class CInterCFG(startTunit: TranslationUnit, fm: FeatureModel = BDDFeatureModel.empty, options: CInterCFGOptions = DefaultCInterCFGOptions)
    extends InterproceduralCFG[Opt[AST], Opt[FunctionDef]] with IntraCFG with CInterCFGCommons with CInterCFGElementsCache {

    Constraint.FACTORY = de.fosd.typechef.featureexpr.bdd.FExprBuilder.bddFactory

    override val cInterCFGElementsCacheEnv = new CInterCFGElementsCacheEnv(startTunit, fm, options)

    val entryFunctions =
        filterAllASTElems[FunctionDef](cInterCFGElementsCacheEnv.startTUnit) filter {
            fdef => options.getGraphEntryFunctionNames.exists(fdef.getName.equalsIgnoreCase)
        } map {
            fdef => Opt(nodeToEnv(fdef).featureExpr(fdef), fdef)
        }

    def getFeatureModel = fm

    def getOptions = options

    /**
      * Returns the method containing a node.
      *
      * @param node The node for which to get the parent method
      */
    override def getMethodOf(node: Opt[AST]): Opt[FunctionDef] = {
        val env = nodeToEnv(node)
        findPriorASTElem[FunctionDef](node.entry, env) match {
            case Some(f: FunctionDef) => Opt(env.featureExpr(f), f)
            case _ => throw new NoSuchElementException("No prior function found for node: " + node)
        }
    }

    /**
      * Returns whether succ is a branch target of stmt.
      */
    override def isBranchTarget(stmt: Opt[AST], suc: Opt[AST]): Boolean =
        getSuccsOfS(stmt).exists(_.equals(suc))

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
        if (isCallStmt(node)) getSuccsOf(node) else java.util.Collections.emptyList[Opt[AST]]

    /**
      * Returns all callee methods for a given call.
      */
    override def getCalleesOfCallAt(call: Opt[AST]): util.Set[Opt[FunctionDef]] = {
        if (!isCallStmt(call)) return java.util.Collections.emptySet[Opt[FunctionDef]]

        val calleeNames = getCalleeNames(call)
        val callees = calleeNames.flatMap(findCallees(_, nodeToTUnit(call)))

        asJavaIdentitySet(callees.reverse) // Reverse resulting callee list as inner functions are visited first (e.g. outerfunction(innerfunction(x));)
    }

    /**
      * Returns true is this is a method's start statement. For backward analyses
      * those may also be return or throws statements.
      */
    override def isStartPoint(stmt: Opt[AST]): Boolean = {
        val preds = pred(stmt.entry, nodeToEnv(stmt))
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
        val timeStart = System.currentTimeMillis()
        val calc = cInterCFGElementsCacheEnv.getPointerEquivalenceClass(getPresenceNode(pointer), this).get.objectNames
        /* println(System.currentTimeMillis() - timeStart)

        println(calc.toOptList()) */
        false
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
    override def getSuccsOf(stmt: Opt[AST]): util.List[Opt[AST]] = getSuccsOfS(stmt).asJava

    private def getSuccsOfS(stmt: Opt[AST]): List[Opt[AST]] =
        succ(stmt.entry, nodeToEnv(stmt)).filter {
            case Opt(_, f: FunctionDef) => false
            case _ => true
        }

    /**
      * Returns <code>true</code> if the given statement leads to a method return
      * (exceptional or not). For backward analyses may also be start statements.
      */
    override def isExitStmt(stmt: Opt[AST]): Boolean =
        succ(stmt.entry, nodeToEnv(stmt)).exists {
            case Opt(_, f: FunctionDef) => true //TODO Calls?
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
        def nonCallStartNode(n: AST) = {
            val node = getPresenceNode(n)
            !(isCallStmt(node) || isStartPoint(node))
        }
        val allNonCallStartNodes = cInterCFGElementsCacheEnv.getAllKnownTUnits flatMap {filterAllASTElems[Statement](_) filter nonCallStartNode}
        asJavaIdentitySet(allNonCallStartNodes.map(getPresenceNode))
    }

    /**
      * Returns whether succ is the fall-through successor of stmt,
      * i.e., the unique successor that is be reached when stmt
      * does not branch.
      */
    override def isFallThroughSuccessor(stmt: Opt[AST], succ: Opt[AST]): Boolean = {
        val successors = getSuccsOfS(stmt)
        (successors.size == 1) && successors.contains(succ)
    }

    // undocumented function call to cifg from spllift -> gets current condition or flow condition
    def getConstraint(node: Opt[AST]): Constraint[String] = Constraint.make(node.condition.asInstanceOf[BDDFeatureExpr])

    private def findCallees(name: Opt[String], callTUnit: TranslationUnit): List[Opt[FunctionDef]] = {
        if (SystemLinker.allLibs.contains(name.entry) && options.pseudoVisitingSystemLibFunctions)
            return List(cInterCFGElementsCacheEnv.PSEUDO_SYSTEM_FUNCTION_CALL)

        val localDef = callTUnit.defs.flatMap {
            case o@Opt(ft, f@FunctionDef(_, decl, _, _)) if (decl.getName.equalsIgnoreCase(name.entry) /*&& ft.and(name.condition).isSatisfiable( TODO FM )*/) =>
                Some(Opt(ft, f))
            case _ => None
        }

        val externalDef = getExternalDefinitions(name)
        val result = externalDef ++ localDef

        if (result.isEmpty) Console.err.println("No function definiton found for " + name + "!")

        result
    }

    private def isRecursive(fCallName: String, fCallStmt: Opt[AST]): Boolean = fCallName.equalsIgnoreCase(getMethodOf(fCallStmt).entry.getName)

    private def getCalleeNames(call: Opt[AST]): List[Opt[String]] =
        filterAllASTElems[PostfixExpr](call).flatMap {
            case pf@PostfixExpr(Id(calleeName), FunctionCall(_)) => Some(Opt(nodeToEnv(pf).featureExpr(pf), calleeName))
            case _ => None
        }

    def nodeToEnv(node: Opt[AST]): ASTEnv = nodeToEnv(node.entry)
    def nodeToEnv(node: AST): ASTEnv =
        findEnv(node) match {
            case Some(env) => env
            case _ => throw new NoSuchElementException("No env found for node: " + node)
        }

    def nodeToTUnit(node: Opt[AST]): TranslationUnit = nodeToTUnit(node.entry)
    def nodeToTUnit(node: AST): TranslationUnit =
        getTranslationUnit(node) match {
            case Some(tunit) => tunit
            case _ => throw new NoSuchElementException("No tunit found for node: " + node)
        }

    def nodeToTS(node: Opt[AST]): CTypeSystemFrontend with CTypeCache with CDeclUse = nodeToTS(node.entry)
    def nodeToTS(node: AST): CTypeSystemFrontend with CTypeCache with CDeclUse =
        getTypeSystem(node) match {
            case Some(ts) => ts
            case _ => throw new NoSuchElementException("No TypeSystem found for node: " + node)
        }

    private def getPresenceNode[T <: AST](node: T): Opt[T] = Opt(nodeToEnv(node).featureExpr(node), node)

}