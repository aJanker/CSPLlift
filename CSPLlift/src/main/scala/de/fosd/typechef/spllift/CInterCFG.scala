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
    extends InterproceduralCFG[AST, FunctionDef] with IntraCFG with ASTHelper with CFGElementsCache {

    Constraint.FACTORY = de.fosd.typechef.featureexpr.bdd.FExprBuilder.bddFactory

    private val CFGElementsCacheEnv = new CFGElementsCacheEnv(startTunit, fm, options)

    lazy val entryFunctions = filterAllASTElems[FunctionDef](CFGElementsCacheEnv.startTUnit).filter(fdef => options.getGraphEntryFunctionNames.exists(fdef.getName.equalsIgnoreCase))

    def getFeatureModel = fm

    def getOptions = options

    /**
      * Returns the method containing a node.
      *
      * @param node The node for which to get the parent method
      */
    override def getMethodOf(node: AST): FunctionDef =
        findPriorASTElem[FunctionDef](node, nodeToEnv(node)) match {
            case Some(f: FunctionDef) => f
            case _ => throw new NoSuchElementException("No prior function found for node: " + node)
        }

    /**
      * Returns whether succ is a branch target of stmt.
      */
    override def isBranchTarget(stmt: AST, suc: AST): Boolean =
        getSuccsOfS(stmt).exists(_.equals(suc))

    /**
      * Returns all caller statements/nodes of a given method.
      */
    override def getCallersOf(m: FunctionDef): util.Set[AST] =
        throw new UnsupportedOperationException("HIT TODO FUNCTION!")
        // TODO

    /**
      * Returns all statements to which a call could return.
      * In the RHS paper, for every call there is just one return site.
      * We, however, use as return site the successor statements, of which
      * there can be many in case of exceptional flow.
      */
    override def getReturnSitesOfCallAt(node: AST): util.List[AST] =
        if (isCallStmt(node)) getSuccsOf(node) else java.util.Collections.emptyList[AST]

    /**
      * Returns all callee methods for a given call.
      */
    override def getCalleesOfCallAt(call: AST): util.Set[FunctionDef] = {
        if (!isCallStmt(call)) return java.util.Collections.emptySet[FunctionDef]

        val calleeNames = getCalleeNames(call)
        val callees = calleeNames.flatMap(findCallees(_, nodeToTUnit(call)))

        toJavaIdentitySet(callees.map(_.entry).reverse) // Reverse resulting callee list as inner functions are visited first (e.g. outerfunction(innerfunction(x));)
    }

    /**
      * Returns true is this is a method's start statement. For backward analyses
      * those may also be return or throws statements.
      */
    override def isStartPoint(stmt: AST): Boolean = {
        val preds = pred(stmt, nodeToEnv(stmt))
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
    override def getStartPointsOf(fDef: FunctionDef): util.Set[AST] =
        toJavaIdentitySet(getSuccsOf(fDef))

    /**
      * Returns <code>true</code> if the given statement is a call site.
      */
    override def isCallStmt(stmt: AST): Boolean =
        filterAllASTElems[PostfixExpr](stmt).exists {
            case PostfixExpr(Id(name), FunctionCall(_)) => !isRecursive(name, stmt)
            case PostfixExpr(uncovered, FunctionCall(_)) => throw new IllegalArgumentException("Do not know rule for: " + uncovered)
            case _ => false
        }

    /**
      * Returns the successor nodes.
      */
    override def getSuccsOf(stmt: AST): util.List[AST] = getSuccsOfS(stmt).asJava

    private def getSuccsOfS(stmt: AST): List[AST] =
        succ(stmt, nodeToEnv(stmt)).flatMap {
            case Opt(_, f: FunctionDef) => None
            case Opt(_, a: AST) => Some(a.asInstanceOf[AST]) // required casting otherwise java compilation will fail
            case _ => None
        }

    /**
      * Returns <code>true</code> if the given statement leads to a method return
      * (exceptional or not). For backward analyses may also be start statements.
      */
    override def isExitStmt(stmt: AST): Boolean =
        succ(stmt, nodeToEnv(stmt)).exists {
            case Opt(_, f: FunctionDef) => true //TODO Calls?
            case _ => false
        }

    /**
      * Returns all call sites within a given method.
      */
    override def getCallsFromWithin(method: FunctionDef): util.Set[AST] = {
        val callsWithIn = filterAllASTElems[PostfixExpr](method.stmt.innerStatements).filter(isCallStmt)
        toJavaIdentitySet(callsWithIn)
    }


    /**
      * Returns the set of all nodes that are neither call nor start nodes.
      */
    override def allNonCallStartNodes(): util.Set[AST] = {
        // TODO beware only nodes from start tunit.
        val allNonCallStartNodes = filterAllASTElems[Statement](CFGElementsCacheEnv.startTUnit).filterNot(stmt => isCallStmt(stmt) || isStartPoint(stmt))
        toJavaIdentitySet(allNonCallStartNodes)
    }

    /**
      * Returns whether succ is the fall-through successor of stmt,
      * i.e., the unique successor that is be reached when stmt
      * does not branch.
      */
    override def isFallThroughSuccessor(stmt: AST, succ: AST): Boolean =
        throw new UnsupportedOperationException("oops, isFallThroughSuccessor is not supported!")
    // TODO Check function purpose

    // TODO undocumented function call to cifg from spllift -> gets current condition
    def getConstraint(node: AST): Constraint[String] = {
        val featureExpr: BDDFeatureExpr = nodeToEnv(node).featureExpr(node).asInstanceOf[BDDFeatureExpr]
        Constraint.make(featureExpr)
    }

    private def findCallees(name: Opt[String], callTUnit: TranslationUnit): List[Opt[FunctionDef]] = {
        if (SystemLinker.allLibs.contains(name.entry) && options.pseudoVisitingSystemLibFunctions)
            return List(CFGElementsCacheEnv.PSEUDO_SYSTEM_FUNCTION_CALL)

        val localDef = callTUnit.defs.flatMap {
            case o@Opt(ft, f@FunctionDef(_, decl, _, _)) if (decl.getName.equalsIgnoreCase(name.entry) /*&& ft.and(name.condition).isSatisfiable( TODO FM )*/) =>
                Some(Opt(ft, f))
            case _ => None
        }

        val externalDef = getExternalDefinitions(name, CFGElementsCacheEnv)
        val result = externalDef ++ localDef

        if (result.isEmpty) Console.err.println("No function definiton found for " + name + "!")

        result
    }

    private def isRecursive(fCallName: String, fCallStmt: AST) : Boolean = fCallName.equalsIgnoreCase(getMethodOf(fCallStmt).getName)

    private def getCalleeNames(call: AST): List[Opt[String]] =
        filterAllASTElems[PostfixExpr](call).flatMap {
            case pf@PostfixExpr(Id(calleeName), FunctionCall(_)) => Some(Opt(nodeToEnv(pf).featureExpr(pf), calleeName))
        }

    def nodeToEnv(node: AST): ASTEnv =
        findEnv(node, CFGElementsCacheEnv) match {
            case Some(env) => env
            case _ => throw new NoSuchElementException("No env found for node: " + node)
        }

    def nodeToTUnit(node: AST): TranslationUnit =
        getTranslationUnit(node, CFGElementsCacheEnv) match {
            case Some(tunit) => tunit
            case _ => throw new NoSuchElementException("No tunit found for node: " + node)
        }

    def nodeToTS(node: AST): CTypeSystemFrontend with CTypeCache with CDeclUse =
        getTypeSystem(node, CFGElementsCacheEnv) match {
            case Some(ts) => ts
            case _ => throw new NoSuchElementException("No TypeSystem found for node: " + node)
        }
}