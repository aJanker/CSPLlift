package de.fosd.typechef.spllift

import java.util

import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.crewrite.IntraCFG
import de.fosd.typechef.featureexpr.FeatureExpr
import de.fosd.typechef.featureexpr.bdd.BDDFeatureExpr
import de.fosd.typechef.parser.c._
import heros.InterproceduralCFG
import soot.spl.ifds.Constraint

import scala.collection.JavaConverters._

// TODO Options for loading? -> Handler for linking
// TODO Linking
class CInterCFG(startTunit: TranslationUnit, options: CSPLliftOptions) extends InterproceduralCFG[AST, FunctionDef] with IntraCFG with ASTNavigation with ConditionalNavigation with CModuleCache {

  private val moduleCacheEnv = new CModuleCacheEnv(startTunit, options.getModuleInterface, options.getPointerInterface)

  private def nodeToEnv(node: AST): ASTEnv =
    findEnv(node, moduleCacheEnv) match {
      case Some(env) => env
      case _ => throw new NoSuchElementException("No env found for node: " + node)
    }

  private def nodeToTUnit(node: AST): TranslationUnit =
    getTranslationUnit(node, moduleCacheEnv) match {
      case Some(tunit) => tunit
      case _ => throw new NoSuchElementException("No tunit found for node: " + node)
    }

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
    succ(stmt, nodeToEnv(stmt)).par.exists(_.entry.equals(suc)) // TODO Check for inter vs intra

  /**
    * Returns all caller statements/nodes of a given method.
    */
  override def getCallersOf(m: FunctionDef): util.Set[AST] = {
      // TODO Pointer & Linking

      null
  }

  /**
    * Returns all statements to which a call could return.
    * In the RHS paper, for every call there is just one return site.
    * We, however, use as return site the successor statements, of which
    * there can be many in case of exceptional flow.
    */
  override def getReturnSitesOfCallAt(node: AST): util.List[AST] = {
    if (!isCallStmt(node)) return java.util.Collections.emptyList[AST]

    // what is the return-site of an exit node? // Hamid
    getSuccsOf(node)
  }

  /**
    * Returns all callee methods for a given call.
    */
  override def getCalleesOfCallAt(call: AST): util.Set[FunctionDef] = {
    if (!isCallStmt(call)) return java.util.Collections.emptySet[FunctionDef]

    val calleeNames = getCalleeNames(call)
    val callees = calleeNames.flatMap(findCallees(_, nodeToTUnit(call)))

    callees.map(_.entry).toSet.asJava
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
        case Opt(_, f: FunctionDef) => true
        case _ => false
      }
  }

  override def getStartPointsOf(m: FunctionDef): util.Set[AST] = ???

  /**
    * Returns <code>true</code> if the given statement is a call site.
    */
  override def isCallStmt(stmt: AST): Boolean =
    filterAllASTElems[PostfixExpr](stmt).exists {
      case PostfixExpr(_, FunctionCall(_)) => true
    }

  /**
    * Returns the successor nodes.
    */
  override def getSuccsOf(stmt: AST): util.List[AST] =
  // TODO Why Filter FDefs?
    succ(stmt, nodeToEnv(stmt)).flatMap {
      case Opt(_, f: FunctionDef) => None
      case Opt(_, a: AST) => Some(a.asInstanceOf[AST]) // required casting otherwise java compilation will fail
    }.asJava

  override def isExitStmt(stmt: AST): Boolean = ???

  override def getCallsFromWithin(m: FunctionDef): util.Set[AST] = ???

  override def allNonCallStartNodes(): util.Set[AST] = ???

  /**
    * Returns whether succ is the fall-through successor of stmt,
    * i.e., the unique successor that is be reached when stmt
    * does not branch.
    */
  override def isFallThroughSuccessor(stmt: AST, succ: AST): Boolean =
    throw new UnsupportedOperationException("oops, isFallThroughSuccessor is not supported!")

  // TODO Check function purpose

  // TODO undocumented function call to cifg from spllift
  def getConstraint(node: AST): Constraint[String] = {
    val featureExpr: BDDFeatureExpr = nodeToEnv(node).featureExpr(node).asInstanceOf[BDDFeatureExpr]
    Constraint.make(featureExpr.leak)
  }


  override def findMethodCalls(t: AST, env: ASTEnv, oldres: CFGRes, ctx: FeatureExpr, _res: CFGRes): CFGRes = {
    val tunit = nodeToTUnit(t)

    filterAllASTElems[PostfixExpr](t).foldLeft(_res)((res, pf) =>
      pf match {
        case PostfixExpr(Id(funName), FunctionCall(_)) => {
          val fexpr = env.featureExpr(pf)
          val newresctx = getNewResCtx(oldres, ctx, fexpr)
          val callees = findCallees(Opt(fexpr, funName), tunit)

          callees.foldLeft(res)((nres, callee) =>
            (newresctx and callee.condition, callee.condition, callee.entry) :: nres)
        }
        case _ => res
      })
  }

  override def getExprSucc(exp: Expr, ctx: FeatureExpr, oldres: CFGRes, env: ASTEnv): CFGRes =
    findMethodCalls(exp, env, oldres, ctx, oldres) ++ super.getExprSucc(exp, ctx, oldres, env)

  private def findCallees(name: Opt[String], callTUnit: TranslationUnit): List[Opt[FunctionDef]] = {
    val localDef = callTUnit.defs.flatMap {
      case o@Opt(ft, f@FunctionDef(_, decl, _, _)) if (decl.getName.equalsIgnoreCase(name.entry) && ft.and(name.condition).isTautology(/* TODO FM */)) =>
        Some(Opt(ft, f))
    }

    val externalDef =
      if (!nameIsLinked(name, moduleCacheEnv)) List()
      else {
        // GET DEFINITIONS
        List()
      }

    localDef ++ externalDef

    List()
  }

  private def getCalleeNames(call: AST): List[Opt[String]] =
    filterAllASTElems[PostfixExpr](call).flatMap {
      case pf@PostfixExpr(Id(calleeName), FunctionCall(_)) => Some(Opt(nodeToEnv(pf).featureExpr(pf), calleeName))
    }
}
