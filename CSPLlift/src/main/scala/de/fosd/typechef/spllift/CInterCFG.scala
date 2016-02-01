package de.fosd.typechef.spllift

import java.util

import de.fosd.typechef.crewrite.InterCFG
import de.fosd.typechef.parser.c._
import heros.InterproceduralCFG

// TODO Options for loading?
// TODO Linking
class CInterCFG(tunit : TranslationUnit) extends InterproceduralCFG[AST, FunctionDef] with InterCFG with ASTNavigation with ConditionalNavigation {

  private val env = CASTEnv.createASTEnv(tunit)

  /**
    * Returns the method containing a node.
    *
    * @param n The node for which to get the parent method
    */
  override def getMethodOf(n: AST): FunctionDef = {
    findPriorASTElem[FunctionDef](n, env) match {
      case Some(f: FunctionDef) => f
      case _ => null
    }
  }

  /**
    * Returns whether succ is a branch target of stmt.
    */
  override def isBranchTarget(stmt: AST, suc: AST): Boolean = {
    val nodes = succ(stmt, env) // TODO Check for inter vs intra
    nodes.par.exists(_.entry.equals(suc))
  }

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
  override def getReturnSitesOfCallAt(n: AST): util.List[AST] = {
    // TODO Pointer & Linking

    // Hamid
    if (!isCallStmt(n)) {
      return java.util.Collections.emptyList[AST];
    }
    // what is the return-site of an exit node?
    getSuccsOf(n)
  }

  /**
    * Returns all callee methods for a given call.
    */
  override def getCalleesOfCallAt(n: AST): util.Set[FunctionDef] = {
    // TODO Pointer & Linking
    // Hamid
    if (isCallStmt(n)) {
      var res = new java.util.HashSet[FunctionDef]()
      for (succ <- succ(n, env)) {
        succ.entry match {
          case s: FunctionDef => res.add(s)
          case _ =>
        }
      }
      res
    } else {
      java.util.Collections.emptySet[FunctionDef]
    }



  }

  /**
    * Returns true is this is a method's start statement. For backward analyses
    * those may also be return or throws statements.
    */
  override def isStartPoint(stmt: AST): Boolean = ???

  override def getStartPointsOf(m: FunctionDef): util.Set[AST] = ???

  /**
    * Returns <code>true</code> if the given statement is a call site.
    */
  override def isCallStmt(stmt: AST): Boolean = {
    stmt match {
      case s: FunctionCall => true
      case _ => false
    }
  }

  override def getSuccsOf(n: AST): util.List[AST] = ???

  override def isExitStmt(stmt: AST): Boolean = ???

  override def getCallsFromWithin(m: FunctionDef): util.Set[AST] = ???

  override def allNonCallStartNodes(): util.Set[AST] = ???

  override def isFallThroughSuccessor(stmt: AST, succ: AST): Boolean = ???

  // provide a lookup mechanism for function defs (from the type system or selfimplemented)
  override def getTranslationUnit(): TranslationUnit = tunit
}
