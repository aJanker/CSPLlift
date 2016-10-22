package de.fosd.typechef.cspllift

import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.parser.c.{AST, FunctionDef}

sealed abstract class CICFGStmt(stmt: Opt[AST]) extends Product {
    def getStmt = stmt

    /**
      * The equality operation for AST elements does not consider the position within the Translation Unit.
      * However for the IDE-Solver SOOT this behaviour is false: as it distinguishes between cfg nodes with the
      * equals operator. However, two different return() statements would return true in the AST implementation of
      * TypeChef. Therefore we are wrapping these elements for SOOT.
      */
    override def equals(x: Any) = x match {
        case c: CICFGStmt => (c.getStmt.condition eq getStmt.condition) && equalsASTElement(c.getStmt.entry)
        case _ => false
    }

    override def canEqual(that: Any): Boolean = that.isInstanceOf[CICFGStmt]

    private def equalsASTElement(that: AST) : Boolean = {
        val ast = this.getStmt.entry
        val eqPosition =
            if (ast.hasPosition && that.hasPosition) ast.range.equals(that.range)
            else true
        eqPosition && ast.equals(that)
    }

   override def hashCode(): Int = {
        val positionHashCode =
            if (this.getStmt.entry.hasPosition) this.getStmt.entry.range.hashCode()
            else 0
       this.getStmt.hashCode() + positionHashCode
    }
}

case class CICFGConcreteStmt(stmt: Opt[AST]) extends CICFGStmt(stmt)

case class CICFGFDef(method: Opt[FunctionDef]) extends CICFGStmt(method)

