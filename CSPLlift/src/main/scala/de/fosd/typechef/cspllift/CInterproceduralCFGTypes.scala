package de.fosd.typechef.cspllift

import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.parser.c._

sealed abstract class CICFGStmt(stmt: Opt[AST]) extends Product {
    def getStmt = stmt
    def getCondition = getStmt.condition
    def getASTEntry = getStmt.entry

    def hasPosition = getASTEntry.hasPosition

    def getPosition = getASTEntry.range

    def toText: String

    /**
      * The equality operation for AST elements does not consider the position within the Translation Unit.
      * However for the IDE-Solver Heros this behaviour is false: as it distinguishes between cfg nodes with the
      * equals operator. However, two different return() statements would return true in the AST implementation of
      * TypeChef. Therefore we are wrapping these elements for Heros.
      */
    override def equals(x: Any) = x match {
        case c: CICFGStmt => (c.getCondition eq getCondition) && equalsASTElement(c)
        case _ => false
    }

    override def canEqual(that: Any): Boolean = that.isInstanceOf[CICFGStmt]

    private def equalsASTElement(that: CICFGStmt): Boolean = {
        val eqPosition =
            if (hasPosition && that.hasPosition) getPosition.equals(that.getPosition)
            else true
        lazy val eqAST = getASTEntry.equals(that.getASTEntry)

        eqPosition && eqAST
    }

    override def hashCode(): Int = {
        val positionHashCode =
            if (hasPosition) getPosition.get.hashCode()
            else 0
        this.getStmt.hashCode() + positionHashCode
    }
}

case class CICFGConcreteStmt(stmt: Opt[AST]) extends CICFGStmt(stmt) {
    override def toText: String =
        stmt match {
            case Opt(condition, s: Statement) => PrettyPrinter.print(CompoundStatement(List(Opt(condition, s))))
            case Opt(condition, e: Expr) => PrettyPrinter.print(CompoundStatement(List(Opt(condition, ExprStatement(e)))))
            case _ => PrettyPrinter.print(stmt.entry)
        }
}

case class CICFGFDef(method: Opt[FunctionDef]) extends CICFGStmt(method) {
    override def toText: String = PrettyPrinter.print(TranslationUnit(List(method)))
}

