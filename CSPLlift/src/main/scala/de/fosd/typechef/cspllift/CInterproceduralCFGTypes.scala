package de.fosd.typechef.cspllift

import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.error.Position
import de.fosd.typechef.parser.c.{AST, FunctionDef}

sealed abstract class CICFGStmt(stmt: Opt[AST], position: Position) {
    def getPosition = position
    def getStmt = stmt
}

case class CICFGConcreteStmt(stmt: Opt[AST], position: Position) extends CICFGStmt(stmt, position)

case class CICFGFDef(method: Opt[FunctionDef], position: Position) extends CICFGStmt(method, position)

