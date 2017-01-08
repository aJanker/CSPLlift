package de.fosd.typechef.cspllift.cintercfg

import java.io.Writer

import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.error.Position
import de.fosd.typechef.parser.c._

sealed abstract class CInterCFGNode() extends Product with Serializable {
    def get: AST

    def hasPosition: Boolean = get.hasPosition

    def getPosition: Option[(Position, Position)] = get.range

    def toText: String = PrettyPrinter.print(get)

    def write(writer: Writer): Writer = {
        writer.write(toText)
        writer
    }

    /**
      * The equality operation for AST elements does not consider the position within the Translation Unit.
      * However for the IDE-Solver Heros this behaviour is wrong: as it distinguishes between cfg nodes using the
      * equals operator. However, for example two different return() statements would return true in the AST implementation of
      * TypeChef. Therefore we are wrapping these elements for Heros.
      */
    override def equals(other: Any): Boolean = other match {
        case c: CInterCFGNode => c.get eq get
        case _ => false
    }

    def equivalentTo(other: Any): Boolean = other match {
        case c: CInterCFGNode if c.get.canEqual(get) => c.get.equals(get)
        case _ => false
    }

    override def canEqual(that: Any): Boolean = that.isInstanceOf[CInterCFGNode]

    override def hashCode(): Int = System.identityHashCode(get)
}

case class CInterCFGStmt(stmt: AST) extends CInterCFGNode() {
    override def get: AST = stmt
}

case class CInterCFGFDef(method: Opt[FunctionDef]) extends CInterCFGNode() {
    override def get: AST = getMethod.entry

    def getMethod: Opt[FunctionDef] = method
}
