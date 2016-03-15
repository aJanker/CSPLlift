package de.fosd.typechef.spllift.ifdsproblem

import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.parser.c.{AST, Id, PrettyPrinter}

import scala.collection.mutable.ListBuffer


sealed trait InformationFlow

case object Zero extends InformationFlow {
    def dbgString : String = super.toString
}

case class Source(name: Opt[Id], stmt: Opt[_], reachingSources: ListBuffer[Source] = ListBuffer(), globalFile: Option[String] = None) extends InformationFlow {
    override def hashCode() = name.hashCode + stmt.hashCode() + globalFile.hashCode()
    override def equals(other: Any) = other match {
        case s@Source(oName, oStmt, _, oGlobalFile) => oName.equals(name) && oStmt.equals(stmt) && oGlobalFile.equals(globalFile)
        case _ => false
    }
}

case class Reach(to: Opt[AST], from: List[Opt[Id]], sources: List[Source]) extends InformationFlow {
    override def toString: String = {
        val buffer = new StringBuffer
        buffer.append("Reach under condition " + to.condition.toTextExpr + " at " + PrettyPrinter.print(to.entry) + "\n")

        if (from.nonEmpty) {
            buffer.append("\tFrom:\t")
            from.foreach(entry => buffer.append(entry.entry.name + " when " + entry.condition.toTextExpr + ";\t"))
        }

        if (sources.nonEmpty) buffer.append("\n\tSources: " + sources)
        buffer.toString
    }
}

