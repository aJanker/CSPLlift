package de.fosd.typechef.spllift.ifdsproblem

import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.parser.c.{AST, Id, PrettyPrinter}

import scala.collection.mutable.ListBuffer


sealed trait InformationFlow

case object Zero extends InformationFlow {
    def dbgString : String = super.toString
}

sealed abstract class Source(val name: Opt[Id], val stmt: Opt[_], val reachingSources: ListBuffer[Source] = ListBuffer(), val globalFile: Option[String] = None) extends InformationFlow

case class VarSource(override val name: Opt[Id], override val stmt: Opt[_], override val reachingSources: ListBuffer[Source] = ListBuffer(), override val globalFile: Option[String] = None) extends Source(name, stmt, reachingSources, globalFile) {
    override def hashCode() = name.hashCode + stmt.hashCode() + globalFile.hashCode()
    override def equals(other: Any) = other match {
        case s@VarSource(oName, oStmt, _, oGlobalFile) => oName.equals(name) && oStmt.equals(stmt) && oGlobalFile.equals(globalFile)
        case _ => false
    }
}

case class StructSource(field: Option[Source], override val name: Opt[Id], override val stmt: Opt[_], override val reachingSources: ListBuffer[Source] = ListBuffer(), override val globalFile: Option[String] = None) extends Source(name, stmt, reachingSources, globalFile) {
    override def hashCode() = super.hashCode() + field.hashCode()
    override def equals(other: Any) = other match {
        case s@StructSource(oField, _, _ , _, _) if field.equals(oField) => super.equals(other)
        case _ => false
    }
}

// TODO to implement
case class PointerSource(override val name: Opt[Id], override val stmt: Opt[_], override val reachingSources: ListBuffer[Source] = ListBuffer(), override val globalFile: Option[String] = None) extends Source(name, stmt, reachingSources, globalFile) {
    override def hashCode() = name.hashCode + stmt.hashCode() + globalFile.hashCode()
    override def equals(other: Any) = other match {
        case s@PointerSource(oName, oStmt, _, oGlobalFile) => oName.equals(name) && oStmt.equals(stmt) && oGlobalFile.equals(globalFile)
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

