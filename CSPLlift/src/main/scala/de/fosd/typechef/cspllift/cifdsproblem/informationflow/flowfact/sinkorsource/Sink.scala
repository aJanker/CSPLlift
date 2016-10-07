package de.fosd.typechef.cspllift.cifdsproblem.informationflow.flowfact.sinkorsource

import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.crewrite.ProductDerivation
import de.fosd.typechef.cspllift.cifdsproblem.CFlowFact
import de.fosd.typechef.cspllift.evaluation.SimpleConfiguration
import de.fosd.typechef.parser.c.{AST, Id, PrettyPrinter}


sealed abstract class Sink(override val stmt: Opt[AST], val source: Source) extends SinkOrSource(stmt) {

    override def isInterestingFact: Boolean = true

    override def toText: String = {
        val from = "\t\tFrom:\t" + source.getType.getName + " at: " + source.getType.getName.getPositionFrom
        val stmt = "\t\tSourcestatement:\t" + PrettyPrinter.print(source.getStmt.entry)
        val stmt2 = "\t\tSinkstatement:\t" + PrettyPrinter.print(this.stmt.entry) + this.stmt.entry.getPositionFrom
        from + "\n" + stmt + "\n" + stmt2
    }

    override def isEquivalentTo(other: CFlowFact, configuration: SimpleConfiguration): Boolean = {
        if (!other.isInstanceOf[Sink]) return false

        val otherSink = other.asInstanceOf[Sink]

        lazy val stmtProduct = ProductDerivation.deriveProduct(stmt.entry, configuration.getTrueFeatures)
        lazy val eqStmt = stmtProduct.equals(otherSink.stmt.entry)

        source.isEquivalentTo(otherSink.source, configuration) && eqStmt
    }
}

case class SinkToAssignment(override val stmt: Opt[AST], override val source: Source, assignee: Id) extends Sink(stmt, source) {
    override def get: CFlowFact = SinkToAssignment(stmt, source.get, assignee)

    override def isEquivalentTo(other: CFlowFact, configuration: SimpleConfiguration): Boolean = {
        if (!other.isInstanceOf[SinkToAssignment]) return false

        val otherSink = other.asInstanceOf[SinkToAssignment]

        assignee.equals(otherSink.assignee) && super.isEquivalentTo(other, configuration)
    }
}

case class SinkToUse(override val stmt: Opt[AST], override val source: Source) extends Sink(stmt, source) {
    override def get: CFlowFact = SinkToUse(stmt, source.get)

    override def isEquivalentTo(other: CFlowFact, configuration: SimpleConfiguration): Boolean =
        if (!other.isInstanceOf[SinkToUse]) false
        else super.isEquivalentTo(other, configuration)
}