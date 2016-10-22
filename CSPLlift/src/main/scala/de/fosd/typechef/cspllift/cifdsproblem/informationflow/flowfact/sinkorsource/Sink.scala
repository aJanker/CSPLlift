package de.fosd.typechef.cspllift.cifdsproblem.informationflow.flowfact.sinkorsource

import de.fosd.typechef.crewrite.ProductDerivation
import de.fosd.typechef.cspllift.CICFGStmt
import de.fosd.typechef.cspllift.cifdsproblem.CFlowFact
import de.fosd.typechef.cspllift.evaluation.SimpleConfiguration
import de.fosd.typechef.parser.c.{Id, PrettyPrinter}


sealed abstract class Sink(override val cICFGStmt: CICFGStmt, val source: Source) extends SinkOrSource(cICFGStmt) {

    override def isInterestingFact: Boolean = true

    override def toText: String = {
        val from = "\t\tFrom:\t" + source.getType.getName + " at: " + source.getType.getName.getPositionFrom
        val stmt = "\t\tSourcestatement:\t" + PrettyPrinter.print(source.getCIFGStmt.getStmt.entry)
        val stmt2 = "\t\tSinkstatement:\t" + PrettyPrinter.print(this.cICFGStmt.getStmt.entry) + this.cICFGStmt.getStmt.entry.getPositionFrom
        from + "\n" + stmt + "\n" + stmt2
    }

    override def isEquivalentTo(other: CFlowFact, configuration: SimpleConfiguration): Boolean = {
        if (!other.isInstanceOf[Sink]) return false

        val otherSink = other.asInstanceOf[Sink]

        lazy val stmtProduct = ProductDerivation.deriveProduct(cICFGStmt.getStmt.entry, configuration.getTrueFeatures)
        lazy val eqStmt = stmtProduct.equals(otherSink.cICFGStmt.getStmt.entry)

        source.isEquivalentTo(otherSink.source, configuration) && eqStmt
    }
}

case class SinkToAssignment(override val cICFGStmt: CICFGStmt, override val source: Source, assignee: Id) extends Sink(cICFGStmt, source) {
    override def get: CFlowFact = SinkToAssignment(cICFGStmt, source.get, assignee)

    override def isEquivalentTo(other: CFlowFact, configuration: SimpleConfiguration): Boolean = {
        if (!other.isInstanceOf[SinkToAssignment]) return false

        val otherSink = other.asInstanceOf[SinkToAssignment]

        assignee.equals(otherSink.assignee) && super.isEquivalentTo(other, configuration)
    }
}

case class SinkToUse(override val cICFGStmt: CICFGStmt, override val source: Source) extends Sink(cICFGStmt, source) {
    override def get: CFlowFact = SinkToUse(cICFGStmt, source.get)

    override def isEquivalentTo(other: CFlowFact, configuration: SimpleConfiguration): Boolean =
        if (!other.isInstanceOf[SinkToUse]) false
        else super.isEquivalentTo(other, configuration)
}