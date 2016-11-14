package de.fosd.typechef.cspllift.cifdsproblem.informationflow.flowfact.sinkorsource

import de.fosd.typechef.crewrite.ProductDerivation
import de.fosd.typechef.cspllift.CICFGStmt
import de.fosd.typechef.cspllift.cifdsproblem.CFlowFact
import de.fosd.typechef.cspllift.cifdsproblem.informationflow.InformationFlowHelper
import de.fosd.typechef.cspllift.evaluation.SimpleConfiguration
import de.fosd.typechef.parser.c.{Id, PrettyPrinter}


sealed abstract class Sink(override val cICFGStmt: CICFGStmt, val source: Source) extends SinkOrSource(cICFGStmt) with InformationFlowHelper {

    override def isEvaluationFact: Boolean = true

    override def toText: String = {
        val originSource = getOriginSource
        val from = "\t\tFrom:\t" + originSource.getType.getName + " at: " + originSource.getType.getName.getPositionFrom
        val stmt = "\t\tSourcestatement:\t" + PrettyPrinter.print(originSource.getCIFGStmt.getStmt.entry)
        val stmt2 = "\t\tSinkstatement:\t" + PrettyPrinter.print(this.cICFGStmt.getStmt.entry) + this.cICFGStmt.getStmt.entry.getPositionFrom
        from + "\n" + stmt + "\n" + stmt2
    }

    override def isEquivalentTo(other: CFlowFact, configuration: SimpleConfiguration): Boolean = {
        if (!other.isInstanceOf[Sink]) return false

        val otherSink = other.asInstanceOf[Sink]

        lazy val stmtProduct = ProductDerivation.deriveProduct(cICFGStmt.getStmt.entry, configuration.getTrueFeatures)
        lazy val eqStmt = stmtProduct.equals(otherSink.cICFGStmt.getStmt.entry)

        lazy val originStmtProduc = ProductDerivation.deriveProduct(getDefinition(source).getCIFGStmt.getStmt.entry, configuration.getTrueFeatures)
        lazy val eqOriginStmt = originStmtProduc.equals(getDefinition(otherSink.source).getCIFGStmt.getStmt.entry)

        getDefinition(otherSink.source).getCIFGStmt.getASTEntry.range.equals(getDefinition(source).getCIFGStmt.getASTEntry.range) && otherSink.cICFGStmt.getASTEntry.range.equals(cICFGStmt.getASTEntry.range)
    }

    def getOriginSource : Source = getDefinition(source)

    def getOriginId : Id = getOriginSource.getType.getName
}

case class SinkToAssignment(override val cICFGStmt: CICFGStmt, override val source: Source, assignee: Id) extends Sink(cICFGStmt, source) {
    override def get: CFlowFact = SinkToAssignment(cICFGStmt, source.get, assignee)

    override def isEquivalentTo(other: CFlowFact, configuration: SimpleConfiguration): Boolean = {
        if (!other.isInstanceOf[SinkToAssignment]) return false

        val otherSink = other.asInstanceOf[SinkToAssignment]

        assignee.equals(otherSink.assignee) && super.isEquivalentTo(other, configuration)
    }

    override def canEqual(that: Any): Boolean = that.isInstanceOf[SinkToAssignment]
}

case class SinkToUse(override val cICFGStmt: CICFGStmt, override val source: Source) extends Sink(cICFGStmt, source) {
    override def get: CFlowFact = SinkToUse(cICFGStmt, source.get)

    override def isEquivalentTo(other: CFlowFact, configuration: SimpleConfiguration): Boolean =
        if (!other.isInstanceOf[SinkToUse]) false
        else super.isEquivalentTo(other, configuration)

    override def canEqual(that: Any): Boolean = that.isInstanceOf[SinkToUse]
}