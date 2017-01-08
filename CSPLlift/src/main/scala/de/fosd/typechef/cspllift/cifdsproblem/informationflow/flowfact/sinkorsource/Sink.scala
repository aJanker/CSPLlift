package de.fosd.typechef.cspllift.cifdsproblem.informationflow.flowfact.sinkorsource

import de.fosd.typechef.crewrite.ProductDerivation
import de.fosd.typechef.cspllift.cifdsproblem.{CFlowConstants, CFlowFact}
import de.fosd.typechef.cspllift.cintercfg.CInterCFGNode
import de.fosd.typechef.customization.conditional.SimpleConfiguration
import de.fosd.typechef.parser.c.Id


sealed abstract class Sink(override val cfgNode: CInterCFGNode, val source: Source) extends SinkOrSource(cfgNode) with CFlowConstants {

    /**
      * Sinks are always interesting facts for our evaluation strategy, however rewriting introduces some variant specific flows only.
      * We ignore flows for intermediate variables now as they differ for each variant.
      * Nevertheless, no interesting original flow is ignored, as we only ignore the tmp-variable flow itself,
      * but not the reaching information flow from the origin.
      */
    override def isEvaluationFact: Boolean = !getOriginId.name.startsWith(SPLLIFT_REWRITE_PREFIX)

    override def toText: String = {
        val originSource = getOriginSource
        val from = "\t\tFrom:\t" + originSource.getType.getName + " at: " + originSource.getType.getName.getPositionFrom
        val stmt = "\t\tSourcestatement:\t" + originSource.getCIFGStmt.toText
        val stmt2 = "\t\tSinkstatement:\t" + this.cfgNode.toText + " at: " + this.cfgNode.get.getPositionFrom
        from + "\n" + stmt + "\n" + stmt2
    }

    override def isEquivalentTo(other: CFlowFact, configuration: SimpleConfiguration): Boolean = {
        if (!other.isInstanceOf[Sink]) return false

        val otherSink = other.asInstanceOf[Sink]

        lazy val stmtProduct = ProductDerivation.deriveProduct(cfgNode.get, configuration.getTrueFeatures)
        lazy val eqStmt = stmtProduct.equals(otherSink.cfgNode.get)

        lazy val originStmtProduc = ProductDerivation.deriveProduct(getDefinition(source).getCIFGStmt.get, configuration.getTrueFeatures)
        lazy val eqOriginStmt = originStmtProduc.equals(getDefinition(otherSink.source).getCIFGStmt.get)

        lazy val eqLocation = otherSink.getOriginSource.getCIFGStmt.get.range.equals(getOriginSource.getCIFGStmt.get.range)
        lazy val eqOrigin = otherSink.cfgNode.get.range.equals(cfgNode.get.range)

        // By comparing the origin and target location we save expensive ast product generation but still get the same result as we only compare satisfiable lifting facts according to the current configuration
        eqLocation && eqOrigin
    }

    def getOriginSource : Source = getDefinition(source)

    def getOriginId : Id = getOriginSource.getType.getName
}

case class SinkToAssignment(override val cfgNode: CInterCFGNode, override val source: Source, assignee: Id) extends Sink(cfgNode, source) {

    /**
      * Sinks are always interesting facts for our evaluation strategy, however rewriting introduces some variant specific flows only.
      * We ignore flows for intermediate variables now as they differ for each variant.
      * Nevertheless, no interesting original flow is ignored, as we only ignore the tmp-variable flow itself,
      * but not the reaching information flow from the origin.
      */
    override def isEvaluationFact: Boolean = !assignee.name.startsWith(SPLLIFT_REWRITE_PREFIX) && super.isEvaluationFact

    override def isEquivalentTo(other: CFlowFact, configuration: SimpleConfiguration): Boolean = {
        if (!other.isInstanceOf[SinkToAssignment]) return false

        val otherSink = other.asInstanceOf[SinkToAssignment]

        assignee.equals(otherSink.assignee) && super.isEquivalentTo(other, configuration)
    }

    override def canEqual(that: Any): Boolean = that.isInstanceOf[SinkToAssignment]
}

case class SinkToUse(override val cfgNode: CInterCFGNode, override val source: Source) extends Sink(cfgNode, source) {
    override def isEquivalentTo(other: CFlowFact, configuration: SimpleConfiguration): Boolean =
        if (!other.isInstanceOf[SinkToUse]) false
        else super.isEquivalentTo(other, configuration)

    override def canEqual(that: Any): Boolean = that.isInstanceOf[SinkToUse]
}