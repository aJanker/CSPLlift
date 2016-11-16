package de.fosd.typechef.cspllift.cifdsproblem.informationflow.flowfact.sinkorsource

import de.fosd.typechef.crewrite.ProductDerivation
import de.fosd.typechef.cspllift.CICFGNode
import de.fosd.typechef.cspllift.cifdsproblem.CFlowFact
import de.fosd.typechef.cspllift.evaluation.SimpleConfiguration
import de.fosd.typechef.parser.c.Id

sealed abstract class SourceType(name: Id) {
    def getName: Id = name
    def isEquivalentTo(other : SourceType, configuration: SimpleConfiguration): Boolean
}

case class Struct(name: Id, field: Option[Source]) extends SourceType(name) {
    override def canEqual(that: Any): Boolean = that.isInstanceOf[Struct]
    override def isEquivalentTo(other: SourceType, configuration: SimpleConfiguration): Boolean = {
        if (!canEqual(other)) return false
        val that = other.asInstanceOf[Struct]
        that.getName.equals(getName) && equivalentField(field, that.field, configuration)
    }

    private def equivalentField(thisField : Option[Source], otherField : Option[Source], configuration: SimpleConfiguration) : Boolean = {
        if (thisField.isDefined && otherField.isDefined) thisField.get.isEquivalentTo(otherField.get, configuration)
        else thisField.isEmpty && otherField.isEmpty
    }
}

case class Variable(name: Id) extends SourceType(name) {
    override def canEqual(that: Any): Boolean = that.isInstanceOf[Variable]
    override def isEquivalentTo(other: SourceType, configuration: SimpleConfiguration): Boolean = {
        if (!canEqual(other)) return false
        other.getName.equals(getName)
    }
}

sealed abstract class Source(sourceType: SourceType, override val cICFGStmt: CICFGNode, scope: Int) extends SinkOrSource(cICFGStmt) {

    def getScope = scope

    def getType : SourceType = sourceType

    def getCIFGStmt = cICFGStmt

    override def isEquivalentTo(other: CFlowFact, configuration: SimpleConfiguration): Boolean = {
        if (!canEqual(other)) return false

        val otherSource = other.asInstanceOf[Source]

        lazy val stmtProduct = ProductDerivation.deriveProduct(getCIFGStmt.getStmt.entry, configuration.getTrueFeatures)
        lazy val eqStmt = stmtProduct.equals(otherSource.getCIFGStmt.getStmt.entry)

        otherSource.getType.isEquivalentTo(getType, configuration) && eqStmt
    }
}

case class SourceDefinition(sourceType: SourceType, override val cICFGStmt: CICFGNode, scope: Int) extends Source(sourceType, cICFGStmt, scope)

case class SourceDefinitionOf(sourceType: SourceType, override val cICFGStmt: CICFGNode, define: SourceDefinition, scope: Int) extends Source(sourceType, cICFGStmt, scope) {
    def getDefinition = define
}