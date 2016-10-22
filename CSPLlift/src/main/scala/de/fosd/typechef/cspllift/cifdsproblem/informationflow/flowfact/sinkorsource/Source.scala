package de.fosd.typechef.cspllift.cifdsproblem.informationflow.flowfact.sinkorsource

import de.fosd.typechef.crewrite.ProductDerivation
import de.fosd.typechef.cspllift.CICFGStmt
import de.fosd.typechef.cspllift.cifdsproblem.CFlowFact
import de.fosd.typechef.cspllift.evaluation.SimpleConfiguration
import de.fosd.typechef.parser.c.Id

sealed abstract class SourceType(name: Id) {
    def getName: Id = name
}

case class Struct(name: Id, field: Option[Source]) extends SourceType(name)

case class Variable(name: Id) extends SourceType(name)

sealed abstract class Source(sourceType: SourceType, override val cICFGStmt: CICFGStmt, scope: Int) extends SinkOrSource(cICFGStmt) {
    def getScope = scope

    def getType : SourceType = sourceType

    def getCIFGStmt = cICFGStmt

    override def get: Source = this

    override def isEquivalentTo(other: CFlowFact, configuration: SimpleConfiguration): Boolean = {
        if (!canEqual(other)) return false

        val otherSource = other.asInstanceOf[Source]

        lazy val stmtProduct = ProductDerivation.deriveProduct(getCIFGStmt.getStmt.entry, configuration.getTrueFeatures)
        lazy val eqStmt = stmtProduct.equals(otherSource.getCIFGStmt.getStmt.entry)

        otherSource.getType.equals(getType) && eqStmt
    }
}

case class SourceDefinition(sourceType: SourceType, override val cICFGStmt: CICFGStmt, scope: Int) extends Source(sourceType, cICFGStmt, scope)

case class SourceDefinitionOf(sourceType: SourceType, override val cICFGStmt: CICFGStmt, define: SourceDefinition, scope: Int) extends Source(sourceType, cICFGStmt, scope) {
    def getDefinition = define
}