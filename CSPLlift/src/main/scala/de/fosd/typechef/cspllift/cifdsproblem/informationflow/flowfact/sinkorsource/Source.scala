package de.fosd.typechef.cspllift.cifdsproblem.informationflow.flowfact.sinkorsource

import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.crewrite.ProductDerivation
import de.fosd.typechef.cspllift.cifdsproblem.CFlowFact
import de.fosd.typechef.cspllift.evaluation.SimpleConfiguration
import de.fosd.typechef.parser.c.{AST, Id}

sealed abstract class SourceType(name: Id) {
    def getName: Id = name
}

case class Struct(name: Id, field: Option[Source]) extends SourceType(name)

case class Variable(name: Id) extends SourceType(name)

sealed abstract class Source(sourceType: SourceType, override val stmt: Opt[AST], scope: Int, previousStmt: Option[AST]) extends SinkOrSource(stmt) {
    def getScope = scope

    def getType : SourceType = sourceType

    def getStmt = stmt

    def getPreviousStmt = previousStmt

    override def get: Source = this

    override def isEquivalentTo(other: CFlowFact, configuration: SimpleConfiguration): Boolean = {
        if (!canEqual(other)) return false

        val otherSource = other.asInstanceOf[Source]

        lazy val stmtProduct = ProductDerivation.deriveProduct(getStmt.entry, configuration.getTrueFeatures)
        lazy val eqStmt = stmtProduct.equals(otherSource.getStmt.entry)

        otherSource.getType.equals(getType) && eqStmt
    }
}

case class SourceDefinition(sourceType: SourceType, override val stmt: Opt[AST], scope: Int, previousStmt: Option[AST]) extends Source(sourceType, stmt, scope, previousStmt)

case class SourceDefinitionOf(sourceType: SourceType, override val stmt: Opt[AST], define: SourceDefinition, scope: Int, previousStmt: Option[AST]) extends Source(sourceType, stmt, scope, previousStmt) {
    def getDefinition = define
}