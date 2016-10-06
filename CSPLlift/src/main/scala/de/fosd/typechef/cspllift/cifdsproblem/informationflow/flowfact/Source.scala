package de.fosd.typechef.cspllift.cifdsproblem.informationflow.flowfact

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

    def getType : SourceType

    def getStmt = stmt

    def getPreviousStmt = previousStmt

    override def get: Source = this

    override def isEquivalentTo(other: CFlowFact, configuration: SimpleConfiguration): Boolean = {
        if (!canEqual(other)) return false

        val otherSource = other.asInstanceOf[SourceDefinition]

        lazy val stmtProduct = ProductDerivation.deriveProduct(getStmt.entry, configuration.getTrueFeatures)
        lazy val eqStmt = stmtProduct.equals(otherSource.getStmt.entry)

        otherSource.getType.equals(getType) && eqStmt
    }

    override def canEqual(other: Any): Boolean = other.isInstanceOf[Source]

    override def equals(other: scala.Any): Boolean = {
        if (!canEqual(other)) return false

        val otherSource = other.asInstanceOf[Source]
        lazy val eqStmt = getStmt.equals(otherSource.getStmt)

        otherSource.getType.equals(getType) && eqStmt && scope.equals(otherSource.getScope) && getPreviousStmt.equals(otherSource.getPreviousStmt)
    }

    override def hashCode(): Int = getType.hashCode() + getStmt.entry.hashCode() + getStmt.condition.hashCode() + getScope.hashCode() + previousStmt.hashCode()
}

case class SourceDefinition(sourceType: SourceType, override val stmt: Opt[AST], scope: Int, previousStmt: Option[AST]) extends Source(sourceType, stmt, scope, previousStmt) {
    override def canEqual(other: Any): Boolean = other.isInstanceOf[SourceDefinition]

    override def equals(other: scala.Any): Boolean = {
        if (!canEqual(other)) false
        else super.equals(other)
    }

    override def getType: SourceType = sourceType
}

case class SourceDefinitionOf(sourceType: SourceType, override val stmt: Opt[AST], define: SourceDefinition, scope: Int, previousStmt: Option[AST]) extends Source(sourceType, stmt, scope, previousStmt) {

    def getDefinition: SourceDefinition = define

    override def canEqual(other: Any): Boolean = other.isInstanceOf[SourceDefinitionOf]

    override def equals(other: scala.Any): Boolean = {
        if (!canEqual(other)) return false

        val otherSourceOf = other.asInstanceOf[SourceDefinitionOf]

        getDefinition.equals(otherSourceOf.getDefinition) && super.equals(other)
    }

    override def getType: SourceType = sourceType

    override def hashCode(): Int = super.hashCode() + define.hashCode()
}

/*
case class StructSource(name: Id, field: Option[Source], override val stmt: Opt[AST], scope: Int, last: Option[AST]) extends Source(name, stmt, scope, last) {
    override def get : StructSource = StructSource(name, field, stmt, scope, None)

    override def isEquivalentTo(other: CFlowFact, configuration: SimpleConfiguration): Boolean =
        if (!other.isInstanceOf[StructSource]) false
        else {
            val otherStructSource = other.asInstanceOf[StructSource]

            if (field.isDefined && otherStructSource.field.isDefined) field.get.isEquivalentTo(otherStructSource.field.get, configuration) && super.isEquivalentTo(other, configuration)
            else if (field.isEmpty && otherStructSource.field.isEmpty) super.isEquivalentTo(other, configuration)
            else false
        }


    override def equals(other: scala.Any): Boolean = {
        if (!other.isInstanceOf[StructSource]) false
        else other.asInstanceOf[StructSource].field.equals(field) && super.equals(other)
    }

    override def hashCode(): Int = getId.hashCode() + getStmt.entry.hashCode() + getStmt.condition.hashCode() + getScope.hashCode() + field.hashCode() + last.hashCode()
}

case class VarSource(name: Id, override val stmt: Opt[AST], scope: Int, last: Option[AST]) extends Source(name, stmt, scope, last) {
    override def get : VarSource = VarSource(name, stmt, scope, None)

    override def isEquivalentTo(other: CFlowFact, configuration: SimpleConfiguration): Boolean =
        if (!other.isInstanceOf[VarSource]) false
        else super.isEquivalentTo(other, configuration)

    override def equals(other: scala.Any): Boolean = {
        if (!other.isInstanceOf[VarSource]) false
        else super.equals(other)
    }

    override def hashCode(): Int = getId.hashCode() + getStmt.entry.hashCode() + getStmt.condition.hashCode() + getScope.hashCode() + last.hashCode()
}

case class StructSourceOf(name: Id, field: Option[Source], override val stmt: Opt[AST], source: Source, scope: Int, last : Option[AST]) extends SourceOf(name, stmt, source, scope, last) {
    override def get : StructSourceOf = StructSourceOf(name, field, stmt, source.get, scope, None)

    override def isEquivalentTo(other: CFlowFact, configuration: SimpleConfiguration): Boolean =
        if (!other.isInstanceOf[StructSourceOf]) false
        else {
            val otherStructSource = other.asInstanceOf[StructSource]

            if (field.isDefined && otherStructSource.field.isDefined) field.get.isEquivalentTo(otherStructSource.field.get, configuration) && super.isEquivalentTo(other, configuration)
            else if (field.isEmpty && otherStructSource.field.isEmpty) super.isEquivalentTo(other, configuration)
            else false
        }

    override def equals(other: scala.Any): Boolean =
        if (!other.isInstanceOf[StructSourceOf]) false
        else other.asInstanceOf[StructSourceOf].field.equals(field) && super.equals(other)

    override def hashCode(): Int = getId.hashCode() + getStmt.entry.hashCode() + getStmt.condition.hashCode() + getScope.hashCode() + source.hashCode() + field.hashCode() + last.hashCode()
}

case class VarSourceOf(name: Id, override val stmt: Opt[AST], source: Source, scope: Int, last : Option[AST]) extends SourceOf(name, stmt, source, scope, last) {
    override def get : VarSourceOf = VarSourceOf(name, stmt, source.get, scope, None)

    override def isEquivalentTo(other: CFlowFact, configuration: SimpleConfiguration): Boolean =
        if (!other.isInstanceOf[VarSourceOf]) false
        else super.isEquivalentTo(other, configuration)

     override def equals(other: scala.Any): Boolean =
        if (!other.isInstanceOf[VarSourceOf]) false
        else super.equals(other)


    override def hashCode(): Int = getId.hashCode() + getStmt.entry.hashCode() + getStmt.condition.hashCode() + getScope.hashCode() + source.hashCode() + last.hashCode()
} */