package de.fosd.typechef.cspllift.cifdsproblem.informationflow

import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.cspllift.cifdsproblem.{CFlowFact, CZeroFact}
import de.fosd.typechef.cspllift.commons.RewritingRules
import de.fosd.typechef.cspllift.evaluation.SimpleConfiguration
import de.fosd.typechef.featureexpr.FeatureExpr
import de.fosd.typechef.featureexpr.bdd.BDDFeatureExprFactory
import de.fosd.typechef.parser.c.{AST, Id, PrettyPrinter}

trait FlowFact extends Product with CFlowFact with RewritingRules with Cloneable {
    override def clone(): FlowFact.this.type = super.clone().asInstanceOf[FlowFact.this.type]

    override def isEquivalentTo(other: CFlowFact, configuration: SimpleConfiguration): Boolean = equals(other)

    override def isInterestingFact: Boolean = false

    override def toText: String = toString

    override def get: CFlowFact = this
}

case class Zero(override val flowCondition: FeatureExpr = BDDFeatureExprFactory.True) extends FlowFact with CZeroFact

sealed abstract class InformationFlowFact(val stmt: Opt[AST]) extends FlowFact

sealed abstract class Sink(override val stmt: Opt[AST], val source: Source) extends InformationFlowFact(stmt) {

    override def isInterestingFact: Boolean = true

    override def toText: String = {
        val from = "\t\tFrom:\t" + source.getId.name + " at: " + source.getId.getPositionFrom
        val stmt = "\t\tSourcestatement:\t" + PrettyPrinter.print(source.getStmt.entry)
        val stmt2 = "\t\tSinkstatement:\t" + PrettyPrinter.print(this.stmt.entry) + this.stmt.entry.getPositionFrom
        from + "\n" + stmt + "\n" + stmt2
    }

    override def isEquivalentTo(other: CFlowFact, configuration: SimpleConfiguration): Boolean = {
        if (!other.isInstanceOf[Sink]) return false

        val otherSink = other.asInstanceOf[Sink]

        lazy val stmtProduct = deriveProductWithCondition(stmt.entry, configuration.getTrueFeatures)
        lazy val eqStmt = stmtProduct.equals(otherSink.stmt.entry)

        source.isEquivalentTo(otherSink.source, configuration) && eqStmt
    }
}

case class SinkToUse(override val stmt: Opt[AST], override val source: Source) extends Sink(stmt, source) {
    override def get: CFlowFact = SinkToUse(stmt, source.get)

    override def isEquivalentTo(other: CFlowFact, configuration: SimpleConfiguration): Boolean =
        if (!other.isInstanceOf[SinkToUse]) false
        else super.isEquivalentTo(other, configuration)
}

case class SinkToAssignment(override val stmt: Opt[AST], override val source: Source, assignee: Id) extends Sink(stmt, source) {
    override def get: CFlowFact = SinkToAssignment(stmt, source.get, assignee)

    override def isEquivalentTo(other: CFlowFact, configuration: SimpleConfiguration): Boolean = {
        if (!other.isInstanceOf[SinkToAssignment]) return false

        val otherSink = other.asInstanceOf[SinkToAssignment]

        assignee.equals(otherSink.assignee) && super.isEquivalentTo(other, configuration)
    }
}

sealed abstract class Source(sourceId: Id, stmt: Opt[AST], scope: Int, last: Option[AST]) extends InformationFlowFact(stmt) {
    def getScope = scope

    def getId = sourceId

    def getStmt = stmt

    def getLastStmt = last

    override def get: Source = this

    override def isEquivalentTo(other: CFlowFact, configuration: SimpleConfiguration): Boolean = {
        if (!other.isInstanceOf[Source]) return false

        val otherSource = other.asInstanceOf[Source]

        lazy val stmtProduct = deriveProductWithCondition(getStmt.entry, configuration.getTrueFeatures)
        lazy val eqStmt = stmtProduct.equals(otherSource.getStmt.entry)

        otherSource.getId.equals(getId) && eqStmt
    }

    override def equals(other: scala.Any): Boolean = {
        if (!other.isInstanceOf[Source]) return false

        val otherSource = other.asInstanceOf[Source]

        lazy val eqStmt = getStmt.equals(otherSource.getStmt)

        otherSource.getId.equals(getId) && eqStmt && scope.equals(otherSource.getScope) && getLastStmt.equals(otherSource.getLastStmt)
    }
}

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

sealed abstract class SourceOf(id: Id, stmt: Opt[AST], source: Source, scope: Int, last : Option[AST]) extends Source(id, stmt, scope, last) {

    def getSource : Source = source

    override def equals(other: scala.Any): Boolean = {
        if (!other.isInstanceOf[SourceOf]) return false

        val otherSourceOf = other.asInstanceOf[SourceOf]

        val eqStmt = getStmt.equals(otherSourceOf.getStmt)
        otherSourceOf.getId.equals(getId) && eqStmt && scope.equals(otherSourceOf.getScope) && getLastStmt.equals(otherSourceOf.getLastStmt) && source.equals(otherSourceOf.getSource)
    }
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
}