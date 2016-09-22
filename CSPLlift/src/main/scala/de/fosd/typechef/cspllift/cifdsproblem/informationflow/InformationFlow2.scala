package de.fosd.typechef.cspllift.cifdsproblem.informationflow

import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.crewrite.ProductDerivation
import de.fosd.typechef.cspllift.cifdsproblem.{CFlowFact, CZeroFact}
import de.fosd.typechef.cspllift.evaluation.SimpleConfiguration
import de.fosd.typechef.featureexpr.FeatureExpr
import de.fosd.typechef.featureexpr.bdd.BDDFeatureExprFactory
import de.fosd.typechef.parser.c.{AST, Id, PrettyPrinter}

trait InformationFlow2 extends Product with Cloneable with CFlowFact {
    override def clone(): InformationFlow2.this.type = super.clone().asInstanceOf[InformationFlow2.this.type]

    override def isEquivalentTo(other: CFlowFact, configuration: SimpleConfiguration): Boolean = equals(other)

    override def isInterestingFact: Boolean = false

    override def toText: String = toString
}

case class Zero(override val flowCondition: FeatureExpr = BDDFeatureExprFactory.True) extends InformationFlow2 with CZeroFact

sealed abstract class InformationFlowFact(val stmt: Opt[AST]) extends InformationFlow2

sealed abstract class Sink(override val stmt: Opt[AST], val source: Source) extends InformationFlowFact(stmt) {

    override def isInterestingFact: Boolean = true

    override def toText: String = {
        val from = "\t\tFrom:\t" + source.getId.name + " at: " + source.getId.getPositionFrom
        val stmt = "\t\tSourcestatement:\t" + PrettyPrinter.print(source.getStmt.entry)
        val stmt2 = "\t\tSinkstatement:\t" + PrettyPrinter.print(this.stmt.entry)
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

case class SinkToUse(override val stmt: Opt[AST], override val source: Source) extends Sink(stmt, source) {
    override def isEquivalentTo(other: CFlowFact, configuration: SimpleConfiguration): Boolean =
        if (!other.isInstanceOf[SinkToUse]) false
        else super.isEquivalentTo(other, configuration)
}

case class SinkToAssignment(override val stmt: Opt[AST], override val source: Source, assignee: Id) extends Sink(stmt, source) {
    override def isEquivalentTo(other: CFlowFact, configuration: SimpleConfiguration): Boolean = {
        if (!other.isInstanceOf[SinkToAssignment]) return false

        val otherSink = other.asInstanceOf[SinkToAssignment]

        assignee.equals(otherSink.assignee) && super.isEquivalentTo(other, configuration)
    }
}

sealed abstract class Source(id: Id, stmt: Opt[AST], scope: Int) extends InformationFlowFact(stmt) {
    def getScope = scope

    def getId = id

    def getStmt = stmt

    override def isEquivalentTo(other: CFlowFact, configuration: SimpleConfiguration): Boolean = {
        if (!other.isInstanceOf[Source]) return false

        val otherSource = other.asInstanceOf[Source]

        lazy val stmtProduct = ProductDerivation.deriveProduct(getStmt.entry, configuration.getTrueFeatures)
        lazy val eqStmt = stmtProduct.equals(otherSource.getStmt.entry)

        otherSource.getId.equals(getId) && eqStmt
    }
}

case class VarSource(name: Id, override val stmt: Opt[AST], isSourceOf: List[Source], usedIn: List[Opt[AST]], scope: Int) extends Source(name, stmt, scope) {
    override def isEquivalentTo(other: CFlowFact, configuration: SimpleConfiguration): Boolean =
        if (!other.isInstanceOf[VarSource]) false
        else super.isEquivalentTo(other, configuration)

    override def equals(other: scala.Any): Boolean = {
        if (!other.isInstanceOf[VarSource]) return false

        val otherSource = other.asInstanceOf[VarSource]

        lazy val eqStmt = getStmt.equals(otherSource.getStmt)

        otherSource.getId.equals(getId) && eqStmt && scope.equals(otherSource.getScope)
    }

    override def hashCode(): Int = getId.hashCode() + getStmt.entry.hashCode() + getStmt.condition.hashCode() + getScope.hashCode()
}

case class VarSourceOf(name: Id, override val stmt: Opt[AST], source: Source, usedIn: List[Opt[AST]], scope: Int) extends Source(name, stmt, scope) {
    override def isEquivalentTo(other: CFlowFact, configuration: SimpleConfiguration): Boolean =
        if (!other.isInstanceOf[VarSourceOf]) false
        else super.isEquivalentTo(other, configuration)

    override def equals(other: scala.Any): Boolean = {
        if (!other.isInstanceOf[VarSourceOf]) return false

        val otherSource = other.asInstanceOf[VarSourceOf]

        lazy val eqStmt = getStmt.equals(otherSource.getStmt)

        otherSource.getId.equals(getId) && eqStmt && scope.equals(otherSource.getScope)
    }

    override def hashCode(): Int = getId.hashCode() + getStmt.entry.hashCode() + getStmt.condition.hashCode() + getScope.hashCode() + source.hashCode()
}