package de.fosd.typechef.cspllift.cifdsproblem.informationflow

import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.cspllift.cifdsproblem.{CFlowFact, CZeroFact}
import de.fosd.typechef.cspllift.evaluation.SimpleConfiguration
import de.fosd.typechef.featureexpr.FeatureExpr
import de.fosd.typechef.featureexpr.bdd.{BDDFeatureExpr, BDDFeatureExprFactory}
import de.fosd.typechef.parser.c.{AST, Id, PrettyPrinter}

trait InformationFlow2 extends CFlowFact  {
    override def isEquivalentTo(other: CFlowFact, configuration: SimpleConfiguration): Boolean = false
    override def isInterestingFact: Boolean = false
    override def getConditions : Set[BDDFeatureExpr] = Set()
    override def toText: String = toString
}

case class Zero(override val flowCondition: FeatureExpr = BDDFeatureExprFactory.True) extends InformationFlow2 with CZeroFact

sealed abstract class InformationFlowFact(val stmt : Opt[AST]) extends InformationFlow2

sealed abstract class Sink(override val stmt : Opt[AST], val source : Source) extends InformationFlowFact(stmt) {
    override def toText: String = {
        val from = "\t\tFrom:\t" + source.getId.name + " at: " + source.getStmt.entry.getPositionFrom
        val stmt = "\t\tStatement:\t" + PrettyPrinter.print(source.getStmt.entry)
        from + "\n" + stmt
    }
}

case class SinkToUse(override val stmt : Opt[AST], override val source : Source) extends Sink(stmt, source)

case class SinkToAssignment(override val stmt : Opt[AST], override val source : Source, assignee : Id) extends Sink(stmt, source)

sealed abstract class Source(id : Id, stmt : Opt[AST], scope : Int) extends InformationFlowFact(stmt) {
    def getScope = scope
    def getId = id
    def getStmt = stmt
}

case class VarSource(name : Id, override val stmt : Opt[AST], isSourceOf : List[Source], usedIn : List[Opt[AST]], scope : Int) extends Source(name, stmt, scope)

case class VarSourceOf(name : Id, override val stmt : Opt[AST], source : Source, usedIn : List[Opt[AST]], scope : Int) extends Source(name, stmt, scope)

