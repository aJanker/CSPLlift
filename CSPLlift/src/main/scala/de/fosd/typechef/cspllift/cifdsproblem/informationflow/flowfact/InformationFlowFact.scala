package de.fosd.typechef.cspllift.cifdsproblem.informationflow.flowfact

import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.cspllift.cifdsproblem.{CFlowFact, CZeroFact}
import de.fosd.typechef.cspllift.evaluation.SimpleConfiguration
import de.fosd.typechef.featureexpr.FeatureExpr
import de.fosd.typechef.featureexpr.bdd.BDDFeatureExprFactory
import de.fosd.typechef.parser.c.AST

trait InformationFlowFact extends CFlowFact  {
    override def isEquivalentTo(other: CFlowFact, configuration: SimpleConfiguration): Boolean = equals(other)

    override def isInterestingFact: Boolean = false

    override def toText: String = toString

    override def get: CFlowFact = this
}

case class Zero(override val flowCondition: FeatureExpr = BDDFeatureExprFactory.True) extends InformationFlowFact with CZeroFact

abstract class SinkOrSource(val stmt: Opt[AST]) extends InformationFlowFact


