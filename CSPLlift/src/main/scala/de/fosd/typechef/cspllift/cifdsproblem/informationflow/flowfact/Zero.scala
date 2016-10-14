package de.fosd.typechef.cspllift.cifdsproblem.informationflow.flowfact

import de.fosd.typechef.cspllift.CICFGStmt
import de.fosd.typechef.cspllift.cifdsproblem.CZeroFact
import de.fosd.typechef.featureexpr.FeatureExpr
import de.fosd.typechef.featureexpr.bdd.BDDFeatureExprFactory

case class Zero(override val flowCondition: FeatureExpr = BDDFeatureExprFactory.True, stack: List[CICFGStmt] = List()) extends InformationFlowFact with CZeroFact {
    override def canEqual(that: Any): Boolean = that.isInstanceOf[Zero]

    override def equals(obj: scala.Any): Boolean = canEqual(obj)
}
