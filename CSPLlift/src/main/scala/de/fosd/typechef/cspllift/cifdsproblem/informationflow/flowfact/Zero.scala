package de.fosd.typechef.cspllift.cifdsproblem.informationflow.flowfact

import de.fosd.typechef.cspllift.cifdsproblem.CZeroFact
import de.fosd.typechef.featureexpr.FeatureExpr
import de.fosd.typechef.featureexpr.bdd.BDDFeatureExprFactory

case class Zero(override val flowCondition: FeatureExpr = BDDFeatureExprFactory.True) extends InformationFlowFact with CZeroFact
