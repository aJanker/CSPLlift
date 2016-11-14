package de.fosd.typechef.cspllift.cifdsproblem.informationflow.flowfact

import de.fosd.typechef.cspllift.cifdsproblem.CFlowFact
import de.fosd.typechef.cspllift.evaluation.SimpleConfiguration

trait InformationFlowFact extends CFlowFact  {
    override def isEquivalentTo(other: CFlowFact, configuration: SimpleConfiguration): Boolean = equals(other)

    override def isEvaluationFact: Boolean = false

    override def toText: String = toString
}




