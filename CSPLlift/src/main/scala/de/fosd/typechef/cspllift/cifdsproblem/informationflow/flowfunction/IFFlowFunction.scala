package de.fosd.typechef.cspllift.cifdsproblem.informationflow.flowfunction

import java.util

import de.fosd.typechef.cspllift.cifdsproblem.informationflow.flowfact.sinkorsource.{Sink, Source, Struct, Variable}
import de.fosd.typechef.cspllift.cifdsproblem.informationflow.flowfact.{InformationFlowFact, Zero}
import de.fosd.typechef.cspllift.cifdsproblem.informationflow.{InformationFlowConfiguration, InformationFlowProblemOperationsInformation}
import heros.FlowFunction

/**
  * Extended Flow-Function definition for Information-Flow Anlysis in IFDS.
  */
trait IFFlowFunction extends FlowFunction[InformationFlowFact] with InformationFlowConfiguration with InformationFlowProblemOperationsInformation {
    override def computeTargets(fact: InformationFlowFact): util.Set[InformationFlowFact] = fact match {
        case s: Source =>
            s.getType match {
                case _: Struct => computeStruct(s)
                case _: Variable => computeVariable(s)
            }
        case s: Sink => computeSink(s)
        case z: Zero => computeZero(z)
    }

    def computeVariable(source: Source): util.Set[InformationFlowFact]

    def computeStruct(source: Source): util.Set[InformationFlowFact]

    def computeSink(s: Sink): util.Set[InformationFlowFact]

    def computeZero(z: Zero): util.Set[InformationFlowFact]
}
