package de.fosd.typechef.cspllift.cifdsproblem.informationflow.flowfact.sinkorsource

import de.fosd.typechef.cspllift.cifdsproblem.informationflow.InformationFlowFactOperations
import de.fosd.typechef.cspllift.cifdsproblem.informationflow.flowfact.InformationFlowFact
import de.fosd.typechef.cspllift.cintercfg.CInterCFGNode

abstract class SinkOrSource(val cfgNode: CInterCFGNode) extends InformationFlowFact with InformationFlowFactOperations
