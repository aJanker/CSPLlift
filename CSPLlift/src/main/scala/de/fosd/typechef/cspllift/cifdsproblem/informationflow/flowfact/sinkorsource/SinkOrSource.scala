package de.fosd.typechef.cspllift.cifdsproblem.informationflow.flowfact.sinkorsource

import de.fosd.typechef.cspllift.cifdsproblem.informationflow.InformationFactOperations
import de.fosd.typechef.cspllift.cifdsproblem.informationflow.flowfact.InformationFlowFact
import de.fosd.typechef.cspllift.cintercfg.CICFGNode

abstract class SinkOrSource(val cICFGStmt: CICFGNode) extends InformationFlowFact with InformationFactOperations
