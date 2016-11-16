package de.fosd.typechef.cspllift.cifdsproblem.informationflow.flowfact.sinkorsource

import de.fosd.typechef.cspllift.CICFGNode
import de.fosd.typechef.cspllift.cifdsproblem.informationflow.InformationFactOperations
import de.fosd.typechef.cspllift.cifdsproblem.informationflow.flowfact.InformationFlowFact

abstract class SinkOrSource(val cICFGStmt: CICFGNode) extends InformationFlowFact with InformationFactOperations
