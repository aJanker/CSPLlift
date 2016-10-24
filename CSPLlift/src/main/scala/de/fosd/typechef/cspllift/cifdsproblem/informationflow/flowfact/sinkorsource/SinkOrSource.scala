package de.fosd.typechef.cspllift.cifdsproblem.informationflow.flowfact.sinkorsource

import de.fosd.typechef.cspllift.CICFGStmt
import de.fosd.typechef.cspllift.cifdsproblem.informationflow.flowfact.InformationFlowFact

abstract class SinkOrSource(val cICFGStmt: CICFGStmt) extends InformationFlowFact
