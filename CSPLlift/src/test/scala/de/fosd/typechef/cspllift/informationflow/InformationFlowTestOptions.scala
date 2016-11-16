package de.fosd.typechef.cspllift.informationflow

import de.fosd.typechef.cspllift.CSPLliftTestOptions

class InformationFlowTestOptions(interface : Option[String] = None) extends CSPLliftTestOptions(interface) {
    override def liftTaintAnalysis: Boolean = true

    override def resolveFunctionPointer: Boolean = true

    override def isProfiling: Boolean = false

    override def initProfiling: Boolean = false

    override def getProfileType: Option[String] = None
}
