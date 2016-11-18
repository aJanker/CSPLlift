package de.fosd.typechef.cspllift.informationflow

import de.fosd.typechef.cspllift.CSPLliftTestOptions

/*
 * Test configuration of the TypeChef-IFDS connector to Heros.
 */
class InformationFlowTestOptions(interface : Option[String] = None) extends CSPLliftTestOptions(interface) {
    override def IFDSTaintAnalysis: Boolean = true

    override def resolveFunctionPointer: Boolean = true

    override def isProfiling: Boolean = false

    override def initProfiling: Boolean = false

    override def getProfileType: Option[String] = None

    override def noInitalSeeds: Boolean = false

    override def warmupJVM: Boolean = false

    override def includeHeaderVariability: Boolean = false
}
