package de.fosd.typechef.cspllift

import de.fosd.typechef.cspllift.evaluation.SimpleConfiguration

/**
  *
  * Configuration of the ICFG for SPLlift.
  *
  */
trait CInterCFGConfiguration {

    def getModuleInterfacePath: Option[String]

    def getConfiguration: Option[SimpleConfiguration] = None
    def getTrueSet: Option[Set[String]] = None
    def getFalseSet: Option[Set[String]] = None

    def getStopWatchPrefix: String

    def getGraphEntryFunctionNames: List[String] = List("main")

    /*
     * If enabled, no warnings for discovered type errors are shown.
     */
    def silentTypeCheck: Boolean = true

    /*
     * Sets the option if in our analysis system functions should be visited or ignored.
     */
    def pseudoVisitingSystemLibFunctions: Boolean

    /**
      * Enables or disables the function pointer computation for a more precise callgraph.
      */
    def computePointer: Boolean = true
}

class DefaultCInterCFGConfiguration(moduleInterfacePath: Option[String] = None, stopWatchPrefix: String = "") extends CInterCFGConfiguration {
    override def getModuleInterfacePath: Option[String] = moduleInterfacePath
    override def pseudoVisitingSystemLibFunctions = true
    override def getStopWatchPrefix: String = stopWatchPrefix
}

class ConfigurationBasedCInterCFGConfiguration(moduleInterfacePath: Option[String] = None, configuration: Option[SimpleConfiguration] = None, stopWatchPrefix: String = "") extends DefaultCInterCFGConfiguration(moduleInterfacePath, stopWatchPrefix) {
    override def getConfiguration: Option[SimpleConfiguration] = configuration

    override def getTrueSet: Option[Set[String]] = if (getConfiguration.isDefined) Some(getConfiguration.get.getTrueFeatures) else None

    override def getFalseSet: Option[Set[String]] = if (getConfiguration.isDefined) Some(getConfiguration.get.getFalseFeatures) else None

    override def computePointer: Boolean = false
}