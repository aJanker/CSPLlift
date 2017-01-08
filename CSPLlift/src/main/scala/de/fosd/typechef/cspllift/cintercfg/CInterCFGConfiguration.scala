package de.fosd.typechef.cspllift.cintercfg

import de.fosd.typechef.customization.conditional.SimpleConfiguration

/**
  * Configuration of the interprocedural control-flow graph for TypeChef IFDS connector.
  */
trait CInterCFGConfiguration {

    def getModuleInterfacePath: Option[String]

    /*
     * Set if we are analyzing a concrete product variant.
     */
    def getConfiguration: Option[SimpleConfiguration] = None
    def getTrueSet: Option[Set[String]] = None
    def getFalseSet: Option[Set[String]] = None

    /**
      * A list of function names which are considered as entry points for the interprocedural control-flow graph.
      */
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
    def computePointer: Boolean
}

class DefaultCInterCFGConfiguration(moduleInterfacePath: Option[String] = None, pointerComputation: Boolean = true) extends CInterCFGConfiguration {
    override def getModuleInterfacePath: Option[String] = moduleInterfacePath
    override def pseudoVisitingSystemLibFunctions = true
    override def computePointer: Boolean = pointerComputation
}

class ConfigurationBasedCInterCFGConfiguration(moduleInterfacePath: Option[String] = None, pointerComputation: Boolean = true, configuration: Option[SimpleConfiguration] = None, stopWatchPrefix: String = "") extends DefaultCInterCFGConfiguration(moduleInterfacePath, pointerComputation) {
    override def getConfiguration: Option[SimpleConfiguration] = configuration
    override def getTrueSet: Option[Set[String]] = if (getConfiguration.isDefined) Some(getConfiguration.get.getTrueFeatures) else None
    override def getFalseSet: Option[Set[String]] = if (getConfiguration.isDefined) Some(getConfiguration.get.getFalseFeatures) else None
}