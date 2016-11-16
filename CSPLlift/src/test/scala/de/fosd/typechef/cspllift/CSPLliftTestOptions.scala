package de.fosd.typechef.cspllift

import de.fosd.typechef.cspllift.options.CSPLliftOptions


abstract class CSPLliftTestOptions(interface: Option[String] = None) extends CSPLliftOptions {
    override def getCLinkingInterfacePath: Option[String] = interface

    override def getInformationFlowGraphExtension: String = ""

    override def getInformationFLowGraphFilename: String = ""

    override def getInformationFlowGraphsOutputDir: String = ""

    override def getVariantsOutputDir: String = ""

    override def getProfilingDir: String = ""

    override def getCModuleInterfaceMergeDir: String = ""

    override def isLiftEvaluationModeEnabled: Boolean = true

    override def isLiftAnalysisEnabled: Boolean = false

    override def isLiftBenchmarkEnabled: Boolean = false

    override def isMergeLinkingInterfacesEnabled: Boolean = false

    override def isLiftSamplingEvaluationEnabled: Boolean = true

    override def isLiftSingleEvaluationEnabled: Boolean = true

    override def isLiftPrintExplodedSuperCallGraphEnabled: Boolean = false

    override def writeVariants: Boolean = false
}
