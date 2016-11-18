package de.fosd.typechef.cspllift

import de.fosd.typechef.cspllift.options.CSPLliftOptions


abstract class CSPLliftTestOptions(interface: Option[String] = None) extends CSPLliftOptions {
    override def getCLinkingInterfacePath: Option[String] = interface

    override def getInformationFlowGraphExtension: String = ""

    override def getInformationFLowGraphFilename: String = ""

    override def getInformationFlowGraphsOutputDir: String = ""

    override def getVariantsOutputDir: String = ""

    override def getOutputLocation: String = ""

    override def getCModuleInterfaceMergeDir: String = ""

    override def isIFDSEvaluationModeEnabled: Boolean = true

    override def isIFDSAnalysisEnabled: Boolean = false

    override def isIFDSBenchmarkEnabled: Boolean = false

    override def isMergeLinkingInterfacesEnabled: Boolean = false

    override def isIFDSSamplingEvaluationEnabled: Boolean = true

    override def isIFDSSingleEvaluationEnabled: Boolean = true

    override def isIFDSPrintExplodedSuperCallGraphEnabled: Boolean = false

    override def writeVariants: Boolean = false
}
