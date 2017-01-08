package de.fosd.typechef.cspllift

import java.io.File

import de.fosd.typechef.cspllift.options.CSPLliftOptions


abstract class CSPLliftTestOptions(interface: Option[String] = None) extends CSPLliftOptions {
    override def getCLinkingInterfacePath: Option[String] = interface

    override def getRootDir: Option[String] =
        if (getCLinkingInterfacePath.isDefined) Some(getCLinkingInterfacePath.get.substring(0, getCLinkingInterfacePath.get.lastIndexOf(File.separator)) + "typechef_modules")
        else None

    override def getInformationFlowGraphExtension: String = ""

    override def getInformationFLowGraphFilename: String = ""

    override def getInformationFlowGraphsOutputDir: String = ""

    override def getVariantsOutputDir: String = ""

    override def getOutputLocation: String = ""

    override def getCLinkMapMergeDir: String = ""

    override def isIFDSEvaluationModeEnabled: Boolean = true

    override def isIFDSAnalysisEnabled: Boolean = false

    override def isIFDSBenchmarkEnabled: Boolean = false

    override def isMergeLinkingInterfacesEnabled: Boolean = false

    override def isIFDSSamplingEvaluationEnabled: Boolean = true

    override def isIFDSSingleEvaluationEnabled: Boolean = true

    override def isIFDSPrintExplodedSuperCallGraphEnabled: Boolean = false

    override def writeVariants: Boolean = false
}
