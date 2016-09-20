package de.fosd.typechef.cspllift

import de.fosd.typechef.cspllift.options.CSPLliftOptions


class CSPLliftTestOptions extends CSPLliftOptions {
  override def getCLinkingInterfacePath: Option[String] = None

  override def getInformationFlowGraphExtension: String = ""

  override def getInformationFLowGraphFilename: String = ""

  override def getInformationFlowGraphsOutputDir: String = ""

  override def getCModuleInterfaceMergeDir: String = ""

  override def isLiftEvaluationModeEnabled: Boolean = true

  override def isLiftAnalysisEnabled: Boolean = false

  override def isLiftBenchmarkEnabled: Boolean = false

  override def isMergeLinkingInterfacesEnabled: Boolean = false

  override def isLiftSamplingEvaluationEnabled: Boolean = true

  override def isLiftSingleEvaluationEnabled: Boolean = true

  override def liftTaintAnalysis: Boolean = true
}
