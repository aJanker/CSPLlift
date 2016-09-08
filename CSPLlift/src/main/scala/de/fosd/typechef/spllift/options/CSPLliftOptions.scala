package de.fosd.typechef.spllift.options


trait CSPLliftOptions {

    def getCLinkingInterfacePath: Option[String]
    def getInformationFlowGraphExtension: String
    def getInformationFLowGraphFilename: String
    def getInformationFlowGraphsOutputDir: String
    def getCModuleInterfaceMergeDir: String

    def isLiftEvaluationModeEnabled: Boolean

    def isLiftAnalysisEnabled: Boolean
    def isLiftBenchmarkEnabled: Boolean
    def isMergeLinkingInterfacesEnabled: Boolean

    def isLiftSamplingEvaluationEnabled: Boolean
    def isLiftSingleEvaluationEnabled: Boolean

    def liftTaintAnalysis : Boolean
}
