package de.fosd.typechef.cspllift.options

trait CSPLliftOptions {

    def resolveFunctionPointer: Boolean
    def noInitalSeeds: Boolean
    def warmupJVM: Boolean
    def includeHeaderVariability : Boolean

    def getCLinkingInterfacePath: Option[String]
    def getInformationFlowGraphExtension: String
    def getInformationFLowGraphFilename: String
    def getInformationFlowGraphsOutputDir: String
    def getVariantsOutputDir: String
    def getOutputLocation: String
    def getCModuleInterfaceMergeDir: String

    def isLiftEvaluationModeEnabled: Boolean

    def isLiftAnalysisEnabled: Boolean
    def isLiftBenchmarkEnabled: Boolean
    def isMergeLinkingInterfacesEnabled: Boolean

    def isLiftSamplingEvaluationEnabled: Boolean
    def isLiftSingleEvaluationEnabled: Boolean
    def isLiftPrintExplodedSuperCallGraphEnabled: Boolean

    def liftTaintAnalysis : Boolean

    def writeVariants : Boolean

    def isProfiling: Boolean
    def initProfiling: Boolean
    def getProfileType: Option[String]
}
