package de.fosd.typechef.cspllift.options

trait CSPLliftOptions {

    def resolveFunctionPointer: Boolean
    def noInitalSeeds: Boolean
    def warmupJVM: Boolean
    def includeHeaderVariability : Boolean

    def getCLinkingInterfacePath: Option[String]
    def getRootDir: Option[String]
    def getInformationFlowGraphExtension: String
    def getInformationFLowGraphFilename: String
    def getInformationFlowGraphsOutputDir: String
    def getVariantsOutputDir: String
    def getOutputLocation: String

    def getCLinkMapMergeDir: String

    def isIFDSEvaluationModeEnabled: Boolean

    def isIFDSAnalysisEnabled: Boolean
    def isIFDSBenchmarkEnabled: Boolean
    def isMergeLinkingInterfacesEnabled: Boolean

    def genCodeCoverageConfigurations: Boolean

    def isIFDSSamplingEvaluationEnabled: Boolean
    def isIFDSSingleEvaluationEnabled: Boolean
    def isIFDSPrintExplodedSuperCallGraphEnabled: Boolean

    def IFDSTaintAnalysis : Boolean

    def writeVariants : Boolean

    def isProfiling: Boolean
    def initProfiling: Boolean
    def getProfileType: Option[String]
}
