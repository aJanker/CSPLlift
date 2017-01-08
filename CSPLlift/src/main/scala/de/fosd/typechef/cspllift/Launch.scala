package de.fosd.typechef.cspllift

import java.io.File

import de.fosd.typechef.Frontend
import de.fosd.typechef.Frontend._
import de.fosd.typechef.cmodule.CModule
import de.fosd.typechef.cmodule.ccallgraph.clinking.CModuleLinkMapGenerator
import de.fosd.typechef.crewrite.IntraCFG
import de.fosd.typechef.cspllift.evaluation.CSPLliftEvaluationFrontend
import de.fosd.typechef.cspllift.options.CInterAnalysisOptions
import de.fosd.typechef.customization.StopWatch
import de.fosd.typechef.customization.conditional.Sampling
import de.fosd.typechef.featureexpr.{FeatureExpr, FeatureModel}
import de.fosd.typechef.options.{FrontendOptions, OptionException}
import de.fosd.typechef.parser.TokenReader
import de.fosd.typechef.parser.c._
import org.slf4j.{Logger, LoggerFactory}

object Launch extends App with ASTNavigation with IntraCFG {

    private lazy val logger: Logger = LoggerFactory.getLogger(getClass)

    setDefaultLogging()
    liftMain()

    private def liftMain(): Unit = {
        val opt = new CInterAnalysisOptions

        try {
            try {
                opt.parseOptions(args)
            } catch {
                case o: OptionException => if (!opt.isMergeLinkingInterfacesEnabled) throw o
            }

            if (opt.isMergeLinkingInterfacesEnabled) {
                val (mergeTime, _) = StopWatch.measureProcessCPUTime({
                    CModuleLinkMapGenerator.mergeAndWriteMaps(opt.getCLinkMapMergeDir)
                })

                logger.info("Merged linking maps in " + mergeTime + "ms.")

                return
            }
        } catch {
            case o: OptionException =>
                println("Invocation error: " + o.getMessage)
                println("use parameter --help for more information.")
                return
        }

        processFile(opt)
    }

    private def processFile(opt: CInterAnalysisOptions) {
        val (smallFM: FeatureModel, fullFM: FeatureModel) = setFeatureModels(opt)
        var ast: TranslationUnit = null

        if (!opt.getFilePresenceCondition.isSatisfiable(fullFM)) {
            logger.error("file has contradictory presence condition. existing.") //otherwise this can lead to strange parser errors, because True is satisfiable, but anything else isn't
            return
        }

        if (opt.reuseAST && new File(opt.getSerializedASTFilename).exists()) {
            logger.info("Loading AST " + opt.getSerializedASTFilename)
            try {
                ast = loadSerializedAST(opt.getSerializedASTFilename)
                ast = prepareAST[TranslationUnit](ast)
            } catch {
                case e: Throwable => logger.error(e.toString); e.printStackTrace(); ast = null
            }
            if (ast == null)
                logger.error("... failed reading AST\n")
        }

        //no parsing and serialization if read serialized ast
        if (ast == null && opt.parse) {
            logger.info("Parsing")

            val parserMain = new ParserMain(new CParser(smallFM))
            ast = parserMain.parserMain(lex(opt), opt, fullFM)
            ast = prepareAST[TranslationUnit](ast)

            if (ast != null && opt.serializeAST)
                serializeAST(ast, opt.getSerializedASTFilename)
        }

        if (ast == null) {
            println("Invocation error: no file loaded.")
            println("use parameter --help for more information.")
            return
        }

        if (opt.genCodeCoverageConfigurations) {
            val sampling = new Sampling(ast, fullFM)
            val configs = sampling.getCodeCoverageConfigs()
            val configsFile = opt.getOutputLocation + "_codeCov.config"
            logger.info("Saving codecoverage configuration to:\t" + configsFile)
            sampling.saveConfigurations(configs, configsFile)

            // do not perform any further computation if this option is set
            return
        }

        if (opt.isIFDSAnalysisEnabled) {
            logger.info("Starting static analysis with IFDS-Lifting")

            opt.getCLinkingInterfacePath

            val moduleEnv = new CModule(rewriteTasks = CSPLlift.getIFDSRewriteRules, linkingMapDir = opt.getRootDir)
            moduleEnv.addTUnit(ast)
            moduleEnv.solveAliasing()

            if (opt.isIFDSEvaluationModeEnabled) {
                val cSPLliftEvalFrontend = new CSPLliftEvaluationFrontend(moduleEnv, opt)
                val successful = cSPLliftEvalFrontend.evaluate()

                logger.info("Static analysis with IFDS-Lifting was complete:\t" + successful)
            } else {
                val cSPLliftFrontend = new CSPLliftFrontend(moduleEnv)
                cSPLliftFrontend.analyze(opt)
            }
        }

        if (opt.recordTiming) {
            logger.info("Stopwatch")
            logger.info(StopWatch.toCSV)
            FeatureExpr.printSatStatistics
        }

    }

    private def setFeatureModels(opt: CInterAnalysisOptions): (FeatureModel, FeatureModel) = {
        val smallFM = opt.getSmallFeatureModel.and(opt.getLocalFeatureModel).and(opt.getFilePresenceCondition)
        opt.setSmallFeatureModel(smallFM)
        //otherwise the lexer does not get the updated feature model with file presence conditions
        val fullFM = opt.getFullFeatureModel.and(opt.getLocalFeatureModel).and(opt.getFilePresenceCondition)
        opt.setFullFeatureModel(fullFM) // should probably be fixed in how options are read
        (smallFM, fullFM)
    }

    private def lex(opt: FrontendOptions): TokenReader[CToken, CTypeContext] = Frontend.lex(opt)

    private def serializeAST(ast: AST, filename: String) = Frontend.serializeAST(ast, filename)

    private def loadSerializedAST(filename: String): TranslationUnit = Frontend.loadSerializedAST(filename)

    private def setDefaultLogging() = if (System.getProperty(org.slf4j.impl.SimpleLogger.DEFAULT_LOG_LEVEL_KEY) == null) System.setProperty(org.slf4j.impl.SimpleLogger.DEFAULT_LOG_LEVEL_KEY, "INFO")
}