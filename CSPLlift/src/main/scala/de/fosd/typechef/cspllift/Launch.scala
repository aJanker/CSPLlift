package de.fosd.typechef.cspllift

import java.io.File

import de.fosd.typechef.Frontend
import de.fosd.typechef.Frontend._
import de.fosd.typechef.cspllift.evaluation.CSPLliftEvaluationFrontend
import de.fosd.typechef.cspllift.options.CInterAnalysisOptions
import de.fosd.typechef.cspllift.setup.CModuleInterfaceGenerator
import de.fosd.typechef.customization.StopWatch
import de.fosd.typechef.options.{FrontendOptions, OptionException}
import de.fosd.typechef.parser.TokenReader
import de.fosd.typechef.parser.c._
import org.slf4j.{Logger, LoggerFactory}

object Launch extends App {

    private lazy val logger: Logger = LoggerFactory.getLogger(getClass)

    override def main(args: Array[String]): Unit = {
        setDefaultLogging()

        val opt = new CInterAnalysisOptions

        try {
            try {
                opt.parseOptions(args)
            } catch {
                case o: OptionException => if (!opt.isMergeLinkingInterfacesEnabled) throw o
            }

            if (opt.isMergeLinkingInterfacesEnabled) {
                CModuleInterfaceGenerator.mergeAndWriteInterfaces(opt.getCModuleInterfaceMergeDir)
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
        var ast: TranslationUnit = null

        val smallFM = opt.getSmallFeatureModel.and(opt.getLocalFeatureModel).and(opt.getFilePresenceCondition)
        opt.setSmallFeatureModel(smallFM) //otherwise the lexer does not get the updated feature model with file presence conditions
        val fullFM = opt.getFullFeatureModel.and(opt.getLocalFeatureModel).and(opt.getFilePresenceCondition)
        opt.setFullFeatureModel(fullFM) // should probably be fixed in how options are read

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

        if (opt.isLiftAnalysisEnabled) {
            logger.info("Starting static analysis with IFDS-Lifting")

            if (opt.isLiftEvaluationModeEnabled) {
                val cSPLliftEvalFrontend = new CSPLliftEvaluationFrontend(ast, fullFM)
                val successful = cSPLliftEvalFrontend.evaluate(opt)

                logger.info("Static analysis with IFDS-Lifting was complete:\t" + successful)
            } else {
                val cSPLliftFrontend = new CSPLliftFrontend(ast, fullFM)
                cSPLliftFrontend.analyze(opt)
            }
        }

        if (opt.recordTiming) {
            logger.info("Stopwatch")
            logger.info(StopWatch.toCSV)
        }

    }

    private def lex(opt: FrontendOptions): TokenReader[CToken, CTypeContext] = Frontend.lex(opt)

    private def serializeAST(ast: AST, filename: String) = Frontend.serializeAST(ast, filename)

    private def loadSerializedAST(filename: String): TranslationUnit = Frontend.loadSerializedAST(filename)

    private def setDefaultLogging() = if (System.getProperty(org.slf4j.impl.SimpleLogger.DEFAULT_LOG_LEVEL_KEY) == null) System.setProperty(org.slf4j.impl.SimpleLogger.DEFAULT_LOG_LEVEL_KEY, "INFO")
}