package de.fosd.typechef.cmodule.cmodulecache

import java.io.{FileInputStream, ObjectInputStream}
import java.util.zip.GZIPInputStream

import de.fosd.typechef.customization.StopWatch
import de.fosd.typechef.lexer
import de.fosd.typechef.options.FrontendOptions
import de.fosd.typechef.parser.TokenReader
import de.fosd.typechef.parser.c._
import org.slf4j.LoggerFactory

trait CModuleCacheOptions {

    private lazy val logger = LoggerFactory.getLogger(getClass)

    /**
      * Perform typecheck only if typesystem is queried. (Not yet implemented)
      */
    val lazyTypeCheck = false // TODO Implement

    /**
      * Perform silent typecheck silently without issuing any warnings.
      */
    val silentTypeCheck = true

    val debug = true

    def getTunitFromSource(source: String, opt: FrontendOptions): AST =
        if (opt.reuseAST) loadTUnit(source)
        else parseTUnit(source, opt)

    private def parseTUnit(source: String, opt: FrontendOptions) = {
        if (logger.isInfoEnabled) logger.info("Parsing: " + source)
        val parserMain = new ParserMain(new CParser(opt.getSmallFeatureModel))
        parserMain.parserMain(lex(source, opt), opt, opt.getFullFeatureModel)
    }

    private def lex(source: String, opt: FrontendOptions): TokenReader[CToken, CTypeContext] =
        CLexerAdapter.prepareTokens(new lexer.LexerFrontend().run(opt, opt.parse))

    private def loadTUnit(inputfile: String): TranslationUnit = StopWatch.measureProcessCPUTime("", {
        val fileExtension = if (inputfile.endsWith(".pi")) ".pi" else ".c"
        val path = if (inputfile.startsWith("file ")) inputfile.substring("file ".length) else inputfile
        val fileName = if (debug) path.replace("/home/janker/Masterarbeit", "/Users/andi/Masterarbeit") else path

        val (source, _) = fileName.splitAt(fileName.lastIndexOf(fileExtension))

        if (logger.isInfoEnabled) logger.info("Loading: " + source)

        val inputStream = new ObjectInputStream(new GZIPInputStream(new FileInputStream(source + ".ast")))
        val loaded = inputStream.readObject().asInstanceOf[TranslationUnit]
        inputStream.close()

        loaded
    })._2
}
