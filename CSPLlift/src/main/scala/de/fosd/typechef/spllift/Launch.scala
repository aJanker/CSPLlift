package de.fosd.typechef.spllift

import java.io.File

import de.fosd.typechef.Frontend._
import de.fosd.typechef.commons.StopWatch
import de.fosd.typechef.options.{FrontendOptions, OptionException}
import de.fosd.typechef.parser.TokenReader
import de.fosd.typechef.parser.c._
import de.fosd.typechef.spllift.evaluation.CSPLliftEvaluationFrontend
import de.fosd.typechef.spllift.options.CInterAnalysisOptions
import de.fosd.typechef.spllift.setup.CModuleInterfaceGenerator
import de.fosd.typechef.typesystem.{CDeclUse, CTypeCache, CTypeSystemFrontend}
import de.fosd.typechef.{ErrorXML, Frontend}

object Launch extends App {

    override def main(args: Array[String]): Unit = {
        // load options
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
        val errorXML = new ErrorXML(opt.getErrorXMLFile)
        opt.setRenderParserError(errorXML.renderParserError)

        val smallFM = opt.getSmallFeatureModel.and(opt.getLocalFeatureModel).and(opt.getFilePresenceCondition)
        opt.setSmallFeatureModel(smallFM) //otherwise the lexer does not get the updated feature model with file presence conditions
        val fullFM = opt.getFullFeatureModel.and(opt.getLocalFeatureModel).and(opt.getFilePresenceCondition)
        opt.setFullFeatureModel(fullFM) // should probably be fixed in how options are read

        if (!opt.getFilePresenceCondition.isSatisfiable(fullFM)) {
            println("file has contradictory presence condition. existing.") //otherwise this can lead to strange parser errors, because True is satisfiable, but anything else isn't
            return
        }

        var ast: TranslationUnit = null
        if (opt.reuseAST && opt.parse && new File(opt.getSerializedASTFilename).exists()) {
            println("loading AST.")
            try {
                ast = loadSerializedAST(opt.getSerializedASTFilename)
                ast = prepareAST[TranslationUnit](ast)
            } catch {
                case e: Throwable => println(e.toString); e.printStackTrace(); ast = null
            }
            if (ast == null)
                println("... failed reading AST\n")
        }

        //no parsing if read serialized ast
        val in =
            if (ast == null) {
                println("#lexing")
                lex(opt)
            } else null


        if (opt.parse) {
            println("#parsing")

            if (ast == null) {
                //no parsing and serialization if read serialized ast
                val parserMain = new ParserMain(new CParser(smallFM))
                ast = parserMain.parserMain(in, opt, fullFM)
                ast = prepareAST[TranslationUnit](ast)
                // checkPositionInformation(ast)

                if (ast != null && opt.serializeAST) serializeAST(ast, opt.getSerializedASTFilename)

            }

            if (ast != null) {
                // some dataflow analyses require typing information
                val ts = new CTypeSystemFrontend(ast, fullFM, opt) with CTypeCache with CDeclUse

                if (opt.typecheck || opt.writeInterface || opt.typechecksa) {
                    println("#type checking")
                    ts.checkAST(printResults = true)
                    ts.errors.map(errorXML.renderTypeError)
                }

                if (opt.writeInterface) {
                    val interface = ts.getInferredInterface().and(opt.getFilePresenceCondition)

                    ts.writeInterface(interface, new File(opt.getInterfaceFilename))
                    if (opt.writeDebugInterface)
                        ts.debugInterface(interface, new File(opt.getDebugInterfaceFilename))
                }

                if (opt.isLiftAnalysisEnabled) {
                    println("#static analysis with spllift")

                    val cSPLliftFrontend = new CSPLliftFrontend(ast, fullFM)
                    cSPLliftFrontend.analyze(opt)

                    if (opt.isLiftEvaluationModeEnabled) {
                        val cSPLliftEvalFrontend = new CSPLliftEvaluationFrontend(ast, fullFM)
                        cSPLliftEvalFrontend.evaluate(opt)
                    }
                }

            }
        }

        errorXML.write()

        if (opt.recordTiming) {
            println("\n### Stopwatch.")
            println(StopWatch.toCSV)
        }
    }


    private def lex(opt: FrontendOptions): TokenReader[CToken, CTypeContext] = Frontend.lex(opt)

    private def serializeAST(ast: AST, filename: String) = Frontend.serializeAST(ast, filename)

    private def loadSerializedAST(filename: String): TranslationUnit = Frontend.loadSerializedAST(filename)
}

