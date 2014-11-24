package de.fosd.typechef.ccallgraph

import java.io.{FileNotFoundException, InputStream}

import de.fosd.typechef.featureexpr.FeatureExprFactory
import de.fosd.typechef.parser.c._
import org.junit.Test


/**
 * Created by gferreir on 11/16/14.
 */
class CCallGraphTest extends TestHelper {

    @Test def test_init_equivalence_classes() {
        val c: CCallGraph = new CCallGraph
        c.extractedObjectNames = Set("a")
        c.initEquivalanceClasses()

        assert(c.equivalenceClasses.size equals c.extractedObjectNames.size)
        c.equivalenceClasses().map(s => s.objectNames().size equals 1)
    }

    @Test def test_paper_example_fig1() {
        val ast = loadAST("table_dispatch.c")

        val c: CCallGraph = new CCallGraph
        c.extractObjectNames(ast)
        c.calculatePERelation(ast)

        c.showCallGraph

    }

    @Test def test_paper_example_fig2() {
        val ast = loadAST("extensible_func.c")

        val c: CCallGraph = new CCallGraph
        c.extractObjectNames(ast)
        c.calculatePERelation(ast)

        c.showCallGraph

    }

    @Test def test_paper_example_fig4() {
        val ast = loadAST("simple_calls.c")

        val c: CCallGraph = new CCallGraph
        c.extractObjectNames(ast)
        c.calculatePERelation(ast)

        c.showCallGraph

    }

    val parser = new CParser()
    val emptyFM = FeatureExprFactory.dflt.featureModelFactory.empty

    private def loadAST(filename: String): TranslationUnit = {
        val folder = "testfiles/"
        val instream: InputStream = getClass.getResourceAsStream("/" + folder + filename)
        if (instream == null)
            throw new FileNotFoundException("Input file not found!")
        parseFile(instream, folder, filename)
    }

    private def loadASTFromCodeSnippet(codeSnippet: String): TranslationUnit = {
        val code = "void main() {\n  %s\n}\n".format(codeSnippet)
        val ast: TranslationUnit = new ParserMain(parser).parserMain(lex(code), SilentParserOptions, emptyFM)
        ast
    }
}
