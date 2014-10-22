package de.fosd.typechef.ccallgraph

import java.io.{FileNotFoundException, InputStream}

import de.fosd.typechef.featureexpr.FeatureExprFactory
import de.fosd.typechef.parser.c._
import de.fosd.typechef.typesystem.{CDeclUse, CTypeCache, CTypeSystemFrontend}
import org.junit.Assert.fail
import org.junit.Test

/**
 * Created by gferreir on 9/20/14.
 */
class CCallGraphTest extends TestHelper with ASTNavigation {

    @Test def test_paper_example_fig4() {
        val ast = loadAST("simple_calls.c")
        testAST(ast, Set())
    }

    @Test def test_paper_example_fig1() {
        val ast = loadAST("table_dispatch.c")
        testAST(ast, Set())
    }

    @Test def test_paper_example_fig2() {
        val ast = loadAST("extensible_func.c")
        testAST(ast, Set())
    }

    @Test def testExpr1() { testExpr("x->a = y;", Set("*x", "x", "x->a", "y")) }
    @Test def testExpr2() { testExpr("a=&(&b);", Set("a", "b", "&b", "&&b")) }
    @Test def testExpr3() { testExpr("&(a+b);", Set()) }

    val parser = new CParser()
    val emptyFM = FeatureExprFactory.dflt.featureModelFactory.empty

    private def loadAST(filename: String): TranslationUnit = {
        val folder = "testfiles/"
        val instream: InputStream = getClass.getResourceAsStream("/" + folder + filename)
        if (instream == null)
            throw new FileNotFoundException("Input file not found!")
        parseFile(instream, folder, filename)
    }

    private def testExpr(expr: String, expected: Set[String]) {
        val code = "void foo() {\n  %s\n}\n".format(expr)
        val ast: TranslationUnit = new ParserMain(parser).parserMain(lex(code), SilentParserOptions, emptyFM)
        testAST(ast, expected)
    }

    private def testAST(ast: TranslationUnit, expected: Set[String]) {
        val typeSystem = new CTypeSystemFrontend(ast) with CTypeCache with CDeclUse
        val env = typeSystem.checkASTEnv()

        val c = new CCallGraph(typeSystem.lookupExprType, env.structEnv.getFields)
        c.extractObjectNames(ast)

        assert(c.objectNames equals expected, "expected %s, but found %s".format(expected.mkString("[", ", ", "]"), c.objectNames.mkString("[", ", ", "]")))
    }

    private def parse(code: String, production: parser.MultiParser[AST]): Any = {
        val result = parser.parseAny(lex(code.stripMargin), production)
        result match {
            case parser.Success(ast, unparsed) => {
                ast
            }
            case parser.NoSuccess(msg, unparsed, inner) =>
                fail(msg + " at " + unparsed + " " + inner)
        }
    }
}
