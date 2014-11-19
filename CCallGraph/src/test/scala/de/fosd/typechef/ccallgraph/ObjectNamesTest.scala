package de.fosd.typechef.ccallgraph

import java.io.{FileNotFoundException, InputStream}

import de.fosd.typechef.featureexpr.FeatureExprFactory
import de.fosd.typechef.parser.c._
import de.fosd.typechef.typesystem.{CDeclUse, CTypeCache, CTypeSystemFrontend}
import org.junit.Assert.fail
import org.junit.Test


/**
 * Created by gferreir on 9/21/14.
 */
class ObjectNamesTest extends TestHelper with ASTNavigation {

    @Test def test_paper_example_fig1() {
        val ast = loadAST("table_dispatch.c")
        testTranslationUnit(ast, Set("func1", "func2", "&func1", "&func2", "parse_func", "*parse_func", "table[]", "table[].func", "table[].name"))
    }

    @Test def test_paper_example_fig2() {
        val ast = loadAST("extensible_func.c")
        testTranslationUnit(ast, Set("h->freefun", "*h", "xmalloc", "h->chunkfun", "xfree", "&h", "h", "&xfree", "&xmalloc"))
    }

    @Test def test_paper_example_fig4() {
        val ast = loadAST("simple_calls.c")
        testTranslationUnit(ast, Set("x", "&x", "t", "*p", "p->f", "p", "&z", "z"))
    }

    @Test def testExpr1() { testExpr("x->a = y;", Set("*x", "x", "x->a", "y")) }
    @Test def testExpr2() { testExpr("a=&(&b);", Set("a", "b", "&b", "&&b")) }
    @Test def testExpr3() { testExpr("&(a+b);", Set("a", "&a")) }
    @Test def testExpr4() { testExpr("&(a+b+c);", Set("a", "&a")) }
    @Test def testStmt1() { testExpr("while(a = p->x) { }", Set("a", "p", "*p", "p->x")) }
    @Test def testStmt2() { testExpr("for(b=1; a = p->x; p = p->y) { }", Set("a", "p", "*p", "p->x", "p->y")) }
    @Test def testStmt3() { testExpr("if (a = p->x > z) { }", Set("a", "p", "*p", "p->x")) }

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
        testTranslationUnit(ast, expected)
    }

    private def testTranslationUnit(ast: TranslationUnit, expected: Set[String]) {
        val c = new CCallGraph()
        c.extractObjectNames(ast)

        assert(c.extractedObjectNames equals expected, "expected %s, but found %s".format(expected.mkString("[", ", ", "]"), c.extractedObjectNames.mkString("[", ", ", "]")))
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
