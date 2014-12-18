package de.fosd.typechef.ccallgraph

import java.io.{FileNotFoundException, InputStream}

import de.fosd.typechef.featureexpr.FeatureExprFactory
import de.fosd.typechef.parser.c._
import org.junit.Test
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner


/**
 * Created by gferreir on 9/21/14.
 */
class ObjectNamesTest extends TestHelper {

    @Test def testFunctionDecl() {
        test("int* foo(int* x) { return 0; }", Set("foo$x", "foo$*x"))
        test("int* foo(int* x) { return x; } int main() { int b; int* a = id(&b); }", Set("foo$x", "foo$*x", "main$a", "main$*a", "main$b", "main$&b", "main$id()"))
    }

    @Test def testVariableDeclarations() {
        test("int a;", Set("GLOBAL$a"))
        test("int *b;", Set("GLOBAL$b", "GLOBAL$*b"))
        test("int **b;", Set("GLOBAL$b", "GLOBAL$*b"))
        test("int a, b;", Set("GLOBAL$a", "GLOBAL$b"))
        test("int *c, *d;", Set("GLOBAL$c", "GLOBAL$*c", "GLOBAL$d", "GLOBAL$*d"))
        test("int e, *f;", Set("GLOBAL$e", "GLOBAL$*f", "GLOBAL$f"))
        test("typedef int* PBF; PBF *g, h;", Set("GLOBAL$g", "GLOBAL$*g", "GLOBAL$h"))
        test("int a, b; a = b;", Set("GLOBAL$a", "GLOBAL$b"))
        test("int a, *b; b = &a;", Set("GLOBAL$a", "GLOBAL$&a", "GLOBAL$b", "GLOBAL$*b"))
        test("int *a, **b; b = &a;", Set("GLOBAL$a", "GLOBAL$&a","GLOBAL$*a", "GLOBAL$b", "GLOBAL$*b"))
        test("int *a, **b; b = *(&a);", Set("GLOBAL$a", "GLOBAL$&a","GLOBAL$*a", "GLOBAL$b", "GLOBAL$*b", "GLOBAL$*(&a)"))
    }

    @Test def testAssignmentExpressions() {
        testExprStmt("a = b;", Set("foo$a", "foo$b"))
        testExprStmt("a = *b;", Set("foo$a", "foo$b", "foo$*b"))
        testExprStmt("a = *(*b);", Set("foo$a", "foo$b", "foo$*b","foo$*(*b)"))
        testExprStmt("a = *(&b);", Set("foo$a", "foo$b", "foo$&b", "foo$*(&b)"))
        testExprStmt("*a = b;", Set("foo$a", "foo$*a", "foo$b"))
        testExprStmt("a = &b;", Set("foo$a", "foo$b", "foo$&b"))
        testExprStmt("&a = b;", Set("foo$a", "foo$&a", "foo$b"))
        testExprStmt("y->a = x;", Set("foo$*y", "foo$y", "foo$y->a", "foo$x"))
        testExprStmt("y->a = &x;", Set("foo$*y", "foo$y", "foo$y->a", "foo$x", "foo$&x"))
        testExprStmt("y->a = *x;", Set("foo$*y", "foo$y", "foo$y->a", "foo$x", "foo$*x"))
        testExprStmt("a = &(&b);", Set("foo$a", "foo$b", "foo$&b", "foo$&(&b)"))
        testExprStmt("c = &(a+b);", Set("foo$a", "foo$&a", "foo$c"))
        testExprStmt("c = &(a+b+c);", Set("foo$a", "foo$&a", "foo$c"))
        testExprStmt("(((a->b)->c)->d) = a;", Set("foo$a", "foo$*a", "foo$a->b", "foo$*(a->b)", "foo$(a->b)->c", "foo$*((a->b)->c)", "foo$((a->b)->c)->d"))
        testExprStmt("a->b->c->d = a;", Set("foo$a", "foo$*a", "foo$a->b", "foo$*(a->b)", "foo$(a->b)->c", "foo$*((a->b)->c)", "foo$((a->b)->c)->d"))
    }

    @Test def testStatements() {
        testExprStmt("while(a) { }", Set("foo$a"))
        testExprStmt("while(*a) { }", Set("foo$a", "foo$*a"))
        testExprStmt("while(&a) { }", Set("foo$a", "foo$&a"))
        testExprStmt("while(a==1) { }", Set("foo$a"))

        // weird assignments but they can exist
        testExprStmt("while(a = 1) { }", Set()) // assignments with constants are ignored
        testExprStmt("while(a = b) { }", Set("foo$a", "foo$b"))
        testExprStmt("while(a = &b) { }", Set("foo$a", "foo$b", "foo$&b"))
        testExprStmt("while(a = p->x) { }", Set("foo$a", "foo$p", "foo$*p", "foo$p->x"))
        testExprStmt("while(a = &(p->x)) { }", Set("foo$a", "foo$p", "foo$*p", "foo$p->x", "foo$&(p->x)"))

        // n-ary-subexpressions are ignored (unless they contain assignments)
        testExprStmt("while (a == 1) { }", Set("foo$a"))
        testExprStmt("while (a == b) { }", Set("foo$a"))
        testExprStmt("while (a >= b) { }", Set("foo$a"))
        testExprStmt("while (a >= (b = c)) { }", Set("foo$a", "foo$b", "foo$c"))
        testExprStmt("while (a >= (b = &c)) { }", Set("foo$a", "foo$b", "foo$&c", "foo$c"))
        testExprStmt("while (a >= (b = *c)) { }", Set("foo$a", "foo$b", "foo$*c", "foo$c"))
        testExprStmt("while (a < b) { }", Set("foo$a"))
        testExprStmt("while (a || b) { }", Set("foo$a"))
        testExprStmt("while (a == &b) { }", Set("foo$a"))
        testExprStmt("while (a != p->x) { }", Set("foo$a"))
        testExprStmt("while (a != p->x) { }", Set("foo$a"))

        testExprStmt("for (a=1; b == p->x; p = p->y) { }", Set("foo$b", "foo$p", "foo$*p", "foo$p->y"))
        testExprStmt("for (a=1; b == p->x; p = p+c) { }", Set("foo$b", "foo$p"))
        testExprStmt("for (a=d; b != p->x; p = p->y) { }", Set("foo$a", "foo$d", "foo$b", "foo$p", "foo$*p", "foo$p->y"))
        testExprStmt("for (a=d; b < p->x; p = p->y) { }", Set("foo$a", "foo$d", "foo$b", "foo$p", "foo$*p", "foo$p->y"))
        testExprStmt("for (a=d, c=1; b = p->x; p = p+a) { }", Set("foo$a", "foo$d", "foo$b", "foo$p", "foo$*p", "foo$p->x"))

        testExprStmt("if (a) { }", Set("foo$a"))
        testExprStmt("if (a = 1) { }", Set())
        testExprStmt("if (a = b) { }", Set("foo$a", "foo$b"))
        testExprStmt("if (a == b) { }", Set("foo$a"))
        testExprStmt("if (a >= b) { }", Set("foo$a"))
        testExprStmt("if (a || b) { }", Set("foo$a"))
        testExprStmt("if (*a) { }", Set("foo$a", "foo$*a"))
        testExprStmt("if (a = p->x) { }", Set("foo$a", "foo$p", "foo$*p", "foo$p->x"))
        testExprStmt("if (a == (z = p->x)) { }", Set("foo$a", "foo$z", "foo$p", "foo$*p", "foo$p->x"))
        testExprStmt("if (a = p->x > z) { }", Set("foo$a", "foo$p", "foo$*p", "foo$p->x"))
        testExprStmt("if (1) { } else if (a) { } else { }", Set("foo$a"))
        testExprStmt("if (1) { } else if (a = 1) { } else { }", Set())
        testExprStmt("if (1) { } else if (a = b) { } else { }", Set("foo$a", "foo$b"))
        testExprStmt("if (1) { } else if (a == b) { } else { }", Set("foo$a"))

    }

    @Test def testPaperExamples() {
        testFile("fig1_table_dispatch.c", Set("func1", "func2", "&func1", "&func2", "parse_func", "*parse_func", "table[]", "table[].func", "table[].name"))
        //testFile("fig2_extensible_func.c", Set("*h", "*f", "&h", "f", "h", "xmalloc", "xfree", "h->freefun", "h->chunkfun", "&xfree", "&xmalloc"))
        //testFile("fig3_sample_prog.c", Set("h->freefun", "*h", "xmalloc", "h->chunkfun", "xfree", "&h", "h", "&xfree", "&xmalloc"))
        //testFile("fig4_simple_sets_statements.c", Set("x", "&x", "t", "*p", "p->f", "p", "&z", "z"))
    }

    val parser = new CParser()
    val emptyFM = FeatureExprFactory.dflt.featureModelFactory.empty

    private def test(code: String, expected: Set[String]) {
        val ast: TranslationUnit = loadAST(code)
        testObjectNames(ast, expected)
    }

    private def testExprStmt(expr: String, expected: Set[String]) {
        val code = "void foo() {\n  %s\n}\n".format(expr)
        val ast: TranslationUnit = loadAST(code)
        testObjectNames(ast, expected)
    }

    private def testFile(filename: String, expected: Set[String]) {
        val ast = loadASTFromFile(filename)
        testObjectNames(ast, expected)
    }

    private def testObjectNames(ast: TranslationUnit, expected: Set[String]) {
        val c = new CCallGraph
        c.extractObjectNames(ast)
        assert(c.extractedObjectNames equals expected, "expected %s, but found %s".format(expected.mkString("[", ", ", "]"), c.extractedObjectNames.mkString("[", ", ", "]")))
    }


    private def loadAST(code: String): TranslationUnit = {
        val ast: TranslationUnit = new ParserMain(parser).parserMain(lex(code), SilentParserOptions, emptyFM)
        assert(ast != null, "AST is null")
        ast.asInstanceOf[TranslationUnit]
    }

    private def loadASTFromFile(filename: String): TranslationUnit = {
        val folder = "testfiles/"
        val instream: InputStream = getClass.getResourceAsStream("/" + folder + filename)
        if (instream == null)
            throw new FileNotFoundException("Input file not found!")
        val ast: TranslationUnit = parseFile(instream, folder, filename)
        assert(ast != null, "AST is null")
        ast.asInstanceOf[TranslationUnit]
    }
}
