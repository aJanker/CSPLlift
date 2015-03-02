package de.fosd.typechef.ccallgraph

import java.io.{FileNotFoundException, InputStream}

import de.fosd.typechef.featureexpr._
import de.fosd.typechef.featureexpr.sat.True
import de.fosd.typechef.parser.c._
import org.junit.Test


/**
 * Created by gferreir on 9/21/14.
 */
class ObjectNamesTest extends TestHelper {

    @Test def testFunctionDecl() {
//        test("int* foo(int* x) { return 0; }", Set("foo$x", "foo$*x"))
//        test("int* id(int* x) { return x; } int main() { int b; int* a = id(&b); }", Set("id$x", "id$*x", "main$a", "main$*a", "main$b", "main$&b", "main$id()"))
//        test("int* id(int* x) { return x; } int main() { int b; int* x = id(&b); }", Set("id$x", "id$*x", "main$x", "main$*x", "main$b", "main$&b", "main$id()"))
//        test("int* foo(int* x) { return x; } int bar(int *x) { return x; } int main() { int *x; foo(); bar(); return x; }", Set("foo$x", "foo$*x", "main$x", "main$*x", "bar$x", "bar$*x"))
          test("typedef int (*stat_func)(const char *fn, struct stat *ps); int foo(stat_func sf) { sf(); } ", Set("GLOBAL$stat_func", "stat_func$*ps", "stat_func$ps", "stat_func$*fn", "stat_func$fn", "GLOBAL$foo", "foo$sf"))

    }

    @Test def testVariableDeclarations() {
        test("#ifdef A\n int a; \n#endif" , Set("GLOBAL$a"))
//        testExprStmt("int a;", Set("foo$a"))
//        test("int *b;", Set("GLOBAL$b", "GLOBAL$*b"))
//        testExprStmt("int *b;", Set("foo$b", "foo$*b"))
//        test("int **b;", Set("GLOBAL$b", "GLOBAL$*b"))
//        testExprStmt("int **b;", Set("foo$b", "foo$*b"))
//        test("int a, b;", Set("GLOBAL$a", "GLOBAL$b"))
//        testExprStmt("int a, b;", Set("foo$a", "foo$b"))
//        test("int *c, *d;", Set("GLOBAL$c", "GLOBAL$*c", "GLOBAL$d", "GLOBAL$*d"))
//        testExprStmt("int *c, *d;", Set("foo$c", "foo$*c", "foo$d", "foo$*d"))
//        test("int e, *f;", Set("GLOBAL$e", "GLOBAL$*f", "GLOBAL$f"))
//        testExprStmt("int e, *f;", Set("foo$e", "foo$*f", "foo$f"))
//        test("int a, b; a = b;", Set("GLOBAL$a", "GLOBAL$b"))
//        testExprStmt("int a, b; a = b;", Set("foo$a", "foo$b"))
//        test("int a, *b; b = &a;", Set("GLOBAL$a", "GLOBAL$&a", "GLOBAL$b", "GLOBAL$*b"))
//        testExprStmt("int a, *b; b = &a;", Set("foo$a", "foo$&a", "foo$b", "foo$*b"))
//        test("int *a, **b; b = &a;", Set("GLOBAL$a", "GLOBAL$&a", "GLOBAL$*a", "GLOBAL$b", "GLOBAL$*b"))
//        testExprStmt("int *a, **b; b = &a;", Set("foo$a", "foo$&a", "foo$*a", "foo$b", "foo$*b"))
//        test("int *a, **b; b = *(&a);", Set("GLOBAL$a", "GLOBAL$&a", "GLOBAL$*a", "GLOBAL$b", "GLOBAL$*b", "GLOBAL$*(&a)"))
//        testExprStmt("int *a, **b; b = *(&a);", Set("foo$a", "foo$&a", "foo$*a", "foo$b", "foo$*b", "foo$*(&a)"))
//        test("typedef int* PBF; PBF *g, h;", Set("GLOBAL$g", "GLOBAL$*g", "GLOBAL$h"))
    }

    @Test def testAssignmentExpressions() {
        // function foo is also an object name
        testExprStmt("a = b;", Set("GLOBAL$foo", "GLOBAL$a", "GLOBAL$b"))
        testExprStmt("a = *b;", Set("GLOBAL$foo", "GLOBAL$a", "GLOBAL$b", "GLOBAL$*b"))
        testExprStmt("a = *(*b);", Set("GLOBAL$foo", "GLOBAL$a", "GLOBAL$b", "GLOBAL$*b", "GLOBAL$*(*b)"))
        testExprStmt("a = *(&b);", Set("GLOBAL$foo", "GLOBAL$a", "GLOBAL$b", "GLOBAL$&b", "GLOBAL$*(&b)"))
        testExprStmt("*a = b;", Set("GLOBAL$foo", "GLOBAL$a", "GLOBAL$*a", "GLOBAL$b"))
        testExprStmt("a = &b;", Set("GLOBAL$foo", "GLOBAL$a", "GLOBAL$b", "GLOBAL$&b"))
        testExprStmt("&a = b;", Set("GLOBAL$foo", "GLOBAL$a", "GLOBAL$&a", "GLOBAL$b"))
        testExprStmt("y->a = x;", Set("GLOBAL$foo", "GLOBAL$*y", "GLOBAL$y", "GLOBAL$y->a", "GLOBAL$x"))
        testExprStmt("y->a = &x;", Set("GLOBAL$foo", "GLOBAL$*y", "GLOBAL$y", "GLOBAL$y->a", "GLOBAL$x", "GLOBAL$&x"))
        testExprStmt("y->a = *x;", Set("GLOBAL$foo", "GLOBAL$*y", "GLOBAL$y", "GLOBAL$y->a", "GLOBAL$x", "GLOBAL$*x"))
        testExprStmt("a = &(&b);", Set("GLOBAL$foo", "GLOBAL$a", "GLOBAL$b", "GLOBAL$&b", "GLOBAL$&(&b)"))
        testExprStmt("c = &(a+b);", Set("GLOBAL$foo", "GLOBAL$a", "GLOBAL$&a", "GLOBAL$c"))
        testExprStmt("c = &(a+b+c);", Set("GLOBAL$foo", "GLOBAL$a", "GLOBAL$&a", "GLOBAL$c"))
        testExprStmt("int *a; (((a->b)->c)->d) = a;", Set("GLOBAL$foo", "foo$a", "foo$*a", "foo$a->b", "foo$*(a->b)", "foo$(a->b)->c", "foo$*((a->b)->c)", "foo$((a->b)->c)->d"))
        testExprStmt("a->b->c->d = a;", Set("GLOBAL$foo", "GLOBAL$a", "GLOBAL$*a", "GLOBAL$a->b", "GLOBAL$*(a->b)", "GLOBAL$(a->b)->c", "GLOBAL$*((a->b)->c)", "GLOBAL$((a->b)->c)->d"))
    }

    @Test def testStatements() {
        testExprStmt("while(a) { }", Set("GLOBAL$foo", "GLOBAL$a"))
        testExprStmt("while(*a) { }", Set("GLOBAL$foo", "GLOBAL$a", "GLOBAL$*a"))
        testExprStmt("while(&a) { }", Set("GLOBAL$foo", "GLOBAL$a", "GLOBAL$&a"))
        testExprStmt("while(a==1) { }", Set("GLOBAL$foo", "GLOBAL$a"))

        // weird assignments but they can exist
        testExprStmt("while(a = 1) { }", Set("GLOBAL$foo")) // assignments with constants are ignored
        testExprStmt("while(a = b) { }", Set("GLOBAL$foo", "GLOBAL$a", "GLOBAL$b"))
        testExprStmt("while(a = &b) { }", Set("GLOBAL$foo", "GLOBAL$a", "GLOBAL$b", "GLOBAL$&b"))
        testExprStmt("while(a = p->x) { }", Set("GLOBAL$foo", "GLOBAL$a", "GLOBAL$p", "GLOBAL$*p", "GLOBAL$p->x"))
        testExprStmt("while(a = &(p->x)) { }", Set("GLOBAL$foo", "GLOBAL$a", "GLOBAL$p", "GLOBAL$*p", "GLOBAL$p->x", "GLOBAL$&(p->x)"))

        // n-ary-subexpressions are ignored (unless they contain assignments)
        testExprStmt("while (a == 1) { }", Set("GLOBAL$foo", "GLOBAL$a"))
        testExprStmt("while (a == b) { }", Set("GLOBAL$foo", "GLOBAL$a"))
        testExprStmt("while (a >= b) { }", Set("GLOBAL$foo", "GLOBAL$a"))
        testExprStmt("while (a >= (b = c)) { }", Set("GLOBAL$foo", "GLOBAL$a", "GLOBAL$b", "GLOBAL$c"))
        testExprStmt("while (a >= (b = &c)) { }", Set("GLOBAL$foo", "GLOBAL$a", "GLOBAL$b", "GLOBAL$&c", "GLOBAL$c"))
        testExprStmt("while (a >= (b = *c)) { }", Set("GLOBAL$foo", "GLOBAL$a", "GLOBAL$b", "GLOBAL$*c", "GLOBAL$c"))
        testExprStmt("while (a < b) { }", Set("GLOBAL$foo", "GLOBAL$a"))
        testExprStmt("while (a || b) { }", Set("GLOBAL$foo", "GLOBAL$a"))
        testExprStmt("while (a == &b) { }", Set("GLOBAL$foo", "GLOBAL$a"))
        testExprStmt("while (a != p->x) { }", Set("GLOBAL$foo", "GLOBAL$a"))
        testExprStmt("while (a != p->x) { }", Set("GLOBAL$foo", "GLOBAL$a"))

        testExprStmt("for (a=1; b == p->x; p = p->y) { }", Set("GLOBAL$foo", "GLOBAL$b", "GLOBAL$p", "GLOBAL$*p", "GLOBAL$p->y"))
        testExprStmt("for (a=1; b == p->x; p = p+c) { }", Set("GLOBAL$foo", "GLOBAL$b", "GLOBAL$p"))
        testExprStmt("for (a=d; b != p->x; p = p->y) { }", Set("GLOBAL$foo", "GLOBAL$a", "GLOBAL$d", "GLOBAL$b", "GLOBAL$p", "GLOBAL$*p", "GLOBAL$p->y"))
        testExprStmt("for (a=d; b < p->x; p = p->y) { }", Set("GLOBAL$foo", "GLOBAL$a", "GLOBAL$d", "GLOBAL$b", "GLOBAL$p", "GLOBAL$*p", "GLOBAL$p->y"))
        testExprStmt("for (a=d, c=1; b = p->x; p = p+a) { }", Set("GLOBAL$foo", "GLOBAL$a", "GLOBAL$d", "GLOBAL$b", "GLOBAL$p", "GLOBAL$*p", "GLOBAL$p->x"))

        testExprStmt("if (a) { }", Set("GLOBAL$foo", "GLOBAL$a"))
        testExprStmt("if (a = 1) { }", Set("GLOBAL$foo"))
        testExprStmt("if (a = b) { }", Set("GLOBAL$foo", "GLOBAL$a", "GLOBAL$b"))
        testExprStmt("if (a == b) { }", Set("GLOBAL$foo", "GLOBAL$a"))
        testExprStmt("if (a >= b) { }", Set("GLOBAL$foo", "GLOBAL$a"))
        testExprStmt("if (a || b) { }", Set("GLOBAL$foo", "GLOBAL$a"))
        testExprStmt("if (*a) { }", Set("GLOBAL$foo", "GLOBAL$a", "GLOBAL$*a"))
        testExprStmt("if (a = p->x) { }", Set("GLOBAL$foo", "GLOBAL$a", "GLOBAL$p", "GLOBAL$*p", "GLOBAL$p->x"))
        testExprStmt("if (a == (z = p->x)) { }", Set("GLOBAL$foo", "GLOBAL$a", "GLOBAL$z", "GLOBAL$p", "GLOBAL$*p", "GLOBAL$p->x"))
        testExprStmt("if (a = p->x > z) { }", Set("GLOBAL$foo", "GLOBAL$a", "GLOBAL$p", "GLOBAL$*p", "GLOBAL$p->x"))
        testExprStmt("if (1) { } else if (a) { } else { }", Set("GLOBAL$foo", "GLOBAL$a"))
        testExprStmt("if (1) { } else if (a = 1) { } else { }", Set("GLOBAL$foo"))
        testExprStmt("if (1) { } else if (a = b) { } else { }", Set("GLOBAL$foo", "GLOBAL$a", "GLOBAL$b"))
        testExprStmt("if (1) { } else if (a == b) { } else { }", Set("GLOBAL$foo", "GLOBAL$a"))

    }

    @Test def testRemoveScope() {
        val c = new CCallGraph
        assert(c.unscope("foo$x") == "x")
        assert(c.unscope("foo$*p") == "*p")
        assert(c.unscope("GLOBAL$p->x") == "p->x")
        assert(c.unscope("GLOBAL$&(p->x)") == "&(p->x)")
    }

    @Test def testVariablesScope() {
        test("int x;", Set("GLOBAL$x"))
        test("int x; int foo() { int x; }", Set("GLOBAL$foo", "GLOBAL$x", "foo$x"))
        test("int x; int foo(int y) { int x; return x+y; }", Set("GLOBAL$foo", "GLOBAL$x", "GLOBAL$foo", "foo$x", "foo$y"))
        testFile("scope.c", Set("GLOBAL$foo", "GLOBAL$ptr", "GLOBAL$*ptr", "GLOBAL$ptr->a", "foo$ptr", "foo$*ptr", "foo$c", "foo$ptr->b", "main$a"))
    }

    @Test def testPaperExamples() {
        //testFile("fig1_table_dispatch.c", Set("func1", "func2", "&func1", "&func2", "parse_func", "*parse_func", "table[]", "table[].func", "table[].name"))
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
        c.extractObjectNames(ast, True)
        c.initEquivalanceClasses()
        assert(c.extractedObjectNames.toPlainSet() equals expected, "expected %s, but found %s".format(expected.mkString("[", ", ", "]"), c.extractedObjectNames.toPlainSet().mkString("[", ", ", "]")))
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
