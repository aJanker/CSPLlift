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
        testObjectNames("int* foo(int* x) { return 0; }", Set("GLOBAL$foo", "foo$x", "foo$*x"))
        testObjectNames("int* id(int* x) { return x; } int main() { int b; int* a = id(&b); }", Set("GLOBAL$id", "GLOBAL$main", "id$x", "id$*x", "main$a", "main$*a", "main$b", "main$&b", "main$id()"))
        testObjectNames("int* id(int* x) { return x; } int main() { int b; int* x = id(&b); }", Set("GLOBAL$main", "GLOBAL$id", "id$x", "id$*x", "main$x", "main$*x", "main$b", "main$&b", "main$id()"))
        testObjectNames("int* foo(int* x) { return x; } int bar(int *x) { return x; } int main() { int *x; foo(); bar(); return x; }", Set("GLOBAL$main", "GLOBAL$foo", "GLOBAL$bar", "foo$x", "foo$*x", "main$x", "main$*x", "bar$x", "bar$*x", "main$foo()", "main$bar()"))
        testObjectNames("typedef int (*stat_func)(const char *fn, struct stat *ps); int foo(stat_func sf) { sf(); } ", Set("GLOBAL$foo", "foo$sf", "foo$sf()"))

    }

    @Test def testVariableDeclarations() {
//        testObjectNames("#ifdef A\n int a; \n#endif", Set("GLOBAL$a"))
//        testExprObjectNames("int a;", Set("GLOBAL$foo", "foo$a"))
//        testObjectNames("int *b;", Set("GLOBAL$b", "GLOBAL$*b"))
//        testExprObjectNames("int *b;", Set("GLOBAL$foo", "foo$b", "foo$*b"))
//        testObjectNames("int **b;", Set("GLOBAL$b", "GLOBAL$*b"))
//        testExprObjectNames("int **b;", Set("GLOBAL$foo", "foo$b", "foo$*b"))
//        testObjectNames("int a, b;", Set("GLOBAL$a", "GLOBAL$b"))
//        testExprObjectNames("int a, b;", Set("GLOBAL$foo", "foo$a", "foo$b"))
//        testObjectNames("int *c, *d;", Set("GLOBAL$c", "GLOBAL$*c", "GLOBAL$d", "GLOBAL$*d"))
//        testExprObjectNames("int *c, *d;", Set("GLOBAL$foo", "foo$c", "foo$*c", "foo$d", "foo$*d"))
//        testObjectNames("int e, *f;", Set("GLOBAL$e", "GLOBAL$*f", "GLOBAL$f"))
//        testExprObjectNames("int e, *f;", Set("GLOBAL$foo", "foo$e", "foo$*f", "foo$f"))
//        testObjectNames("int a, b; a = b;", Set("GLOBAL$a", "GLOBAL$b"))
//        testExprObjectNames("int a, b; a = b;", Set("GLOBAL$foo", "foo$a", "foo$b"))
//        testObjectNames("int a, *b; b = &a;", Set("GLOBAL$a", "GLOBAL$&a", "GLOBAL$b", "GLOBAL$*b"))
//        testExprObjectNames("int a, *b; b = &a;", Set("GLOBAL$foo", "foo$a", "foo$&a", "foo$b", "foo$*b"))
//        testObjectNames("int *a, **b; b = &a;", Set("GLOBAL$a", "GLOBAL$&a", "GLOBAL$*a", "GLOBAL$b", "GLOBAL$*b"))
//        testExprObjectNames("int *a, **b; b = &a;", Set("GLOBAL$foo", "foo$a", "foo$&a", "foo$*a", "foo$b", "foo$*b"))
//        testObjectNames("int *a, **b; b = *(&a);", Set("GLOBAL$a", "GLOBAL$&a", "GLOBAL$*a", "GLOBAL$b", "GLOBAL$*b", "GLOBAL$*(&a)"))
//        testExprObjectNames("int *a, **b; b = *(&a);", Set("GLOBAL$foo", "foo$a", "foo$&a", "foo$*a", "foo$b", "foo$*b", "foo$*(&a)"))
//        testObjectNames("typedef int* PBF; PBF *g, h;", Set("GLOBAL$g", "GLOBAL$*g", "GLOBAL$h"))
//        testObjectNames("typedef int (*stat_func)(const char *fn, struct stat *ps); stat_func f;", Set("GLOBAL$f"))
//        testObjectNames("typedef int (*stat_func)(const char *fn, struct stat *ps); stat_func *f;", Set("GLOBAL$f", "GLOBAL$*f"))
        testObjectNames("typedef int (*stat_func)(const char *fn, struct stat *ps); stat_func *f = dostat;", Set("GLOBAL$f", "GLOBAL$*f", "GLOBAL$dostat"))

    }

    @Test def testAssignmentExpressions() {
        // function foo is also an object name
        testExprObjectNames("a = b;", Set("GLOBAL$foo", "GLOBAL$a", "GLOBAL$b"))
        testExprObjectNames("a = *b;", Set("GLOBAL$foo", "GLOBAL$a", "GLOBAL$b", "GLOBAL$*b"))
        testExprObjectNames("a = *(*b);", Set("GLOBAL$foo", "GLOBAL$a", "GLOBAL$b", "GLOBAL$*b", "GLOBAL$*(*b)"))
        testExprObjectNames("a = *(&b);", Set("GLOBAL$foo", "GLOBAL$a", "GLOBAL$b", "GLOBAL$&b", "GLOBAL$*(&b)"))
        testExprObjectNames("*a = b;", Set("GLOBAL$foo", "GLOBAL$a", "GLOBAL$*a", "GLOBAL$b"))
        testExprObjectNames("a = &b;", Set("GLOBAL$foo", "GLOBAL$a", "GLOBAL$b", "GLOBAL$&b"))
        testExprObjectNames("&a = b;", Set("GLOBAL$foo", "GLOBAL$a", "GLOBAL$&a", "GLOBAL$b"))
        testExprObjectNames("y->a = x;", Set("GLOBAL$foo", "GLOBAL$*y", "GLOBAL$y", "GLOBAL$y->a", "GLOBAL$x"))
        testExprObjectNames("y->a = &x;", Set("GLOBAL$foo", "GLOBAL$*y", "GLOBAL$y", "GLOBAL$y->a", "GLOBAL$x", "GLOBAL$&x"))
        testExprObjectNames("y->a = *x;", Set("GLOBAL$foo", "GLOBAL$*y", "GLOBAL$y", "GLOBAL$y->a", "GLOBAL$x", "GLOBAL$*x"))
        testExprObjectNames("a = &(&b);", Set("GLOBAL$foo", "GLOBAL$a", "GLOBAL$b", "GLOBAL$&b", "GLOBAL$&(&b)"))
        testExprObjectNames("c = &(a+b);", Set("GLOBAL$foo", "GLOBAL$a", "GLOBAL$&a", "GLOBAL$c"))
        testExprObjectNames("c = &(a+b+c);", Set("GLOBAL$foo", "GLOBAL$a", "GLOBAL$&a", "GLOBAL$c"))
        testExprObjectNames("int *a; (((a->b)->c)->d) = a;", Set("GLOBAL$foo", "foo$a", "foo$*a", "foo$a->b", "foo$*(a->b)", "foo$(a->b)->c", "foo$*((a->b)->c)", "foo$((a->b)->c)->d"))
        testExprObjectNames("a->b->c->d = a;", Set("GLOBAL$foo", "GLOBAL$a", "GLOBAL$*a", "GLOBAL$a->b", "GLOBAL$*(a->b)", "GLOBAL$(a->b)->c", "GLOBAL$*((a->b)->c)", "GLOBAL$((a->b)->c)->d"))

    }

    @Test def testStatements() {
        testExprObjectNames("while(a) { }", Set("GLOBAL$foo", "GLOBAL$a"))
        testExprObjectNames("while(*a) { }", Set("GLOBAL$foo", "GLOBAL$a", "GLOBAL$*a"))
        testExprObjectNames("while(&a) { }", Set("GLOBAL$foo", "GLOBAL$a", "GLOBAL$&a"))
        testExprObjectNames("while(a==1) { }", Set("GLOBAL$foo", "GLOBAL$a"))

        // weird assignments but they can exist
        testExprObjectNames("while(a = 1) { }", Set("GLOBAL$foo")) // assignments with constants are ignored
        testExprObjectNames("while(a = b) { }", Set("GLOBAL$foo", "GLOBAL$a", "GLOBAL$b"))
        testExprObjectNames("while(a = &b) { }", Set("GLOBAL$foo", "GLOBAL$a", "GLOBAL$b", "GLOBAL$&b"))
        testExprObjectNames("while(a = p->x) { }", Set("GLOBAL$foo", "GLOBAL$a", "GLOBAL$p", "GLOBAL$*p", "GLOBAL$p->x"))
        testExprObjectNames("while(a = &(p->x)) { }", Set("GLOBAL$foo", "GLOBAL$a", "GLOBAL$p", "GLOBAL$*p", "GLOBAL$p->x", "GLOBAL$&(p->x)"))

        // n-ary-subexpressions are ignored (unless they contain assignments)
        testExprObjectNames("while (a == 1) { }", Set("GLOBAL$foo", "GLOBAL$a"))
        testExprObjectNames("while (a == b) { }", Set("GLOBAL$foo", "GLOBAL$a"))
        testExprObjectNames("while (a >= b) { }", Set("GLOBAL$foo", "GLOBAL$a"))
        testExprObjectNames("while (a >= (b = c)) { }", Set("GLOBAL$foo", "GLOBAL$a", "GLOBAL$b", "GLOBAL$c"))
        testExprObjectNames("while (a >= (b = &c)) { }", Set("GLOBAL$foo", "GLOBAL$a", "GLOBAL$b", "GLOBAL$&c", "GLOBAL$c"))
        testExprObjectNames("while (a >= (b = *c)) { }", Set("GLOBAL$foo", "GLOBAL$a", "GLOBAL$b", "GLOBAL$*c", "GLOBAL$c"))
        testExprObjectNames("while (a < b) { }", Set("GLOBAL$foo", "GLOBAL$a"))
        testExprObjectNames("while (a || b) { }", Set("GLOBAL$foo", "GLOBAL$a"))
        testExprObjectNames("while (a == &b) { }", Set("GLOBAL$foo", "GLOBAL$a"))
        testExprObjectNames("while (a != p->x) { }", Set("GLOBAL$foo", "GLOBAL$a"))
        testExprObjectNames("while (a != p->x) { }", Set("GLOBAL$foo", "GLOBAL$a"))

        testExprObjectNames("for (a=1; b == p->x; p = p->y) { }", Set("GLOBAL$foo", "GLOBAL$b", "GLOBAL$p", "GLOBAL$*p", "GLOBAL$p->y"))
        testExprObjectNames("for (a=1; b == p->x; p = p+c) { }", Set("GLOBAL$foo", "GLOBAL$b", "GLOBAL$p"))
        testExprObjectNames("for (a=d; b != p->x; p = p->y) { }", Set("GLOBAL$foo", "GLOBAL$a", "GLOBAL$d", "GLOBAL$b", "GLOBAL$p", "GLOBAL$*p", "GLOBAL$p->y"))
        testExprObjectNames("for (a=d; b < p->x; p = p->y) { }", Set("GLOBAL$foo", "GLOBAL$a", "GLOBAL$d", "GLOBAL$b", "GLOBAL$p", "GLOBAL$*p", "GLOBAL$p->y"))
        testExprObjectNames("for (a=d, c=1; b = p->x; p = p+a) { }", Set("GLOBAL$foo", "GLOBAL$a", "GLOBAL$d", "GLOBAL$b", "GLOBAL$p", "GLOBAL$*p", "GLOBAL$p->x"))

        testExprObjectNames("if (a) { }", Set("GLOBAL$foo", "GLOBAL$a"))
        testExprObjectNames("if (a = 1) { }", Set("GLOBAL$foo"))
        testExprObjectNames("if (a = b) { }", Set("GLOBAL$foo", "GLOBAL$a", "GLOBAL$b"))
        testExprObjectNames("if (a == b) { }", Set("GLOBAL$foo", "GLOBAL$a"))
        testExprObjectNames("if (a >= b) { }", Set("GLOBAL$foo", "GLOBAL$a"))
        testExprObjectNames("if (a || b) { }", Set("GLOBAL$foo", "GLOBAL$a"))
        testExprObjectNames("if (*a) { }", Set("GLOBAL$foo", "GLOBAL$a", "GLOBAL$*a"))
        testExprObjectNames("if (a = p->x) { }", Set("GLOBAL$foo", "GLOBAL$a", "GLOBAL$p", "GLOBAL$*p", "GLOBAL$p->x"))
        testExprObjectNames("if (a == (z = p->x)) { }", Set("GLOBAL$foo", "GLOBAL$a", "GLOBAL$z", "GLOBAL$p", "GLOBAL$*p", "GLOBAL$p->x"))
        testExprObjectNames("if (a = p->x > z) { }", Set("GLOBAL$foo", "GLOBAL$a", "GLOBAL$p", "GLOBAL$*p", "GLOBAL$p->x"))
        testExprObjectNames("if (1) { } else if (a) { } else { }", Set("GLOBAL$foo", "GLOBAL$a"))
        testExprObjectNames("if (1) { } else if (a = 1) { } else { }", Set("GLOBAL$foo"))
        testExprObjectNames("if (1) { } else if (a = b) { } else { }", Set("GLOBAL$foo", "GLOBAL$a", "GLOBAL$b"))
        testExprObjectNames("if (1) { } else if (a == b) { } else { }", Set("GLOBAL$foo", "GLOBAL$a"))

    }

    @Test def testRemoveScope() {
        val c = new CCallGraph
        assert(c.unscope("foo$x") == "x")
        assert(c.unscope("foo$*p") == "*p")
        assert(c.unscope("GLOBAL$p->x") == "p->x")
        assert(c.unscope("GLOBAL$&(p->x)") == "&(p->x)")
    }

    @Test def testVariablesScope() {
        testObjectNames("int x;", Set("GLOBAL$x"))
        testObjectNames("int x; int foo() { int x; }", Set("GLOBAL$foo", "GLOBAL$x", "foo$x"))
        testObjectNames("int x; int foo(int y) { int x; return x+y; }", Set("GLOBAL$foo", "GLOBAL$x", "GLOBAL$foo", "foo$x", "foo$y"))
        testFile("scope.c", Set("GLOBAL$foo", "GLOBAL$main", "GLOBAL$ptr", "GLOBAL$*ptr", "GLOBAL$ptr->a", "foo$ptr", "foo$*ptr", "foo$c", "foo$ptr->b", "main$a"))
    }

    @Test def busyboxExcerpts() {
        testObjectNames(
            """
              | typedef _Bool (*statfunc_ptr)(const char *);
              | int stat_main() {
              |	   statfunc_ptr statfunc = do_stat;
              |    ok &= statfunc(argv[i]
              |    #if definedEx(CONFIG_FEATURE_STAT_FORMAT)
              |      , format
              |    #endif
              |    );
              | }
            """.stripMargin, Set("GLOBAL$argv[]", "stat_main$statfunc", "GLOBAL$ok", "stat_main$statfunc", "stat_main$statfunc()", "GLOBAL$do_stat"))

    }

    @Test def testPaperExamples() {
        //testFile("fig1_table_dispatch.c", Set("func1", "func2", "&func1", "&func2", "parse_func", "*parse_func", "table[]", "table[].func", "table[].name"))
        //testFile("fig2_extensible_func.c", Set("*h", "*f", "&h", "f", "h", "xmalloc", "xfree", "h->freefun", "h->chunkfun", "&xfree", "&xmalloc"))
        //testFile("fig3_sample_prog.c", Set("h->freefun", "*h", "xmalloc", "h->chunkfun", "xfree", "&h", "h", "&xfree", "&xmalloc"))
        //testFile("fig4_simple_sets_statements.c", Set("x", "&x", "t", "*p", "p->f", "p", "&z", "z"))
    }

    val parser = new CParser()
    val emptyFM = FeatureExprFactory.dflt.featureModelFactory.empty

    private def testObjectNames(code: String, expected: Set[String]) {
        val ast: TranslationUnit = loadAST(code)
        testObjectNamesSet(ast, expected)
    }

    private def testExprObjectNames(expr: String, expected: Set[String]) {
        val code = "void foo() {\n  %s\n}\n".format(expr)
        val ast: TranslationUnit = loadAST(code)
        testObjectNamesSet(ast, expected)
    }

    private def testFile(filename: String, expected: Set[String]) {
        val ast = loadASTFromFile(filename)
        testObjectNamesSet(ast, expected)
    }

    private def testObjectNamesSet(ast: TranslationUnit, expected: Set[String]) {
        val c = new CCallGraph
        c.extractObjectNames(ast, True)
        c.initEquivalanceClasses()
        assert(c.objectNames.toPlainSet() equals expected, "expected %s, but found %s".format(expected.mkString("[", ", ", "]"), c.objectNames.toPlainSet().mkString("[", ", ", "]")))
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
