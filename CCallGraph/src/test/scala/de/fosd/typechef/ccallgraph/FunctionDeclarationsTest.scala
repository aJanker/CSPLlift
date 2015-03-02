package de.fosd.typechef.ccallgraph

import java.io.{FileNotFoundException, InputStream}

import de.fosd.typechef.conditional.{ConditionalSet, Opt}
import de.fosd.typechef.featureexpr.FeatureExprFactory.True
import de.fosd.typechef.featureexpr.{FeatureExpr, FeatureExprFactory}
import de.fosd.typechef.parser.c._
import org.junit.Test


/**
 * Created by gferreir on 9/21/14.
 */
class FunctionDeclarationsTest extends TestHelper {

    @Test def testFunctionDecl() {
        // constant or none return values
        testFunctionReturns("void foo() { }", Map(("foo", ConditionalSet())))
        testFunctionReturns("int foo(int x) { return 0; }", Map(("foo", ConditionalSet())))

        // single return values
        testFunctionReturns("int foo(int x) { return NULL; }", Map(("foo", ConditionalSet("GLOBAL$NULL", True))))
        testFunctionReturns("int foo(int x) { return x; }", Map(("foo", ConditionalSet("foo$x", True))))
        testFunctionReturns("int foo(int x) { return x+y; }", Map(("foo", ConditionalSet("foo$x", True))))
        testFunctionReturns("float* foo(float* x) { if (1) return x; }", Map(("foo", ConditionalSet("foo$x", True))))

        // alternative return values
        testFunctionReturns(
            """int foo(int x) { return
              |#ifdef A
              | NULL
              |#else
              | x
              |#endif
              |; }""", Map(("foo", ConditionalSet(Map("GLOBAL$NULL" -> feature("A"), "foo$x" -> feature("A").not())))))

        // local variable
        testFunctionReturns(
            """int foo(int x, int y) { return
              |#ifdef A
              | x
              |#else
              | y
              |#endif
              |; }""", Map(("foo", ConditionalSet(Map("foo$x" -> feature("A"), "foo$y" -> feature("A").not())))))

        // GLOBAL variable
        testFunctionReturns(
            """int foo(int x) { return
              |#ifdef A
              | x
              |#else
              | y
              |#endif
              |; }""", Map(("foo", ConditionalSet(Map("foo$x" -> feature("A"), "GLOBAL$y" -> feature("A").not())))))

        // multiple return values
        //        testCondSet("float* foo(float* x) { if (1) return y; return x; }", Map(("foo", ConditionalSet("foo$x", "GLOBAL$y"))), testFunctionReturns)
        //        testCondSet("float* foo(float* x) { if (1) return y; else return z; return x; }", Map(("foo", ConditionalSet("foo$x", "GLOBAL$y", "GLOBAL$z"))), testFunctionReturns)
        //        testCondSet("float* foo(float* x) { if (1) return y+m; else return z+n; return x+w; }", Map(("foo", ConditionalSet("foo$x", "GLOBAL$y", "GLOBAL$z"))), testFunctionReturns)
        ////
        //        // multiple functions
        //        testCondSet("int* foo(int* x) { return x; } int main() { int b; int* a = id(&b); return 0; }", Map(("foo", Set("foo$x")), ("main", Set())), testFunctionReturns)
        //        testCondSet("int* foo(int* x) { return x; } int main() { int b; int* a = id(&b); return a; }", Map(("foo", Set("foo$x")), ("main", Set("main$a"))), testFunctionReturns)
        //        testCondSet("int* foo(int* x) { return x; } int main() { int b; int* a = id(&b); return x+a; }", Map(("foo", Set("foo$x")), ("main", Set("GLOBAL$x"))), testFunctionReturns)
        //
        //        // typedefs followed by functions using the new types
        //        test("typedef int* PBF; PBF* id(int x) { return x; }", Map(("id", Set("id$x"))), testFunctionReturns)

    }

    @Test def testFunctionDeclParameters() {
        testFunctionParameters("void foo(int x) { }", Map(("foo", List(Opt(True, "foo$x")))))
        testFunctionParameters("void foo() { }", Map(("foo", List())))
        testFunctionParameters("int foo(int x, float y) { }", Map(("foo", List(Opt(True, "foo$x"), Opt(True, "foo$y")))))
        testFunctionParameters("int foo(int x, float y, char z) { }", Map(("foo", List(Opt(True, "foo$x"), Opt(True, "foo$y"), Opt(True, "foo$z")))))
        testFunctionParameters("typedef int* PBF; int foo(int x, float y, char z, PBF* w) { }", Map(("foo", List(Opt(True, "foo$x"), Opt(True, "foo$y"), Opt(True, "foo$z"), Opt(True, "foo$w")))))

    }

    @Test def testFunctionDefinitions(): Unit = {
        testFunctionDefs("void f() { } f();", Set(("f", True)))
        testFunctionDefs(
            """#ifdef A
              | void f() { }
              | #endif """.stripMargin, Set(("f", FeatureExprFactory.createDefinedExternal("A"))))

        testFunctionDefs("void g(int a, int b) { } g();", Set(("g", True)))
        // typedefs followed by list of parameters (DeclParameterDeclList) are interpretedas function definitions
        //        testFunctionDefs("typedef int (*stat_func)(const char *fn, struct stat *ps); stat_func sf1; sf1(); ", Set("stat_func"))
        //        testFunctionDefs("typedef int (*stat_func)(const char *fn, struct stat *ps); int foo(stat_func sf) { sf(); } ", Set("stat_func", "foo"))
        //        testFunctionDefs("extern int stat (const char *__restrict __file,\n\t\t struct stat *__restrict __buf) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));", Set("stat"))

    }

    val parser = new CParser()
    val emptyFM = FeatureExprFactory.dflt.featureModelFactory.empty

    def testExprStmt(expr: String, expected: Map[String, Iterable[String]], f: (TranslationUnit, Map[String, Iterable[String]]) => Unit) {
        val code = "void foo() {\n  %s\n}\n".format(expr)
        val ast: TranslationUnit = loadAST(code)
        f(ast, expected)
    }

    def testFile(filename: String, expected: Map[String, Iterable[String]], f: (TranslationUnit, Map[String, Iterable[String]]) => Unit) {
        val ast = loadASTFromFile(filename)
        f(ast, expected)
    }

    private def testFunctionReturns(code: String, expected: Map[String, ConditionalSet[String]]) {
        val c = setupCallGraph(code, true)
        assert(c.functionDefReturns equals expected, "expected %s, but found %s".format(expected.mkString("[", ", ", "]"), c.functionDefReturns.mkString("[", ", ", "]")))
    }

    private def testFunctionParameters(code: String, expected: Map[String, Iterable[Opt[String]]]) {
        val c = setupCallGraph(code, true)
        assert(c.functionDefParameters equals expected, "expected %s, but found %s".format(expected.mkString("[", ", ", "]"), c.functionDefParameters.mkString("[", ", ", "]")))
    }

    private def testFunctionDefs(code: String, expected: Set[(String, FeatureExpr)]) {
        val c = setupCallGraph(code, true)
        assert(c.functionDefs.toPlainSetWithConditionals() equals expected, "expected %s, but found %s".format(expected.mkString("[", ", ", "]"), c.functionDefs.toPlainSetWithConditionals().mkString("[", ", ", "]")))
    }


    private def setupCallGraph(code: String, onlyObjectNames: Boolean): CCallGraph = {
        val ast = loadAST(code.stripMargin)
        val c = new CCallGraph
        if (onlyObjectNames) {c.extractObjectNames(ast, True)}
        else {c.calculatePointerEquivalenceRelation(ast)}
        c
    }

    private def loadAST(code: String): TranslationUnit = {
        val ast: TranslationUnit = new ParserMain(parser).parserMain(lex(code.stripMargin), SilentParserOptions, emptyFM)
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

    private def feature(name: String): FeatureExpr = {
        FeatureExprFactory.createDefinedExternal(name)
    }
}
