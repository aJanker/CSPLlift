package de.fosd.typechef.ccallgraph

import java.io.{FileNotFoundException, InputStream}

import de.fosd.typechef.featureexpr.FeatureExprFactory
import de.fosd.typechef.featureexpr.FeatureExprFactory.{False, True}
import de.fosd.typechef.parser.c._
import org.junit.Test


/**
 * Created by gferreir on 9/21/14.
 */
class FunctionDeclarationsTest extends TestHelper {

    @Test def testFunctionDecl() {
        // single return values
        test("void foo() { }", Map(("foo", Set())), testFunctionReturns)
        test("int foo(int x) { return 0; }", Map(("foo", Set())), testFunctionReturns)
        test("int foo(int x) { return NULL; }", Map(("foo", Set("GLOBAL$NULL"))), testFunctionReturns)
        test("int foo(int x) { return x; }", Map(("foo", Set("foo$x"))), testFunctionReturns)
        test("int foo(int x) { return x+y; }", Map(("foo", Set("foo$x"))), testFunctionReturns)

        // multiple return values
        test("float* foo(float* x) { if (1) return x; }", Map(("foo", Set("foo$x"))), testFunctionReturns)
        test("float* foo(float* x) { if (1) return y; return x; }", Map(("foo", Set("foo$x", "GLOBAL$y"))), testFunctionReturns)
        test("float* foo(float* x) { if (1) return y; else return z; return x; }", Map(("foo", Set("foo$x", "GLOBAL$y", "GLOBAL$z"))), testFunctionReturns)
        test("float* foo(float* x) { if (1) return y+m; else return z+n; return x+w; }", Map(("foo", Set("foo$x", "GLOBAL$y", "GLOBAL$z"))), testFunctionReturns)

        // multiple functions
        test("int* foo(int* x) { return x; } int main() { int b; int* a = id(&b); return 0; }", Map(("foo", Set("foo$x")), ("main", Set())), testFunctionReturns)
        test("int* foo(int* x) { return x; } int main() { int b; int* a = id(&b); return a; }", Map(("foo", Set("foo$x")), ("main", Set("main$a"))), testFunctionReturns)
        test("int* foo(int* x) { return x; } int main() { int b; int* a = id(&b); return x+a; }", Map(("foo", Set("foo$x")), ("main", Set("GLOBAL$x"))), testFunctionReturns)

        // typedefs followed by functions using the new types
        test("typedef int* PBF; PBF* id(int x) { return x; }", Map(("id", Set("id$x"))), testFunctionReturns)

    }

    @Test def testFunctionDeclParameters() {
        test("void foo(int x) { }", Map(("foo", List("foo$x"))), testFunctionParameters)
        test("void foo() { }", Map(("foo", List())), testFunctionParameters)
        test("int foo(int x, float y) { }", Map(("foo", List("foo$x", "foo$y"))), testFunctionParameters)
        test("int foo(int x, float y, char z) { }", Map(("foo", List("foo$x", "foo$y", "foo$z"))), testFunctionParameters)
        test("typedef int* PBF; int foo(int x, float y, char z, PBF* w) { }", Map(("foo", List("foo$x", "foo$y", "foo$z", "foo$w"))), testFunctionParameters)

    }

    @Test def testFunctionDefinitions(): Unit = {
        //        testSet("void f() { } f();", Set("f"), testFunctionDefs)
        //        testSet("void g(int a, int b) { } g();", Set("g"), testFunctionDefs)
        // typedefs followed by list of parameters (DeclParameterDeclList) are interpreted as function definitions
        //        testSet("typedef int (*stat_func)(const char *fn, struct stat *ps); stat_func sf1; sf1(); ", Set("stat_func"), testFunctionDefs)
        testSet("typedef int (*stat_func)(const char *fn, struct stat *ps); int foo(stat_func sf) { sf(); } ", Set("stat_func", "foo"), testFunctionDefs)
        testSet("extern int stat (const char *__restrict __file,\n\t\t struct stat *__restrict __buf) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));", Set("stat"), testFunctionDefs)

    }

    val parser = new CParser()
    val emptyFM = FeatureExprFactory.dflt.featureModelFactory.empty

    def test(code: String, expected: Map[String, Iterable[String]], f: (TranslationUnit, Map[String, Iterable[String]]) => Unit) {
        val ast: TranslationUnit = loadAST(code)
        f(ast, expected)
    }

    def testSet(code: String, expected: Set[String], f: (TranslationUnit, Set[String]) => Unit) {
        val ast: TranslationUnit = loadAST(code)
        f(ast, expected)
    }

    def testExprStmt(expr: String, expected: Map[String, Iterable[String]], f: (TranslationUnit, Map[String, Iterable[String]]) => Unit) {
        val code = "void foo() {\n  %s\n}\n".format(expr)
        val ast: TranslationUnit = loadAST(code)
        f(ast, expected)
    }

    def testFile(filename: String, expected: Map[String, Iterable[String]], f: (TranslationUnit, Map[String, Iterable[String]]) => Unit) {
        val ast = loadASTFromFile(filename)
        f(ast, expected)
    }

    private def testFunctionReturns(ast: TranslationUnit, expected: Map[String, Iterable[String]]) {
        val c = new CCallGraph
        c.extractObjectNames(ast, True)
        assert(c.functionDefReturns equals expected, "expected %s, but found %s".format(expected.mkString("[", ", ", "]"), c.functionDefReturns.mkString("[", ", ", "]")))
    }

    private def testFunctionParameters(ast: TranslationUnit, expected: Map[String, Iterable[String]]) {
        val c = new CCallGraph
        c.extractObjectNames(ast, True)
        assert(c.functionDefParameters equals expected, "expected %s, but found %s".format(expected.mkString("[", ", ", "]"), c.functionDefParameters.mkString("[", ", ", "]")))
    }

    private def testFunctionDefs(ast: TranslationUnit, expected: Set[String]) {
        val c = new CCallGraph
        //c.extractObjectNames(ast, True)
        c.calculatePointerEquivalenceRelation(ast)
        c.showFunctionDefs()
        c.showExtractedObjectNames()
        assert(c.functionDefs.toPlainSet() equals expected, "expected %s, but found %s".format(expected.mkString("[", ", ", "]"), c.functionDefs.toPlainSet().mkString("[", ", ", "]")))
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
