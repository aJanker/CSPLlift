package de.fosd.typechef.ccallgraph

import java.io.{FileNotFoundException, InputStream}

import de.fosd.typechef.featureexpr.FeatureExprFactory
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
        test("int foo(int x) { return NULL; }", Map(("foo", Set("NULL"))), testFunctionReturns)
        test("int foo(int x) { return x; }", Map(("foo", Set("x"))), testFunctionReturns)
        test("int foo(int x) { return x+y; }", Map(("foo", Set("x"))), testFunctionReturns)

        // multiple return values
        test("float* foo(float* x) { if (1) return x; }", Map(("foo", Set("x"))), testFunctionReturns)
        test("float* foo(float* x) { if (1) return y; return x; }", Map(("foo", Set("x", "y"))), testFunctionReturns)
        test("float* foo(float* x) { if (1) return y; else return z; return x; }", Map(("foo", Set("x", "y", "z"))), testFunctionReturns)
        test("float* foo(float* x) { if (1) return y+m; else return z+n; return x+w; }", Map(("foo", Set("x", "y", "z"))), testFunctionReturns)

        // multiple functions
        test("int* foo(int* x) { return x; } int main() { int b; int* a = id(&b); return 0; }", Map(("foo", Set("x")), ("main", Set())), testFunctionReturns)
        test("int* foo(int* x) { return x; } int main() { int b; int* a = id(&b); return x; }", Map(("foo", Set("x")), ("main", Set("x"))), testFunctionReturns)
        test("int* foo(int* x) { return x; } int main() { int b; int* a = id(&b); return x+y; }", Map(("foo", Set("x")), ("main", Set("x"))), testFunctionReturns)

        // typedefs followed by functions using the new types
        test("typedef int* PBF; PBF* id(int x) { return x; }", Map(("id", Set("x"))), testFunctionReturns)

    }

    @Test def testFunctionDeclParameters() {
        test("void foo() { }", Map(("foo", List())), testFunctionParameters)
        test("void foo(int x) { }", Map(("foo", List("x"))), testFunctionParameters)
        test("int foo(int x, float y) { }", Map(("foo", List("x", "y"))), testFunctionParameters)
        test("int foo(int x, float y, char z) { }", Map(("foo", List("x", "y", "z"))), testFunctionParameters)
        test("typedef int* PBF; int foo(int x, float y, char z, PBF* w) { }", Map(("foo", List("x", "y", "z", "w"))), testFunctionParameters)
    }

    @Test def testVariablesOrParametersDeclarations(): Unit = {
        test("void foo(int x) { }", Map(("foo", Set("x"))), testVariablesDeclarations)
        test("void foo(int x) { int y; }", Map(("foo", Set("x", "y"))), testVariablesDeclarations)
        test("int x; void foo(int y) { int z; }", Map(("foo", Set("y", "z")), ("GLOBAL", Set("x"))), testVariablesDeclarations)
        test("int *x; void foo(int y) { int z; }", Map(("foo", Set("y", "z")), ("GLOBAL", Set("x", "*x"))), testVariablesDeclarations)
        test("int x; void foo(int x) { int y; }", Map(("foo", Set("x", "y")), ("GLOBAL", Set("x"))), testVariablesDeclarations)
        test("int x; void foo(int *x) { int y; }", Map(("foo", Set("x", "*x", "y")), ("GLOBAL", Set("x"))), testVariablesDeclarations)

    }

    val parser = new CParser()
    val emptyFM = FeatureExprFactory.dflt.featureModelFactory.empty

    def test(code: String, expected: Map[String,Iterable[String]], f : (TranslationUnit, Map[String, Iterable[String]]) => Unit) {
        val ast: TranslationUnit = loadAST(code)
        f(ast, expected)
    }

    def testExprStmt(expr: String, expected: Map[String,Iterable[String]], f : (TranslationUnit, Map[String, Iterable[String]]) => Unit) {
        val code = "void foo() {\n  %s\n}\n".format(expr)
        val ast: TranslationUnit = loadAST(code)
        f(ast, expected)
    }

    def testFile(filename: String, expected: Map[String,Iterable[String]], f : (TranslationUnit, Map[String, Iterable[String]]) => Unit) {
        val ast = loadASTFromFile(filename)
        f(ast, expected)
    }

    private def testFunctionReturns(ast: TranslationUnit, expected: Map[String, Iterable[String]]) {
        val c = new CCallGraph
        c.extractObjectNames(ast)
        assert(c.functionDefReturns equals expected, "expected %s, but found %s".format(expected.mkString("[", ", ", "]"), c.functionDefReturns.mkString("[", ", ", "]")))
    }

    private def testFunctionParameters(ast: TranslationUnit, expected: Map[String, Iterable[String]]) {
        val c = new CCallGraph
        c.extractObjectNames(ast)
        assert(c.functionDefParameters equals expected, "expected %s, but found %s".format(expected.mkString("[", ", ", "]"), c.functionDefParameters.mkString("[", ", ", "]")))
    }

    private def testVariablesDeclarations(ast: TranslationUnit, expected: Map[String, Iterable[String]])  {
        val c = new CCallGraph
        c.extractObjectNames(ast)
        assert(c.objectNamesScope equals expected, "expected %s, but found %s".format(expected.mkString("[", ", ", "]"), c.objectNamesScope.mkString("[", ", ", "]")))
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
