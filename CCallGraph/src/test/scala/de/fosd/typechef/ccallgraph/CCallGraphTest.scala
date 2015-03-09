package de.fosd.typechef.ccallgraph

import java.io.{FileNotFoundException, InputStream}

import de.fosd.typechef.conditional.ConditionalSet
import de.fosd.typechef.featureexpr.FeatureExprFactory
import de.fosd.typechef.featureexpr.FeatureExprFactory.{False, True}
import de.fosd.typechef.parser.c._
import org.junit.Test


/**
 * Created by gferreir on 11/16/14.
 */
class CCallGraphTest extends TestHelper {

    @Test def unscopedObjectNames() {
        var cset = ConditionalSet[String]()
        cset +=("GLOBAL$x", True)
        cset +=("bar$y", True)
        cset +=("stat_main$statfunc", True)
        cset +=("a$b|c", True)

        val eq = new EquivalenceClass(cset, ConditionalSet())
        assert(eq.unscopedObjectNames() equals Set("x", "y", "statfunc", "b|c"), "expected %s, but found %s".format(eq.unscopedObjectNames(), Set("x", "y", "statfunc", "b|c")))
    }

    @Test def test_paper_example_fig1() {
        val ast = loadAST("fig1_table_dispatch.c")

        val c: CCallGraph = new CCallGraph()

        c.calculatePointerEquivalenceRelation(ast)
        c.showPointerEquivalenceClasses()
        c.showFunctionCalls()

        c.extractCallGraph()
        c.showCallGraphStatistics()

    }

    @Test def test_paper_example_fig2() {
        val ast = loadAST("fig2_extensible_func.c")

        val c: CCallGraph = new CCallGraph()
        c.calculatePointerEquivalenceRelation(ast)

        c.showCallGraphStatistics()

    }

    @Test def test_paper_example_fig3() {
        val ast = loadAST("fig3_sample_prog.c")

        val c: CCallGraph = new CCallGraph()

        c.calculatePointerEquivalenceRelation(ast)
        c.showPointerEquivalenceClasses()
        c.showFunctionCalls()

        c.extractCallGraph()
        c.showCallGraphStatistics()

    }

    @Test def test_paper_example_fig4() {
        val ast = loadAST("fig4_simple_sets_statements.c")

        val c: CCallGraph = new CCallGraph()
        c.calculatePointerEquivalenceRelation(ast)

        c.showPointerEquivalenceClasses()

    }

    @Test def test_variational_code() {
        val ast = loadAST("test_variational_code.c")

        val c: CCallGraph = new CCallGraph()

        c.calculatePointerEquivalenceRelation(ast)
        c.showPointerEquivalenceClasses()
        c.showFunctionCalls()

        c.extractCallGraph()
        c.showCallGraphStatistics()

    }

    @Test def test_variational_function_calls() {
        val ast = loadAST("variational_function_calls.c")

        val c: CCallGraph = new CCallGraph()
        c.calculatePointerEquivalenceRelation(ast)
        c.extractCallGraph()
        c.showCallGraphStatistics()
        c.showFunctionDefs()
        c.showFunctionCalls()

        val expectedNodes = ConditionalSet(Map(Node("foo", "function", 1) -> True, Node("bar", "function", 2) -> True, Node("baz", "function", 3) -> True, Node("main", "function",
            5) -> True))
        assert(c.callGraphNodes equals expectedNodes, "expected %s, but found %s".format(c.callGraphNodes, expectedNodes))

        val expectedEdges = ConditionalSet(Map(Edge("main", "foo", "D") -> FeatureExprFactory.createDefinedExternal("B"),
            Edge("main", "bar", "I") -> FeatureExprFactory.createDefinedExternal("B").and(FeatureExprFactory.createDefinedExternal("A")),
            Edge("main", "baz", "I") -> FeatureExprFactory.createDefinedExternal("B").and(FeatureExprFactory.createDefinedExternal("A").not)))

        assert(c.callGraphEdges equals expectedEdges, "expected %s, but found %s".format(c.callGraphEdges, expectedEdges))

    }

    @Test def test_variational_function_calls2() {
        val ast = loadAST("variational_function_calls2.c")

        val c: CCallGraph = new CCallGraph()

        c.calculatePointerEquivalenceRelation(ast)
        c.extractCallGraph()
        c.showCallGraphStatistics()
        c.showPointerEquivalenceClasses()
        c.showFunctionCalls()

        val expectedNodes = ConditionalSet(Map(Node("foo", "function", 1) -> True, Node("bar", "function", 2) -> True, Node("baz", "function", 3) -> True, Node("main", "function", 5) -> True))
        assert(c.callGraphNodes equals expectedNodes, "expected %s, but found %s".format(c.callGraphNodes, expectedNodes))

        val expectedEdges = ConditionalSet(Map(Edge("main", "foo", "D") -> FeatureExprFactory.createDefinedExternal("B"),
            Edge("main", "bar", "I") -> FeatureExprFactory.createDefinedExternal("B").and(FeatureExprFactory.createDefinedExternal("A")),
            Edge("main", "baz", "I") -> FeatureExprFactory.createDefinedExternal("B").and(FeatureExprFactory.createDefinedExternal("A").not)))

        assert(c.callGraphEdges equals expectedEdges, "expected %s, but found %s".format(c.callGraphEdges, expectedEdges))

    }

    @Test def test_variational_function_calls3() {
        val ast = loadAST("variational_function_calls3.c")

        val c: CCallGraph = new CCallGraph()

        c.calculatePointerEquivalenceRelation(ast)
        c.extractCallGraph()
        c.showCallGraphStatistics()
        c.showPointerEquivalenceClasses()
        c.showFunctionCalls()

        val expectedNodes = ConditionalSet(Map(Node("foo", "function", 1) -> True, Node("bar", "function", 2) -> True, Node("baz", "function", 3) -> True, Node("main", "function",
            5) -> True))
        assert(c.callGraphNodes equals expectedNodes, "expected %s, but found %s".format(c.callGraphNodes, expectedNodes))

        val expectedEdges = ConditionalSet(Map(Edge("main", "foo", "D") -> FeatureExprFactory.createDefinedExternal("B"),
            Edge("main", "bar", "I") -> FeatureExprFactory.createDefinedExternal("B").and(FeatureExprFactory.createDefinedExternal("A")),
            Edge("main", "baz", "I") -> FeatureExprFactory.createDefinedExternal("B").and(FeatureExprFactory.createDefinedExternal("A").not)))

        assert(c.callGraphEdges equals expectedEdges, "expected %s, but found %s".format(c.callGraphEdges, expectedEdges))

    }


    val parser = new CParser()
    val emptyFM = FeatureExprFactory.dflt.featureModelFactory.empty

    private def loadAST(filename: String): TranslationUnit = {
        val folder = "testfiles/"
        val instream: InputStream = getClass.getResourceAsStream("/" + folder + filename)
        if (instream == null)
            throw new FileNotFoundException("Input file not found!")
        val ast = parseFile(instream, folder, filename)
        ast
    }

    private def loadASTFromCodeSnippet(codeSnippet: String): TranslationUnit = {
        val code = "void main() {\n  %s\n}\n".format(codeSnippet)
        val ast: TranslationUnit = new ParserMain(parser).parserMain(lex(code), SilentParserOptions, emptyFM)
        ast
    }
}
