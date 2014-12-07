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
        c.equivalenceClasses.map(s => s.prefixes().size equals 0)
    }

    @Test def test_create_initial_prefix_sets_1() {
        val c: CCallGraph = new CCallGraph
        c.extractedObjectNames = Set("x")

        c.initEquivalanceClasses()
        c.createInitialPrefixSets()
        c.showCallGraph()

        assert(c.equivalenceClasses.size equals 1)
        assert(c.find("x").get equals new EquivalenceClass(Set("x"), Set()))
    }

    @Test def test_create_initial_prefix_sets_2() {
        val c: CCallGraph = new CCallGraph
        c.extractedObjectNames = Set("x", "&x")

        c.initEquivalanceClasses()
        c.createInitialPrefixSets()
        c.showCallGraph()

        assert(c.equivalenceClasses.size equals 2)
        assert(c.find("x").get equals new EquivalenceClass(Set("x"), Set()))
        assert(c.find("&x").get equals new EquivalenceClass(Set("&x"), Set(("*", "x"))))

    }

    @Test def test_create_initial_prefix_sets_3() {
        val c: CCallGraph = new CCallGraph
        c.extractedObjectNames = Set("x", "&x", "&(&x)")

        c.initEquivalanceClasses()
        c.createInitialPrefixSets()
        c.showCallGraph()

        assert(c.equivalenceClasses.size equals 3)
        assert(c.find("x").get equals new EquivalenceClass(Set("x"), Set()))
        assert(c.find("&x").get equals new EquivalenceClass(Set("&x"), Set(("*", "x"))))
        assert(c.find("&(&x)").get equals new EquivalenceClass(Set("&(&x)"), Set(("*", "&x"))))
    }

    @Test def test_create_initial_prefix_sets_4() {
        val c: CCallGraph = new CCallGraph
        c.extractedObjectNames = Set("a", "*a", "a->b", "*(a->b)", "(a->b)->c", "*((a->b)->c)", "((a->b)->c)->d")

        c.initEquivalanceClasses()
        c.createInitialPrefixSets()
        c.showCallGraph()

        assert(c.equivalenceClasses.size equals 7)
        assert(c.find("a").get equals new EquivalenceClass(Set("a"), Set(("*","*a"))))
        assert(c.find("*a").get equals new EquivalenceClass(Set("*a"), Set(("b","a->b"))))
        assert(c.find("a->b").get equals new EquivalenceClass(Set("a->b"), Set(("*","*(a->b)"))))
        assert(c.find("*(a->b)").get equals new EquivalenceClass(Set("*(a->b)"), Set(("c","(a->b)->c"))))
        assert(c.find("(a->b)->c").get equals new EquivalenceClass(Set("(a->b)->c"), Set(("*","*((a->b)->c)"))))
        assert(c.find("*((a->b)->c)").get equals new EquivalenceClass(Set("*((a->b)->c)"), Set(("d","((a->b)->c)->d"))))
        assert(c.find("((a->b)->c)->d").get equals new EquivalenceClass(Set("((a->b)->c)->d"), Set()))
    }

    @Test def test_create_initial_prefix_sets_fig4() {
        val c: CCallGraph = new CCallGraph
        c.extractedObjectNames = Set("x", "&x", "p", "*p", "t", "p->f", "z", "&z")

        c.initEquivalanceClasses()
        c.createInitialPrefixSets()
        c.showCallGraph()

        assert(c.equivalenceClasses.filter(eqC => eqC.prefixes().size > 0).size equals 4)
        assert(c.find("p").get equals new EquivalenceClass(Set("p"), Set(("*", "*p"))))
        assert(c.find("*p").get equals new EquivalenceClass(Set("*p"), Set(("f", "p->f"))))
        assert(c.find("&x").get equals new EquivalenceClass(Set("&x"), Set(("*", "x"))))
        assert(c.find("&z").get equals new EquivalenceClass(Set("&z"), Set(("*", "z"))))
    }

    @Test def test_merge_eq_classes_fig4() {
        val c: CCallGraph = new CCallGraph
        c.extractedObjectNames = Set("x", "&x", "p", "*p", "t", "p->f", "z", "&z")

        c.initEquivalanceClasses()
        c.createInitialPrefixSets()

        c.merge(c.find("p").get, c.find("&x").get)
        c.merge(c.find("p").get, c.find("t").get)
        c.merge(c.find("p->f").get, c.find("&z").get)
        c.showCallGraph

        assert(c.equivalenceClasses.size equals 4)
        assert(c.find("p").get equals new EquivalenceClass(Set("p", "&x", "t"), Set(("*", "*p"))))
        assert(c.find("*p").get equals new EquivalenceClass(Set("*p", "x"), Set(("f", "p->f"))))
        assert(c.find("p->f").get equals new EquivalenceClass(Set("p->f", "&z"), Set(("*", "z"))))
        assert(c.find("z").get equals new EquivalenceClass(Set("z"), Set()))


    }

    @Test def test_merge_eq_classes() {
        val c: CCallGraph = new CCallGraph
        c.extractedObjectNames = Set("x", "&x", "p", "*p", "t", "p->f", "z", "&z")

        c.initEquivalanceClasses()

        c.createInitialPrefixSets()
        c.merge(c.find("p").get, c.find("&x").get)
        c.merge(c.find("p").get, c.find("t").get)
        c.merge(c.find("p->f").get, c.find("&z").get)

        c.showCallGraph()
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

        assert(c.equivalenceClasses.size equals 4)
        assert(c.find("p").get equals new EquivalenceClass(Set("p", "&x", "t"), Set(("*", "*p"))))
        assert(c.find("*p").get equals new EquivalenceClass(Set("*p", "x"), Set(("f", "p->f"))))
        assert(c.find("p->f").get equals new EquivalenceClass(Set("p->f", "&z"), Set(("*", "z"))))
        assert(c.find("z").get equals new EquivalenceClass(Set("z"), Set()))

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
