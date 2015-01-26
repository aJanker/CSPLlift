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

    @Test def test_init_equivalence_classes() {
        val c: CCallGraph = new CCallGraph()
        c.extractedObjectNames = ConditionalSet("a", True)
        c.initEquivalanceClasses()

        assert(c.equivalenceClasses.size equals c.extractedObjectNames.toPlainSet().size)
        c.equivalenceClasses.map(s => s.prefixes().toPlainSet().size equals 0)
    }

    @Test def test_create_initial_prefix_sets_1() {
        val c: CCallGraph = new CCallGraph()
        c.extractedObjectNames = ConditionalSet("x", True)

        c.initEquivalanceClasses()
        //c.createInitialPrefixSets()
        c.showCallGraph()

        assert(c.equivalenceClasses.size equals 1)
        assert(c.find("x").get equals new EquivalenceClass(ConditionalSet("x", True), ConditionalSet()))
    }

    @Test def test_create_initial_prefix_sets_2() {
        val c: CCallGraph = new CCallGraph()
        val map = Map("x" -> True, "&x" -> True)
        c.extractedObjectNames = ConditionalSet(map)

        c.initEquivalanceClasses()
        c.createInitialPrefixSets()
        c.showCallGraph()

        assert(c.equivalenceClasses.size equals 2)

        assert(c.find("x").get equals new EquivalenceClass(ConditionalSet("x", True), ConditionalSet()))
        assert(c.find("&x").get equals new EquivalenceClass(ConditionalSet("&x", True), ConditionalSet(("*", "x"), True)))

    }

    @Test def test_create_initial_prefix_sets_3() {
        val c: CCallGraph = new CCallGraph()
        val map = Map("x" -> True, "&x" -> True, "&(&x)" -> True)
        c.extractedObjectNames = ConditionalSet(map)

        c.initEquivalanceClasses()
        c.createInitialPrefixSets()
        c.showCallGraph()

        assert(c.equivalenceClasses.size equals 3)
        assert(c.find("x").get equals new EquivalenceClass(ConditionalSet("x", True), ConditionalSet()))
        assert(c.find("&x").get equals new EquivalenceClass(ConditionalSet("&x", True), ConditionalSet(("*", "x"), True)))
        assert(c.find("&(&x)").get equals new EquivalenceClass(ConditionalSet("&(&x)", True), ConditionalSet(("*", "&x"), True)))
    }

    @Test def test_create_initial_prefix_sets_4() {
        val c: CCallGraph = new CCallGraph()
        val map = Map("a" -> True, "*a" -> True, "a->b" -> True, "*(a->b)" -> True, "(a->b)->c" -> True, "*((a->b)->c)" -> True, "((a->b)->c)->d" -> True)
        c.extractedObjectNames = ConditionalSet(map)

        c.initEquivalanceClasses()
        c.createInitialPrefixSets()
        c.showCallGraph()

        assert(c.equivalenceClasses.size equals 7)
        assert(c.find("a").get equals new EquivalenceClass(ConditionalSet("a", True), ConditionalSet(("*", "*a"), True)))
        assert(c.find("*a").get equals new EquivalenceClass(ConditionalSet("*a", True), ConditionalSet(("b", "a->b"), True)))
        assert(c.find("a->b").get equals new EquivalenceClass(ConditionalSet("a->b", True), ConditionalSet(("*", "*(a->b)"), True)))
        assert(c.find("*(a->b)").get equals new EquivalenceClass(ConditionalSet("*(a->b)", True), ConditionalSet(("c", "(a->b)->c"), True)))
        assert(c.find("(a->b)->c").get equals new EquivalenceClass(ConditionalSet("(a->b)->c", True), ConditionalSet(("*", "*((a->b)->c)"), True)))
        assert(c.find("*((a->b)->c)").get equals new EquivalenceClass(ConditionalSet("*((a->b)->c)", True), ConditionalSet(("d", "((a->b)->c)->d"), True)))
        assert(c.find("((a->b)->c)->d").get equals new EquivalenceClass(ConditionalSet("((a->b)->c)->d", True), ConditionalSet()))
    }

    @Test def test_create_initial_prefix_sets_fig4() {
        val c: CCallGraph = new CCallGraph()
        val map = Map("x" -> True, "&x" -> True, "p" -> True, "*p" -> True, "t" -> True, "p->f" -> True, "z" -> True, "&z" -> True)
        c.extractedObjectNames = ConditionalSet(map)

        c.initEquivalanceClasses()
        c.createInitialPrefixSets()
        c.showCallGraph()

        assert(c.equivalenceClasses.count(eqC => eqC.prefixes().toPlainSet().size > 0) equals 4)
        assert(c.find("p").get equals new EquivalenceClass(ConditionalSet("p", True), ConditionalSet(("*", "*p"), True)))
        assert(c.find("*p").get equals new EquivalenceClass(ConditionalSet("*p", True), ConditionalSet(("f", "p->f"), True)))
        assert(c.find("&x").get equals new EquivalenceClass(ConditionalSet("&x", True), ConditionalSet(("*", "x"), True)))
        assert(c.find("&z").get equals new EquivalenceClass(ConditionalSet("&z", True), ConditionalSet(("*", "z"), True)))
    }

    @Test def test_merge_eq_classes_fig4() {
        val c: CCallGraph = new CCallGraph()
        val map = Map("x" -> True, "&x" -> True, "p" -> True, "*p" -> True, "t" -> True, "p->f" -> True, "z" -> True, "&z" -> True)
        c.extractedObjectNames = ConditionalSet(map)

        c.initEquivalanceClasses()
        c.createInitialPrefixSets()

        c.merge(c.find("p").get, c.find("&x").get)
        c.merge(c.find("p").get, c.find("t").get)
        c.merge(c.find("p->f").get, c.find("&z").get)
        c.showCallGraph()

        assert(c.equivalenceClasses.size equals 4)
        // TODO: fix
        val map1 = Map("p" -> True, "&x" -> True, "t" -> True)
        assert(c.find("p").get equals new EquivalenceClass(ConditionalSet(map1), ConditionalSet(("*", "*p"), True)))
        val map2 = Map("*p" -> True, "x" -> True)
        assert(c.find("*p").get equals new EquivalenceClass(ConditionalSet(map2), ConditionalSet(("f", "p->f"), True)))
        val map3 = Map("p->f" -> True, "&z" -> True)
        assert(c.find("p->f").get equals new EquivalenceClass(ConditionalSet(map3), ConditionalSet(("*", "z"), True)))
        assert(c.find("z").get equals new EquivalenceClass(ConditionalSet("z", True), ConditionalSet()))


    }

    @Test def test_paper_example_fig1() {
        val ast = loadAST("fig1_table_dispatch.c")

        val c: CCallGraph = new CCallGraph()
        c.calculatePERelation(ast)

        c.showCallGraph()
    }


    //
    //    @Test def test_paper_example_fig2() {
    //        val ast = loadAST("fig2_extensible_func.c")
    //
    //        val c: CCallGraph = new CCallGraph()
    //        c.calculatePERelation(ast)
    //
    //        c.showCallGraph()
    //
    //    }
    //
        @Test def test_paper_example_fig3() {
            val ast = loadAST("fig3_sample_prog.c")

            val c: CCallGraph = new CCallGraph()
            c.calculatePERelation(ast)

            c.showCallGraph()

        }

    @Test def test_paper_example_fig4() {
        val ast = loadAST("fig4_simple_sets_statements.c")

        val c: CCallGraph = new CCallGraph()
        c.calculatePERelation(ast)

        c.showCallGraph()

        //        assert(c.equivalenceClasses.size equals 4)
        //        assert(c.find("p").get equals new EquivalenceClass(Set("p", "&x", "t"), Set(("*", "*p"))))
        //        assert(c.find("*p").get equals new EquivalenceClass(Set("*p", "x"), Set(("f", "p->f"))))
        //        assert(c.find("p->f").get equals new EquivalenceClass(Set("p->f", "&z"), Set(("*", "z"))))
        //        assert(c.find("z").get equals new EquivalenceClass(Set("z"), Set()))

    }

    @Test def test_variational_function_calls() {
        val ast = loadAST("variational_function_calls.c")

        val c: CCallGraph = new CCallGraph()
        c.calculatePERelation(ast)

        c.showCallGraph()

        //        assert(c.equivalenceClasses.size equals 4)
        //        assert(c.find("p").get equals new EquivalenceClass(Set("p", "&x", "t"), Set(("*", "*p"))))
        //        assert(c.find("*p").get equals new EquivalenceClass(Set("*p", "x"), Set(("f", "p->f"))))
        //        assert(c.find("p->f").get equals new EquivalenceClass(Set("p->f", "&z"), Set(("*", "z"))))
        //        assert(c.find("z").get equals new EquivalenceClass(Set("z"), Set()))

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
