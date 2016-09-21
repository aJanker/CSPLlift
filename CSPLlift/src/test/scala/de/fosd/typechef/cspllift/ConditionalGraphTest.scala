package de.fosd.typechef.cspllift

import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.cspllift.analysis.Taint
import de.fosd.typechef.cspllift.evaluation.CSPLliftEvaluationFrontend
import de.fosd.typechef.featureexpr.FeatureExpr
import de.fosd.typechef.featureexpr.bdd.True
import de.fosd.typechef.parser.c._
import org.junit.Test


class ConditionalGraphTest extends CSPLliftTestHelper {

    @Test def loopTest() = {
        val sinkStmt = ExprStatement(AssignExpr(Id("sink"),"=",Id("bound")))

        var expectedReaches : List[(FeatureExpr, List[Opt[Id]])] = List()

        // sink1 : True, (sink = bound = 2 (true))
        expectedReaches ::= (True, List(Opt(True, Id("bound"))))

        // sink2 : True, (sink = bound = foo = 1 (true))
        expectedReaches ::= (True, List(Opt(True, Id("bound")), Opt(True, Id("foo"))))

        // sink3 : A&&B, (sink = bound = foo = z = 0 (A&&B))
        expectedReaches ::= (fa.and(fb), List(Opt(fa.and(fb), Id("bound")), Opt(fa.and(fb), Id("foo")), Opt(True, Id("z"))))

        // sink4 : A&&!B, (sink = bound = foo = z = 0 (A&&!B)) // duplication of for -loop
        expectedReaches ::= (fa.and(fb.not()), List(Opt(fa.and(fb.not()), Id("bound")), Opt(fa.and(fb.not()), Id("foo")), Opt(True, Id("z"))))

        val sinks = defaultSingleSinkTest("loop.c", sinkStmt, expectedReaches)

        val tunit = parseTUnitFromFile("loop.c")
        val evaluation = new CSPLliftEvaluationFrontend(tunit)
        val eval = evaluation.evaluate(new CSPLliftTestOptions)

        (eval && sinks) should be(true)
    }

    @Test def ifTest1() = {
        /*val sinkStmt = ExprStatement(AssignExpr(Id("sink"),"=",Id("res")))

        var expectedReaches : List[(FeatureExpr, List[Opt[Id]])] = List()

        // sink1 : True, (sink = bound = 2 (true))
        expectedReaches ::= (True, List(Opt(True, Id("bound"))))

        // sink2 : True, (sink = bound = foo = 1 (true))
        expectedReaches ::= (True, List(Opt(True, Id("bound")), Opt(True, Id("foo"))))

        // sink3 : A&&B, (sink = bound = foo = z = 0 (A&&B))
        expectedReaches ::= (fa.and(fb), List(Opt(fa.and(fb), Id("bound")), Opt(fa.and(fb), Id("foo")), Opt(True, Id("z"))))

        // sink4 : A&&!B, (sink = bound = foo = z = 0 (A&&!B)) // duplication of for -loop
        expectedReaches ::= (fa.and(fb.not()), List(Opt(fa.and(fb.not()), Id("bound")), Opt(fa.and(fb.not()), Id("foo")), Opt(True, Id("z"))))

        val sinks = defaultSingleSinkTest("if.c", sinkStmt, expectedReaches) */

        val all = defaultTestInit("if1.c", allSinks)

        println("Result:")
        println(Taint.prettyPrintSinks(all._4))
        println(PrettyPrinter.print(all._1))

        val tunit = parseTUnitFromFile("if1.c")
        val evaluation = new CSPLliftEvaluationFrontend(tunit)
        val eval = evaluation.evaluate(new CSPLliftTestOptions)

        eval should be(true)
    }

    @Test def ifTest2() = {
        val sinkStmt = ExprStatement(AssignExpr(Id("sink"),"=",Id("res")))

        /*var expectedReaches : List[(FeatureExpr, List[Opt[Id]])] = List()

        // sink1 : True, (sink = bound = 2 (true))
        expectedReaches ::= (True, List(Opt(True, Id("bound"))))

        // sink2 : True, (sink = bound = foo = 1 (true))
        expectedReaches ::= (True, List(Opt(True, Id("bound")), Opt(True, Id("foo"))))

        // sink3 : A&&B, (sink = bound = foo = z = 0 (A&&B))
        expectedReaches ::= (fa.and(fb), List(Opt(fa.and(fb), Id("bound")), Opt(fa.and(fb), Id("foo")), Opt(True, Id("z"))))

        // sink4 : A&&!B, (sink = bound = foo = z = 0 (A&&!B)) // duplication of for -loop
        expectedReaches ::= (fa.and(fb.not()), List(Opt(fa.and(fb.not()), Id("bound")), Opt(fa.and(fb.not()), Id("foo")), Opt(True, Id("z")))) */


        val sinks = defaultSingleSinkTest("if2.c", sinkStmt, List())

        val tunit = parseTUnitFromFile("if2.c")
        val evaluation = new CSPLliftEvaluationFrontend(tunit)
        val eval = evaluation.evaluate(new CSPLliftTestOptions)

        eval should be(true)
    }
}
