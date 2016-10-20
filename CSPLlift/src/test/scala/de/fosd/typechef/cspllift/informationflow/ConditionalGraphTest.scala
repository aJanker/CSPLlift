package de.fosd.typechef.cspllift.informationflow

import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.featureexpr.bdd.True
import de.fosd.typechef.parser.c._
import org.junit.Test


class ConditionalGraphTest extends InformationFlowTestHelper {

    @Test def loopTest() = {
        var expectedSinks : List[(AST, List[Opt[Id]])] = List()

        val sinkStmt1 = ExprStatement(AssignExpr(Id("sink"),"=",Id("bound")))
        val sinkStmt2 = ExprStatement(AssignExpr(Id("sink2"),"=",Id("i")))

        expectedSinks ::= (sinkStmt1, List(Opt(True, Id("start")), Opt(True, Id("low")), Opt(fa, Id("z")))) //Opt(fa.and(fb.not()), Id("z")), Opt(fa.and(fb), Id("z"))))
        expectedSinks ::= (sinkStmt2, List(Opt(fa.and(fb.not()), Id("start")), Opt(fa.and(fb), Id("low"))))

        defaultTest("loop.c", expectedSinks) should be(true)
    }

    @Test def ifTest1() = {
        var expectedSinks : List[(AST, List[Opt[Id]])] = List()

        val sinkStmt1 = ExprStatement(AssignExpr(Id("sink"),"=",Id("res")))

        expectedSinks ::= (sinkStmt1, List(Opt(fb.and(fc), Id("secret"))))

        defaultTest("if1.c", expectedSinks) should be(true)
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


        defaultTest("if2.c", List()) should be(true)
    }
}
