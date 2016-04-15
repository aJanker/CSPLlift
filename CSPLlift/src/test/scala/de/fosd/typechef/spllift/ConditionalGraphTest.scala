package de.fosd.typechef.spllift

import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.featureexpr.FeatureExpr
import de.fosd.typechef.featureexpr.bdd.True
import de.fosd.typechef.parser.c._
import org.junit.Test


class ConditionalGraphTest extends SPLLiftTestHelper {

    @Test def simpleLoopTest() = {
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

        val (tunit, _, _, sinks) = defaultTestInit("loop.c", allSinks)

        println(PrettyPrinter.print(tunit))

        defaultSingleSinkTest("loop.c", sinkStmt, expectedReaches) should be(true)
    }
}
