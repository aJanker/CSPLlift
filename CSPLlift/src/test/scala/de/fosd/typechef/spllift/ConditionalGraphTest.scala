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

        // sink3 : A, (sink = bound = foo = z = 0 (A))
        expectedReaches ::= (fa, List(Opt(fa, Id("bound")), Opt(fa, Id("foo")), Opt(True, Id("z"))))


        defaultSingleSinkTest("loop.c", sinkStmt, expectedReaches) should be(true)
    }
}
