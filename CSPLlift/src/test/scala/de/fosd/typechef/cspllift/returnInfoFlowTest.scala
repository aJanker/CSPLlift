package de.fosd.typechef.cspllift

import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.cspllift.cifdsproblem.Reach
import de.fosd.typechef.featureexpr.FeatureExpr
import de.fosd.typechef.featureexpr.bdd.True
import de.fosd.typechef.parser.c.{AssignExpr, ExprStatement, Id}
import org.junit.Test


class ReturnInfoFlowTest extends CSPLliftTestHelper {

    @Test def testMultipleExitReturnFlows() = {
        def isSink(r: Reach): Boolean = {
            r.to.entry match {
                case ExprStatement(AssignExpr(Id(name), _, _)) if name.equalsIgnoreCase("sink") => true
                case _ => false
            }
        }

        var successful = true

        val (_, _, _, sinks) = defaultTestInit("returnFlow1.c", isSink)

        successful = successful && sinks.size == 1 // only one sink location should be found

        val sink = sinks.head
        successful = successful && (sink._1 match {
            case ExprStatement(AssignExpr(Id("sink"), "=", Id("y"))) => true // correct sink statement should be found
            case _ => false
        })

        var expectedReaches : List[(FeatureExpr, List[Opt[Id]])] = List()

        // sink1 : True, (y, True) //
        expectedReaches ::= (True, List(Opt(True, Id("y"))))

        // sink2 : A&B&C, (y, A&B&C) , (p, A&C) //
        expectedReaches ::= (fa.and(fb).and(fc), List(Opt(fa.and(fb).and(fc), Id("y")), Opt(fa.and(fc), Id("p"))))

        // sink3 : B&C, (y, B&C) , (p, C), (x, True)
        expectedReaches ::= (fb.and(fc), List(Opt(fb.and(fc), Id("y")), Opt(fc, Id("p")), Opt(True, Id("x"))))

        // sink4 : !B&C, (y, !B&C)
        expectedReaches ::= (fb.not().and(fc), List(Opt(fb.not().and(fc), Id("y"))))

        successful = successful && allReachesMatch(sink._2, expectedReaches)

        successful should be(true)
    }

}
