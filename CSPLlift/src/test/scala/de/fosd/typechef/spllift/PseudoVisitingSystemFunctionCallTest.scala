package de.fosd.typechef.spllift

import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.featureexpr.FeatureExpr
import de.fosd.typechef.featureexpr.bdd.True
import de.fosd.typechef.parser.c._
import de.fosd.typechef.spllift.cifdsproblem.Reach
import org.junit.Test

class PseudoVisitingSystemFunctionCallTest extends CSPLliftTestHelper {

    @Test def testMulitpleSourcesReachingSystemCall() = {
        def isSink(r: Reach): Boolean = {
            r.to.entry match {
                case ExprStatement(PostfixExpr(Id(name), FunctionCall(_))) if name.equalsIgnoreCase("printf") => true
                case _ => false
            }
        }

        var successful = true

        val (_, _, _, sinks) = defaultTestInit("pseudoVistingSystemFunction_Complex.c", isSink)

        successful = successful && sinks.size == 1 // only one sink location should be found

        val sink = sinks.head
        successful = successful && (sink._1 match {
            case ExprStatement(PostfixExpr(Id("printf"), FunctionCall(ExprList(List(Opt(True, StringLit(List(Opt(True, _)))), Opt(True, PointerCreationExpr(Id("returnSite")))))))) => true // correct sink statement should be found
            case _ => false
        })

        var expectedReaches: List[(FeatureExpr, List[Opt[Id]])] = List()

        // sink1 : True, (returnSite, True), (y, True)
        expectedReaches ::=(True,  List(Opt(True, Id("returnSite")), Opt(True, Id("y"))))

        // sink2 : True (returnSite, B & D), (p, B & D), (x, true) (secret, true)
        expectedReaches ::=(fb.and(fd), List(Opt(fb.and(fd), Id("returnSite")), Opt(fb.and(fd), Id("y")), Opt(fd, Id("p")), Opt(True, Id("x")), Opt(True, Id("secret"))))

        // sink3 : True, (returnSite, A & B & D), (y, A & B & D), (p, A)
        expectedReaches ::=(fa.and(fb.and(fd)), List(Opt(fa.and(fb.and(fd)), Id("returnSite")), Opt(fa.and(fb.and(fd)), Id("y")), Opt(fa, Id("p"))))

        // sink4: True, (returnSite, !B & D), (y, !B & D)
        expectedReaches ::=(fb.not.and(fd), List(Opt(fb.not.and(fd), Id("returnSite")), Opt(fb.not.and(fd), Id("y"))))

        // sink5: True, (returnSite, B & C & D), (y, B & C & D),  (p ,C & D), (x, C)
        expectedReaches ::=(fb.and(fc).and(fd), List(Opt(fb.and(fc).and(fd), Id("returnSite")), Opt(fb.and(fc).and(fd), Id("y")), Opt(fc.and(fd), Id("p")), Opt(fc, Id("x"))))

        successful = successful && allReachesMatch(sink._2, expectedReaches)

        successful should be(true)
    }
}
