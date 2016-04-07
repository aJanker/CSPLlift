package de.fosd.typechef.spllift

import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.featureexpr.FeatureExpr
import de.fosd.typechef.featureexpr.bdd.True
import de.fosd.typechef.parser.c._
import de.fosd.typechef.spllift.analysis.Taint
import de.fosd.typechef.spllift.ifdsproblem.Reach
import org.junit.Test

class StructsTest extends SPLLiftTestHelper {

    @Test def simpleStructFields() = {
        var successful = true

        val (tunit, _, _, sinks) = defaultTest("struct1.c", allSinks)

        println(PrettyPrinter.print(tunit))

        println(Taint.prettyPrintSinks(sinks))

        println(tunit)

        successful should be(true)
    }

    /**
      * Tests the correct assignment from variables to struct fields (x.y = z);
      */
    @Test def basicStructAssignments() = {
        def isSink(r: Reach): Boolean = {
            r.to.entry match {
                case ExprStatement(AssignExpr(PostfixExpr(Id(p1),PointerPostfixSuffix(_,Id("x"))),_,Id("mx2"))) => true
                case _ => false
            }
        }
        var successful = true

        val (tunit, _, _, sinks) = defaultTest("basicStructAssignments.c", isSink)

        successful = successful && sinks.size == 1 // only one sink location should be found

        val sink = sinks.head

        successful = successful && (sink._1 match {
            case ExprStatement(AssignExpr(PostfixExpr(Id(p1),PointerPostfixSuffix(_,Id("x"))),_,Id("mx2"))) => true // correct sink statement should be found
            case _ => false
        })

        var expectedReaches : List[(FeatureExpr, List[Opt[Id]])] = List()

        // sink1 : True, (mx2, True), (mx, True)
        expectedReaches ::= (True, List(Opt(True, Id("mx2")), Opt(True, Id("mx"))))

        successful = successful && allReachesMatch(sink._2, expectedReaches)

        successful should be(true)
    }

    @Test def allStructAssignments() = {
        var successful = true

        val (tunit, _, _, sinks) = defaultTest("basicStructAssignments.c", allSinks)

        println(PrettyPrinter.print(tunit))

        println(Taint.prettyPrintSinks(sinks))

        println(tunit)

        successful should be(true)
    }

}
