package de.fosd.typechef.cspllift.informationflow

import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.cspllift.CSPLliftTestHelper
import de.fosd.typechef.featureexpr.FeatureExpr
import de.fosd.typechef.featureexpr.bdd.True
import de.fosd.typechef.parser.c._
import org.junit.Test

class StructsTest extends CSPLliftTestHelper {

    /**
      * Tests the correct assignment from variables to struct fields (x.y = z);
      */
    @Test def basicStructAssignments1() = {
        var expectedReaches : List[(FeatureExpr, List[Opt[Id]])] = List()

        // sink1 : A, (mx2, A), (mx, True)
        expectedReaches ::= (fa, List(Opt(fa, Id("mx2")), Opt(True, Id("mx"))))

        // sink1 : A, (mx2, A), (mx, True)
        expectedReaches ::= (fa.not, List(Opt(fa.not, Id("mx2")), Opt(True, Id("my"))))

        val sinkStmt = ExprStatement(AssignExpr(PostfixExpr(Id("p1"),PointerPostfixSuffix(".", Id("x"))),"=",Id("mx2")))

        true should be(true)
    }

    /**
      * Tests the correct assignment from struct field to variable (x = y.z);
      */
    @Test def basicStructAssignments2() = {

        var expectedReaches : List[(FeatureExpr, List[Opt[Id]])] = List()

        // sink1 : A, (p1, A), (mx2, A), (mx, True)
        expectedReaches ::= (fa, List(Opt(fa, Id("p1")) , Opt(fa, Id("mx2")), Opt(True, Id("mx"))))

        // sink2 : !A, (p1, !A), (mx2, !A), (my, True)
        expectedReaches ::= (fa.not, List(Opt(fa.not, Id("p1")), Opt(fa.not, Id("mx2")), Opt(True, Id("my"))))

        val sinkStmt = ExprStatement(AssignExpr(Id("my2"),"=",PostfixExpr(Id("p1"),PointerPostfixSuffix(".",Id("x")))))

        true should be(true)
    }

    @Test def allStructAssignments() = {
        var successful = true

        val tunit = parseTUnitFromFile("basicStructAssignments.c")

        println(tunit)

        successful should be(true)
    }

    @Test def simpleStructFields() = {
        var successful = true

        val tunit = parseTUnitFromFile("structToStruct.c")

        println(PrettyPrinter.print(tunit))

        println(tunit)

        successful should be(true)
    }


}
