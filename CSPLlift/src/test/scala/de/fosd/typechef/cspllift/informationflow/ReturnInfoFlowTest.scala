package de.fosd.typechef.cspllift.informationflow

import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.parser.c.{AST, AssignExpr, ExprStatement, Id}
import org.junit.Test


class ReturnInfoFlowTest extends InformationFlowTestHelper {

    @Test def testMultipleExitReturnFlows() = {
        var expectedSinks : List[(AST, List[Opt[Id]])] = List()

        val sinkStmt1 = ExprStatement(AssignExpr(Id("sink"),"=",Id("y")))

        expectedSinks ::= (sinkStmt1, List(Opt(fb.and(fc).and(fe), Id("secret"))))

        defaultTest("returnFlow1.c", expectedSinks) should be(true)
    }
}
