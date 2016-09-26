package de.fosd.typechef.cspllift.informationflow

import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.featureexpr.bdd.True
import de.fosd.typechef.parser.c.{AST, AssignExpr, ExprStatement, Id}
import org.junit.Test


class SingleReturnStmt extends InformationFlowTestHelper {


    @Test def singleReturnStmt(): Unit = {

        var expectedSinks : List[(AST, List[Opt[Id]])] = List()

        val sinkStmt1 = ExprStatement(AssignExpr(Id("y"),"=",Id("sink")))
        val sinkStmt2 = ExprStatement(AssignExpr(Id("x"),"=",Id("sink2")))


        expectedSinks ::= (sinkStmt1, List(Opt(True, Id("x"))))

        expectedSinks ::= (sinkStmt2, List(Opt(fa, Id("x")), Opt(True, Id("sink2"))))

        defaultTest("singleReturnStatements.c", List()) should be(true)
    }


}
