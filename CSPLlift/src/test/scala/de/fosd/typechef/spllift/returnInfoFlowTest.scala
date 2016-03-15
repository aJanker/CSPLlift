package de.fosd.typechef.spllift

import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.featureexpr.FeatureExpr
import de.fosd.typechef.featureexpr.bdd.True
import de.fosd.typechef.parser.c.{AssignExpr, ExprStatement, Id}
import de.fosd.typechef.spllift.analysis.Taint
import de.fosd.typechef.spllift.ifdsproblem.{InformationFlowProblem, Reach}
import org.junit.Test


class returnInfoFlowTest extends SPLLiftTestHelper {

    private val dbg = true

    @Test def testMultipleExitReturnFlows() = {
        def isSink(r: Reach): Boolean = {
            r.to.entry match {
                case ExprStatement(AssignExpr(Id(name), _, _)) if name.equalsIgnoreCase("sink") => true
                case _ => false
            }
        }


        var successful = true

        val tunit = parseTUnitFromFile("returnFlow1.c")

        val cInterCFG = new CInterCFG(tunit)
        val problem = new InformationFlowProblem(cInterCFG)
        val solution = CSPLliftFrontend.solve(problem)

        val sinks = Taint.findSinks[String](solution, isSink)

        // dbg print
        if (dbg) sinks.foreach(sink => {
            println("Sink at:\t" + sink._1)
            sink._2.foreach(ssink => println("CFGcondition " + ssink._1 + ":\t" + ssink._2))
        })

        successful = successful && sinks.size == 1 // only one sink location should be found

        val sink = sinks.head
        successful = successful && (sink._1 match {
            case ExprStatement(AssignExpr(Id("sink"), "=", Id("y"))) => true // correct sink statement should be found
            case _ => false
        })

        def isReachMatch(r: Reach, condition: FeatureExpr, reachingIds: List[Opt[Id]]): Boolean =
            r.to.condition.equivalentTo(condition) && r.from.forall(reachingIds contains)

        // sink1 : True, (y, True)
        successful = successful && sink._2.exists(s => {
            val reachingIds = List(Opt(True, Id("y")))
            val condition = True
            isReachMatch(s._2, condition, reachingIds)
        })
        // sink2 : A&B&C, (y, A&B&C) , (p, A)
        successful = successful && sink._2.exists(s => {
            val reachingIds = List(Opt(fa.and(fb).and(fc), Id("y")), Opt(fa, Id("p")))
            val condition = fa.and(fb).and(fc)
            isReachMatch(s._2, condition, reachingIds)
        })
        // sink3 : B&C, (y, B&C) , (p, C), (x, True)
        successful = successful && sink._2.exists(s => {
            val reachingIds = List(Opt(fb.and(fc), Id("y")), Opt(fc, Id("p")), Opt(True, Id("x")))
            val condition = fb.and(fc)
            isReachMatch(s._2, condition, reachingIds)
        })
        // sink4 : !B&C, (y, !B&C)
        successful = successful && sink._2.exists(s => {
            val reachingIds = List(Opt(fb.not().and(fc), Id("y")))
            val condition = fb.not().and(fc)
            isReachMatch(s._2, condition, reachingIds)
        })

        successful should be(true)
    }

}
