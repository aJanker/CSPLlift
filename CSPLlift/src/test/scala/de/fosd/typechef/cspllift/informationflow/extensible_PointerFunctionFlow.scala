package de.fosd.typechef.cspllift.informationflow

import de.fosd.typechef.cspllift.CSPLliftTestHelper
import de.fosd.typechef.cspllift.analysis.Taint
import de.fosd.typechef.cspllift.evaluation.CSPLliftEvaluationFrontend
import org.junit.Test

class extensible_PointerFunctionFlow extends CSPLliftTestHelper {

    @Test def defaultFlows() = {
        var successful = true

        val (tunit, _, _, sinks) = defaultTestInit("simplePointerFunctionFlow.c", allSinks)

        val evaluation = new CSPLliftEvaluationFrontend(tunit)
        val eval = evaluation.evaluate(new InformationFlowTestOptions)

        println(Taint.prettyPrintSinks(sinks))

        successful && eval should be(true)

    }

    @Test def defaultFlows2() = {
        var successful = true

        val (tunit, _, _, sinks) = defaultTestInit("extensible_PointerFunctionFlow.c", allSinks)

        val evaluation = new CSPLliftEvaluationFrontend(tunit)
        val eval = evaluation.evaluate(new InformationFlowTestOptions)

        println(Taint.prettyPrintSinks(sinks))

        successful && eval should be(true)

    }
}
