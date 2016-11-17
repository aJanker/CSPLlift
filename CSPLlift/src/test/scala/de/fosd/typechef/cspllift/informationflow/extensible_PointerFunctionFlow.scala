package de.fosd.typechef.cspllift.informationflow

import de.fosd.typechef.cspllift.CSPLliftTestHelper
import de.fosd.typechef.cspllift.evaluation.CSPLliftEvaluationFrontend
import org.junit.Test

class extensible_PointerFunctionFlow extends CSPLliftTestHelper {

    @Test def defaultFlows() = {
        var successful = true

        val tunit= parseTUnitFromFile("simplePointerFunctionFlow.c")

        val evaluation = new CSPLliftEvaluationFrontend(tunit, options = new InformationFlowTestOptions)
        val eval = evaluation.evaluate()


        successful && eval should be(true)

    }

    @Test def defaultFlows2() = {
        var successful = true

        val tunit= parseTUnitFromFile("extensible_PointerFunctionFlow.c")

        val evaluation = new CSPLliftEvaluationFrontend(tunit, options = new InformationFlowTestOptions)
        val eval = evaluation.evaluate()

        successful && eval should be(true)

    }
}
