package de.fosd.typechef.cspllift.informationflow

import de.fosd.typechef.cspllift.CSPLliftTestHelper
import de.fosd.typechef.cspllift.evaluation.CSPLliftEvaluationFrontend
import de.fosd.typechef.parser.c.PrettyPrinter
import org.junit.Test


class MinimalLinkingWithFunctionPointerTest extends CSPLliftTestHelper {

        @Test def minimalLinkingWithFunctionPointerTest() = {
            val interface = getClass.getResource("/" + testfileDir ).getFile + "CModuleInterface.interface"

            val tunit = parseTUnitFromFile("mininmalLinking/minimal_linking_main.c")

            println(PrettyPrinter.print(tunit))

            println(tunit)

            val evaluation = new CSPLliftEvaluationFrontend(tunit, options = new InformationFlowTestOptions(Some(interface)))
            val eval = evaluation.evaluate()

            eval should be(true)

        }

}
