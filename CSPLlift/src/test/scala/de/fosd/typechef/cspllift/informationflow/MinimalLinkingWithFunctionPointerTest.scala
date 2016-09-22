package de.fosd.typechef.cspllift.informationflow

import de.fosd.typechef.cspllift.evaluation.CSPLliftEvaluationFrontend
import de.fosd.typechef.cspllift.{CSPLliftTestHelper, CSPLliftTestOptions}
import de.fosd.typechef.parser.c.PrettyPrinter
import org.junit.Test


class MinimalLinkingWithFunctionPointerTest extends CSPLliftTestHelper {

        @Test def minimalLinkingWithFunctionPointerTest() = {
            var successful = true
            val interface = getClass.getResource("/" + testfileDir ).getFile + "CModuleInterface.interface"

            val (tunit, _, _, sinks) = defaultTestInit("minimal_linking_main.c", allSinks, Some(interface))

            println(PrettyPrinter.print(tunit))

            println(tunit)

            //defaultTestInit("simplePointerFunctionFlow.c", allSinks)

            val evaluation = new CSPLliftEvaluationFrontend(tunit)
            val eval = evaluation.evaluate(new CSPLliftTestOptions(Some(interface)))

            eval && successful should be(true)

        }

}
