package de.fosd.typechef.spllift

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

            successful should be(true)

        }

}
