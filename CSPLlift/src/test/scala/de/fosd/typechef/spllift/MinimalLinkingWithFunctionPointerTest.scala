package de.fosd.typechef.spllift

import de.fosd.typechef.parser.c.PrettyPrinter
import de.fosd.typechef.spllift.analysis.Taint
import org.junit.Test


class MinimalLinkingWithFunctionPointerTest extends SPLLiftTestHelper {

        @Test def minimalLinkingWithFunctionPointerTest() = {
            var successful = true

            val (tunit, _, _, sinks) = defaultTestInit("minimal_linking_main.c", allSinks)

            println(PrettyPrinter.print(tunit))

            println(Taint.prettyPrintSinks(sinks))

            println(tunit)

            successful should be(true)

        }

}
