package de.fosd.typechef.spllift

import de.fosd.typechef.parser.c.PrettyPrinter
import de.fosd.typechef.spllift.analysis.Taint
import org.junit.Test

class extensible_PointerFunctionFlow extends SPLLiftTestHelper {

    @Test def defaultFlows() = {
        var successful = true

        val (tunit, _, _, sinks) = defaultTestInit("simplePointerFunctionFlow.c", allSinks)

        println(Taint.prettyPrintSinks(sinks))

        println(PrettyPrinter.print(tunit))

        successful should be(true)

    }

    @Test def defaultFlows2() = {
        var successful = true

        val (tunit, _, _, sinks) = defaultTestInit("extensible_PointerFunctionFlow.c", allSinks)

        println(PrettyPrinter.print(tunit))

        successful should be(true)

    }
}
