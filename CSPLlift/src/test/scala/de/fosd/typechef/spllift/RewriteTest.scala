package de.fosd.typechef.spllift

import de.fosd.typechef.spllift.commons.StopWatch
import org.junit.Test

class RewriteTest extends SPLLiftTestHelper {

    @Test def rewriteNestedFunctionCalls(): Unit = {

        var successful = true

        val (tunit, _, _, sinks) = defaultTestInit("rewrite.c", allSinks)

        println(sinks)

        println(StopWatch.toCSV)

        successful should be(true)

    }

}
