package de.fosd.typechef.spllift

import de.fosd.typechef.StopWatch
import org.junit.Test

class RewriteTest extends SPLLiftTestHelper {

    @Test def rewriteNestedFunctionCallsAndReturns(): Unit = {

        var successful = true

        val (tunit, _, _, sinks) = defaultTestInit("rewrite.c", allSinks)


        println(sinks)

        println(StopWatch.toCSV)

        successful should be(true)

    }

    @Test def rewriteNestedFunctionCalls(): Unit = {

        var successful = true

        val (tunit, _, _, sinks) = defaultTestInit("rewriteMinimal.c", allSinks)


        println(sinks)

        println(StopWatch.toCSV)

        successful should be(true)

    }

}
