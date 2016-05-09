package de.fosd.typechef.spllift

import org.junit.Test

class RewriteTest extends SPLLiftTestHelper {

    @Test def rewriteNestedFunctionCalls(): Unit = {

        var successful = true

        val (tunit, _, _, sinks) = defaultTestInit("rewrite.c", allSinks)

        println(tunit)

        println(sinks)

        successful should be(true)

    }

}
