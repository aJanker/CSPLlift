package de.fosd.typechef.cspllift.informationflow

import de.fosd.typechef.cspllift.CSPLliftTestHelper
import org.junit.Test

class RewriteTest extends CSPLliftTestHelper {

    @Test def rewriteNestedFunctionCallsAndReturns(): Unit = {

        var successful = true

        val (tunit, _, _, sinks) = defaultTestInit("rewrite.c", allSinks)

        successful should be(true)

    }

    @Test def rewriteNestedFunctionCalls(): Unit = {

        var successful = true

        val (tunit, _, _, sinks) = defaultTestInit("rewriteMinimal.c", allSinks)

        successful should be(true)

    }

}
