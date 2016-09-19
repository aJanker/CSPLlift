package de.fosd.typechef.spllift

import org.junit.Test


class SingleStatements extends CSPLliftTestHelper {


    @Test def singleStatements(): Unit = {

        var successful = true

        val (tunit, _, _, sinks) = defaultTestInit("singleStatements.c", allSinks)

        successful should be(true)
    }


}
