package de.fosd.typechef.cspllift.informationflow

import de.fosd.typechef.cspllift.CSPLliftTestHelper
import org.junit.Test


class StructDereference extends CSPLliftTestHelper {


    @Test def structDerefernce(): Unit = {

        var successful = true

        val (tunit, _, _, sinks) = defaultTestInit("structDereference.c", allSinks)

        successful should be(true)
    }


}
