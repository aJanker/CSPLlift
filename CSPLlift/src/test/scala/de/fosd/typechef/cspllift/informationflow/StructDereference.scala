package de.fosd.typechef.cspllift.informationflow

import de.fosd.typechef.cspllift.CSPLliftTestHelper
import org.junit.Test


class StructDereference extends CSPLliftTestHelper {


    @Test def structDerefernce(): Unit = {

        var successful = true

        val tunit= parseTUnitFromFile("structDereference.c")

        successful should be(true)
    }


}
