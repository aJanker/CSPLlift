package de.fosd.typechef.spllift

import org.junit.Test


class StructDereference extends SPLLiftTestHelper {


    @Test def structDerefernce(): Unit = {

        var successful = true

        val (tunit, _, _, sinks) = defaultTestInit("structDereference.c", allSinks)

        successful should be(true)
    }


}
