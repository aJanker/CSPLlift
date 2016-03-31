package de.fosd.typechef.spllift

import de.fosd.typechef.spllift.ifdsproblem.Reach
import org.junit.Test

class StructFields extends SPLLiftTestHelper {

    @Test def simpleStructFields() = {
        def isSink(r: Reach): Boolean = true

        var successful = true

        val (tunit, _, _, sinks) = defaultInit("struct1.c", isSink)

        println(tunit)
        println(sinks)
    }

}
