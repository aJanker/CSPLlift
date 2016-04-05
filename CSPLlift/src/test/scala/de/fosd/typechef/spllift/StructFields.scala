package de.fosd.typechef.spllift

import de.fosd.typechef.parser.c.PrettyPrinter
import de.fosd.typechef.spllift.analysis.Taint
import de.fosd.typechef.spllift.ifdsproblem.Reach
import org.junit.Test

class StructFields extends SPLLiftTestHelper {

    @Test def simpleStructFields() = {
        def isSink(r: Reach): Boolean = true

        var successful = true

        val (tunit, _, _, sinks) = defaultTest("struct1.c", isSink)

        println(PrettyPrinter.print(tunit))

        println(Taint.prettyPrintSinks(sinks))

        println(tunit)

        successful should be(true)
    }

}
