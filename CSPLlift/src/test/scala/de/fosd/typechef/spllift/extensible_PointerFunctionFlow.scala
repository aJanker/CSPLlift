package de.fosd.typechef.spllift

import de.fosd.typechef.cpointeranalysis.{CPointerAnalysisFrontend, DefaultOpenSSLPointerAnalysisOptions}
import de.fosd.typechef.parser.c.PrettyPrinter
import de.fosd.typechef.spllift.analysis.Taint
import org.junit.Test

class extensible_PointerFunctionFlow extends SPLLiftTestHelper {

    @Test def defaultFlows() = {
        var successful = true

        val ast = parseTUnitFromFile("simplePointerFunctionFlow.c")
        val pa = new CPointerAnalysisFrontend(DefaultOpenSSLPointerAnalysisOptions)
        val pointer = pa.calculateInitialPointerEquivalenceRelation(ast, "simplePointerFunctionFlow.c")

       pointer.getEquivalenceClassesMap.map(println)

        println("###")

        pointer.getEquivalenceClasses.map(println)

        val (tunit, _, _, sinks) = defaultTestInit("simplePointerFunctionFlow.c", allSinks)

        println(PrettyPrinter.print(tunit))

        println(Taint.prettyPrintSinks(sinks))

        println(tunit)

        successful should be(true)

    }
}
