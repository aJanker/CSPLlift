package de.fosd.typechef.cspllift.informationflow

import de.fosd.typechef.cmodule.CModule
import de.fosd.typechef.cspllift.CSPLliftTestHelper
import de.fosd.typechef.cspllift.evaluation.CSPLliftEvaluationFrontend
import de.fosd.typechef.parser.c.PrettyPrinter
import org.junit.Test


class MinimalLinkingWithFunctionPointerTest extends CSPLliftTestHelper {

        @Test def minimalLinkingWithFunctionPointerTest() = {
            val interface = getClass.getResource("/" + testfileDir ).getFile + "CModuleInterface.interface"

            val tunit = parseTUnitFromFile("mininmalLinking/minimal_linking_main.c")
            val cModule = new CModule()
            cModule.addTUnit(tunit)

            println(PrettyPrinter.print(tunit))

            println(tunit)

            val evaluation = new CSPLliftEvaluationFrontend(cModule, options = new InformationFlowTestOptions(Some(interface)))
            val eval = evaluation.evaluate()

            eval should be(true)

        }

}
