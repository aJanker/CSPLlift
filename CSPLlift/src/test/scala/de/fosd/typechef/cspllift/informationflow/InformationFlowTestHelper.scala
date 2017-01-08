package de.fosd.typechef.cspllift.informationflow

import de.fosd.typechef.cmodule.CModule
import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.cspllift._
import de.fosd.typechef.cspllift.analysis.InformationFlow
import de.fosd.typechef.cspllift.cifdsproblem.informationflow.InformationFlowProblem
import de.fosd.typechef.cspllift.cintercfg.{CInterCFG, DefaultCInterCFGConfiguration}
import de.fosd.typechef.cspllift.evaluation.CSPLliftEvaluationFrontend
import de.fosd.typechef.featureexpr.FeatureModel
import de.fosd.typechef.featureexpr.bdd.BDDFeatureModel
import de.fosd.typechef.parser.c.{AST, Id}

trait InformationFlowTestHelper extends CSPLliftTestHelper {

    def defaultTest(initialFile: String, expectedSinks: List[(AST, List[(Opt[Id])])], fm: FeatureModel = BDDFeatureModel.empty, interface: Option[String] = None): Boolean = {
        val tunit = parseTUnitFromFile(initialFile)
        val cModule = new CModule(rewriteTasks = CSPLlift.getIFDSRewriteRules)
        cModule.addTUnit(tunit)
        val testOptions = new InformationFlowTestOptions(interface)
        val cInterCFGConfiguration = new DefaultCInterCFGConfiguration(testOptions.getCLinkingInterfacePath)

        val cInterCFG = new CInterCFG(cModule, cInterCFGConfiguration)
        val problem = new InformationFlowProblem(cInterCFG)
        val solution = CSPLlift.solveAndCollectResults(problem)
        val allSinks = InformationFlow.allSinks(solution)

        // matches all expectes sinks with found ones
        lazy val sinks = expectedSinks.forall {
            case (expectedStmt, expectedSink) =>
                allSinks.exists {
                    case (aStmt, aSink) if aStmt.get.equals(expectedStmt) =>
                        expectedSink.forall(currSink => {
                            aSink.exists(asEntry =>
                                asEntry._1.getOriginId.equals(currSink.entry) && asEntry._2.equivalentTo(currSink.condition)
                            )
                        })
                    case _ => false
                }
        }


        val evaluation = new CSPLliftEvaluationFrontend(cModule, options = testOptions)
        val eval = evaluation.evaluate()

        eval && sinks
    }

}
