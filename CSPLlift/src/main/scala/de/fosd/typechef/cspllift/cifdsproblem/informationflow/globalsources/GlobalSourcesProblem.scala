package de.fosd.typechef.cspllift.cifdsproblem.informationflow.globalsources

import java.util

import de.fosd.typechef.cspllift.cifdsproblem.CIFDSProblem
import de.fosd.typechef.cspllift.cifdsproblem.informationflow.flowfact.{InformationFlowFact, Zero}
import de.fosd.typechef.cspllift.cintercfg.{CICFGFDef, CICFGNode, CInterCFG}
import de.fosd.typechef.parser.c.FunctionDef
import heros.{FlowFunction, FlowFunctions}
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.JavaConverters._

class GlobalSourcesProblem (cICFG: CInterCFG) extends CIFDSProblem[InformationFlowFact](cICFG) with GlobalSourcesConfiguration {
    private lazy val logger: Logger = LoggerFactory.getLogger(getClass)

    override def zeroValue(): InformationFlowFact = zero
    private lazy val zero = Zero()

    override def initialSeeds(): util.Map[CICFGNode, util.Set[InformationFlowFact]] = {
        val initialSeeds = new util.HashMap[CICFGNode, util.Set[InformationFlowFact]]()

        interproceduralCFG.getEntryFunctions.foreach {
            entryFunction => interproceduralCFG.getStartPointsOf(entryFunction).asScala.foreach(initialSeeds.put(_, GEN(zeroValue())))
        }

        initialSeeds
    }

    private var initialGlobalsFile: String = ""
    private var filesWithSeeds: Set[String] = Set()

    private def globalsAsInitialSeeds(fDef: CICFGFDef): util.Set[InformationFlowFact] = GEN(globalsAsInitialSeedsL(fDef) :+ zeroValue())

    private def globalsAsInitialSeedsL(fDef: CICFGFDef): List[InformationFlowFact] = {
        /*val globalVariables = interproceduralCFG.getTUnit(fDef).defs.filterNot {
            // Ignore function and typedef definitions
            case Opt(_, f: FunctionDef) => true //
            case Opt(_, d: Declaration) =>
                d.declSpecs.exists {
                    case Opt(_, t: TypedefSpecifier) => true
                    case _ => false
                }
            case _ => false
        }

        val globalInfoFlowFacts = globalVariables.flatMap(x => {
            val decls = declares(x)

            // Note: we ignore the actual file of the declaration as it may be declared in a header file.
            // As variables declared in header files may be included across several files, this way prevents matching errors.
            if (decls.nonEmpty) decls.map(decl => SourceDefinition(Variable(decl), CICFGConcreteStmt(x), SCOPE_GLOBAL))
            else None
        })

        globalInfoFlowFacts */
        List()
    }

    private def initialSeedsExists(destinationMethod: FunctionDef): Boolean = {
        val destinationMethodFile = destinationMethod.getFile.getOrElse("")
        initialGlobalsFile.equalsIgnoreCase(destinationMethodFile) || filesWithSeeds.exists(destinationMethodFile.equalsIgnoreCase)
    }

    /**
      * In order to detect global variables as inital seed values for the information flow analysis problem,
      * we do not need to compute any flow function. Therefore the following method stubs.
      */
    override def flowFunctions(): FlowFunctions[CICFGNode, InformationFlowFact, CICFGFDef] = flowFunctionFactory
    private lazy val flowFunctionFactory: FlowFunctions[CICFGNode, InformationFlowFact, CICFGFDef] = new FlowFunctions[CICFGNode, InformationFlowFact, CICFGFDef] {
        private lazy val defaultIdentityFlowFunction = new FlowFunction[InformationFlowFact] { override def computeTargets(fact: InformationFlowFact): util.Set[InformationFlowFact] = GEN(fact) }

        override def getNormalFlowFunction(curr: CICFGNode, succ: CICFGNode): FlowFunction[InformationFlowFact] = defaultIdentityFlowFunction

        override def getCallFlowFunction(callStmt: CICFGNode, destinationMethod: CICFGFDef): FlowFunction[InformationFlowFact] = defaultIdentityFlowFunction

        override def getReturnFlowFunction(callSite: CICFGNode, calleeMethod: CICFGFDef, exitStmt: CICFGNode, returnSite: CICFGNode): FlowFunction[InformationFlowFact] = defaultIdentityFlowFunction

        override def getCallToReturnFlowFunction(callSite: CICFGNode, returnSite: CICFGNode): FlowFunction[InformationFlowFact] = defaultIdentityFlowFunction
    }
}
