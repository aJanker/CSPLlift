package de.fosd.typechef.cspllift.cifdsproblem.informationflow.globalsources

import java.util

import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.cspllift.cifdsproblem.CIFDSProblem
import de.fosd.typechef.cspllift.cifdsproblem.informationflow.flowfact.sinkorsource.{SourceDefinition, SourceType, Variable}
import de.fosd.typechef.cspllift.cifdsproblem.informationflow.flowfact.{InformationFlowFact, Zero}
import de.fosd.typechef.cspllift.cintercfg.{CInterCFGNode, _}
import de.fosd.typechef.parser.c._
import heros.{FlowFunction, FlowFunctions}
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.JavaConverters._

class GlobalSourcesProblem(cICFG: CInterCFG) extends CIFDSProblem[InformationFlowFact](cICFG, List()) with GlobalSourcesConfiguration {
    private lazy val logger: Logger = LoggerFactory.getLogger(getClass)

    private var globalSeeds = scala.collection.mutable.Map[String, List[InformationFlowFact]]()

    def getGlobalSources: List[InformationFlowFact] = {
        val input = globalSeeds.values.toList.flatten
        logger.info("Seeds initial:\t" + input.size)
        val res = filterDuplicates(input)
        logger.info("Seeds without duplicates:\t" + res.size)
        res
    }

    private def filterDuplicates(seeds: List[InformationFlowFact]): List[InformationFlowFact] = {
        var names = scala.collection.mutable.Set[SourceType]()

        seeds.filter {
            case s: SourceDefinition if !names.contains(s.getType) =>
                names += s.getType
                true
            case _ => false
        }
    }

    override def initialSeeds(): util.Map[CInterCFGNode, util.Set[InformationFlowFact]] = {
        val initialSeeds = new util.HashMap[CInterCFGNode, util.Set[InformationFlowFact]]()

        interproceduralCFG.getEntryFunctions.foreach {
            entryFunction =>
                if (!initialSeedsExists(entryFunction))
                    globalSeeds += (getDestinationFile(entryFunction) -> globalsAsInitialSeeds(entryFunction))

                interproceduralCFG.getStartPointsOf(entryFunction).asScala.foreach(initialSeeds.put(_, GEN(zeroValue())))
        }

        initialSeeds
    }

    private def globalsAsInitialSeeds(fDef: CInterCFGFDef): List[InformationFlowFact] = {
        val globalVariables = interproceduralCFG.getTUnit(fDef).defs.filterNot {
            // Ignore function and typedef definitions
            case Opt(_, f: FunctionDef) => true
            case Opt(_, d: Declaration) =>
                d.declSpecs.exists {
                    case Opt(_, t: TypedefSpecifier) => true
                    case _ => false
                }
            case _ => false
        }

        globalVariables.flatMap(x => {
            // do not trace definitions as they are very expensive but do not propagate any information
            val decls = assignsVariables(x).map(_._1)

            decls.map(decl => SourceDefinition(Variable(decl), CInterCFGStmt(x.entry), SCOPE_GLOBAL))
        })
    }

    private def initialSeedsExists(destinationMethod: CInterCFGFDef): Boolean = globalSeeds.keys.exists(getDestinationFile(destinationMethod).equalsIgnoreCase)

    private def getDestinationFile(destinationMethod: CInterCFGFDef): String = destinationMethod.get.getFile.getOrElse("")

    /**
      * In order to detect global variables as inital seed values for the information flow analysis problem,
      * we do not need to compute any flow function. Therefore the following method stubs.
      */
    override def flowFunctions(): FlowFunctions[CInterCFGNode, InformationFlowFact, CInterCFGFDef] = flowFunctionFactory

    private lazy val flowFunctionFactory: FlowFunctions[CInterCFGNode, InformationFlowFact, CInterCFGFDef] = new FlowFunctions[CInterCFGNode, InformationFlowFact, CInterCFGFDef] {
        private lazy val defaultIdentityFlowFunction = new FlowFunction[InformationFlowFact] {
            override def computeTargets(fact: InformationFlowFact): util.Set[InformationFlowFact] = GEN(fact)
        }

        override def getCallFlowFunction(callStmt: CInterCFGNode, destinationMethod: CInterCFGFDef): FlowFunction[InformationFlowFact] = {
            if (!initialSeedsExists(destinationMethod))
                globalSeeds += (getDestinationFile(destinationMethod) -> globalsAsInitialSeeds(destinationMethod))

            defaultIdentityFlowFunction
        }

        override def getNormalFlowFunction(curr: CInterCFGNode, succ: CInterCFGNode): FlowFunction[InformationFlowFact] = defaultIdentityFlowFunction

        override def getReturnFlowFunction(callSite: CInterCFGNode, calleeMethod: CInterCFGFDef, exitStmt: CInterCFGNode, returnSite: CInterCFGNode): FlowFunction[InformationFlowFact] = defaultIdentityFlowFunction

        override def getCallToReturnFlowFunction(callSite: CInterCFGNode, returnSite: CInterCFGNode): FlowFunction[InformationFlowFact] = defaultIdentityFlowFunction
    }

    private lazy val zero = Zero()
    override def zeroValue(): InformationFlowFact = zero
}
