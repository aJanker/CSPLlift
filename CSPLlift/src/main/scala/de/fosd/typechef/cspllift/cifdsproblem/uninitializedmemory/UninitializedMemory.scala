package de.fosd.typechef.cspllift.cifdsproblem.uninitializedmemory

import java.util

import de.fosd.typechef.cspllift.cifdsproblem.uninitializedmemory.flowfact.{UninitializedMemoryFlowFact, Zero}
import de.fosd.typechef.cspllift.cifdsproblem.{CIFDSDefaultConfiguration, CIFDSProblem}
import de.fosd.typechef.cspllift.cintercfg._
import de.fosd.typechef.customization.crewrite.AssignDeclDefUse
import de.fosd.typechef.parser.c.Id
import heros.{FlowFunction, FlowFunctions}

class UninitializedMemory(cICFG: CInterCFG) extends CIFDSProblem[UninitializedMemoryFlowFact](cICFG, List()) with CIFDSDefaultConfiguration with AssignDeclDefUse {

    private val zero = Zero()

    private val flowFunctionsFactory = new FlowFunctions[CInterCFGNode, UninitializedMemoryFlowFact, CInterCFGFDef] {
        override def getNormalFlowFunction(curr: CInterCFGNode, succ: CInterCFGNode): FlowFunction[UninitializedMemoryFlowFact] = new FlowFunction[UninitializedMemoryFlowFact] {

            private lazy val currDecls: List[Id] = declares(curr.get)
            private lazy val currDefines: List[Id] = defines(curr.get) diff currDecls
            private lazy val currUses: List[Id] = uses(curr.get)

            override def computeTargets(source: UninitializedMemoryFlowFact): util.Set[UninitializedMemoryFlowFact] = {
                source match {
                    case z: Zero if cICFG.isStartPoint(curr) => util.Collections.singleton(source.KILL(currDecls).GEN(currDefines))
                    case z: Zero => util.Collections.singleton(source.KILL(currDecls).GEN(currDefines))
                    case _ =>
                        // currUses.forall(source.assignedNames.contains)
                        util.Collections.singleton(source.KILL(currDecls).GEN(currDefines))
                }
            }
        }

        override def getCallFlowFunction(callStmt: CInterCFGNode, destinationMethod: CInterCFGFDef): FlowFunction[UninitializedMemoryFlowFact] = new FlowFunction[UninitializedMemoryFlowFact] {
            override def computeTargets(source: UninitializedMemoryFlowFact): util.Set[UninitializedMemoryFlowFact] = util.Collections.singleton(source)
        }

        override def getReturnFlowFunction(callSite: CInterCFGNode, calleeMethod: CInterCFGFDef, exitStmt: CInterCFGNode, returnSite: CInterCFGNode): FlowFunction[UninitializedMemoryFlowFact] = new FlowFunction[UninitializedMemoryFlowFact] {
            override def computeTargets(source: UninitializedMemoryFlowFact): util.Set[UninitializedMemoryFlowFact] = util.Collections.singleton(source)
        }

        override def getCallToReturnFlowFunction(callSite: CInterCFGNode, returnSite: CInterCFGNode): FlowFunction[UninitializedMemoryFlowFact] = new FlowFunction[UninitializedMemoryFlowFact] {
            override def computeTargets(source: UninitializedMemoryFlowFact): util.Set[UninitializedMemoryFlowFact] = util.Collections.singleton(source)
        }
    }

    override def flowFunctions(): FlowFunctions[CInterCFGNode, UninitializedMemoryFlowFact, CInterCFGFDef] = flowFunctionsFactory

    override def initialSeeds(): util.Map[CInterCFGNode, util.Set[UninitializedMemoryFlowFact]] = util.Collections.EMPTY_MAP.asInstanceOf[util.Map[CInterCFGNode, util.Set[UninitializedMemoryFlowFact]]]

    override def zeroValue(): UninitializedMemoryFlowFact = zero
}
