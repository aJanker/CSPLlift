package de.fosd.typechef.cspllift.cifdsproblem

import de.fosd.typechef.cspllift.commons.CInterCFGCommons
import de.fosd.typechef.cspllift.evaluation.SimpleConfiguration
import de.fosd.typechef.cspllift.{CInterCFG, CInterCFGPseudoVistingSystemLibFunctions, IFDSProblem}
import de.fosd.typechef.featureexpr.bdd.BDDFeatureExpr

abstract class CIFDSProblem[D <: CFlowFact](cICFG: CInterCFG) extends IFDSProblem[D] with CInterCFGCommons with CInterCFGPseudoVistingSystemLibFunctions {

    /**
      * Returns the interprocedural control-flow graph which this problem is computed over.
      *
      * <b>NOTE:</b> this method could be called many times. Implementations of this
      * interface should therefore cache the return value!
      */
    override def interproceduralCFG: CInterCFG = cICFG
}

trait CFlowFact {

    def isEquivalentTo(other : CFlowFact, configuration: SimpleConfiguration) : Boolean

    def getConditions : Set[BDDFeatureExpr]

    def isInterestingFact : Boolean

    def toText: String

}