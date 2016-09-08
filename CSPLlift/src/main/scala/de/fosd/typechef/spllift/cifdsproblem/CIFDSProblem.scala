package de.fosd.typechef.spllift.cifdsproblem

import de.fosd.typechef.spllift.commons.CInterCFGCommons
import de.fosd.typechef.spllift.{CInterCFG, CInterCFGPseudoVistingSystemLibFunctions, IFDSProblem}

abstract class CIFDSProblem[D](cICFG: CInterCFG) extends IFDSProblem[D] with CInterCFGCommons with CInterCFGPseudoVistingSystemLibFunctions {

    /**
      * Returns the interprocedural control-flow graph which this problem is computed over.
      *
      * <b>NOTE:</b> this method could be called many times. Implementations of this
      * interface should therefore cache the return value!
      */
    override def interproceduralCFG: CInterCFG = cICFG
}
