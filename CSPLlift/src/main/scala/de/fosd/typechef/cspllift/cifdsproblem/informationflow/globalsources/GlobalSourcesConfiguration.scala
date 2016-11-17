package de.fosd.typechef.cspllift.cifdsproblem.informationflow.globalsources

import de.fosd.typechef.cspllift.cifdsproblem.informationflow.InformationFlowConfiguration

trait GlobalSourcesConfiguration extends InformationFlowConfiguration {
    /**
      * If false, then the solver will only compute the exploded super graph but not propagate values.
      * This can save time for IFDS problems where all of the interesting results are collected already
      * during the computation of the super graph.
      */
    override def computeValues(): Boolean = true
}
