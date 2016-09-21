package de.fosd.typechef.cspllift.cifdsproblem.informationflow

import heros.SolverConfiguration

trait InformationFlow2Configuration extends SolverConfiguration {
    /**
      * If false, then the solver will only compute the exploded super graph but not propagate values.
      * This can save time for IFDS problems where all of the interesting results are collected already
      * during the computation of the super graph.
      */
    override def computeValues(): Boolean = true

    /**
      * If true, the analysis will compute a partially unbalanced analysis problem in which
      * function returns are followed also further up the call stack than where the initial seeds
      * started.
      *
      * If this is enabled, when reaching the exit of a method that is <i>nowhere</i> called, in order
      * to avoid not at all processing the exit statement, the {@link IDESolver} will call
      * the <i>return</i> flow function with a <code>null</code> call site and return site.
      */
    override def followReturnsPastSeeds(): Boolean = false

    /**
      * Returns the number of threads to be used by the solver.
      */
    override def numThreads(): Int = 1

    /**
      * If true, the solver will automatically add the zero value to each flow-function call's result set.
      *
      * @see #zeroValue()
      */
    override def autoAddZero(): Boolean =  true

    /**
      * If true, then the solver will cache flow functions and edge functions.
      */
    override def cacheFlowFunctions(): Boolean = false
}

