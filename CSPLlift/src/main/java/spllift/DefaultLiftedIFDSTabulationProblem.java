package spllift;

import de.fosd.typechef.featureexpr.FeatureExpr;
import heros.IFDSTabulationProblem;
import heros.InterproceduralCFG;
import heros.solver.IDESolver;
import heros.template.DefaultIDETabulationProblem;

abstract class DefaultLiftedIFDSTabulationProblem<N,D,M, I extends InterproceduralCFG<N,M>> extends DefaultIDETabulationProblem<N, D, M, FeatureExpr, I> {
    private final IFDSTabulationProblem<N, D, M, I> problem;

    @SuppressWarnings("unchecked")
    DefaultLiftedIFDSTabulationProblem(final IFDSTabulationProblem<N, D, M, I> ifdsProblem) {
        super(ifdsProblem.interproceduralCFG());
        this.problem = ifdsProblem;
    }

    /**
     * If true, the analysis will compute a partially unbalanced analysis problem in which
     * function returns are followed also further up the call stack than where the initial seeds
     * started.
     *
     * If this is enabled, when reaching the exit of a method that is <i>nowhere</i> called, in order
     * to avoid not at all processing the exit statement, the {@link IDESolver} will call
     * the <i>return</i> flow function with a <code>null</code> call site and return site.
     */
    @Override
    public boolean followReturnsPastSeeds() {
        return this.problem.followReturnsPastSeeds();
    }

    /**
     * If true, the solver will automatically add the zero value to each flow-function call's result set.
     * @see #zeroValue()
     */
    @Override
    public boolean autoAddZero() {
        return this.problem.autoAddZero();
    }

    /**
     * Returns the number of threads to be used by the solver.
     */
    @Override
    public int numThreads() {
        return this.problem.numThreads();
    }

    /**
     * If false, then the solver will only compute the exploded super graph but not propagate values.
     * This can save time for IFDS problems where all of the interesting results are collected already
     * during the computation of the super graph.
     */
    @Override
    public boolean computeValues() {
        return this.problem.computeValues();
    }

    /**
     * If true, then the solver will cache flow functions and edge functions.
     */
    @Override
    public boolean cacheFlowFunctions() {
        return this.problem.cacheFlowFunctions();
    }
}
