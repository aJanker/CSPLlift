package spllift;

import de.fosd.typechef.cspllift.cifdsproblem.informationflow.flowfact.sinkorsource.Source;
import de.fosd.typechef.cspllift.cintercfg.CICFGFDef;
import de.fosd.typechef.cspllift.cintercfg.CICFGNode;
import de.fosd.typechef.cspllift.cintercfg.CInterCFG;
import de.fosd.typechef.featureexpr.FeatureExpr;
import heros.EdgeFunction;
import heros.IDETabulationProblem;
import heros.edgefunc.EdgeIdentity;
import heros.solver.IDESolver;

import java.util.Map;
import java.util.Set;

public class LiftedIDESolver<D> extends IDESolver<CICFGNode, D, CICFGFDef, FeatureExpr, CInterCFG>  {

    /**
     * Creates a solver for the given problem, which caches flow functions and edge functions.
     * The solver must then be started by calling {@link #solve()}.
     *
     * @param tabulationProblem
     */
    public LiftedIDESolver(final IDETabulationProblem<CICFGNode, D, CICFGFDef, FeatureExpr, CInterCFG> tabulationProblem) {
        super(tabulationProblem);
    }

    /**
     * Schedules the processing of initial seeds, initiating the analysis.
     * Clients should only call this methods if performing synchronization on
     * their own. Normally, {@link #solve()} should be called instead.
     */
    protected void submitInitialSeeds() {
        for (final Map.Entry<CICFGNode, Set<D>> seed : initialSeeds.entrySet()) {
            final CICFGNode startPoint = seed.getKey();

            for (D val : seed.getValue()) {
                propagate(zeroValue, startPoint, val, getInitialSeedEdge(startPoint ,val), null, false);
            }

            jumpFn.addFunction(zeroValue, startPoint, zeroValue, getInitialSeedEdge(startPoint, zeroValue));
        }
    }

    /**
     * Retrieves for lifting based IFDS problems the correct flow-condition of an initial seed value.
     * Otherwise we would assume an inital fact is always true.
     */
    private EdgeFunction<FeatureExpr> getInitialSeedEdge(final CICFGNode startPoint, final D seedFact) {
        final EdgeFunction<FeatureExpr> startEdge = icfg.getConditionalEdgeFunction(startPoint);
        final EdgeFunction<FeatureExpr> seedEdge = (seedFact instanceof Source) ?
                icfg.getConditionalEdgeFunction(((Source) seedFact).getCIFGStmt()) : EdgeIdentity.<FeatureExpr>v();

        return startEdge.composeWith(seedEdge);
    }
}
