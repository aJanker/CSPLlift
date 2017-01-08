package spllift;

import de.fosd.typechef.cspllift.cintercfg.CInterCFG;
import de.fosd.typechef.cspllift.cintercfg.CInterCFGFDef;
import de.fosd.typechef.cspllift.cintercfg.CInterCFGNode;
import de.fosd.typechef.featureexpr.FeatureExpr;
import heros.EdgeFunction;
import heros.IDETabulationProblem;
import heros.edgefunc.EdgeIdentity;
import heros.solver.IDESolver;

import java.util.Map;
import java.util.Set;

public class LiftedCIDESolver<D> extends IDESolver<CInterCFGNode, D, CInterCFGFDef, FeatureExpr, CInterCFG> {

    /**
     * Creates a solver for the given problem, which caches flow functions and edge functions.
     * The solver must then be started by calling {@link #solve()}.
     *
     * @param tabulationProblem
     */
    public LiftedCIDESolver(final IDETabulationProblem<CInterCFGNode, D, CInterCFGFDef, FeatureExpr, CInterCFG> tabulationProblem) {
        super(tabulationProblem);
    }

    /**
     * Schedules the processing of initial seeds, initiating the analysis.
     * Clients should only call this method if performing synchronization on
     * their own. Normally, {@link #solve()} should be called instead.
     */
    protected void submitInitialSeeds() {
        for (final Map.Entry<CInterCFGNode, Set<D>> seed : initialSeeds.entrySet()) {
            final CInterCFGNode startPoint = seed.getKey();

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
    private EdgeFunction<FeatureExpr> getInitialSeedEdge(final CInterCFGNode startPoint, final D seedFact) {
        final EdgeFunction<FeatureExpr> startEdge = icfg.getConditionalEdgeFunction(startPoint);
        // TODO Fix
        //final EdgeFunction<FeatureExpr> seedEdge = (seedFact instanceof Source) ?
        //        icfg.getConditionalEdgeFunction(((Source) seedFact).getCIFGStmt()) : EdgeIdentity.<FeatureExpr>v();

        return startEdge.composeWith(EdgeIdentity.<FeatureExpr>v());
    }
}
