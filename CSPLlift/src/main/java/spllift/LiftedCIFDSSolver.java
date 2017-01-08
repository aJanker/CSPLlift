package spllift;

import de.fosd.typechef.cspllift.cintercfg.CInterCFG;
import de.fosd.typechef.cspllift.cintercfg.CInterCFGFDef;
import de.fosd.typechef.cspllift.cintercfg.CInterCFGNode;
import de.fosd.typechef.featureexpr.FeatureModel;
import heros.IFDSTabulationProblem;

public class LiftedCIFDSSolver<D> extends LiftedCIDESolver<D> {

    public LiftedCIFDSSolver(final IFDSTabulationProblem<CInterCFGNode, D, CInterCFGFDef, CInterCFG> ifdsProblem, final FeatureModel fm, final boolean useFMInEdgeComputations) {
        super(new DefaultCLiftedIFDSTabulationProblem<D>(ifdsProblem, fm, useFMInEdgeComputations));
    }
}