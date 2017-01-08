package spllift;

import de.fosd.typechef.cspllift.cintercfg.CInterCFG;
import de.fosd.typechef.cspllift.cintercfg.CInterCFGFDef;
import de.fosd.typechef.cspllift.cintercfg.CInterCFGNode;
import de.fosd.typechef.featureexpr.FeatureExpr;
import de.fosd.typechef.featureexpr.FeatureExprFactory;
import de.fosd.typechef.featureexpr.FeatureModel;
import heros.*;

import java.util.Map;
import java.util.Set;

public class DefaultCLiftedIFDSTabulationProblem<D> extends DefaultLiftedIFDSTabulationProblem<CInterCFGNode, D, CInterCFGFDef, CInterCFG> {

    private final IFDSTabulationProblem<CInterCFGNode, D, CInterCFGFDef, CInterCFG> ifdsProblem;
    private final FeatureModel fm;
    private final boolean useFMInEdgeComputations;
    private final FlowFunctions<CInterCFGNode, D, CInterCFGFDef> flowFunctions;


    /**
     * Default formulation of a lifted IFDS problem as IDE for the C programming language using TypeChef as provider of th CFG.
     *
     * @param ifdsProblem
     * @param fm
     * @param useFMInEdgeComputations
     */
    public DefaultCLiftedIFDSTabulationProblem(final IFDSTabulationProblem<CInterCFGNode, D, CInterCFGFDef, CInterCFG> ifdsProblem, final FeatureModel fm, final boolean useFMInEdgeComputations) {
        super(ifdsProblem);
        this.ifdsProblem = ifdsProblem;
        this.fm = fm;
        this.useFMInEdgeComputations = useFMInEdgeComputations;
        this.flowFunctions = this.ifdsProblem.flowFunctions();
    }

    @Override
    public Map<CInterCFGNode, Set<D>> initialSeeds() {
        return ifdsProblem.initialSeeds();
    }

    class ConditionalIFDSEdgeFunctions implements EdgeFunctions<CInterCFGNode, D, CInterCFGFDef, FeatureExpr> {
        private final CInterCFG icfg;

        private ConditionalIFDSEdgeFunctions(CInterCFG icfg) {
            this.icfg = icfg;
        }

        public EdgeFunction<FeatureExpr> getNormalEdgeFunction(CInterCFGNode currStmt, D srcFact, CInterCFGNode succStmt, D succFact) {
            return conditionalFlowFunction(currStmt, succStmt);
        }

        public EdgeFunction<FeatureExpr> getCallEdgeFunction(CInterCFGNode callStmt, D srcFact, CInterCFGFDef destinationMethod, D destFact) {
                    /*
                     * Calculates the points-to presence condition and annotates the resulting edge function with the correct flow presence condition.
                     * Otherwise we would assume this flow has the presence condition of true.
                     * Further, we manipulate the presence condition of the current call statement and the corresponding destination method with the help of
                     * the icfg. The concrete analysis is now able to generate a custom zero element with current flow condition to propagate the correct
                     * flow presence condition along the normal-flow edges.
                     */
            FeatureExpr pointsToFlowCondition = icfg.getPointsToCondition(callStmt, destinationMethod);
            return conditionalFlowFunction(callStmt, destinationMethod, pointsToFlowCondition);
        }

        public EdgeFunction<FeatureExpr> getReturnEdgeFunction(CInterCFGNode callSite, CInterCFGFDef calleeMethod, CInterCFGNode exitStmt, D srcFact, CInterCFGNode returnSite, D succFact) {
                    /*
                     * Calculates the points-to presence condition and annotates the resulting edge function with the correct flow presence condition.
                     * Otherwise we would assume this flow has the presence condition of true.
                     */
            FeatureExpr pointsToFlowCondition = icfg.getPointsToCondition(callSite, calleeMethod).and(icfg.getCondition(callSite)).and(icfg.getCondition(calleeMethod));
            return conditionalFlowFunction(callSite, returnSite, pointsToFlowCondition);
        }

        public EdgeFunction<FeatureExpr> getCallToReturnEdgeFunction(CInterCFGNode callSite, D srcFact, CInterCFGNode returnSite, D succFact) {
            return conditionalFlowFunction(callSite, returnSite);
        }

        private EdgeFunction<FeatureExpr> conditionalFlowFunction(CInterCFGNode src, CInterCFGNode successor) {
            return conditionalFlowFunction(src, successor, null);
        }

        private EdgeFunction<FeatureExpr> conditionalFlowFunction(CInterCFGNode src, CInterCFGNode successor, FeatureExpr pointsToFlowCondition) {
            FeatureExpr cfgCondition = icfg.getSuccFlowCondition(src, successor);

            if (pointsToFlowCondition != null)
                cfgCondition = cfgCondition.and(pointsToFlowCondition);

            return new ConditionalEdgeFunction(cfgCondition, fm, useFMInEdgeComputations);
        }
    }


    @Override
    protected EdgeFunction<FeatureExpr> createAllTopFunction() {
        return new ConditionalEdgeFunction(FeatureExprFactory.False(), fm, useFMInEdgeComputations);
    }

    @Override
    protected JoinLattice<FeatureExpr> createJoinLattice() {
        return new JoinLattice<FeatureExpr>() {

            public FeatureExpr topElement() {
                return FeatureExprFactory.False();
            }

            public FeatureExpr bottomElement() {
                return FeatureExprFactory.True();
            }

            public FeatureExpr join(FeatureExpr left, FeatureExpr right) {
                return left.or(right);
            }
        };

    }

    @Override
    protected EdgeFunctions<CInterCFGNode, D, CInterCFGFDef, FeatureExpr> createEdgeFunctionsFactory() {
        return new ConditionalIFDSEdgeFunctions(interproceduralCFG());
    }

    @Override
    protected FlowFunctions<CInterCFGNode, D, CInterCFGFDef> createFlowFunctionsFactory() {
        return new FlowFunctions<CInterCFGNode, D, CInterCFGFDef>() {

            @Override
            public FlowFunction<D> getNormalFlowFunction(CInterCFGNode curr,
                                                         CInterCFGNode succ) {
                return flowFunctions.getNormalFlowFunction(curr, succ);
            }

            @Override
            public FlowFunction<D> getCallFlowFunction(CInterCFGNode callStmt,
                                                       CInterCFGFDef destinationMethod) {
                return flowFunctions.getCallFlowFunction(callStmt, destinationMethod);
            }

            @Override
            public FlowFunction<D> getReturnFlowFunction(CInterCFGNode callSite,
                                                         CInterCFGFDef calleeMethod, CInterCFGNode exitStmt,
                                                         CInterCFGNode returnSite) {
                return flowFunctions.getReturnFlowFunction(callSite, calleeMethod, exitStmt, returnSite);
            }

            @Override
            public FlowFunction<D> getCallToReturnFlowFunction(
                    CInterCFGNode callSite, CInterCFGNode returnSite) {
                return flowFunctions.getCallToReturnFlowFunction(callSite, returnSite);
            }
        };
    }

    @Override
    protected D createZeroValue() {
        return ifdsProblem.zeroValue();
    }
}
