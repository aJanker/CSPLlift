package spllift;

import de.fosd.typechef.cspllift.cintercfg.CICFGFDef;
import de.fosd.typechef.cspllift.cintercfg.CICFGNode;
import de.fosd.typechef.cspllift.cintercfg.CInterCFG;
import de.fosd.typechef.featureexpr.FeatureExpr;
import de.fosd.typechef.featureexpr.FeatureExprFactory;
import de.fosd.typechef.featureexpr.FeatureModel;
import heros.*;

import java.util.Map;
import java.util.Set;


public class LiftedIFDSSolver<D> extends LiftedIDESolver<D> {

    public LiftedIFDSSolver(final IFDSTabulationProblem<CICFGNode, D, CICFGFDef, CInterCFG> ifdsProblem, final FeatureModel fm, final boolean useFMInEdgeComputations) {
        super(new DefaultLiftedIFDSTabulationProblem<CICFGNode, D, CICFGFDef, CInterCFG>(ifdsProblem) {
            @Override
            public Map<CICFGNode, Set<D>> initialSeeds() {
                return ifdsProblem.initialSeeds();
            }

            class ConditionalIFDSEdgeFunctions implements EdgeFunctions<CICFGNode, D, CICFGFDef, FeatureExpr> {
                private final CInterCFG icfg;

                private ConditionalIFDSEdgeFunctions(CInterCFG icfg) {
                    this.icfg = icfg;
                }

                public EdgeFunction<FeatureExpr> getNormalEdgeFunction(CICFGNode currStmt, D srcFact, CICFGNode succStmt, D succFact) {
                    return conditionalFlowFunction(currStmt, succStmt);
                }

                public EdgeFunction<FeatureExpr> getCallEdgeFunction(CICFGNode callStmt, D srcFact, CICFGFDef destinationMethod, D destFact) {
                    /*
                     * Calculates the points-to presence condition and annotates the resulting edge function with the correct flow presence condition.
                     * Otherwise we would assume this flow has the presence condition of true.
                     * Further, we manipulate the presence condition of the current call statement and the corresponding destination method with the help of
                     * the icfg. The concrete analysis is now able to generate a custom zero element with current flow condition to propagate the correct
                     * flow presence condition along the normal-flow edges.
                     */
                    FeatureExpr pointsToFlowCondition= icfg.getPointsToCondition(callStmt, destinationMethod);
                    return conditionalFlowFunction(callStmt, destinationMethod, pointsToFlowCondition);
                }

                public EdgeFunction<FeatureExpr> getReturnEdgeFunction(CICFGNode callSite, CICFGFDef calleeMethod, CICFGNode exitStmt, D srcFact, CICFGNode returnSite, D succFact) {
                    /*
                     * Calculates the points-to presence condition and annotates the resulting edge function with the correct flow presence condition.
                     * Otherwise we would assume this flow has the presence condition of true.
                     */
                    FeatureExpr pointsToFlowCondition = icfg.getPointsToCondition(callSite, calleeMethod).and(icfg.getCondition(callSite)).and(icfg.getCondition(calleeMethod));
                    return conditionalFlowFunction(exitStmt, returnSite, pointsToFlowCondition);
                }

                public EdgeFunction<FeatureExpr> getCallToReturnEdgeFunction(CICFGNode callSite, D srcFact, CICFGNode returnSite, D succFact) {
                    return conditionalFlowFunction(callSite, returnSite);
                }

                private EdgeFunction<FeatureExpr> conditionalFlowFunction(CICFGNode src, CICFGNode successor) {
                    return conditionalFlowFunction(src, successor, null);
                }

                private EdgeFunction<FeatureExpr> conditionalFlowFunction(CICFGNode src, CICFGNode successor, FeatureExpr pointsToFlowCondition) {
                    FeatureExpr cfgCondition = icfg.getFlowCondition(src, src);

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
            protected EdgeFunctions<CICFGNode, D, CICFGFDef, FeatureExpr> createEdgeFunctionsFactory() {
                return new ConditionalIFDSEdgeFunctions(interproceduralCFG());
            }

            @Override
            protected FlowFunctions<CICFGNode, D, CICFGFDef> createFlowFunctionsFactory() {
                return new FlowFunctions<CICFGNode, D, CICFGFDef>() {

                    @Override
                    public FlowFunction<D> getNormalFlowFunction(CICFGNode curr,
                                                                 CICFGNode succ) {
                        return flowFunctions.getNormalFlowFunction(curr, succ);
                    }

                    @Override
                    public FlowFunction<D> getCallFlowFunction(CICFGNode callStmt,
                                                               CICFGFDef destinationMethod) {
                        return flowFunctions.getCallFlowFunction(callStmt, destinationMethod);
                    }

                    @Override
                    public FlowFunction<D> getReturnFlowFunction(CICFGNode callSite,
                                                                 CICFGFDef calleeMethod, CICFGNode exitStmt,
                                                                 CICFGNode returnSite) {
                        return flowFunctions.getReturnFlowFunction(callSite, calleeMethod, exitStmt, returnSite);
                    }

                    @Override
                    public FlowFunction<D> getCallToReturnFlowFunction(
                            CICFGNode callSite, CICFGNode returnSite) {
                        return flowFunctions.getCallToReturnFlowFunction(callSite, returnSite);
                    }
                };
            }

            @Override
            protected D createZeroValue() {
                return ifdsProblem.zeroValue();
            }

            private final FlowFunctions<CICFGNode, D, CICFGFDef> flowFunctions = ifdsProblem.flowFunctions();

        });
    }
}