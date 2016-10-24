package spllift;

import de.fosd.typechef.cspllift.CICFGFDef;
import de.fosd.typechef.cspllift.CICFGStmt;
import de.fosd.typechef.cspllift.CInterCFG;
import de.fosd.typechef.featureexpr.FeatureExpr;
import de.fosd.typechef.featureexpr.FeatureExprFactory;
import de.fosd.typechef.featureexpr.FeatureModel;
import heros.*;
import heros.solver.IDESolver;

import java.util.Map;
import java.util.Set;


public class SPLIFDSSolver<D> extends IDESolver<CICFGStmt, D, CICFGFDef, FeatureExpr, CInterCFG> {

    public SPLIFDSSolver(final IFDSTabulationProblem<CICFGStmt, D, CICFGFDef, CInterCFG> ifdsProblem, final FeatureModel fm, final boolean useFMInEdgeComputations) {
        super(new DefaultSPLIFDSTabulationProblem<CICFGStmt, D, CICFGFDef, CInterCFG>(ifdsProblem) {
            @Override
            public Map<CICFGStmt, Set<D>> initialSeeds() {
                return ifdsProblem.initialSeeds();
            }

            class SPLIFDSEdgeFunctions implements EdgeFunctions<CICFGStmt, D, CICFGFDef, FeatureExpr> {
                private final CInterCFG icfg;

                private SPLIFDSEdgeFunctions(CInterCFG icfg) {
                    this.icfg = icfg;
                }

                public EdgeFunction<FeatureExpr> getNormalEdgeFunction(CICFGStmt currStmt, D currNode, CICFGStmt succStmt, D succNode) {
                    return buildFlowFunction(currStmt, succStmt);
                }

                public EdgeFunction<FeatureExpr> getCallEdgeFunction(CICFGStmt callStmt, D srcNode, CICFGFDef destinationMethod, D destNode) {
                    /*
                     * Calculates the points-to presence condition and annotates the resulting edge function with the correct flow presence condition.
                     * Otherwise we would assume this flow has the presence condition of true.
                     * Further, we manipulate the presence condition of the current call statement and the corresponding destination method with the help of
                     * the icfg. The concrete analysis is now able to generate a custom zero element with current flow condition to propagate the correct
                     * flow presence condition along the normal-flow edges.
                     */
                    FeatureExpr pointsToFlowCondition= icfg.getPointsToCondition(callStmt, destinationMethod);
                    return buildFlowFunction(callStmt, destinationMethod, pointsToFlowCondition);
                }

                public EdgeFunction<FeatureExpr> getReturnEdgeFunction(CICFGStmt callSite, CICFGFDef calleeMethod, CICFGStmt exitStmt, D exitNode, CICFGStmt returnSite, D retNode) {
                    /*
                     * Calculates the points-to presence condition and annotates the resulting edge function with the correct flow presence condition.
                     * Otherwise we would assume this flow has the presence condition of true.
                     */
                    FeatureExpr pointsToFlowCondition = icfg.getPointsToCondition(callSite, calleeMethod).and(icfg.getCondition(callSite)).and(icfg.getCondition(calleeMethod));
                    return buildFlowFunction(exitStmt, returnSite, pointsToFlowCondition);
                }

                public EdgeFunction<FeatureExpr> getCallToReturnEdgeFunction(CICFGStmt callSite, D callNode, CICFGStmt returnSite, D returnSideNode) {
                    return buildFlowFunction(callSite, returnSite);
                }

                private EdgeFunction<FeatureExpr> buildFlowFunction(CICFGStmt src, CICFGStmt successor) {
                    return buildFlowFunction(src, successor, null);
                }

                private EdgeFunction<FeatureExpr> buildFlowFunction(CICFGStmt src, CICFGStmt successor, FeatureExpr pointsToflowCondition) {
                    FeatureExpr cfgCondition = icfg.getFlowCondition(src, successor);

                    if (pointsToflowCondition != null)
                        cfgCondition = cfgCondition.and(pointsToflowCondition);

                    return new SPLFeatureFunction(cfgCondition, fm, useFMInEdgeComputations);
                }
            }


            @Override
            protected EdgeFunction<FeatureExpr> createAllTopFunction() {
                return new SPLFeatureFunction(FeatureExprFactory.False(), fm, useFMInEdgeComputations);
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
            protected EdgeFunctions<CICFGStmt, D, CICFGFDef, FeatureExpr> createEdgeFunctionsFactory() {
                return new SPLIFDSEdgeFunctions(interproceduralCFG());
            }

            @Override
            protected FlowFunctions<CICFGStmt, D, CICFGFDef> createFlowFunctionsFactory() {

                // See git history for removed wrapper code.
                // Not sure why some flow functions were wrapped into a another flowfunction without being able to kill flow facts.
                return new FlowFunctions<CICFGStmt, D, CICFGFDef>() {

                    @Override
                    public FlowFunction<D> getNormalFlowFunction(CICFGStmt curr,
                                                                 CICFGStmt succ) {
                        return flowFunctions.getNormalFlowFunction(curr, succ);
                    }

                    @Override
                    public FlowFunction<D> getCallFlowFunction(CICFGStmt callStmt,
                                                               CICFGFDef destinationMethod) {
                        return flowFunctions.getCallFlowFunction(callStmt, destinationMethod);
                    }

                    @Override
                    public FlowFunction<D> getReturnFlowFunction(CICFGStmt callSite,
                                                                 CICFGFDef calleeMethod, CICFGStmt exitStmt,
                                                                 CICFGStmt returnSite) {
                        return flowFunctions.getReturnFlowFunction(callSite, calleeMethod, exitStmt, returnSite);
                    }

                    @Override
                    public FlowFunction<D> getCallToReturnFlowFunction(
                            CICFGStmt callSite, CICFGStmt returnSite) {
                        return flowFunctions.getCallToReturnFlowFunction(callSite, returnSite);
                    }
                };
            }

            @Override
            protected D createZeroValue() {
                return ifdsProblem.zeroValue();
            }

            private final FlowFunctions<CICFGStmt, D, CICFGFDef> flowFunctions =
                    ifdsProblem.autoAddZero() ? new ZeroedFlowFunctions<>(ifdsProblem.flowFunctions(), ifdsProblem.zeroValue()) : ifdsProblem.flowFunctions();

        });
    }
}