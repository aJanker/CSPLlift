package spllift;

import de.fosd.typechef.cspllift.CICFGFDef;
import de.fosd.typechef.cspllift.CICFGStmt;
import de.fosd.typechef.cspllift.CInterCFG;
import de.fosd.typechef.featureexpr.FeatureModel;
import heros.*;
import heros.solver.IDESolver;

import java.util.Map;
import java.util.Set;


public class SPLIFDSSolver<D> extends IDESolver<CICFGStmt, D, CICFGFDef, Constraint, CInterCFG> {

    public SPLIFDSSolver(final IFDSTabulationProblem<CICFGStmt, D, CICFGFDef, CInterCFG> ifdsProblem, final FeatureModel fm, final boolean useFMInEdgeComputations) {
        super(new DefaultSPLIFDSTabulationProblem<CICFGStmt, D, CICFGFDef, CInterCFG>(ifdsProblem) {

            @Override
            public Map<CICFGStmt, Set<D>> initialSeeds() {
                return ifdsProblem.initialSeeds();
            }

            class IFDSEdgeFunctions implements EdgeFunctions<CICFGStmt, D, CICFGFDef, Constraint> {
                private final FlowFunctions<CICFGStmt, D, CICFGFDef> flowFunctions;
                private final CInterCFG icfg;

                private IFDSEdgeFunctions(CInterCFG icfg) {
                    this.icfg = icfg;
                    flowFunctions = new ZeroedFlowFunctions<>(ifdsProblem.flowFunctions(), ifdsProblem.zeroValue());
                }

                public EdgeFunction<Constraint> getNormalEdgeFunction(CICFGStmt currStmt, D currNode, CICFGStmt succStmt, D succNode) {
                    return buildFlowFunction(currStmt, succStmt);
                }

                public EdgeFunction<Constraint> getCallEdgeFunction(CICFGStmt callStmt, D srcNode, CICFGFDef destinationMethod, D destNode) {
                    /*
                     * Calculates the points-to presence condition and annotates the resulting edge function with the correct flow presence condition.
                     * Otherwise we would assume this flow has the presence condition of true.
                     * Further, we manipulate the presence condition of the current call statement and the corresponding destination method with the help of
                     * the icfg. The concrete analysis is now able to generate a custom zero element with current flow condition to propagate the correct
                     * flow presence condition along the normal-flow edges.
                     */
                    Constraint flow = icfg.getPointsToConstraint(callStmt, destinationMethod);
                    return buildFlowFunction(callStmt, destinationMethod, flow);
                }

                public EdgeFunction<Constraint> getReturnEdgeFunction(CICFGStmt callSite, CICFGFDef calleeMethod, CICFGStmt exitStmt, D exitNode, CICFGStmt returnSite, D retNode) {
                    /*
                     * Calculates the points-to presence condition and annotates the resulting edge function with the correct flow presence condition.
                     * Otherwise we would assume this flow has the presence condition of true.
                     */
                    Constraint flow = icfg.getPointsToConstraint(callSite, calleeMethod).and(icfg.getConstraint(callSite)).and(icfg.getConstraint(calleeMethod));
                    return buildFlowFunction(exitStmt, returnSite, flow);
                }

                public EdgeFunction<Constraint> getCallToReturnEdgeFunction(CICFGStmt callSite, D callNode, CICFGStmt returnSite, D returnSideNode) {
                    return buildFlowFunction(callSite, returnSite);
                }

                private EdgeFunction<Constraint> buildFlowFunction(CICFGStmt src, CICFGStmt successor) {
                    return buildFlowFunction(src, successor, Constraint.falseValue());
                }

                private EdgeFunction<Constraint> buildFlowFunction(CICFGStmt src, CICFGStmt successor, Constraint flow) {
                    Constraint cfgConstraint = icfg.getConstraint(src).and(icfg.getConstraint(successor));

                    if (!(Constraint.falseValue().equals(flow)))
                        cfgConstraint = cfgConstraint.and(flow);

                    return new SPLFeatureFunction(cfgConstraint, fm, useFMInEdgeComputations);
                }
            }


            @Override
            protected EdgeFunction<Constraint> createAllTopFunction() {
                return new SPLFeatureFunction(Constraint.falseValue(), fm, useFMInEdgeComputations);
            }

            @Override
            protected JoinLattice<Constraint> createJoinLattice() {
                return new JoinLattice<Constraint>() {

                    public Constraint topElement() {
                        return Constraint.falseValue();
                    }

                    public Constraint bottomElement() {
                        return Constraint.trueValue();
                    }

                    public Constraint join(Constraint left, Constraint right) {
                        return left.or(right);
                    }
                };

            }

            @Override
            protected EdgeFunctions<CICFGStmt, D, CICFGFDef, Constraint> createEdgeFunctionsFactory() {
                return new IFDSEdgeFunctions(interproceduralCFG());
            }

            @Override
            protected FlowFunctions<CICFGStmt, D, CICFGFDef> createFlowFunctionsFactory() {

                // See git history for removed wrapper code.
                // Not sure why some flow functions were wrapped into a another flowfunction without being able to kill flow facts.
                return new FlowFunctions<CICFGStmt, D, CICFGFDef>() {

                    @Override
                    public FlowFunction<D> getNormalFlowFunction(CICFGStmt curr,
                                                                 CICFGStmt succ) {
                        return ifdsProblem.flowFunctions().getNormalFlowFunction(curr, succ);
                    }

                    @Override
                    public FlowFunction<D> getCallFlowFunction(CICFGStmt callStmt,
                                                               CICFGFDef destinationMethod) {
                        return ifdsProblem.flowFunctions().getCallFlowFunction(callStmt, destinationMethod);
                    }

                    @Override
                    public FlowFunction<D> getReturnFlowFunction(CICFGStmt callSite,
                                                                 CICFGFDef calleeMethod, CICFGStmt exitStmt,
                                                                 CICFGStmt returnSite) {
                        return ifdsProblem.flowFunctions().getReturnFlowFunction(callSite, calleeMethod, exitStmt, returnSite);
                    }

                    @Override
                    public FlowFunction<D> getCallToReturnFlowFunction(
                            CICFGStmt callSite, CICFGStmt returnSite) {
                        return ifdsProblem.flowFunctions().getCallToReturnFlowFunction(callSite, returnSite);
                    }
                };
            }

            @Override
            protected D createZeroValue() {
                return ifdsProblem.zeroValue();
            }

            private boolean hasFeatureAnnotation(CICFGStmt stmt) {
                return ifdsProblem.interproceduralCFG().getConstraint(stmt).hasFeatureAnnotation();
            }

        });
    }
}