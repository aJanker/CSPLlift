package spllift;

import de.fosd.typechef.cspllift.CICFGConcreteStmt;
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
                    return buildFlowFunction(currStmt, succStmt, currNode, succNode, flowFunctions.getNormalFlowFunction(currStmt, succStmt), false);
                }

                public EdgeFunction<Constraint> getCallEdgeFunction(CICFGStmt callStmt, D srcNode, CICFGFDef destinationMethod, D destNode) {
                    /*
                     * Calculates the points-to presence condition and annotates the resulting edge function with the correct flow presence condition.
                     * Otherwise we would assume this flow has the presence condition of true.
                     * Further, we manipulate the presence condition of the current call statement and the corresponding destination method with the help of
                     * the icfg. The concrete analysis is now able to generate a custom zero element with current flow condition to propagate the correct
                     * flow presence condition along the normal-flow edges.
                     */
                    ;
                    CICFGStmt liftedCallStmt = new CICFGConcreteStmt(callStmt.getStmt().copy(callStmt.getStmt().condition().and(destinationMethod.getStmt().condition()), callStmt.getStmt().entry()), callStmt.getPosition());
                    CICFGStmt liftedDestinationMethod = icfg.getLiftedMethodOf(callStmt, destinationMethod);

                    Constraint flow = icfg.getConstraint(liftedDestinationMethod).and(icfg.getConstraint(liftedCallStmt));

                    return buildFlowFunction(liftedCallStmt, liftedDestinationMethod, srcNode, destNode, flowFunctions.getCallFlowFunction(liftedCallStmt, destinationMethod), true, flow);
                }

                public EdgeFunction<Constraint> getReturnEdgeFunction(CICFGStmt callSite, CICFGFDef calleeMethod, CICFGStmt exitStmt, D exitNode, CICFGStmt returnSite, D retNode) {
                    /*
                     * Calculates the points-to presence condition and annotates the resulting edge function with the correct flow presence condition.
                     * Otherwise we would assume this flow has the presence condition of true.
                     */
                    CICFGFDef liftedCalleeMethod = icfg.getLiftedMethodOf(callSite, calleeMethod);
                    Constraint flow = icfg.getConstraint(liftedCalleeMethod).and(icfg.getConstraint(exitStmt));
                    return buildFlowFunction(exitStmt, returnSite, exitNode, retNode, flowFunctions.getReturnFlowFunction(callSite, liftedCalleeMethod, exitStmt, returnSite), true, flow);
                }

                public EdgeFunction<Constraint> getCallToReturnEdgeFunction(CICFGStmt callSite, D callNode, CICFGStmt returnSite, D returnSideNode) {
                    return buildFlowFunction(callSite, returnSite, callNode, returnSideNode, flowFunctions.getCallToReturnFlowFunction(callSite, returnSite), false);
                }

                private EdgeFunction<Constraint> buildFlowFunction(CICFGStmt src, CICFGStmt successor, D srcNode, D tgtNode, FlowFunction<D> originalFlowFunction, boolean isCall) {
                    return buildFlowFunction(src, successor, srcNode, tgtNode, originalFlowFunction, isCall, Constraint.falseValue());
                }

                private EdgeFunction<Constraint> buildFlowFunction(CICFGStmt src, CICFGStmt successor, D srcNode, D tgtNode, FlowFunction<D> originalFlowFunction, boolean isCall, Constraint flow) {
                    /*if (flow.equals(Constraint.trueValue())) return EdgeIdentity.v();

                    boolean srcAnnotated = hasFeatureAnnotation(src);
                    boolean succAnnotated = hasFeatureAnnotation(successor);

                    if (!srcAnnotated && !(isCall && succAnnotated)) return EdgeIdentity.v(); */

                    return preciseBuildFlowFunction(src, successor, srcNode, tgtNode, originalFlowFunction, isCall, flow);
                }

                private EdgeFunction<Constraint> preciseBuildFlowFunction(CICFGStmt src, CICFGStmt successor, D srcNode, D tgtNode, FlowFunction<D> originalFlowFunction, boolean isCall, Constraint flow) {
                    boolean srcAnnotated = hasFeatureAnnotation(src);
                    boolean succAnnotated = hasFeatureAnnotation(successor);

                    Constraint features = icfg.getConstraint(src);

                    if (isCall && succAnnotated)
                        features = features.and(icfg.getConstraint(successor));
                    if (!(flow.equals(Constraint.falseValue()) || flow.equals(Constraint.trueValue())))
                        features = features.and(flow);

                    return new SPLFeatureFunction(features, fm, useFMInEdgeComputations);
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