package spllift;

import de.fosd.typechef.cspllift.CICFGConcreteStmt;
import de.fosd.typechef.cspllift.CICFGFDef;
import de.fosd.typechef.cspllift.CICFGStmt;
import de.fosd.typechef.cspllift.CInterCFG;
import de.fosd.typechef.featureexpr.FeatureModel;
import de.fosd.typechef.parser.c.AST;
import heros.*;
import heros.edgefunc.EdgeIdentity;
import heros.solver.IDESolver;

import java.util.HashSet;
import java.util.Map;
import java.util.Set;


public class SPLIFDSSolver<D> extends IDESolver<CICFGStmt<AST>, D, CICFGFDef, Constraint, CInterCFG> {

    public SPLIFDSSolver(final IFDSTabulationProblem<CICFGStmt<AST>, D, CICFGFDef, CInterCFG> ifdsProblem, final FeatureModel fm, final boolean useFMInEdgeComputations) {
        super(new DefaultSPLIFDSTabulationProblem<CICFGStmt<AST>, D, CICFGFDef, CInterCFG>(ifdsProblem) {

            @Override
            public Map<CICFGStmt<AST>, Set<D>> initialSeeds() {
                return ifdsProblem.initialSeeds();
            }

            class IFDSEdgeFunctions implements EdgeFunctions<CICFGStmt<AST>, D, CICFGFDef, Constraint> {
                private final FlowFunctions<CICFGStmt<AST>, D, CICFGFDef> zeroedFlowFunctions;
                private final CInterCFG icfg;

                private IFDSEdgeFunctions(CInterCFG icfg) {
                    this.icfg = icfg;
                    zeroedFlowFunctions = new ZeroedFlowFunctions<>(ifdsProblem.flowFunctions(), ifdsProblem.zeroValue());
                }

                public EdgeFunction<Constraint> getNormalEdgeFunction(CICFGStmt<AST> currStmt, D currNode, CICFGStmt<AST> succStmt, D succNode) {
                    return buildFlowFunction(currStmt, succStmt, currNode, succNode, zeroedFlowFunctions.getNormalFlowFunction(currStmt, succStmt), false);
                }

                public EdgeFunction<Constraint> getCallEdgeFunction(CICFGStmt<AST> callStmt, D srcNode, CICFGFDef destinationMethod, D destNode) {
                    /*
                     * Calculates the points-to presence condition and annotates the resulting edge function with the correct flow presence condition.
                     * Otherwise we would assume this flow has the presence condition of true.
                     * Further, we manipulate the presence condition of the current call statement and the corresponding destination method with the help of
                     * the icfg. The concrete analysis is now able to generate a custom zero element with current flow condition to propagate the correct
                     * flow presence condition along the normal-flow edges.
                     */
                    ;
                    CICFGStmt<AST> liftedCallStmt = new CICFGConcreteStmt(callStmt.getStmt().copy(callStmt.getStmt().condition().and(destinationMethod.getStmt().condition()), callStmt.getStmt().entry()), callStmt.getPosition());
                    CICFGStmt<AST> liftedDestinationMethod = icfg.getLiftedMethodOf(callStmt, destinationMethod);

                    Constraint flow = icfg.getConstraint(liftedDestinationMethod).and(icfg.getConstraint(liftedCallStmt));

                    return buildFlowFunction(liftedCallStmt, liftedDestinationMethod, srcNode, destNode, zeroedFlowFunctions.getCallFlowFunction(liftedCallStmt, destinationMethod), true, flow);
                }

                public EdgeFunction<Constraint> getReturnEdgeFunction(CICFGStmt<AST> callSite, CICFGFDef calleeMethod, CICFGStmt<AST> exitStmt, D exitNode, CICFGStmt<AST> returnSite, D retNode) {
                    /*
                     * Calculates the points-to presence condition and annotates the resulting edge function with the correct flow presence condition.
                     * Otherwise we would assume this flow has the presence condition of true.
                     */
                    CICFGFDef liftedCalleeMethod = icfg.getLiftedMethodOf(callSite, calleeMethod);
                    Constraint flow = icfg.getConstraint(liftedCalleeMethod).and(icfg.getConstraint(exitStmt));
                    return buildFlowFunction(exitStmt, returnSite, exitNode, retNode, zeroedFlowFunctions.getReturnFlowFunction(callSite, liftedCalleeMethod, exitStmt, returnSite), true, flow);
                }

                public EdgeFunction<Constraint> getCallToReturnEdgeFunction(CICFGStmt<AST> callSite, D callNode, CICFGStmt<AST> returnSite, D returnSideNode) {
                    return buildFlowFunction(callSite, returnSite, callNode, returnSideNode, zeroedFlowFunctions.getCallToReturnFlowFunction(callSite, returnSite), false);
                }

                private EdgeFunction<Constraint> buildFlowFunction(CICFGStmt<AST> src, CICFGStmt<AST> successor, D srcNode, D tgtNode, FlowFunction<D> originalFlowFunction, boolean isCall) {
                    return buildFlowFunction(src, successor, srcNode, tgtNode, originalFlowFunction, isCall, Constraint.falseValue());
                }

                private EdgeFunction<Constraint> buildFlowFunction(CICFGStmt<AST> src, CICFGStmt<AST> successor, D srcNode, D tgtNode, FlowFunction<D> originalFlowFunction, boolean isCall, Constraint flow) {
                    if (flow.equals(Constraint.trueValue())) return EdgeIdentity.v();

                    boolean srcAnnotated = hasFeatureAnnotation(src);
                    boolean succAnnotated = hasFeatureAnnotation(successor);

                    if (!srcAnnotated && !(isCall && succAnnotated)) return EdgeIdentity.v();

                    return preciseBuildFlowFunction(src, successor, srcNode, tgtNode, originalFlowFunction, isCall, flow);
                }

                private EdgeFunction<Constraint> preciseBuildFlowFunction(CICFGStmt<AST> src, CICFGStmt<AST> successor, D srcNode, D tgtNode, FlowFunction<D> originalFlowFunction, boolean isCall, Constraint flow) {
                    boolean srcAnnotated = hasFeatureAnnotation(src);
                    boolean succAnnotated = hasFeatureAnnotation(successor);

                    Constraint features = Constraint.falseValue();

                    if (srcAnnotated)
                        features = features.or(icfg.getConstraint(src));
                    if (isCall && succAnnotated)
                        features = features.or(icfg.getConstraint(successor));
                    if (!(flow.equals(Constraint.falseValue()) || flow.equals(Constraint.trueValue())))
                        features = features.equals(Constraint.falseValue()) ? features.or(flow) : features.and(flow);

                    Constraint pos = originalFlowFunction.computeTargets(srcNode).contains(tgtNode) ? features : Constraint.falseValue();
                    Constraint neg = srcNode == tgtNode ? features.not() : Constraint.falseValue();
                    Constraint lifted = pos.or(neg);

                    return new SPLFeatureFunction(lifted, fm, useFMInEdgeComputations);
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
            protected EdgeFunctions<CICFGStmt<AST>, D, CICFGFDef, Constraint> createEdgeFunctionsFactory() {
                return new IFDSEdgeFunctions(interproceduralCFG());
            }

            @Override
            protected FlowFunctions<CICFGStmt<AST>, D, CICFGFDef> createFlowFunctionsFactory() {

                // See git history for removed wrapper code.
                // Not sure why some flow functions were wrapped into a another flowfunction without being able to kill flow facts.
                return new FlowFunctions<CICFGStmt<AST>, D, CICFGFDef>() {

                    @Override
                    public FlowFunction<D> getNormalFlowFunction(CICFGStmt<AST> curr,
                                                                 CICFGStmt<AST> succ) {
                        return ifdsProblem.flowFunctions().getNormalFlowFunction(curr, succ);
                    }

                    @Override
                    public FlowFunction<D> getCallFlowFunction(CICFGStmt<AST> callStmt,
                                                               CICFGFDef destinationMethod) {
                        return ifdsProblem.flowFunctions().getCallFlowFunction(callStmt, destinationMethod);
                    }

                    @Override
                    public FlowFunction<D> getReturnFlowFunction(CICFGStmt<AST> callSite,
                                                                 CICFGFDef calleeMethod, CICFGStmt<AST> exitStmt,
                                                                 CICFGStmt<AST> returnSite) {
                        return ifdsProblem.flowFunctions().getReturnFlowFunction(callSite, calleeMethod, exitStmt, returnSite);
                    }

                    @Override
                    public FlowFunction<D> getCallToReturnFlowFunction(
                            CICFGStmt<AST> callSite, CICFGStmt<AST> returnSite) {
                        FlowFunction<D> original = ifdsProblem.flowFunctions().getCallToReturnFlowFunction(callSite, returnSite);
                        if(hasFeatureAnnotation(callSite)) {
                            return new WrappedFlowFunction<D>(original);
                        } else {
                            return original;
                        }
                    }
                };
            }

            @Override
            protected D createZeroValue() {
                return ifdsProblem.zeroValue();
            }

            private boolean hasFeatureAnnotation(CICFGStmt<AST> stmt) {
                return ifdsProblem.interproceduralCFG().getConstraint(stmt).hasFeatureAnnotation();
            }

        });
    }

    private static class WrappedFlowFunction<D> implements FlowFunction<D> {

        private FlowFunction<D> del;

        private WrappedFlowFunction(FlowFunction<D> del) {
            this.del = del;
        }

        @Override
        public Set<D> computeTargets(D source) {
            Set<D> targets = new HashSet<D>(del.computeTargets(source));
            targets.add(source);
            return targets;
        }
    }
}