package soot.spl.ifds;

import de.fosd.typechef.conditional.Opt;
import de.fosd.typechef.featureexpr.FeatureModel;
import de.fosd.typechef.parser.c.AST;
import de.fosd.typechef.parser.c.FunctionDef;
import de.fosd.typechef.spllift.CInterCFG;
import heros.*;
import heros.edgefunc.EdgeIdentity;
import heros.solver.IDESolver;
import heros.template.DefaultIDETabulationProblem;

import java.util.HashSet;
import java.util.Map;
import java.util.Set;

public class SPLIFDSSolver<D> extends IDESolver<Opt<AST>, D, Opt<FunctionDef>, Constraint, CInterCFG> {
	private static CInterCFG interCfg;

	public SPLIFDSSolver(final IFDSTabulationProblem<Opt<AST>, D, Opt<FunctionDef>, CInterCFG> ifdsProblem, final FeatureModel fm, final boolean useFMInEdgeComputations) {
		super(new DefaultIDETabulationProblem<Opt<AST>, D, Opt<FunctionDef>, Constraint, CInterCFG>(ifdsProblem.interproceduralCFG()) {

			@Override
			public Map<Opt<AST>, Set<D>> initialSeeds() {
				return ifdsProblem.initialSeeds();
			}
			class IFDSEdgeFunctions implements EdgeFunctions<Opt<AST>,D,Opt<FunctionDef>,Constraint> {
				private final FlowFunctions<Opt<AST>, D, Opt<FunctionDef>> zeroedFlowFunctions;
				private final CInterCFG icfg;

				public IFDSEdgeFunctions(CInterCFG icfg) {
					this.icfg = icfg;
					zeroedFlowFunctions = new ZeroedFlowFunctions<Opt<AST>, D, Opt<FunctionDef>>(ifdsProblem.flowFunctions(),ifdsProblem.zeroValue());
				}

				public EdgeFunction<Constraint> getNormalEdgeFunction(Opt<AST> currStmt, D currNode, Opt<AST> succStmt, D succNode) {
					return buildFlowFunction(currStmt, succStmt, currNode, succNode, zeroedFlowFunctions.getNormalFlowFunction(currStmt, succStmt), false);
				}
			
				public EdgeFunction<Constraint> getCallEdgeFunction(Opt<AST> callStmt, D srcNode, Opt<FunctionDef> destinationMethod,D destNode) {
					return buildFlowFunction(callStmt, destinationMethod.copy(destinationMethod.condition(), (AST) destinationMethod.entry()), srcNode, destNode, zeroedFlowFunctions.getCallFlowFunction(callStmt, destinationMethod), true);
				}
			
				public EdgeFunction<Constraint> getReturnEdgeFunction(Opt<AST> callSite, Opt<FunctionDef> calleeMethod, Opt<AST> exitStmt,D exitNode, Opt<AST> returnSite,D retNode) {
					return buildFlowFunction(exitStmt, returnSite, exitNode, retNode, zeroedFlowFunctions.getReturnFlowFunction(callSite, calleeMethod, exitStmt, returnSite), false);
				}
			
				public EdgeFunction<Constraint> getCallToReturnEdgeFunction(Opt<AST> callSite, D callNode, Opt<AST> returnSite, D returnSideNode) {
					return buildFlowFunction(callSite, returnSite, callNode, returnSideNode, zeroedFlowFunctions.getCallToReturnFlowFunction(callSite, returnSite), false);
				}
				private EdgeFunction<Constraint> buildFlowFunction(Opt<AST> src, Opt<AST> successor, D srcNode, D tgtNode, FlowFunction<D> originalFlowFunction, boolean isCall) {
					boolean srcAnnotated = hasFeatureAnnotation(src);
					boolean succAnnotated = hasFeatureAnnotation(successor);
					if(!srcAnnotated && !(isCall && succAnnotated)) return EdgeIdentity.v();
					// TODO Validate


					/* List<Opt<AST>> srcSuccs = interCfg.getSuccsOf(src);
					/* if (interCfg.isFallThroughSuccessor(src, successor)) {
							// (src --> successor) is a fallThroughEdge (as src has only one successor), currently, this is the only case we can handle precisely 
							return preciseBuildFlowFunction(src, successor, srcNode, tgtNode, originalFlowFunction, isCall);
					} */
					return preciseBuildFlowFunction(src, successor, srcNode, tgtNode, originalFlowFunction, isCall);
				}
				private EdgeFunction<Constraint> conservativeBuildFlowFunction(Opt<AST> src, Opt<AST> successor, D srcNode, D tgtNode, FlowFunction<D> originalFlowFunction, boolean isCall) {
					//boolean srcAnnotated = hasFeatureAnnotation(src);
					//boolean succAnnotated = hasFeatureAnnotation(successor);
					//if(!srcAnnotated && !(isCall && succAnnotated)) return EdgeIdentity.v();
					Constraint pos = originalFlowFunction.computeTargets(srcNode).contains(tgtNode) ? Constraint.<String>trueValue() : Constraint.<String>falseValue();
					Constraint neg = srcNode == tgtNode ? Constraint.<String>trueValue() : Constraint.<String>falseValue();
					Constraint lifted = pos.or(neg);
					return new SPLFeatureFunction(lifted, fm);
				}
				private EdgeFunction<Constraint> preciseBuildFlowFunction(Opt<AST> src, Opt<AST> successor, D srcNode, D tgtNode, FlowFunction<D> originalFlowFunction, boolean isCall) {
					boolean srcAnnotated = hasFeatureAnnotation(src);
					boolean succAnnotated = hasFeatureAnnotation(successor);
					//if (!srcAnnotated && !(isCall && succAnnotated)) return EdgeIdentity.v();

					Constraint features = Constraint.falseValue();
					if(srcAnnotated)
						features = features.or(icfg.getConstraint(src));
					if(isCall && succAnnotated)
						features = features.or(icfg.getConstraint(successor));
					Constraint pos = originalFlowFunction.computeTargets(srcNode).contains(tgtNode) ? features : Constraint.<String>falseValue();
					Constraint neg = srcNode == tgtNode ? features.not() : Constraint.<String>falseValue();
					Constraint lifted = pos.or(neg);
					return new SPLFeatureFunction(lifted, fm);
				}
				
							
			}


			@Override
			protected EdgeFunction<Constraint> createAllTopFunction() {
				return new SPLFeatureFunction(Constraint.falseValue(), fm);
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
			protected EdgeFunctions<Opt<AST>, D, Opt<FunctionDef>, Constraint> createEdgeFunctionsFactory() {
				return new IFDSEdgeFunctions(interproceduralCFG());
			}

			@Override
			protected FlowFunctions<Opt<AST>, D, Opt<FunctionDef>> createFlowFunctionsFactory() {
				return new FlowFunctions<Opt<AST>, D, Opt<FunctionDef>>() {

					@Override
					public FlowFunction<D> getNormalFlowFunction(Opt<AST> curr,
																 Opt<AST> succ) {
						FlowFunction<D> original = ifdsProblem.flowFunctions().getNormalFlowFunction(curr, succ);
						if(hasFeatureAnnotation(curr) && interproceduralCFG().isFallThroughSuccessor(curr, succ)) { // m*
							return new WrappedFlowFunction<D>(original);
						} else {
							return original;
						}

					}

					@Override
					public FlowFunction<D> getCallFlowFunction(Opt<AST> callStmt,
															   Opt<FunctionDef> destinationMethod) {
						return ifdsProblem.flowFunctions().getCallFlowFunction(callStmt, destinationMethod);
					}

					@Override
					public FlowFunction<D> getReturnFlowFunction(Opt<AST> callSite,
																 Opt<FunctionDef> calleeMethod, Opt<AST> exitStmt,
																 Opt<AST> returnSite) {
						return ifdsProblem.flowFunctions().getReturnFlowFunction(callSite, calleeMethod, exitStmt, returnSite);
					}

					@Override
					public FlowFunction<D> getCallToReturnFlowFunction(
							Opt<AST> callSite, Opt<AST> returnSite) {
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
			
		});
		interCfg = ifdsProblem.interproceduralCFG();
	}
	
	public static final boolean hasFeatureAnnotation(Opt<AST> stmt) {
		return interCfg.getConstraint(stmt).hasFeatureAnnotation();
	}
	static class WrappedFlowFunction<D> implements FlowFunction<D> {
		
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