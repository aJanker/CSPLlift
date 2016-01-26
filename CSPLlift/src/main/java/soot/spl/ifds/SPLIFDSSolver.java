package soot.spl.ifds;

import de.ecspride.cide.CICFG;
import de.fosd.typechef.parser.c.AST;
import de.fosd.typechef.parser.c.FunctionDef;
import heros.*;
import heros.edgefunc.EdgeIdentity;
import heros.solver.IDESolver;
import heros.template.DefaultIDETabulationProblem;

import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class SPLIFDSSolver <D> extends IDESolver<AST,D,FunctionDef, Constraint<String>,CICFG> {
	private static CICFG interCfg;
	public SPLIFDSSolver(final IFDSTabulationProblem<AST, D, FunctionDef, CICFG> ifdsProblem, final FeatureModelContext fmContext, final boolean useFMInEdgeComputations) {
		super(new DefaultIDETabulationProblem<AST,D,FunctionDef,Constraint<String>,CICFG>(ifdsProblem.interproceduralCFG()) {

			@Override
			public Map<AST, Set<D>> initialSeeds() {
				return ifdsProblem.initialSeeds();
			}
			class IFDSEdgeFunctions implements EdgeFunctions<AST,D,FunctionDef,Constraint<String>> {
				private final FlowFunctions<AST, D, FunctionDef> zeroedFlowFunctions;
				private final CICFG icfg;
				
				public IFDSEdgeFunctions(CICFG icfg) {
					this.icfg = icfg;
					zeroedFlowFunctions = new ZeroedFlowFunctions<AST, D, FunctionDef>(ifdsProblem.flowFunctions(),ifdsProblem.zeroValue());
				}

				public EdgeFunction<Constraint<String>> getNormalEdgeFunction(AST currStmt, D currNode, AST succStmt, D succNode) {
					return buildFlowFunction(currStmt, succStmt, currNode, succNode, zeroedFlowFunctions.getNormalFlowFunction(currStmt, succStmt), false);
				}
			
				public EdgeFunction<Constraint<String>> getCallEdgeFunction(AST callStmt, D srcNode, FunctionDef destinationMethod,D destNode) {
					return buildFlowFunction(callStmt, destinationMethod, srcNode, destNode, zeroedFlowFunctions.getCallFlowFunction(callStmt, destinationMethod), true);
				}
			
				public EdgeFunction<Constraint<String>> getReturnEdgeFunction(AST callSite, FunctionDef calleeMethod, AST exitStmt,D exitNode, AST returnSite,D retNode) {
					return buildFlowFunction(exitStmt, returnSite, exitNode, retNode, zeroedFlowFunctions.getReturnFlowFunction(callSite, calleeMethod, exitStmt, returnSite), false);
				}
			
				public EdgeFunction<Constraint<String>> getCallToReturnEdgeFunction(AST callSite, D callNode, AST returnSite, D returnSideNode) {
					return buildFlowFunction(callSite, returnSite, callNode, returnSideNode, zeroedFlowFunctions.getCallToReturnFlowFunction(callSite, returnSite), false);
				}
				private EdgeFunction<Constraint<String>> buildFlowFunction(AST src, AST successor, D srcNode, D tgtNode, FlowFunction<D> originalFlowFunction, boolean isCall) {
					boolean srcAnnotated = hasFeatureAnnotation(src);
					boolean succAnnotated = hasFeatureAnnotation(successor);
					if(!srcAnnotated && !(isCall && succAnnotated)) return EdgeIdentity.v();
										
					List<AST> srcSuccs = interCfg.getSuccsOf(src);
					if (srcSuccs.size() == 1) {
						AST head = srcSuccs.get(0);
						if (head == successor) {
							// (src --> successor) is a fallThroughEdge (as src has only one successor), currently, this is the only case we can handle precisely 
							return preciseBuildFlowFunction(src, successor, srcNode, tgtNode, originalFlowFunction, isCall);
						}
					}
					return conservativeBuildFlowFunction(src, successor, srcNode, tgtNode, originalFlowFunction, isCall);
				}
				private EdgeFunction<Constraint<String>> conservativeBuildFlowFunction(AST src, AST successor, D srcNode, D tgtNode, FlowFunction<D> originalFlowFunction, boolean isCall) {
					//boolean srcAnnotated = hasFeatureAnnotation(src);
					//boolean succAnnotated = hasFeatureAnnotation(successor);
					//if(!srcAnnotated && !(isCall && succAnnotated)) return EdgeIdentity.v();
					Constraint<String> pos = originalFlowFunction.computeTargets(srcNode).contains(tgtNode) ? Constraint.<String>trueValue() : Constraint.<String>falseValue();
					Constraint<String> neg = srcNode == tgtNode ? Constraint.<String>trueValue() : Constraint.<String>falseValue();
					Constraint<String> lifted = pos.or(neg);
					return new SPLFeatureFunction(lifted, fmContext);
				}
				private EdgeFunction<Constraint<String>> preciseBuildFlowFunction(AST src, AST successor, D srcNode, D tgtNode, FlowFunction<D> originalFlowFunction, boolean isCall) {
					boolean srcAnnotated = hasFeatureAnnotation(src);
					boolean succAnnotated = hasFeatureAnnotation(successor);
					//if (!srcAnnotated && !(isCall && succAnnotated)) return EdgeIdentity.v();
					
					BDD features = Constraint.FACTORY.zero();
					if(srcAnnotated)
						features = features.or(icfg.getConstraint(src).bdd);
					if(isCall && succAnnotated)
						features = features.or(icfg.getConstraint(successor).bdd);
					Constraint<String> pos = originalFlowFunction.computeTargets(srcNode).contains(tgtNode) ? Constraint.<String>make(features) : Constraint.<String>falseValue();
					Constraint<String> neg = srcNode == tgtNode ? Constraint.<String>make(features.not()) : Constraint.<String>falseValue();
					Constraint<String> lifted = pos.or(neg);
					return new SPLFeatureFunction(lifted, fmContext);
				}
				
							
			}


			@Override
			protected EdgeFunction<Constraint<String>> createAllTopFunction() {
				return new SPLFeatureFunction(Constraint.<String>falseValue(), fmContext);			
			}

			@Override
			protected JoinLattice<Constraint<String>> createJoinLattice() {
				return new JoinLattice<Constraint<String>>() {

					public Constraint<String> topElement() {
						return Constraint.falseValue();
					}

					public Constraint<String> bottomElement() {
						return Constraint.trueValue();
					}

					public Constraint<String> join(Constraint<String> left, Constraint<String> right) {
						return left.or(right);
					}
				};

			}

			@Override
			protected EdgeFunctions<AST, D, FunctionDef, Constraint<String>> createEdgeFunctionsFactory() {
				return new IFDSEdgeFunctions(interproceduralCFG());
			}

			@Override
			protected FlowFunctions<AST, D, FunctionDef> createFlowFunctionsFactory() {
				return new FlowFunctions<AST, D, FunctionDef>() {

					@Override
					public FlowFunction<D> getNormalFlowFunction(AST curr,
																 AST succ) {
						FlowFunction<D> original = ifdsProblem.flowFunctions().getNormalFlowFunction(curr, succ);
						if(hasFeatureAnnotation(curr) /*&& interproceduralCFG().isFallThroughSuccessor(curr, succ)*/) { // m*
							return new WrappedFlowFunction<D>(original);
						} else {
							return original;
						}

					}

					@Override
					public FlowFunction<D> getCallFlowFunction(AST callStmt,
															   FunctionDef destinationMethod) {
						return ifdsProblem.flowFunctions().getCallFlowFunction(callStmt, destinationMethod);
					}

					@Override
					public FlowFunction<D> getReturnFlowFunction(AST callSite,
																 FunctionDef calleeMethod, AST exitStmt,
																 AST returnSite) {
						return ifdsProblem.flowFunctions().getReturnFlowFunction(callSite, calleeMethod, exitStmt, returnSite);
					}

					@Override
					public FlowFunction<D> getCallToReturnFlowFunction(
							AST callSite, AST returnSite) {
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
	
	public static final boolean hasFeatureAnnotation(AST stmt) {
		Constraint c = interCfg.getConstraint(stmt);
		if (c == Constraint.trueValue()) return false;
		if (c == Constraint.falseValue()) return true; // c
		return (!(c.bdd.isOne()));
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