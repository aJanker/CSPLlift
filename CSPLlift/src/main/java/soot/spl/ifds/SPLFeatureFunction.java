package soot.spl.ifds;

import de.fosd.typechef.featureexpr.FeatureModel;
import heros.EdgeFunction;
import heros.edgefunc.AllTop;
import heros.edgefunc.EdgeIdentity;

public class SPLFeatureFunction implements EdgeFunction<Constraint> {
	
	private final Constraint condition;
	private final FeatureModel fm;
	private final boolean useFM;
	
	public SPLFeatureFunction(final Constraint conditions, final  FeatureModel fModel, final boolean useFMInEdgeComputations){
		if (conditions == null) throw new RuntimeException();
		this.condition = conditions;
		this.fm = fModel;
		this.useFM = useFMInEdgeComputations;
	} 

	public Constraint computeTarget(Constraint source) {
		return isSatisfiable(this.and(source));
	}

	public EdgeFunction<Constraint> composeWith(EdgeFunction<Constraint> secondFunction) {
		if(secondFunction instanceof EdgeIdentity || secondFunction instanceof AllTop) return this;
		
		SPLFeatureFunction other = (SPLFeatureFunction)secondFunction;
		return new SPLFeatureFunction(this.and(other.condition), fm, useFM);
	}

	public EdgeFunction<Constraint> joinWith(EdgeFunction<Constraint> otherFunction) {
		//here we implement union/"or" semantics
		if(otherFunction instanceof AllTop) return this;
		if(otherFunction instanceof EdgeIdentity) return otherFunction;

		SPLFeatureFunction other = (SPLFeatureFunction)otherFunction;
		return new SPLFeatureFunction(this.or(other.condition), fm, useFM);
	}
	
	public boolean equalTo(EdgeFunction<Constraint> other) {
		if(other instanceof SPLFeatureFunction) {
			SPLFeatureFunction function = (SPLFeatureFunction) other;
			return function.condition.equals(condition);
		}
		return false;
	}

	public String toString() {
		return condition.toString();
	}

	private Constraint and(final Constraint other) {
		return isSatisfiable(this.condition.and(other));
	}

	private Constraint or (final Constraint other) {
		return isSatisfiable(this.condition.or(other));
	}

	private Constraint isSatisfiable(final Constraint cons) {
		if (this.useFM)
			return cons.isSatisfiable(fm) ? cons : Constraint.falseValue();
		else
			return cons;
	}


}
