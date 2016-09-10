package soot.spl.ifds;

import de.fosd.typechef.featureexpr.FeatureModel;
import heros.EdgeFunction;
import heros.edgefunc.AllTop;
import heros.edgefunc.EdgeIdentity;

public class SPLFeatureFunction implements EdgeFunction<Constraint> {
	
	private final Constraint condition;
	private final FeatureModel fm;
	
	public SPLFeatureFunction(Constraint conditions, FeatureModel fModel){
		if (conditions == null) throw new RuntimeException();
		this.condition = conditions;
		this.fm = fModel;
	} 

	public Constraint computeTarget(Constraint source) {
		Constraint conjunction = source.and(condition);
		//return conjunction;
		return conjunction.isSatisfiable(fm) ? conjunction : Constraint.falseValue();
	}

	public EdgeFunction<Constraint> composeWith(EdgeFunction<Constraint> secondFunction) {
		if(secondFunction instanceof EdgeIdentity || secondFunction instanceof AllTop) return this;
		
		SPLFeatureFunction other = (SPLFeatureFunction)secondFunction;
		return new SPLFeatureFunction(condition.and(other.condition), fm);
	}

	public EdgeFunction<Constraint> joinWith(EdgeFunction<Constraint> otherFunction) {
		//here we implement union/"or" semantics
		if(otherFunction instanceof AllTop) return this;
		if(otherFunction instanceof EdgeIdentity) return otherFunction;

		SPLFeatureFunction other = (SPLFeatureFunction)otherFunction;
		return new SPLFeatureFunction(condition.or(other.condition), fm);
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


}
