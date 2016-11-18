package spllift;

import de.fosd.typechef.featureexpr.FeatureExpr;
import de.fosd.typechef.featureexpr.FeatureExprFactory;
import de.fosd.typechef.featureexpr.FeatureModel;
import heros.EdgeFunction;
import heros.edgefunc.AllTop;
import heros.edgefunc.EdgeIdentity;

public class ConditionalEdgeFunction implements EdgeFunction<FeatureExpr> {

    private final FeatureExpr condition;
    private final FeatureModel fm;
    private final boolean useFM;

    public ConditionalEdgeFunction(final FeatureExpr condition, final FeatureModel fModel, final boolean useFMInEdgeComputations) {
        if (condition == null) throw new RuntimeException();
        this.condition = condition;
        this.fm = fModel;
        this.useFM = useFMInEdgeComputations;
    }

    public FeatureExpr computeTarget(FeatureExpr source) {
            return isValidInFM(this.and(source));
    }

    public EdgeFunction<FeatureExpr> composeWith(EdgeFunction<FeatureExpr> secondFunction) {
        if (secondFunction instanceof EdgeIdentity || secondFunction instanceof AllTop ) return this;

        ConditionalEdgeFunction other = (ConditionalEdgeFunction) secondFunction;
        return new ConditionalEdgeFunction(this.and(other.condition), fm, useFM);
    }

    public EdgeFunction<FeatureExpr> joinWith(EdgeFunction<FeatureExpr> otherFunction) {
        //here we implement union/"or" semantics
        if (otherFunction instanceof AllTop) return this;
        if (otherFunction instanceof EdgeIdentity) return otherFunction;

        ConditionalEdgeFunction other = (ConditionalEdgeFunction) otherFunction;
        return new ConditionalEdgeFunction(this.or(other.condition), fm, useFM);
    }

    public boolean equalTo(EdgeFunction<FeatureExpr> other) {
        if (other instanceof ConditionalEdgeFunction) {
            final ConditionalEdgeFunction function = (ConditionalEdgeFunction) other;
            final boolean equalSettings = (this.fm == function.fm) && (this.useFM == function.useFM);
            if (equalSettings)
                return useFM ? function.condition.equivalentTo(this.condition, this.fm) : function.condition.equivalentTo(this.condition);
        }
        return false;
    }

    public String toString() {
        return condition.toString();
    }

    private FeatureExpr and(final FeatureExpr other) {
        return isValidInFM(this.condition.and(other));
    }

    private FeatureExpr or(final FeatureExpr other) {
        return isValidInFM(this.condition.or(other));
    }

    private FeatureExpr isValidInFM(final FeatureExpr constraint) {
        if (this.useFM)
            return constraint.isSatisfiable(fm) ? constraint : FeatureExprFactory.False();
        else
            return constraint;
    }
}