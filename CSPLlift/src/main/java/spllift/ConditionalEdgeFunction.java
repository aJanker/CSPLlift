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

    public FeatureExpr computeTarget(final FeatureExpr source) {
            return isSatisfiableInFM(this.and(source));
    }

    public EdgeFunction<FeatureExpr> composeWith(final EdgeFunction<FeatureExpr> otherFunction) {
        if (otherFunction instanceof EdgeIdentity || otherFunction instanceof AllTop ) return this;

        final ConditionalEdgeFunction other = (ConditionalEdgeFunction) otherFunction;
        return new ConditionalEdgeFunction(this.and(other.condition), this.fm, this.useFM);
    }

    public EdgeFunction<FeatureExpr> joinWith(EdgeFunction<FeatureExpr> otherFunction) {
        //here we implement union/"or" semantics
        if (otherFunction instanceof AllTop) return this;
        if (otherFunction instanceof EdgeIdentity) return otherFunction;

        final ConditionalEdgeFunction other = (ConditionalEdgeFunction) otherFunction;
        return new ConditionalEdgeFunction(this.or(other.condition), this.fm, this.useFM);
    }

    public boolean equalTo(EdgeFunction<FeatureExpr> other) {
        if (other instanceof ConditionalEdgeFunction) {
            final ConditionalEdgeFunction function = (ConditionalEdgeFunction) other;
            final boolean equalSettings = (function.fm == this.fm) && (function.useFM == this.useFM);
            if (equalSettings)
                return this.useFM ? function.condition.equivalentTo(this.condition, this.fm) : function.condition.equivalentTo(this.condition);
        }
        return false;
    }

    public String toString() {
        return this.condition.toTextExpr();
    }

    private FeatureExpr and(final FeatureExpr other) {
        return isSatisfiableInFM(this.condition.and(other));
    }

    private FeatureExpr or(final FeatureExpr other) {
        return isSatisfiableInFM(this.condition.or(other));
    }

    private FeatureExpr isSatisfiableInFM(final FeatureExpr condition) {
        if (this.useFM)
            return condition.isSatisfiable(this.fm) ? condition : FeatureExprFactory.False();
        else
            return condition;
    }
}