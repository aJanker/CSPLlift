package spllift;

import de.fosd.typechef.featureexpr.FeatureModel;
import de.fosd.typechef.featureexpr.bdd.BDDFeatureExpr;
import de.fosd.typechef.featureexpr.bdd.BDDFeatureExprFactory;
import net.sf.javabdd.BDDFactory;

/**
 * SPLlift's own PresenceCondition abstraction, wrapping TypeChef's BDDFeatureExpr implementation to represent presence conditions.
 */
public class Constraint implements Cloneable {

    private final static BDDFactory FACTORY = de.fosd.typechef.featureexpr.bdd.FExprBuilder.bddFactory();

    public synchronized static Constraint make(final BDDFeatureExpr bdd) {
        synchronized (FACTORY) {
            if (bdd.leak().isOne())
                return Constraint.trueValue();
            else if (bdd.leak().isZero())
                return Constraint.falseValue();
            else return new Constraint(bdd);
        }
    }

    private final BDDFeatureExpr bddFeatureExpr;

    private Constraint(BDDFeatureExpr bdd) {
        this.bddFeatureExpr = bdd;
    }

    public BDDFeatureExpr getFeatureExpr() {
        return this.bddFeatureExpr;
    }

    public boolean hasFeatureAnnotation() {
        return !(getFeatureExpr().leak().isOne());
    }

    public boolean isSatisfiable(FeatureModel fm) {
        return getFeatureExpr().isSatisfiable(fm);
    }

    public static Constraint trueValue() {
        return TRUE;
    }

    public static Constraint falseValue() {
        return FALSE;
    }

    public Constraint simplify() {
        return new Constraint((BDDFeatureExpr) getFeatureExpr().simplify(BDDFeatureExprFactory.TrueB()));
    }

    /**
     * Computes the constraint representing this OR other.
     * The constraint is automatically reduced such that
     * a || !a results in true.
     *
     * @see Constraint#trueValue()
     */
    public Constraint or(Constraint other) {
        synchronized (FACTORY) {
            if (other == trueValue()) return other;
            if (other == falseValue()) return this;

            final BDDFeatureExpr disjunction = (BDDFeatureExpr) getFeatureExpr().or(other.getFeatureExpr());

            if (disjunction.leak().isOne())
                return trueValue();
            else if (disjunction.leak().isZero())
                return falseValue();
            else
                return new Constraint(disjunction);
        }
    }

    /**
     * Computes the constraint representing this AND other.
     * The constraint is automatically reduced such that
     * a && !a results in false.
     *
     * @see Constraint#falseValue()
     */
    public Constraint and(Constraint other) {
        synchronized (FACTORY) {
            if (other == trueValue()) return this;
            if (other == falseValue()) return other;

            BDDFeatureExpr conjunction = (BDDFeatureExpr) getFeatureExpr().and(other.getFeatureExpr());
            if (conjunction.leak().isZero())
                return falseValue();
            else
                return new Constraint(conjunction);
        }
    }

    public Constraint not() {
        return Constraint.make((BDDFeatureExpr) getFeatureExpr().not());
    }

    @Override
    public String toString() {
        return getFeatureExpr().toTextExpr();
    }

    @Override
    public int hashCode() {
        synchronized (FACTORY) {
            return this.bddFeatureExpr.hashCode();
        }
    }

    @Override
    public boolean equals(Object obj) {
        synchronized (FACTORY) {
            if (this == obj)
                return true;
            if (obj == null)
                return false;
            if (getClass() != obj.getClass())
                return false;
            @SuppressWarnings("rawtypes")
            Constraint other = (Constraint) obj;
            if (getFeatureExpr() == null) {
                if (other.getFeatureExpr() != null)
                    return false;
            } else if (!getFeatureExpr().equals(other.getFeatureExpr()))
                return false;
            return true;
        }
    }

    public int size() {
        synchronized (FACTORY) {
            return getFeatureExpr().leak().nodeCount();
        }
    }

    @SuppressWarnings({"rawtypes"})
    private final static Constraint FALSE = new Constraint(BDDFeatureExprFactory.FalseB()) {
        public Constraint and(Constraint other) {
            //false && other = false
            return this;
        }

        public Constraint or(Constraint other) {
            //false || other == other
            return other;
        }

        public boolean hasFeatureAnnotation() {
            return false;
        }

        public String toString() {
            return "false";
        }

        public boolean equals(Object obj) {
            return obj == this;
        }

        public int size() {
            return 0;
        }
    };

    @SuppressWarnings({"rawtypes"})
    private final static Constraint TRUE = new Constraint(BDDFeatureExprFactory.TrueB()) {
        public Constraint and(Constraint other) {
            //true && other == other
            return other;
        }

        public Constraint or(Constraint other) {
            //true || other == true
            return this;
        }

        public boolean hasFeatureAnnotation() {
            return false;
        }

        public String toString() {
            return "true";
        }

        public boolean equals(Object obj) {
            return obj == this;
        }

        public int size() {
            return 0;
        }
    };
}
