package soot.spl.ifds;

import de.fosd.typechef.featureexpr.bdd.BDDFeatureExpr;
import net.sf.javabdd.BDDFactory;
import soot.util.NumberedString;
import soot.util.StringNumberer;

import java.util.Collection;

import static soot.spl.ifds.Constraint.FeatureModelMode.NO_SINGLETON;

public class Constraint<T> implements Cloneable {
	
	public static BDDFactory FACTORY;
	
	public enum FeatureModelMode{
		NONE, 			//do not consider the feature model at all
		ALL,			//consider all feature constraints
		NO_SINGLETON	//consider all feature constraints but singleton constraints of the form "A" or "!A"
	};
	
	public static FeatureModelMode fmMode = NO_SINGLETON;

	@SuppressWarnings({ "rawtypes" })
	private final static Constraint FALSE = new Constraint(null) {
		public Constraint and(Constraint other) {
			//false && other = false
			return this;
		}		

		public Constraint or(Constraint other) {
			//false || other == other
			return other;
		}		

		public String toString() {
			return "false";
		}
		
		public String toString(StringNumberer featureNumberer) {
			return toString();
		}

		public int hashCode() {
			return -436534;
		}
		
		public boolean equals(Object obj) {
			return obj==this;
		}
		
		protected Constraint exists(NumberedString varToQuantify) {
			return this;
		}
		
		public Constraint simplify(Iterable allFeatures, Collection usedFeatures) {
			return this;
		}
		
		public int size() {
			return 0;
		}
	};
	
	@SuppressWarnings({ "rawtypes" })
	private final static Constraint TRUE = new Constraint(null) {
		public Constraint and(Constraint other) {
			//true && other == other
			return other;
		}

		public Constraint or(Constraint other) {
			//true || other == true
			return this;
		}

		public String toString() {
			return "true";
		}
		
		public String toString(StringNumberer featureNumberer) {
			return toString();
		}

		public int hashCode() {
			return -23214;
		}
		
		public boolean equals(Object obj) {
			return obj==this;
		}
		
		protected Constraint exists(NumberedString varToQuantify) {
			return this;
		}
		
		public Constraint simplify(Iterable allFeatures, Collection usedFeatures) {
			return this;
		}

		public int size() {
			return 0;
		}
	};

	public final BDDFeatureExpr bFexpr;

	public synchronized static <T> Constraint<T> make(BDDFeatureExpr bdd) {
		synchronized (FACTORY) {
			if (bdd.leak().isOne())
				return Constraint.trueValue();
			else if (bdd.leak().isZero())
				return Constraint.falseValue();
			else return new Constraint<T>(bdd);
		}
	}

	private Constraint(BDDFeatureExpr bdd) {
		this.bFexpr = bdd;
	}
	
	/**
	 * Computes the constraint representing this OR other.
	 * The constraint is automatically reduced such that
	 * a || !a results in true.
	 * @see Constraint#trueValue()
	 */
	public Constraint<T> or(Constraint<T> other) {
		synchronized (FACTORY) {
			if(other==trueValue()) return other;
			if(other==falseValue()) return this;

			BDDFeatureExpr disjunction = (BDDFeatureExpr) bFexpr.or(other.bFexpr);
			if (disjunction.leak().isOne())
				return trueValue();
			else
				return new Constraint<T>(disjunction);
		}
	}
	
	/**
	 * Computes the constraint representing this AND other.
	 * The constraint is automatically reduced such that
	 * a && !a results in false.
	 * @see Constraint#falseValue()
	 */
	public Constraint<T> and(Constraint<T> other) {
		synchronized (FACTORY) {
			if(other==trueValue()) return this;
			if(other==falseValue()) return other;

			BDDFeatureExpr conjunction = (BDDFeatureExpr) bFexpr.and(other.bFexpr);
			if (conjunction.leak().isZero())
				return falseValue();
			else
				return new Constraint<T>(conjunction);
		}
	}
	
	@Override
	public String toString() {
		return this.bFexpr./* toString() */ toTextExpr();
	}

	public Constraint<T> not() {
		return Constraint.make((BDDFeatureExpr) this.bFexpr.not());
	}


	@SuppressWarnings("unchecked")
	public static <T> Constraint<T> trueValue() {
		return TRUE;
	}

	@SuppressWarnings("unchecked")
	public static <T> Constraint<T> falseValue() {
		return FALSE;
	}

	@Override
	public int hashCode() {
		synchronized (FACTORY) {
			return this.bFexpr.hashCode();
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
			if (bFexpr == null) {
				if (other.bFexpr != null)
					return false;
			} else if (!bFexpr.equals(other.bFexpr))
				return false;
			return true;
		}
	}

	public int size() {
		synchronized (FACTORY) {
			return bFexpr.leak().nodeCount();
		}
	}
}
