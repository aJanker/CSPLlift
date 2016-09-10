package soot.spl.ifds;

import soot.util.StringNumberer;

/**
 * Original FeatureModel Implementation of SPLLift -> Replaced in TypeChef own FeatureModel Implementation
 */
public class FeatureModelContext {

	private final StringNumberer featureNumberer = null; // TODO: initialize this in constructor
	private int numFeaturesPresent;
	//private final IJavaProject javaProject;
	private Constraint simplifiedConstraint;
	private Constraint fullConstraint;

	public FeatureModelContext() {}
	/*public FeatureModelContext(FeatureModelInstrumentorTransformer transformer, IJavaProject javaProject) {
		this.javaProject = javaProject;
		this.featureNumberer = transformer.getFeatureNumberer();
		this.numFeaturesPresent = transformer.numFeaturesPresent();
	}*/

	public StringNumberer getFeatureNumberer() {
		return featureNumberer;
	}

	public int getNumFeaturesPresent() {
		return numFeaturesPresent;
	}

	
	public Constraint getSimplifiedFMConstraint() {
		return simplifiedConstraint;
	}

	public void setSimplifiedFMConstraint(Constraint simplifiedConstraint) {
		this.simplifiedConstraint = simplifiedConstraint;
	}

	public Constraint getFullFMConstraint() {
		return fullConstraint;
	}

	public void setFullFMConstraint(Constraint fullConstraint) {
		this.fullConstraint = fullConstraint;
	}

}
