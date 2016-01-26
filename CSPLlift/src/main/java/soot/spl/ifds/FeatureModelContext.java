package soot.spl.ifds;

import soot.util.StringNumberer;

public class FeatureModelContext {

	private final StringNumberer featureNumberer = null; // TODO: initialize this in constructor
	private int numFeaturesPresent;
	//private final IJavaProject javaProject;
	private Constraint<String> simplifiedConstraint;
	private Constraint<String> fullConstraint;

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

	
	public Constraint<String> getSimplifiedFMConstraint() {
		return simplifiedConstraint;
	}

	public void setSimplifiedFMConstraint(Constraint<String> simplifiedConstraint) {
		this.simplifiedConstraint = simplifiedConstraint;
	}

	public Constraint<String> getFullFMConstraint() {
		return fullConstraint;
	}

	public void setFullFMConstraint(Constraint<String> fullConstraint) {
		this.fullConstraint = fullConstraint;
	}

}
