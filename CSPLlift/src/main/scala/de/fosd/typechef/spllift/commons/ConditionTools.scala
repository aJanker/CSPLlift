package de.fosd.typechef.spllift.commons

import de.fosd.typechef.conditional.{Choice, Opt}
import de.fosd.typechef.featureexpr.SingleFeatureExpr
import de.fosd.typechef.spllift.evaluation.SimpleConfiguration
import soot.spl.ifds.Constraint

trait ConditionTools {

    def isSatisfiableInConfiguration(cons : Constraint[_], configuration : SimpleConfiguration) : Boolean = cons.getBDDFeatureExpr.evaluate(configuration.getTrueFeatures)

    /**
      * Returns a sorted list of all features in this AST, including Opt and Choice Nodes
      *
      * @param root input element
      * @return
      */
    def getAllFeatures(root: Product): List[SingleFeatureExpr] = {
        var featuresSorted: List[SingleFeatureExpr] = getAllFeaturesRec(root).toList
        // sort to eliminate any non-determinism caused by the set
        featuresSorted = featuresSorted.sortWith({
            (x: SingleFeatureExpr, y: SingleFeatureExpr) => x.feature.compare(y.feature) > 0
        })
        println("found " + featuresSorted.size + " features")
        featuresSorted //.map({s:String => FeatureExprFactory.createDefinedExternal(s)});
    }

    private def getAllFeaturesRec(root: Any): Set[SingleFeatureExpr] = {
        root match {
            case x: Opt[_] => x.condition.collectDistinctFeatureObjects ++ getAllFeaturesRec(x.entry)
            case x: Choice[_] => x.condition.collectDistinctFeatureObjects ++ getAllFeaturesRec(x.thenBranch) ++ getAllFeaturesRec(x.elseBranch)
            case l: List[_] => {
                var ret: Set[SingleFeatureExpr] = Set()
                for (x <- l) {
                    ret = ret ++ getAllFeaturesRec(x)
                }
                ret
            }
            case x: Product => {
                var ret: Set[SingleFeatureExpr] = Set()
                for (y <- x.productIterator.toList) {
                    ret = ret ++ getAllFeaturesRec(y)
                }
                ret
            }
            case o => Set()
        }
    }
}
