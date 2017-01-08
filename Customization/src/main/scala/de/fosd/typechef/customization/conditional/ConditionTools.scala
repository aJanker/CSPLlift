package de.fosd.typechef.customization.conditional

import de.fosd.typechef.conditional.{Choice, Opt}
import de.fosd.typechef.featureexpr.{FeatureExpr, FeatureExprFactory, SingleFeatureExpr}

trait ConditionTools {

    def isSatisfiableInConfiguration(cons: FeatureExpr, configuration: SimpleConfiguration): Boolean = cons.evaluate(configuration.getTrueFeatures)

    /**
      * Returns a sorted list of all features in this AST, including Opt and Choice Nodes
      *
      * @param root input element
      * @return
      */
    def getAllFeatures(root: Product): List[SingleFeatureExpr] =
        extractFeatures(root).toList.sortWith({
            // sort to eliminate any non-determinism caused by the set
            (x: SingleFeatureExpr, y: SingleFeatureExpr) => x.feature.compare(y.feature) > 0
        })

    def getAllFeatureExpr(root: Product): List[FeatureExpr] = getAllFeatures(root).map(_.and(FeatureExprFactory.True))

    private def extractFeatures(root: Any): Set[SingleFeatureExpr] = {
        root match {
            case x: Opt[_] => x.condition.collectDistinctFeatureObjects ++ extractFeatures(x.entry)
            case x: Choice[_] => x.condition.collectDistinctFeatureObjects ++ extractFeatures(x.thenBranch) ++ extractFeatures(x.elseBranch)
            case l: List[_] => l.foldLeft(Set[SingleFeatureExpr]())((s, x) => s ++ extractFeatures(x))
            case x: Product => x.productIterator.toList.foldLeft(Set[SingleFeatureExpr]())((s, y) => s ++ extractFeatures(y))
            case o => Set()
        }
    }
}
