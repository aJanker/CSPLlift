package de.fosd.typechef.ccallgraph

import de.fosd.typechef.conditional.ConditionalSet
import de.fosd.typechef.featureexpr.FeatureExpr

/**
 * Created by gferreir on 11/25/14.
 */


// Equivalence class of object names
class EquivalenceClass(initialObjNamesSet: ConditionalSet[String], initialPrefixSet: ConditionalSet[(String, String)]) {

    type ObjectName = String
    type PrefixSet = (String, String)

    private var objectNamesSet: ConditionalSet[ObjectName] = initialObjNamesSet
    private var prefixSet: ConditionalSet[PrefixSet] = initialPrefixSet

    def objectNames(): ConditionalSet[ObjectName] = objectNamesSet
    def prefixes(): ConditionalSet[PrefixSet] = prefixSet

    def addPrefix(t: PrefixSet, f : FeatureExpr) = {
        prefixSet = prefixSet. + (t, f)
    }

    def addObjectName(objectName: ObjectName, featExpr : FeatureExpr) = {
        objectNamesSet = objectNamesSet.+(objectName, featExpr)
    }

    def union(other: EquivalenceClass): EquivalenceClass = {
        new EquivalenceClass(this.objectNames().union(other.objectNames()), ConditionalSet())
    }

    def equals(other: EquivalenceClass): Boolean = {
        this.objectNames().equals(other.objectNames()) && this.prefixes().equals(other.prefixes())
    }

    override def toString: String = "%s ---> %s".format(objectNamesSet.toPlainSet().mkString("{", ", ", "}"), prefixes().toPlainSet().mkString("{", ", ", "}"))
}
