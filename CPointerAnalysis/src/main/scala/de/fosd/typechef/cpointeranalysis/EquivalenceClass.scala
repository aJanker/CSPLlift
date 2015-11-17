package de.fosd.typechef.cpointeranalysis

import de.fosd.typechef.conditional.ConditionalSet
import de.fosd.typechef.featureexpr.FeatureExpr

/**
 * Created by gferreir on 11/25/14.
 */


// Equivalence class of object names
class EquivalenceClass(var objectNames: ConditionalSet[String], var prefixSet: ConditionalSet[(String, String)], var beingMerged : Boolean = false) {

    type ObjectName = String
    type PrefixSet = (String, String)

    def unscopedObjectNames() : Set[ObjectName] = objectNames.toPlainSet().map({ case o => unscope(o) })
    def prefixes(): ConditionalSet[PrefixSet] = prefixSet

    def addPrefix(t: PrefixSet, f : FeatureExpr) = {
        prefixSet = prefixSet.+ (t, f)
    }

    def addObjectName(objectName: ObjectName, featExpr : FeatureExpr) = {
        objectNames = objectNames.+(objectName, featExpr)
    }

    def unscope(scopedObjectName: String): String = {
        val unescopedObjectName = scopedObjectName.replaceFirst("[a-zA-Z0-9_]+?\\$", "")
        assert(!unescopedObjectName.contains("$"))
        unescopedObjectName
    }

    def find(scopedObjectName: ObjectName): Boolean = {
        objectNames.toPlainSet().contains(scopedObjectName)
    }

    def union(other: EquivalenceClass): EquivalenceClass = {
        new EquivalenceClass(this.objectNames.union(other.objectNames), ConditionalSet())
    }

    def equals(other: EquivalenceClass): Boolean = {
        this.objectNames.toPlainSet().equals(other.objectNames.toPlainSet()) && this.prefixes().toPlainSet().equals(other.prefixes().toPlainSet())
    }

    override def toString: String = "%s ---> (%d) %s".format(objectNames.toPlainSetWithConditionals().mkString("{", ", ", "}"), prefixes().toPlainSet().size, prefixes().toPlainSetWithConditionals().mkString("{", ", ", "}"))
}

object EquivalenceClass {
    def apply() = new EquivalenceClass(ConditionalSet(), ConditionalSet())
}