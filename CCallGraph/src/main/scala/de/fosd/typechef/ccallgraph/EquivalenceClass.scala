package de.fosd.typechef.ccallgraph

/**
 * Created by gferreir on 11/25/14.
 */
// Equivalence class of object names
class EquivalenceClass(initialObjNamesSet: Set[String], initialPrefixSet: Set[(String, String)]) {

    private var objectNamesSet: Set[String] = initialObjNamesSet
    private var prefixSet: Set[(String, String)] = initialPrefixSet

    def objectNames(): Set[String] = objectNamesSet

    def prefixes(): Set[(String, String)] = prefixSet

    def addPrefix(t: (String, String)) = {
        prefixSet += t
    }

    def addObjectName(objectName: String) = {
        objectNamesSet += objectName
    }

    def union(other: EquivalenceClass): EquivalenceClass = {
        new EquivalenceClass(this.objectNames().union(other.objectNames()), Set())
    }

    def equals(other: EquivalenceClass): Boolean = {
        this.objectNames().equals(other.objectNames()) && this.prefixes().equals(other.prefixes())
    }

    override def toString: String = "ObjNameSet %s\nPrefixSet %s\n".format(objectNamesSet.mkString("(", ", ", ")"), prefixes().mkString("(", ", ", ")"))
}
