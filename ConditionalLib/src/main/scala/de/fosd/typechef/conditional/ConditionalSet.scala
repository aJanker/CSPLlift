package de.fosd.typechef.conditional

import de.fosd.typechef.featureexpr.FeatureExpr
import de.fosd.typechef.featureexpr.FeatureExprFactory.{False, True}

/**
 * maintains a map
 * a name may be mapped to alternative entries with different feature expressions
 */
class ConditionalSet[A](private val entries: Map[A, FeatureExpr]) {
    def this() = this(Map())

    def ++(that: ConditionalSet[A]) = {
        val newMap = for (key <- (this.entries.keys ++ that.entries.keys)) yield
            (key -> (this.entries.getOrElse(key, False) or that.entries.getOrElse(key, False)))
        new ConditionalSet(newMap.toMap)
    }

    def union = ++ _
    def +(key: A, f: FeatureExpr) = new ConditionalSet[A](this.entries.+(key -> (f or this.entries.getOrElse(key, False))))

    def contains(name: A): FeatureExpr =this.entries.getOrElse(name, False)
    def isEmpty = entries.isEmpty

    /**
     * restricts the feature expression of all entries
     */
    def and(f: FeatureExpr): ConditionalSet[A] = new ConditionalSet(entries.mapValues(_ and f))

    override def equals(that: Any) = that match {
        case c: ConditionalSet[_] => entries equals c.entries;
        case _ => false
    }
    override def hashCode = entries.hashCode
    override def toString = entries.toString

    def toPlainSet(): Set[A] = {
        System.err.print(">>> remove me!")
        entries.keys.toSet
    }
}

object ConditionalSet {
    def apply[A]() = new ConditionalSet[A]()
}

