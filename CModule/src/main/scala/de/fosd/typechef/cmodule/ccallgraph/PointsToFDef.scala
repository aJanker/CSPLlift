package de.fosd.typechef.cmodule.ccallgraph

import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.featureexpr.FeatureExpr
import de.fosd.typechef.parser.c.FunctionDef

import scala.collection.JavaConverters._

/**
  * Wrapper class for encoding the pointsTo Condition of a Opt[FunctionDef]
  */
sealed case class PointsToFDef(private val pointsToFDef: Opt[Opt[FunctionDef]]) {
    def getFDef: FunctionDef = getOpt.entry
    def getOpt: Opt[FunctionDef] = pointsToFDef.entry
    def getPointsToCondition: FeatureExpr = pointsToFDef.condition

    def pointsToCondition: FeatureExpr = getPointsToCondition

    def and(condition: FeatureExpr): PointsToFDef = PointsToFDef(pointsToFDef.copy(condition = pointsToFDef.condition and condition))
    def or(condition: FeatureExpr): PointsToFDef = PointsToFDef(pointsToFDef.copy(condition = pointsToFDef.condition or condition))
}

object PointsToFDef {

    def apply(pointsToCondition: FeatureExpr, functionDef: Opt[FunctionDef]) =
        new PointsToFDef(Opt(pointsToCondition, functionDef))

    /**
      * Reduce the number of identical targets by or-ing their presence conditions.
      */
    def reduceFDefs(targets: Iterable[PointsToFDef]): Set[PointsToFDef] = {
        val idFunctions = new java.util.IdentityHashMap[Opt[FunctionDef], FeatureExpr]()

        def add(fDef: PointsToFDef) = {
            val key = fDef.getOpt
            val currPointsToCondition = fDef.getPointsToCondition
            val newPointsToCondition =
                if (idFunctions.containsKey(key)) currPointsToCondition or idFunctions.get(key)
                else currPointsToCondition

            idFunctions.put(key, newPointsToCondition)
        }

        targets.foreach(add)

        idFunctions.entrySet().asScala.map(x => apply(x.getValue, x.getKey)).toSet
    }
}