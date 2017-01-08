package de.fosd.typechef

import java.util

import de.fosd.typechef.cspllift.cifdsproblem.{CFlowFact, CIFDSProblem}
import de.fosd.typechef.cspllift.cintercfg._
import de.fosd.typechef.featureexpr.FeatureExpr
import heros.IFDSTabulationProblem

import scala.collection.JavaConverters._


package object cspllift {

    type IFDSProblem[D <: CFlowFact] = IFDSTabulationProblem[CInterCFGNode, D, CInterCFGFDef, CInterCFG]

    def getCIFDSProblemInstance[D <: CFlowFact, T <: CIFDSProblem[D]](clazz: java.lang.Class[T])(args: AnyRef*): T = clazz.getConstructors()(0).newInstance(args: _*).asInstanceOf[T]

    type LiftedCFlowFact[D <: CFlowFact] = (D, FeatureExpr)

    type StmtFlowFacts[D <: CFlowFact] = (CInterCFGNode, List[LiftedCFlowFact[D]])

    // Looks messy, but required for a clean conversion from java collections to scala collections...
    def liftedFlowFactsAsScala[D <: CFlowFact](javaFacts: util.List[util.Map[D, FeatureExpr]]): List[LiftedCFlowFact[D]] =
        javaFacts.asScala.flatMap(_.asScala).map {
            case (fact, constraint) => (fact, constraint)
        }.toList.distinct

}
