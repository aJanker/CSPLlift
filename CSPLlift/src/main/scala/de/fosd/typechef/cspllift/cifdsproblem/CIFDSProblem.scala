package de.fosd.typechef.cspllift.cifdsproblem

import java.util

import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.cspllift.commons.CInterCFGCommons
import de.fosd.typechef.cspllift.evaluation.SimpleConfiguration
import de.fosd.typechef.cspllift.{CInterCFG, IFDSProblem}
import de.fosd.typechef.featureexpr.bdd.{BDDFeatureExpr, True}
import de.fosd.typechef.featureexpr.{FeatureExpr, FeatureExprFactory}
import de.fosd.typechef.parser.c._

abstract class CIFDSProblem[D <: CFlowFact](cICFG: CInterCFG) extends IFDSProblem[D] with CInterCFGCommons {

    /**
      * Returns the interprocedural control-flow graph which this problem is computed over.
      *
      * <b>NOTE:</b> this method could be called many times. Implementations of this
      * interface should therefore cache the return value!
      */
    override def interproceduralCFG: CInterCFG = cICFG

    // TODO Comment
    override def zeroValue(): D with CZeroFact
}

trait CZeroFact {
    val flowCondition : FeatureExpr
}

trait CFlowFact {

    def isEquivalentTo(other : CFlowFact, configuration: SimpleConfiguration) : Boolean

    def getConditions : Set[BDDFeatureExpr]

    def isInterestingFact : Boolean

    def toText: String

}

trait CFlowOperations[D <: CFlowFact] {

    def GEN(fact: D): util.Set[D]

    def GEN(res: TraversableOnce[D]): util.Set[D]

    def KILL: util.Set[D]

}

trait CFlowConstants {

    lazy val SPLLIFT_CONSTANT_VALUE = "SPLLIFT_CONSTANT_VALUE"

    lazy val SPLLIFT_PSEUDO_SYSTEM_FUNCTION_CALL_NAME = "PSEUDO_SYSTEM_FUNCTION_CALL"

    lazy val SPLLIFT_PSEUDO_SYSTEM_FUNCTION_CALL = Opt(True, FunctionDef(List(Opt(FeatureExprFactory.True, VoidSpecifier())), AtomicNamedDeclarator(List(), Id(SPLLIFT_PSEUDO_SYSTEM_FUNCTION_CALL_NAME), List(Opt(FeatureExprFactory.True, DeclIdentifierList(List())))), List(), CompoundStatement(List(Opt(FeatureExprFactory.True, ReturnStatement(None))))))

}