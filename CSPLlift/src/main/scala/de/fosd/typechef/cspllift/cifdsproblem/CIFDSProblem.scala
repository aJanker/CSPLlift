package de.fosd.typechef.cspllift.cifdsproblem

import java.util
import java.util.Collections

import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.cspllift.cifdsproblem.informationflow.flowfact.{InformationFlowFact, Zero}
import de.fosd.typechef.cspllift.commons.CInterCFGCommons
import de.fosd.typechef.cspllift.evaluation.SimpleConfiguration
import de.fosd.typechef.cspllift.{CInterCFG, IFDSProblem}
import de.fosd.typechef.error.Position
import de.fosd.typechef.featureexpr.FeatureExprFactory
import de.fosd.typechef.featureexpr.bdd.True
import de.fosd.typechef.parser.c._

import scala.collection.JavaConverters._

abstract class CIFDSProblem[D <: CFlowFact](cICFG: CInterCFG) extends IFDSProblem[D] with CInterCFGCommons {

    /**
      * Returns the interprocedural control-flow graph which this problem is computed over.
      *
      * <b>NOTE:</b> this method could be called many times. Implementations of this
      * interface should therefore cache the return value!
      */
    override def interproceduralCFG: CInterCFG = cICFG

    /**
      * This must be a data-flow fact of type {@link D}, but must <i>not</i>
      * be part of the domain of data-flow facts. Typically this will be a
      * singleton object of type {@link D} that is used for nothing else.
      * It must holds that this object does not equals any object
      * within the domain.
      *
      * <b>NOTE:</b> this method could be called many times. Implementations of this
      * interface should therefore cache the return value!
      */
    override def zeroValue(): InformationFlowFact = zero

    private lazy val zero = Zero()
}

trait CFlowFact extends Cloneable with Product{

    override def clone(): CFlowFact.this.type = super.clone().asInstanceOf[CFlowFact.this.type]

    def isEquivalentTo(other: CFlowFact, configuration: SimpleConfiguration): Boolean

    def isEvaluationFact: Boolean

    def toText: String
}

trait CFlowOperations[D <: CFlowFact] extends CFlowConstants {

    import java.util.stream.{Collectors, Stream}

    def GEN(fact: D): util.Set[D] = Collections.singleton(fact)

    def GEN(res: TraversableOnce[D]): util.Set[D] = res.toSet.asJava

    def GEN(res: util.Set[D]*): util.Set[D] =
        res.foldLeft(Collections.emptySet[D]()) {
            case (a, b) => Stream.concat(a.stream(), b.stream()).collect(Collectors.toSet())
        }

    def KILL: util.Set[D] = Collections.emptySet()

}

trait CFlowConstants extends ASTRewriting {

    lazy val SCOPE_UNKNOWN: Int = -1
    lazy val SCOPE_GLOBAL: Int = 0
    lazy val SCOPE_LOCAL: Int = 1

    lazy val SPLLIFT_CONSTANT_VALUE = "SPLLIFT_CONSTANT_VALUE"
    lazy val SPLLIFT_PSEUDO_SYSTEM_FUNCTION_CALL_NAME = "PSEUDO_SYSTEM_FUNCTION_CALL"

    lazy val SPLLIFT_REWRITE_PREFIX = "__SPLLIFT_TMP"

    def genPseudoSystemFunctionCall(range: Option[(Position, Position)]) : Opt[FunctionDef] = {
        val call = Opt(True, FunctionDef(List(Opt(FeatureExprFactory.True, VoidSpecifier())), AtomicNamedDeclarator(List(), Id(SPLLIFT_PSEUDO_SYSTEM_FUNCTION_CALL_NAME), List(Opt(FeatureExprFactory.True, DeclIdentifierList(List())))), List(), CompoundStatement(List(Opt(FeatureExprFactory.True, ReturnStatement(None))))))
        val addRange = everywherebu(query[Product] { case a: AST => if (!a.hasPosition) a.range = range})

        addRange(call)
        call
    }
}