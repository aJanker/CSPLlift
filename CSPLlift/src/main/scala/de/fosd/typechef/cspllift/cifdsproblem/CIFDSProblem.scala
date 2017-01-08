package de.fosd.typechef.cspllift.cifdsproblem

import java.util
import java.util.Collections

import de.fosd.typechef.cspllift.IFDSProblem
import de.fosd.typechef.cspllift.cintercfg.CInterCFG
import de.fosd.typechef.cspllift.commons.CInterCFGCommons
import de.fosd.typechef.customization.conditional.SimpleConfiguration

import scala.collection.JavaConverters._

/**
  * Connector class for solving IFDS variability-aware.
  */
abstract class CIFDSProblem[D <: CFlowFact](cICFG: CInterCFG, seeds: List[D]) extends IFDSProblem[D] with CInterCFGCommons {

    /**
      * Returns the interprocedural control-flow graph which this problem is computed over.
      *
      * <b>NOTE:</b> this method could be called many times. Implementations of this
      * interface should therefore cache the return value!
      */
    override def interproceduralCFG: CInterCFG = cICFG
}

trait CFlowFact extends Cloneable with Product with Serializable {

    override def clone(): CFlowFact.this.type = super.clone().asInstanceOf[CFlowFact.this.type]

    def isEquivalentTo(other: CFlowFact, configuration: SimpleConfiguration): Boolean

    def isEvaluationFact: Boolean

    def toText: String
}

trait CDefaultFlowFact extends CFlowFact {
    def isEquivalentTo(other: CFlowFact, configuration: SimpleConfiguration): Boolean = false

    def isEvaluationFact: Boolean = false

    def toText: String = toString
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

trait CFlowConstants {

    lazy val SCOPE_UNKNOWN: Int = -1
    lazy val SCOPE_GLOBAL: Int = 0
    lazy val SCOPE_LOCAL: Int = 1

    lazy val SPLLIFT_CONSTANT_VALUE = "SPLLIFT_CONSTANT_VALUE"
    lazy val SPLLIFT_PSEUDO_SYSTEM_FUNCTION_CALL_NAME = "PSEUDO_SYSTEM_FUNCTION_CALL"

    lazy val SPLLIFT_REWRITE_PREFIX = "__SPLLIFT_TMP"
}