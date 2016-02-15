package de.fosd.typechef.spllift.analysis

import java.util
import java.util.Collections

import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.crewrite.UsedDefinedDeclaredVariables
import de.fosd.typechef.parser.c._
import de.fosd.typechef.spllift.CInterCFG
import heros.{FlowFunction, FlowFunctions, IFDSTabulationProblem}

import scala.collection.JavaConverters._

class InformationFlow(icfg: CInterCFG) extends IFDSTabulationProblem[AST, InfoFlowFact, FunctionDef, CInterCFG] with InformationFlowConfiguration with UsedDefinedDeclaredVariables with ASTNavigation with ConditionalNavigation {

  /**
    * Returns initial seeds to be used for the analysis. This is a mapping of statements to initial analysis facts.
    * We consider global variables as initial sources.
    */
  override def initialSeeds(): util.Map[AST, util.Set[InfoFlowFact]] =
    icfg.entryFunctions.foldLeft(new util.HashMap[AST, util.Set[InfoFlowFact]])((res, entry) => {

      val intialSeeds: util.Set[InfoFlowFact] = {
        val globalVariables = icfg.nodeToTUnit(entry).defs.filterNot { case Opt(_, f: FunctionDef) => true }
        val globalInfoFlowFacts: Set[InfoFlowFact] = globalVariables.flatMap(x => {
          val decls = declares(x)

          if (decls.nonEmpty) {
            val res: List[InfoFlowFact] = decls.map(decl => Source(List(Opt(x.condition, decl))))
            res
          }
          else None
        }).toSet

        if (globalInfoFlowFacts.isEmpty) Collections.singleton(zeroValue()) else globalInfoFlowFacts.asJava
      }

      res.put(entry, intialSeeds)
      res
    })

  /**
    * Returns the interprocedural control-flow graph which this problem is computed over.
    *
    * <b>NOTE:</b> this method could be called many times. Implementations of this
    * interface should therefore cache the return value!
    */
  override def interproceduralCFG(): CInterCFG = icfg

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
  override def zeroValue(): InfoFlowFact = Zero

  /**
    * Returns a set of flow functions. Those functions are used to compute data-flow facts
    * along the various kinds of control flows.
    *
    * <b>NOTE:</b> this method could be called many times. Implementations of this
    * interface should therefore cache the return value!
    */
  override def flowFunctions(): FlowFunctions[AST, InfoFlowFact, FunctionDef] = new FlowFunctions[AST, InfoFlowFact, FunctionDef] {
    override def getCallFlowFunction(n: AST, m: FunctionDef): FlowFunction[InfoFlowFact] = {
      new FlowFunction[InfoFlowFact] {
        override def computeTargets(source: InfoFlowFact): util.Set[InfoFlowFact] = java.util.Collections.emptySet()
      }
    }

    override def getReturnFlowFunction(n: AST, m: FunctionDef, n1: AST, n2: AST): FlowFunction[InfoFlowFact] = {
      new FlowFunction[InfoFlowFact] {
        override def computeTargets(source: InfoFlowFact): util.Set[InfoFlowFact] = java.util.Collections.emptySet()
      }
    }

    /**
      * Returns the flow function that computes the flow for a normal statement,
      * i.e., a statement that is neither a call nor an exit statement.
      *
      * @param curr
      * The current statement.
      * @param succ
      * The successor for which the flow is computed. This value can
      * be used to compute a branched analysis that propagates
      * different values depending on where control0flow branches.
      */
    override def getNormalFlowFunction(curr: AST, succ: AST): FlowFunction[InfoFlowFact] = {
      // if define -> source
      // if use -> sink check
      new FlowFunction[InfoFlowFact] {

        override def computeTargets(source: InfoFlowFact): util.Set[InfoFlowFact] = java.util.Collections.emptySet()
      }
    }

    override def getCallToReturnFlowFunction(n: AST, n1: AST): FlowFunction[InfoFlowFact] = {
      new FlowFunction[InfoFlowFact] {
        override def computeTargets(source: InfoFlowFact): util.Set[InfoFlowFact] = java.util.Collections.emptySet()
      }
    }
  }
}

