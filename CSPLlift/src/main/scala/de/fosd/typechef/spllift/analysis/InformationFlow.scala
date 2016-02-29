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

  private def GEN(fact: InfoFlowFact): util.Set[InfoFlowFact] = Collections.singleton(fact)

  private def GEN(res: List[InfoFlowFact]): util.Set[InfoFlowFact] = res.toSet.asJava

  private def KILL: util.Set[InfoFlowFact] = Collections.emptySet()

  /**
    * Returns initial seeds to be used for the analysis. This is a mapping of statements to initial analysis facts.
    * We consider global variables as initial sources.
    */
  override def initialSeeds(): util.Map[AST, util.Set[InfoFlowFact]] =
    interproceduralCFG.entryFunctions.foldLeft(new util.HashMap[AST, util.Set[InfoFlowFact]])((res, entry) => {

      val intialSeeds: util.Set[InfoFlowFact] = {
        val globalVariables = icfg.nodeToTUnit(entry).defs.filterNot {
          case Opt(_, f: FunctionDef) => true
          case _ => false
        }

        val globalInfoFlowFacts = globalVariables.flatMap(x => {
          val decls = declares(x)

          if (decls.nonEmpty) {
            val res: List[InfoFlowFact] = decls.map(decl => Source(Opt(x.condition, decl), x))
            res
          }
          else None
        })

        GEN((zeroValue()  :: globalInfoFlowFacts))
      }

      interproceduralCFG.getStartPointsOf(entry).asScala.foreach(res.put(_, intialSeeds))

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

    /**
      * Returns the flow function that computes the flow for a call statement.
      *
      * @param callStmt
      * The statement containing the invoke expression giving rise to
      * this call.
      * @param destinationMethod
      * The concrete target method for which the flow is computed.
      */
    override def getCallFlowFunction(callStmt: AST, destinationMethod: FunctionDef): FlowFunction[InfoFlowFact] = {
      def mapCallParamToFDefParam(callParams: List[Opt[Expr]], fDefParams: List[Opt[DeclaratorExtension]], res: List[Opt[(Id, Id)]] = List()): List[Opt[(Id, Id)]] = {
        if (callParams.isEmpty && fDefParams.isEmpty) return res

        val currentCallParameter = callParams.head
        val currentFDefParameter = fDefParams.head.entry match {
          case DeclParameterDeclList(paramDecls) => paramDecls.flatMap {
            case Opt(ft, p: ParameterDeclarationD) => Some(Opt(ft, p.decl.getId))
            case _ => None
          }
          case missed =>
            throw new IllegalArgumentException("No rule defined for converting parameter to parameter mapping: " + missed)
        }

        // TODO Variability Check
        // TODO Expr other than id
        val currRes = currentFDefParameter.map(currFDefParam => {
          currentCallParameter.entry match {
            case i: Id => Opt(currentCallParameter.condition.and(currFDefParam.condition), (i, currFDefParam.entry))
            case missed =>
              throw new IllegalArgumentException("No rule defined for converting expression to parameter mapping: " + missed)
          }
        })

        mapCallParamToFDefParam(callParams.tail, fDefParams.tail, currRes ::: res)
      }

      val callEnv = interproceduralCFG().nodeToEnv(callStmt)
      val fCallOpt = parentOpt(callStmt, callEnv)
      val fCall = filterAllASTElems[FunctionCall](callStmt, callEnv).head // TODO Check if != 1
      val callExprs = fCall.params.exprs
      val fDefParams = destinationMethod.declarator.extensions
      val callParamToFDefParams = mapCallParamToFDefParam(callExprs, fDefParams)

      new FlowFunction[InfoFlowFact] {
        override def computeTargets(source: InfoFlowFact): util.Set[InfoFlowFact] = {
          source match {
            case s@Source(x, _, _) => {
              callParamToFDefParams.find(callParamToFDefParam => x.entry.name.equalsIgnoreCase(callParamToFDefParam.entry._1.name)) match {
                case None => KILL
                case Some(hit) => GEN(Source(Opt(hit.condition, hit.entry._2), fCallOpt, List(s)))
              }
            }
            case _ => KILL
          }
        }
      }
    }

    /**
      * Returns the flow function that computes the flow for a an exit from a
      * method. An exit can be a return or an exceptional exit.
      *
      * @param callSite
      * One of all the call sites in the program that called the
      * method from which the exitStmt is actually returning. This
      * information can be exploited to compute a value that depends on
      * information from before the call.
      * <b>Note:</b> This value might be <code>null</code> if
      * using a tabulation problem with { @link IFDSTabulationProblem#followReturnsPastSeeds()}
      * returning <code>true</code> in a situation where the call graph
      * does not contain a caller for the method that is returned from.
      * @param calleeMethod
      * The method from which exitStmt returns.
      * @param exitStmt
      * The statement exiting the method, typically a return or throw
      * statement.
      * @param returnSite
      * One of the successor statements of the callSite. There may be
      * multiple successors in case of possible exceptional flow. This
      * method will be called for each such successor.
      * <b>Note:</b> This value might be <code>null</code> if
      * using a tabulation problem with { @link IFDSTabulationProblem#followReturnsPastSeeds()}
      * returning <code>true</code> in a situation where the call graph
      * does not contain a caller for the method that is returned from.
      * @return
      */
    override def getReturnFlowFunction(callSite: AST, calleeMethod: FunctionDef, exitStmt: AST, returnSite: AST): FlowFunction[InfoFlowFact] = {
      new FlowFunction[InfoFlowFact] {
        override def computeTargets(source: InfoFlowFact): util.Set[InfoFlowFact] = {
          GEN(source)
        }
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
      println("CURRRRRRRR" +  curr)
      println("SUCCC" + succ)
      println(icfg.getSuccsOf(succ))

      new FlowFunction[InfoFlowFact] {
        val currOpt = parentOpt(curr, icfg.nodeToEnv(curr))
        val currDefs = defines(curr)
        val currUses = uses(curr)

        override def computeTargets(source: InfoFlowFact): util.Set[InfoFlowFact] = {
          source match {
            case Zero if currDefs.nonEmpty => {
              val res: List[InfoFlowFact] = currDefs.map(id => Source(Opt(currOpt.condition, id), currOpt))
              GEN(res)
            }
            case s@Source(x, _, _) if currDefs.exists(_.name.equalsIgnoreCase(x.entry.name)) => KILL // TODO Validate
            case s@Source(x, _, _) if currUses.nonEmpty =>
              currUses.find(_.name.equalsIgnoreCase(x.entry.name)) match {
                case None => GEN(s)
                case Some(i: Id) => GEN(Sink(Opt(interproceduralCFG().nodeToEnv(i).featureExpr(i), i), curr, List(s)))
              } // TODO Clean VAA
            case s: Sink => KILL
            case _ => GEN(source)
          }
        }
      }
    }

    override def getCallToReturnFlowFunction(n: AST, n1: AST): FlowFunction[InfoFlowFact] = {
      new FlowFunction[InfoFlowFact] {
        override def computeTargets(source: InfoFlowFact): util.Set[InfoFlowFact] = {
          // println("SourcebacktoReturnFlow: " + source)
          GEN(source)
        }
      }
    }
  }
}

