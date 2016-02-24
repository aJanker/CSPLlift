package de.fosd.typechef.spllift

import de.fosd.typechef.parser.c._
import de.fosd.typechef.spllift.analysis.{Sink, Source}
import heros.IFDSTabulationProblem
import soot.spl.ifds.{Constraint, FeatureModelContext, SPLIFDSSolver}

import scala.collection.JavaConverters._
import scala.collection.mutable

class CSPLliftFrontend(startTunit: TranslationUnit, options: CSPLliftOptions = DefaultCSPLliftOptions) extends ASTNavigation {

  private val cintercfg = new CInterCFG(startTunit, options)

  def solve[D](problem: IFDSTabulationProblem[AST, D, FunctionDef, CInterCFG]): List[(Statement, mutable.Map[D, Constraint[String]])] = {
    val fmContext = new FeatureModelContext()
    val solver = new SPLIFDSSolver(problem, fmContext, false)
    solver.solve()

    def sourceToString(sources: List[Source]): List[String] = {
      sources.flatMap(src => PrettyPrinter.print(src.stmt.entry.asInstanceOf[AST]) :: sourceToString(src.linkedSources))
    }

    solver.getAllResults.asScala.distinct.foreach(entry => {
      entry.asScala.foreach(x => {
        x._1 match {
          case s: Sink => {
            println("Debug: " + x._1 + " @ " + x._2)


            /*val sources = s.sources.map(src => PrettyPrinter.print(src.stmt.entry.asInstanceOf[AST]))
            val sources = s.sources.map(src => PrettyPrinter.print(src.linkedSources)) */
            val sources = sourceToString(s.sources)
            println("Sink in under condition  (" + x._2 + ") at " + PrettyPrinter.print(s.stmt) + " from " + sources)
          }
          case _ => {
            // println(x._1 + " @ " + x._2)
          }
        }
      })
    })
    List()
    // TODO Cleaner Solution
    /** filterAllASTElems[Statement](startTunit).filterNot {
      * case c: CompoundStatement => true
      * case _ => false
      * }.flatMap(statement => {
      * val result = solver.resultsAt(statement).asScala

      * //println("###\t" + PrettyPrinter.print(statement))
      * //println(statement)
      * if (result.nonEmpty) {
      * result.foreach(x => {
      * x._1 match {
      * case s: Sink => {
      * println(x._1 + " @ " + x._2)
      * }
      * case _ => {
      * // println(x._1 + " @ " + x._2)
      * }
      * }

      * })

      * Some((statement, result))
      * }
      * else None
      * }) */
  }

  def getCInterCFG = cintercfg

}
