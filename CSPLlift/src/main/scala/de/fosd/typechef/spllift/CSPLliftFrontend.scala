package de.fosd.typechef.spllift

import de.fosd.typechef.featureexpr.FeatureModel
import de.fosd.typechef.featureexpr.bdd.BDDFeatureModel
import de.fosd.typechef.parser.c._
import de.fosd.typechef.spllift.analysis.{Reach, Source}
import heros.IFDSTabulationProblem
import soot.spl.ifds.{Constraint, FeatureModelContext, SPLIFDSSolver}

import scala.collection.JavaConverters._
import scala.collection.mutable

class CSPLliftFrontend(startTunit: TranslationUnit, fm: FeatureModel = BDDFeatureModel.empty, options: CSPLliftOptions = DefaultCSPLliftOptions) extends ASTNavigation {

  private val cintercfg = new CInterCFG(startTunit, fm, options)

  def solve[D](problem: IFDSTabulationProblem[AST, D, FunctionDef, CInterCFG]): List[(Statement, mutable.Map[D, Constraint[String]])] = {
    val fmContext = new FeatureModelContext()
    val solver = new SPLIFDSSolver(problem, fmContext, false)
    solver.solve()

    def sourceToString(sources: List[Source]): List[String] = {
      sources.flatMap(src => PrettyPrinter.print(src.stmt.entry.asInstanceOf[AST]) :: sourceToString(src.reachingSources))
    }

    solver.getAllResults.asScala.distinct.foreach(entry => {
      entry.asScala.foreach(x => {
        x._1 match {
          case s: Reach => {
            val sources = sourceToString(s.sources)
            println("Reach: "+ s.to.entry +" under condition  (" + x._2 + ") from " + s.from + "\nsources: " + s.sources + "\n")
          }
          case _ =>
        }
      })
    })
    List()
    // TODO Cleaner Solution
  }

  def getCInterCFG = cintercfg

}
