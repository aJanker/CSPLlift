package de.fosd.typechef.spllift

import de.fosd.typechef.featureexpr.FeatureModel
import de.fosd.typechef.featureexpr.bdd.BDDFeatureModel
import de.fosd.typechef.parser.c._
import de.fosd.typechef.spllift.analysis.{Reach, Source}
import heros.IFDSTabulationProblem
import soot.spl.ifds.{Constraint, FeatureModelContext, SPLIFDSSolver}

import scala.collection.JavaConverters._

class CSPLliftFrontend(startTunit: TranslationUnit, fm: FeatureModel = BDDFeatureModel.empty, options: CSPLliftOptions = DefaultCSPLliftOptions, dbg: Boolean = false) extends ASTNavigation {

    private val cintercfg = new CInterCFG(startTunit, fm, options)

    def solve[D](problem: IFDSTabulationProblem[AST, D, FunctionDef, CInterCFG], fmContext: FeatureModelContext = new FeatureModelContext())
    : List[(AST, List[(Constraint[_], D)])] = {
        def sourceToString(sources: List[Source]): List[String] = {
            sources.flatMap(src => PrettyPrinter.print(src.stmt.entry.asInstanceOf[AST]) :: sourceToString(src.reachingSources.toList))
        }

        val solver = new SPLIFDSSolver(problem, fmContext, false)
        solver.solve()

        val allReaches = solver.getAllResults.asScala.distinct
        val allSinks = allReaches.foldLeft(Map[AST, List[(Constraint[_], D)]]()) {
            case (m, result) => result.asScala.foldLeft(m) {
                case (lm, x) => x._1 match {
                    case r: Reach if isSink(r) =>
                        val key = r.to.entry
                        if (lm.contains(key)) lm + (key -> (x.swap :: lm.get(key).get))
                        else lm + (key -> List(x.swap))
                    case _ => lm
                }
            }
        }.toList.distinct

        if (dbg) {
            allReaches.foreach(_.asScala.foreach(x => x._1 match {
                case s: Reach => println("Reach: " + s.to.entry + " under condition  (" + x._2 + ") from " + s.from + "\nsources: " + s.sources + "\n")
                case _ =>
            }))

            allSinks.foreach(sink => {
                println("Sink at: \t" + sink._1)
                sink._2.foreach(ssink => {
                    println("\tCFGcondition " + ssink._1 + "\t" + ssink._2)
                })
                println()
            })

            println("######### END OF DBG PRINT ############\n")
        }



        allSinks.foreach(sink => {
            println("Sink at: \t" + sink._1)
            sink._2.take(sink._2.size / 2).foreach(ssink => {
                println("\tCFGcondition " + ssink._1 + "\t" + ssink._2)
            })
            println()

        })
        allSinks

        // TODO CLEANUP
    }

    private def isSink(r: Reach): Boolean = {
        r.to.entry match {
            case ExprStatement(AssignExpr(Id(name), _, _)) if name.equalsIgnoreCase("returnSite") => true
            case _ => false
        }
    }

    def getCInterCFG = cintercfg

}
