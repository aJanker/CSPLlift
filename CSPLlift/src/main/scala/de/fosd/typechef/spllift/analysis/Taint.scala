package de.fosd.typechef.spllift.analysis

import java.io.{File, FileWriter, StringWriter, Writer}

import de.fosd.typechef.conditional.{One, Opt}
import de.fosd.typechef.parser.c._
import de.fosd.typechef.spllift._
import de.fosd.typechef.spllift.cifdsproblem.{InformationFlow, Reach, Source}
import soot.spl.ifds.Constraint

object Taint {

    def allSinks(solverResult: List[LiftedCFlowFact[InformationFlow]]) = matchReaches(solverResult, r => true)

    def findSinks(solverResult: List[LiftedCFlowFact[InformationFlow]], isSink: Reach => Boolean) = matchReaches(solverResult, isSink)

    def allSources(solverResult: List[LiftedCFlowFact[InformationFlow]]) = matchSources[Source](solverResult, s => true)

    def prettyPrintSinks(sinks: Traversable[(AST, List[LiftedCFlowFact[Reach]])]): String = prettyPrintSinks(sinks, new StringWriter).toString

    def prettyPrintSinks(sinks: Traversable[(AST, List[LiftedCFlowFact[Reach]])], writer: Writer): Writer =
        sinks.foldLeft(writer) {
            (writer, sink) => {
                sink._1 match {
                    case f: ForStatement => writer.append("Sink at:\t" + PrettyPrinter.print(f.copy(s = One(EmptyStatement()))) + "\tin:\t" + sink._1.getPositionFrom + "\n")
                    case x => writer.append("Sink at:\t" + PrettyPrinter.print(x) + "\tin:\t" + sink._1.getPositionFrom + "\n")
                }

                sink._2.foreach { ssink => writer.append("CFGcondition " + ssink._2 + ":\t" + ssink._1.toText + "\n") }
                writer.append("\n")
            }
        }

    def writeGraphFromSource(icfg: CInterCFG, sink: Traversable[(AST, List[LiftedCFlowFact[Reach]])], outputDir: String, extension: String) = {
        val sinks = sink.toList
        val uniqueSources = sinks.toList.par.flatMap(sink => sink._2.flatMap(reach => reach._1.sources)).distinct.toList

        val sourceToSink = uniqueSources.par.map(source => {
            (source, sinks.flatMap(sink => sink._2.filter(reach => reach._1.sources.exists(_.equals(source)))))
        })

        sourceToSink.zipWithIndex.foreach{
            case (x, i) =>
                val source = x._1
                val sinks = x._2

                val writer = new InformationFlowGraphWriter(new FileWriter(new File(outputDir + "/" + i + extension)))
                writer.writeHeader()

                val allUniqueSourceNodes = sinks.flatMap(reach => reach._1.to :: reach._1.from).map(getNode(_, icfg)).distinct
                allUniqueSourceNodes.foreach(writer.writeNode)

                val allUniqueDataFlowEdges = sinks.flatMap(getEdges(_, icfg)).distinct
                allUniqueDataFlowEdges.foreach(writer.writeEdge)

                writer.writeFooter()
                writer.close()
        }
    }

    def writeGraphToSink(icfg: CInterCFG, sinks: Traversable[(AST, List[LiftedCFlowFact[Reach]])], outputDir: String, extension: String) = {

        val dir = new File(outputDir)
        if (!dir.exists()) dir.mkdirs()

        sinks.par.zipWithIndex.foreach {
            case (sink, i) =>
                val writer = new InformationFlowGraphWriter(new FileWriter(new File(outputDir + "/" + i + extension)))

                writer.writeHeader()

                // write nodes first
                val allUniqueSourceNodes = sink._2.flatMap(reach => reach._1.to :: reach._1.from).map(getNode(_, icfg)).distinct
                allUniqueSourceNodes.foreach(writer.writeNode)

                val allUniqueDataFlowEdges = sink._2.flatMap(getEdges(_, icfg)).distinct
                allUniqueDataFlowEdges.foreach(writer.writeEdge)

                writer.writeFooter()
                writer.close()
        }
    }

    /**
      * Retrieves the original presence condition in the ast, not the dataflow condition
      */
    private def getNode(x: Opt[AST], icfg: CInterCFG): Node[AST] =
        icfg.getEnv(x.entry) match {
            case Some(env) => Node(Opt(env.featureExpr(x.entry), x.entry))
            case _ => Node(x)
        }

    private def getEdge(f: Opt[AST], t: Opt[AST], icfg: CInterCFG): Edge[AST] =
        Edge(getNode(f, icfg), getNode(t, icfg), f.condition.and(t.condition))

    private def getEdges(reach: LiftedCFlowFact[Reach], icfg: CInterCFG): List[Edge[AST]] = {
        val from = reach._1.from.reverse

        from.foldLeft((from, List[Edge[AST]]()))((x, curr) => {
            val (r, edges) = x
            val remaining = r.tail
            val edge = getEdge(curr, remaining.headOption.getOrElse(reach._1.to), icfg)
            (remaining, edge :: edges)
        })._2
    }


    private def matchSources[S <: Source](solverResult: List[LiftedCFlowFact[InformationFlow]], isMatch: S => Boolean)(implicit m: Manifest[S]): Map[Opt[Id], List[LiftedCFlowFact[S]]] =
        solverResult.foldLeft(Map[Opt[Id], List[LiftedCFlowFact[S]]]()) {
            case (rm, x@(s: S, c: Constraint)) if isMatch(s) =>
                val key = s.name
                val v = x.asInstanceOf[LiftedCFlowFact[S]]
                rm + (key -> (v :: rm.getOrElse(key, List(v))).distinct)
            case (rm, _) => rm
        }

    private def matchReaches(solverResult: List[LiftedCFlowFact[InformationFlow]], isMatch: Reach => Boolean): Map[AST, List[LiftedCFlowFact[Reach]]] =
        solverResult.foldLeft(Map[AST, List[LiftedCFlowFact[Reach]]]()) {
            case (m, x@(r: Reach, c: Constraint)) if isMatch(r) =>
                val key = r.to.entry
                val v = x.asInstanceOf[LiftedCFlowFact[Reach]]
                m + (key -> (v :: m.getOrElse(key, List(v))).distinct)
            case (m, _) => m
        }
}
