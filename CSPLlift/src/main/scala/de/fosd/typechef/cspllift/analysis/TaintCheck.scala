package de.fosd.typechef.cspllift.analysis

import java.io.{File, FileWriter}

import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.cspllift._
import de.fosd.typechef.cspllift.cifdsproblem.informationflow.InformationFlowProblem
import de.fosd.typechef.cspllift.cifdsproblem.informationflow.flowfact.sinkorsource.Sink
import de.fosd.typechef.cspllift.options.CSPLliftOptions
import de.fosd.typechef.customization.StopWatch
import de.fosd.typechef.featureexpr.FeatureModel
import de.fosd.typechef.parser.c.{AST, TranslationUnit}


object TaintCheck {

    def check(tunit: TranslationUnit, fm : FeatureModel, opt : CSPLliftOptions, cInterCFGConfiguration: DefaultCInterCFGConfiguration): Unit = {

        val cInterCFG = new CInterCFG(tunit, fm, cInterCFGConfiguration)

        val (_, (solution)) = StopWatch.measureUserTime("taint_lift", {
            val problem = new InformationFlowProblem(cInterCFG)
            CSPLlift.solve(problem, printWarnings = true)
        })

        if (opt.isLiftPrintExplodedSuperCallGraphEnabled)
            writeExplodedSuperCallGraph(opt)

        val allSinks = InformationFlow.allSinks(solution)

        writeResultGraphs(allSinks, cInterCFG, opt.getInformationFlowGraphsOutputDir, "_flow.dot")

        println("#static taint analysis with spllift - result")

        println("\n#sinks")
        println(InformationFlow.prettyPrintSinks(allSinks))

        println("\n#used tunits number:")
        println(cInterCFG.cInterCFGElementsCacheEnv.getAllKnownTUnits.size + "\n")
        println("#static taint analysis with spllift - finished")
    }

    def checkSource(taintedSource: String, sinks: Traversable[StmtFlowFacts[Sink]]): Traversable[StmtFlowFacts[Sink]] = {
        List()
    }

    def checkSink(taintedSink: String, sinks: Traversable[StmtFlowFacts[Sink]]): Traversable[StmtFlowFacts[Sink]] = {
        List()
    }

    def checkSource(taintedSource: AST, sinks: Traversable[StmtFlowFacts[Sink]]): Traversable[StmtFlowFacts[Sink]] = {
        List()
    }

    def checkSink(taintedSink: AST, sinks: Traversable[StmtFlowFacts[Sink]]): Traversable[StmtFlowFacts[Sink]] = {
        List()
    }

    private def writeResultGraphs(result: Traversable[StmtFlowFacts[Sink]], cICFG : CInterCFG, outputDir: String, fileExtension: String = ""): Unit = {
        val dir = new File(outputDir)
        if (!dir.exists()) dir.mkdirs()

        result.par.zipWithIndex.foreach {
            case (sinks, i) =>
                val writer = new InformationFlowGraphWriter(new FileWriter(new File(outputDir + "/" + i + fileExtension)))
                writer.writeHeader()

                // write nodes first
                val sourceNodes = sinks._2.flatMap(sink => {
                    val stmts = List(sink._1.cICFGStmt, sink._1.source.getCIFGStmt)
                    stmts.map(getNode(_, cICFG))
                }).distinct
                sourceNodes.foreach(writer.writeNode)

                val edges = sinks._2.map(sink => getEdge(sink._1.source.getCIFGStmt, sink._1.cICFGStmt, cICFG)).distinct
                edges.foreach(writer.writeEdge)

                writer.writeFooter()
                writer.close()
        }
    }

    /**
      * Retrieves the original presence condition in the ast, not the dataflow condition
      */
    private def getNode(x: CICFGStmt, icfg: CInterCFG): Node[AST] = {
        val stmt = x.getStmt
        icfg.getEnv(stmt.entry) match {
            case Some(env) => Node(Opt(env.featureExpr(stmt.entry), stmt.entry))
            case _ => Node(stmt)
        }
    }

    private def getEdge(f: CICFGStmt, t: CICFGStmt, icfg: CInterCFG): Edge[AST] =
        Edge(getNode(f, icfg), getNode(t, icfg), f.getCondition.and(t.getCondition))

    private def writeExplodedSuperCallGraph(opt: CSPLliftOptions) : Unit = {
        val graphDir = opt.getInformationFlowGraphsOutputDir
        val dir = new File(graphDir)

        if (!(dir.exists() && dir.isDirectory)) dir.mkdirs()

        SuperCallGraph.write(new InformationFlowGraphWriter(new FileWriter(graphDir + "/callGraph.dot")))

        SuperCallGraph.clear()
    }
}
