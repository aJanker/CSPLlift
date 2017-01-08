package de.fosd.typechef.cspllift.analysis

import java.io.{File, FileWriter}

import de.fosd.typechef.cmodule.CModule
import de.fosd.typechef.cspllift._
import de.fosd.typechef.cspllift.cifdsproblem.informationflow.InformationFlowProblem
import de.fosd.typechef.cspllift.cifdsproblem.informationflow.flowfact.InformationFlowFact
import de.fosd.typechef.cspllift.cifdsproblem.informationflow.flowfact.sinkorsource.{Sink, SourceDefinition}
import de.fosd.typechef.cspllift.cifdsproblem.informationflow.globalsources.GlobalSourcesProblem
import de.fosd.typechef.cspllift.cintercfg.{CInterCFG, CInterCFGNode, DefaultCInterCFGConfiguration}
import de.fosd.typechef.cspllift.options.CSPLliftOptions
import de.fosd.typechef.customization.StopWatch
import de.fosd.typechef.featureexpr.FeatureExpr
import de.fosd.typechef.parser.c.AST
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.GenTraversable


object TaintCheck {

    private lazy val logger: Logger = LoggerFactory.getLogger(getClass)

    private val taintedSource = "key" // TODO External Parameter

    def checkAES(env: CModule, opt: CSPLliftOptions, cInterCFGConfiguration: DefaultCInterCFGConfiguration): Unit = {
        val cInterCFG = new CInterCFG(env, cInterCFGConfiguration)
        val solution = check(cInterCFG)
        val allSinks = InformationFlow.allSinks(solution)

        val taintedSinks = getSinksFromTaintedSource(taintedSource, allSinks)
        val sinksWithTaintedSource = getSinksWithTaintedSourceEntry(taintedSinks, taintedSource)

        writeReachingGraph(taintedSinks, cInterCFG, opt.getInformationFlowGraphsOutputDir, "_reach.flow.dot")
        writeStmtFlowGraph(taintedSinks, cInterCFG, opt.getInformationFlowGraphsOutputDir)
        writeTaintFlowGraph(sinksWithTaintedSource, cInterCFG, opt.getInformationFlowGraphsOutputDir)

        if (opt.isIFDSPrintExplodedSuperCallGraphEnabled)
            writeExplodedSuperCallGraph(opt)

        logger.info("Static taint analysis with spllift - result")

        logger.info("Sinks:\t" + taintedSinks.size)
        //logger.info(InformationFlow.prettyPrintSinks(taintedSinks))
    }

    def checkAll(env: CModule, opt: CSPLliftOptions, cInterCFGConfiguration: DefaultCInterCFGConfiguration): Unit = {

        val cInterCFG = new CInterCFG(env)

        val solution = check(cInterCFG)

        if (opt.isIFDSPrintExplodedSuperCallGraphEnabled)
            writeExplodedSuperCallGraph(opt)

        val allSinks = InformationFlow.allSinks(solution)

        writeReachingGraph(allSinks, cInterCFG, opt.getInformationFlowGraphsOutputDir, "_reach.flow.dot")
        writeStmtFlowGraph(allSinks, cInterCFG, opt.getInformationFlowGraphsOutputDir)
        writeInfoFlowGraph(allSinks, cInterCFG, opt.getInformationFlowGraphsOutputDir)

        logger.info("Static taint analysis result")

        logger.info("Sinks:\t" + allSinks.size)
        // logger.info(InformationFlow.prettyPrintSinks(allSinks))

        logger.info("Used tunits number:\t" + env.getAllKnownTUnits.size)
    }

    def check(cInterCFG : CInterCFG) : List[LiftedCFlowFact[InformationFlowFact]] = {
        val (runtime, solution) = StopWatch.measureProcessCPUTime("IFDS-Solve", {
            val seeds = new GlobalSourcesProblem(cInterCFG)
            CSPLlift.solveAndCollectResults(seeds)

            val problem = new InformationFlowProblem(cInterCFG, seeds.getGlobalSources)
            CSPLlift.solveAndCollectResults(problem)
        })
        logger.info("Finished IFDS Solving in " + runtime + "ms")
        solution
    }

    private def getSinksFromTaintedSource(taintedSource: String, sinks: GenTraversable[StmtFlowFacts[Sink]]): GenTraversable[StmtFlowFacts[Sink]] =
        sinks.par.filter(_._2.exists(sink => containsName(sink._1, taintedSource)))

    private def getSinksAtTaintedSink(taintedSink: String, sinks: GenTraversable[StmtFlowFacts[Sink]]): GenTraversable[StmtFlowFacts[Sink]] =  List()

    private def getSinksFromTaintedSource(taintedSource: AST, sinks: GenTraversable[StmtFlowFacts[Sink]]): GenTraversable[StmtFlowFacts[Sink]] =
        sinks.par.filter(_._2.exists(sink => containsAST(sink._1, taintedSource)))

    private def getSinksAtTaintedSink(taintedSink: AST, sinks: GenTraversable[StmtFlowFacts[Sink]]): GenTraversable[StmtFlowFacts[Sink]] =
        sinks.par.filter {
            case (stmt, _) => stmt.get.equals(taintedSink)
        }

    private def getSinksWithTaintedSourceEntry(taintedSinks: GenTraversable[(CInterCFGNode, List[(Sink, FeatureExpr)])], taintedSource: String) =
        taintedSinks.par.flatMap(_._2).filter(s => containsName(s._1, taintedSource))

    private def containsName(sink : Sink, name : String) : Boolean = {
        val sourceName = sink.getOriginId.name
        sourceName.equalsIgnoreCase(name)
    }

    private def containsAST(sink : Sink, ast : AST) : Boolean = {
        val sourceAST = sink.getOriginSource.getCIFGStmt.get
        sourceAST.equals(ast)
    }

    private def writeStmtFlowGraph(result: GenTraversable[StmtFlowFacts[Sink]], cICFG : CInterCFG, outputDir: String, name : String = "", fileExtension: String = ".dot"): Unit = {
        val sinks = result.toList.flatMap(_._2).filter(p => p._1.source match {
            case _ : SourceDefinition => true
            case _ => false
        })

        writeFlowGraph(sinks, cICFG, outputDir, "stmtFlow", fileExtension)
    }

    private def writeTaintFlowGraph(result: GenTraversable[(Sink, FeatureExpr)], cICFG : CInterCFG, outputDir: String): Unit = writeFlowGraph(result.toList, cICFG, outputDir, "taint")

    private def writeInfoFlowGraph(result: GenTraversable[StmtFlowFacts[Sink]], cICFG : CInterCFG, outputDir: String, name : String = "", fileExtension: String = ".dot"): Unit = {
        val sinks = result.toList.flatMap(_._2)

        writeFlowGraph(sinks, cICFG, outputDir, "infoFlow", fileExtension)
    }

    private def writeReachingGraph(result: GenTraversable[StmtFlowFacts[Sink]], cICFG : CInterCFG, outputDir: String, fileExtension: String = ".dot"): Unit = {
        checkDir(outputDir)

        result.par.zipWithIndex.foreach {
            case (sinks, i) =>
                val writer = new InformationFlowGraphWriter(new FileWriter(new File(outputDir + "/" + i + fileExtension)))
                writer.writeHeader()

                // write nodes first
                val sourceNodes = sinks._2.flatMap(sink => {
                    val stmts = List(sink._1.cfgNode, sink._1.getOriginSource.getCIFGStmt)
                    stmts.map(getNode(_, cICFG))
                }).distinct
                sourceNodes.foreach(writer.writeNode)

                val edges = sinks._2.map(sink => getEdge(sink._1.getOriginSource.getCIFGStmt, sink._1.cfgNode, sink._2, cICFG)).distinct
                edges.foreach(writer.writeEdge)

                writer.writeFooter()
                writer.close()
        }
    }

    private def writeFlowGraph(sinks: List[(Sink, FeatureExpr)], cICFG: CInterCFG, outputDir: String, name: String = "", fileExtension: String = ".dot"): Unit = {
        checkDir(outputDir)

        val writer = new InformationFlowGraphWriter(new FileWriter(new File(outputDir + "/" + name + fileExtension)))
        writer.writeHeader()

        // write nodes first
        val sourceNodes = sinks.flatMap(sink => {
            val stmts = List(sink._1.cfgNode, sink._1.source.getCIFGStmt)
            stmts.map(getNode(_, cICFG))
        }).distinct
        sourceNodes.foreach(writer.writeNode)

        val edges = sinks.map(sink => getEdge(sink._1.getOriginSource.getCIFGStmt, sink._1.cfgNode, sink._2, cICFG)).distinct
        edges.foreach(writer.writeEdge)

        writer.writeFooter()
        writer.close()
    }

    /**
      * Retrieves the original presence condition in the ast, not the dataflow condition
      */
    private def getNode(x: CInterCFGNode, icfg: CInterCFG): Node[AST] = Node(icfg.getCFGPresenceNode(x.get))

    private def getEdge(f: CInterCFGNode, t: CInterCFGNode, icfg: CInterCFG): Edge[AST] = getEdge(f, t, icfg.getSuccFlowCondition(f, t), icfg)

    private def getEdge(f: CInterCFGNode, t: CInterCFGNode, flowCondition: FeatureExpr, icfg: CInterCFG): Edge[AST] =
        Edge(getNode(f, icfg), getNode(t, icfg), flowCondition)

    private def writeExplodedSuperCallGraph(opt: CSPLliftOptions) : Unit = {
        val graphDir = opt.getInformationFlowGraphsOutputDir
        val dir = new File(graphDir)

        if (!(dir.exists() && dir.isDirectory)) dir.mkdirs()

        SuperCallGraph.write(new InformationFlowGraphWriter(new FileWriter(graphDir + "/callGraph.dot")))

        SuperCallGraph.clear()
    }

    private def checkDir(outputDir: String): Boolean = {
        val dir = new File(outputDir)
        if (!dir.exists()) dir.mkdirs()
        else dir.isDirectory
    }
}
