package de.fosd.typechef.cspllift.evaluation

import java.io._

import de.fosd.typechef.cspllift._
import de.fosd.typechef.cspllift.analysis.{InformationFlow, InformationFlowGraphWriter, SuperCallGraph}
import de.fosd.typechef.cspllift.cifdsproblem.informationflow.InformationFlowProblem
import de.fosd.typechef.cspllift.cifdsproblem.informationflow.flowfact.InformationFlowFact
import de.fosd.typechef.cspllift.cifdsproblem.informationflow.globalsources.GlobalSourcesProblem
import de.fosd.typechef.cspllift.cifdsproblem.{CFlowFact, CIFDSProblem}
import de.fosd.typechef.cspllift.cintercfg._
import de.fosd.typechef.cspllift.commons.{CInterCFGCommons, ConditionTools}
import de.fosd.typechef.cspllift.options.CSPLliftOptions
import de.fosd.typechef.customization.StopWatch
import de.fosd.typechef.featureexpr.bdd.BDDFeatureModel
import de.fosd.typechef.featureexpr.{FeatureExpr, FeatureModel}
import de.fosd.typechef.parser.c.{PrettyPrinter, TranslationUnit}
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.concurrent.TrieMap

class CSPLliftEvaluationFrontend(ast: TranslationUnit, fm: FeatureModel = BDDFeatureModel.empty, options: CSPLliftOptions) extends ConditionTools with CInterCFGCommons {

    private lazy val logger: Logger = LoggerFactory.getLogger(getClass)

    private val FILE_EXTENSION = ".lift"

    private val CODECOVERAGE = "codeCoverage"

    private val CONDITIONCOVERAGE = "conditionCoverage"

    private val RUN_MARK = "RUN"

    private var warmJVM = false

    def evaluate(): Boolean = {
        var successful = true

        if (options.isLiftSamplingEvaluationEnabled)
            successful = checkAgainstSampling() && successful

        if (options.isLiftSingleEvaluationEnabled)
            successful = checkAgainstErrorConfiguration() && successful

        successful
    }


    def checkAgainstSampling(): Boolean = {
        var successful = true

        if (options.liftTaintAnalysis)
            successful = runSampling[InformationFlowFact, InformationFlowProblem](classOf[InformationFlowProblem]) && successful

        successful
    }

    def checkAgainstErrorConfiguration(): Boolean = {
        var successful = true

        if (options.liftTaintAnalysis)
            successful = runErrorConditionCoverage[InformationFlowFact, InformationFlowProblem](classOf[InformationFlowProblem]) && successful

        successful
    }

    private def analyzeConfigsAndCompare[D <: CFlowFact, T <: CIFDSProblem[D]](ifdsProblem: java.lang.Class[T], configs: List[SimpleConfiguration], liftedFacts: List[LiftedCFlowFact[D]], strategy: String = "") = {
        var matchedLiftedFacts = scala.collection.concurrent.TrieMap[LiftedCFlowFact[D], Int]()
        val liftedEvalFacts = liftedFacts.par.filter(_._1.isEvaluationFact).toList

        liftedEvalFacts.foreach(fact => matchedLiftedFacts += (fact -> 0))

        val res = configs.zipWithIndex.flatMap {
            case (config, i) => analyzeSingleConfigAndCompare(configs, strategy, ifdsProblem, matchedLiftedFacts, liftedEvalFacts, config, i)
        }

        val unmatchedLiftedFacts = matchedLiftedFacts.par.collect { case ((x, 0)) => x }.toList
        (unmatchedLiftedFacts.distinct, res)
    }

    private def analyzeSingleConfigAndCompare[T <: CIFDSProblem[D], D <: CFlowFact](configs: List[SimpleConfiguration], strategy: String, ifdsProblem: Class[T], matchedLiftedFacts: TrieMap[(D, FeatureExpr), Int], liftedEvalFacts: List[(D, FeatureExpr)], config: SimpleConfiguration, i: Int): Iterable[(List[(D, FeatureExpr)], SimpleConfiguration)] = {
        val run: String = if (strategy.equalsIgnoreCase("")) i.toString else strategy + "_" + i

        logger.info("Starting run:\t" + (i + 1) + " of " + configs.size)
        logger.info(config.toString)

        val cInterCFGOptions = new ConfigurationBasedCInterCFGConfiguration(options.getCLinkingInterfacePath, options.resolveFunctionPointer, Some(config), run)
        val (wallTime, (solution, icfg)) = runSPLLift[D, T](ifdsProblem, cInterCFGOptions, Some(run + "_"))

        if (options.writeVariants) writeVariants(icfg, strategy, Some(i), Some(config))
        if (options.isLiftPrintExplodedSuperCallGraphEnabled) writeExplodedSuperCallGraph(strategy, Some(run))

        val sampleEvalFacts = solution.par.filter(_._1.isEvaluationFact)
        val satLiftedEvalFacts = liftedEvalFacts.par.filter(fact => isSatisfiableInConfiguration(fact._2, config)).toList

        val unmatchedSampleEvalFacts = sampleEvalFacts.filterNot(fact => satLiftedEvalFacts.foldLeft(false)((found, oFact) =>
            if (oFact._1.isEquivalentTo(fact._1, config)) {
                // is match, increase vaa match counter
                matchedLiftedFacts += (oFact -> (matchedLiftedFacts.getOrElse(oFact, 0) + 1))
                true
            } else found
        )).toList

        if (unmatchedSampleEvalFacts.nonEmpty) Some((unmatchedSampleEvalFacts, config))
        else None
    }

    private def runSampling[D <: CFlowFact, T <: CIFDSProblem[D]](ifdsProblem: java.lang.Class[T]): Boolean = {
        val method: String = "codeCoverage"

        // 1. Step -> Run VAA first in order to detect all linked files for codecoverageconfiguration generation
        val cInterCFGOptions = new DefaultCInterCFGConfiguration(options.getCLinkingInterfacePath, options.resolveFunctionPointer)
        // warm up run for the jvm
        warmupJVM[D, T](ifdsProblem, cInterCFGOptions)

        val (vaaUserTime, (liftedFacts, icfg)) = runSPLLift[D, T](ifdsProblem, cInterCFGOptions, Some(method + "_init_"))

        if (options.writeVariants) writeVariants(icfg, method)
        if (options.isLiftPrintExplodedSuperCallGraphEnabled) writeExplodedSuperCallGraph(method)

        // 2. Generate Code Coverage Configurations for all referenced files
        val sampling = new Sampling(icfg.cInterCFGElementsCacheEnv.getAllKnownTUnitsAsSingleTUnit, fm)
        val configs = sampling.codeCoverageConfigs()

        // 3. Run Analysis for every generated config
        // 4. Compare
        val (unmatchedLiftedFacts, unmatchedCoverageFacts) = analyzeConfigsAndCompare(ifdsProblem, configs, liftedFacts, method)

        logger.info("Tested " + configs.size + " unique variants for code coverage.")

        if (unmatchedLiftedFacts.nonEmpty)
            logger.info("Following results were not covered by the coverage approach:\t" + unmatchedLiftedFacts.size)

        if (unmatchedCoverageFacts.nonEmpty) {
            logger.info("Following results were not covered by the lifted approach:\t" + unmatchedCoverageFacts.foldLeft(0)((i, x) => x._1.size + i))

            unmatchedCoverageFacts.foreach(uc => {
                println("Configuration:\t" + uc._2)
                uc._1.foreach(uc2 => {
                    println("Error:\n" + uc2._1)
                    println
                    println(uc2._1.toText)
                })

                println("###\n")
            })
        } else logger.info("All results were covered by the lifted approach!")

        if (unmatchedCoverageFacts.isEmpty)
            sumResults[D](method, icfg, configs, liftedFacts.filter(_._1.isEvaluationFact), unmatchedLiftedFacts, unmatchedCoverageFacts)

        unmatchedCoverageFacts.isEmpty
    }

    private def runErrorConditionCoverage[D <: CFlowFact, T <: CIFDSProblem[D]](ifdsProblem: Class[T]): Boolean = {
        val method: String = "conditionCoverage"

        // 1. Step -> Run VAA first in order to detect all affected features
        val cInterCFGOptions = new DefaultCInterCFGConfiguration(options.getCLinkingInterfacePath, options.resolveFunctionPointer)
        warmupJVM[D, T](ifdsProblem, cInterCFGOptions)

        val (vaaUserTime, (liftedFacts, icfg)) = runSPLLift[D, T](ifdsProblem, cInterCFGOptions, Some(method + "_init_"))

        if (options.writeVariants) writeVariants(icfg, method)
        if (options.isLiftPrintExplodedSuperCallGraphEnabled) writeExplodedSuperCallGraph(method)

        // 2. Collect distinct conditions
        val cfgConditions = liftedFacts.foldLeft(Set[FeatureExpr]())((cfgConds, fact) => {
            val cfgCond = fact._2
            cfgConds + cfgCond
        })

        /* val sinks = InformationFlow.allSinks(liftedFacts.asInstanceOf[List[(InformationFlowFact, FeatureExpr)]])
        println(InformationFlow.prettyPrintSinks(sinks)) */

        // 3. Generate ConditionalEdgeFunction Coverage Configurations for all distinct warning conditions
        val sampling = new Sampling(icfg.cInterCFGElementsCacheEnv.getAllKnownTUnitsAsSingleTUnit, fm)
        val configs = sampling.conditionCoverageConfigs(cfgConditions.asInstanceOf[Set[FeatureExpr]])

        // 5. Compare
        val (unmatchedLiftedFacts, unmatchedCoverageFacts) = analyzeConfigsAndCompare(ifdsProblem, configs, liftedFacts, method)

        logger.info("Tested " + configs.size + " unique variants for condition coverage.")

        if (unmatchedLiftedFacts.nonEmpty) {
            logger.info("Following results were not covered by the condition coverage approach:\t" + unmatchedLiftedFacts.size + " of total: " + liftedFacts.count(_._1.isEvaluationFact))

            var conditions: List[FeatureExpr] = List()

            unmatchedLiftedFacts.foreach(uc => {
                conditions = uc._2 :: conditions
                println("Error:\n" + "\tCondition:" + uc._2 + "\n\t" + uc._1.toText)
            })

            println("#unique conditions")

            conditions.distinct.foreach(cond => {
                println(cond)
            })
        } else logger.info("All results were covered by the condition coverage approach!")

        if (unmatchedCoverageFacts.nonEmpty) {
            logger.info("Following results were not covered by the lifted approach:\t" + unmatchedCoverageFacts.foldLeft(0)((i, x) => x._1.size + i))

            unmatchedCoverageFacts.foreach(uc => {
                println("Configuration:\t" + uc._2)
                val all = InformationFlow.allSinks(uc._1.asInstanceOf[List[(InformationFlowFact, FeatureExpr)]])
                println(InformationFlow.prettyPrintSinks(all))
                println("###\n")
            })
        } else logger.info("All condition coverage results were covered by the lifted approach!")

        if (unmatchedLiftedFacts.isEmpty && unmatchedCoverageFacts.isEmpty)
            sumResults[D](method, icfg, configs, liftedFacts.filter(_._1.isEvaluationFact), unmatchedLiftedFacts, unmatchedCoverageFacts)

        unmatchedLiftedFacts.isEmpty && unmatchedCoverageFacts.isEmpty
    }

    private def warmupJVM[D <: CFlowFact, T <: CIFDSProblem[D]](ifdsProblem: Class[T], cInterCFGOptions: DefaultCInterCFGConfiguration): Unit =
        if (options.warmupJVM && !warmJVM) {
            logger.info("Warming up JVM.")
            runSPLLift[D, T](ifdsProblem, cInterCFGOptions, Some("warmup"))
            logger.info("Finished warmup.")
            warmJVM = true
        }

    private def runSPLLift[D <: CFlowFact, T <: CIFDSProblem[D]](ifdsProblem: Class[T], cInterCFGOptions: CInterCFGConfiguration, benchmarkTag: Option[String] = None): (Long, (List[LiftedCFlowFact[D]], CInterCFG)) =
        StopWatch.measureWallTime(benchmarkTag.getOrElse("") + RUN_MARK, {
            val cInterCFG = new CInterCFG(ast, fm, cInterCFGOptions, benchmarkTag)
            var problem = getCIFDSProblemInstance[D, T](ifdsProblem)(cInterCFG, List())

            if (!options.noInitalSeeds && problem.isInstanceOf[InformationFlowProblem]) {
                val seedCFG = new CInterCFG(ast, fm, cInterCFGOptions)
                val seeds = new GlobalSourcesProblem(seedCFG)
                CSPLlift.solve(seeds)
                logger.info("Computed initial seeds.")
                problem = new InformationFlowProblem(cInterCFG, seeds.getGlobalSources).asInstanceOf[T]
            }

            (CSPLlift.solve[D](problem, benchmarkTag = benchmarkTag), cInterCFG)
        })

    private def writeVariants[T <: CIFDSProblem[D], D <: CFlowFact](icfg: CInterCFG, method: String = "", index: Option[Int] = None, config: Option[SimpleConfiguration] = None) = {
        val printDir = options.getVariantsOutputDir + "/" + method + "/"
        val outputDir = if (index.isDefined) printDir + "/" + index.get else printDir

        val currDir = new File(outputDir)
        if (!(currDir.exists() && currDir.isDirectory)) currDir.mkdirs()

        if (config.isDefined) writeStringToGZipFile(config.get.toString, outputDir + "/config")

        icfg.cInterCFGElementsCacheEnv.getAllFiles.foreach {
            case (file, tunit) =>
                val variant = outputDir + "/" + getPlainFileNameS(file) + ".c"
                writeStringToGZipFile(PrettyPrinter.print(tunit), variant)
        }
    }

    private def writeExplodedSuperCallGraph(method: String, variant: Option[String] = None): Unit = {
        val graphDir = options.getInformationFlowGraphsOutputDir + "/" + method + "/"
        val dir = new File(graphDir)

        if (!(dir.exists() && dir.isDirectory)) dir.mkdirs()

        if (variant.isEmpty) SuperCallGraph.write(new InformationFlowGraphWriter(new FileWriter(graphDir + "/callGraph.dot")))
        else SuperCallGraph.write(new InformationFlowGraphWriter(new FileWriter(graphDir + "/" + variant.get + "_callGraph.dot")))

        SuperCallGraph.clear()
    }

    /**
      * Summarizes the evaluation result in case it was successful (no errors introduced by lifting)
      */
    private def sumResults[D](method: String, icfg: CInterCFG, configs: List[SimpleConfiguration], liftedFacts: List[(D, FeatureExpr)], unmatchedLiftedFacts: List[(D, FeatureExpr)], unmatchedCoverageFacts: List[(List[(D, FeatureExpr)], SimpleConfiguration)]) = {
        val location = options.getOutputLocation
        val extension = method + ".sum"
        val writer = new BufferedWriter(new FileWriter(location + extension))

        // Total Lifted Facts
        writer.write("#TOTAL_LIFTEDFACTS:\t" + liftedFacts.size + "\n")
        // Coverage
        writer.write("#SAMPLING_COVERAGE:\t" + (((liftedFacts.size - unmatchedLiftedFacts.size).toFloat / liftedFacts.size.toFloat) * 100).toInt + "\n")
        // Number of variants
        writer.write("#VARIANTS:\t" + configs.size + "\n")
        // Loaded files
        writer.write("#FILES:\t" + icfg.cInterCFGElementsCacheEnv.getAllKnownTUnits.size + "\n")
        // Timings
        val seperator = "_init_"

        val liftedRun = StopWatch.get(method + seperator + RUN_MARK)
        val liftedInit = StopWatch.get(method + seperator + CInterCFGBenchmarkMarks.TUNIT_INIT)
        val liftedLoad = StopWatch.get(method + seperator + CInterCFGBenchmarkMarks.TUNIT_LOAD)
        val liftedSolving = liftedRun - (liftedInit + liftedLoad)

        writer.write("#LIFTEDRUNTIME\t" + liftedRun + "\n")
        writer.write("#LIFTEDSOLVE\t" + liftedSolving + "\n")
        writer.write("#LIFTEDINIT\t" + liftedInit + "\n")
        writer.write("#LIFTEDLOAD\t" + liftedLoad + "\n")

        val samplingRuns = configs.zipWithIndex.map {
            case (_, i) => StopWatch.get(method + "_" + i + "_" + RUN_MARK)
        }
        val samplingInits = configs.zipWithIndex.map {
            case (_, i) => StopWatch.get(method + "_" + i + "_" + CInterCFGBenchmarkMarks.TUNIT_INIT)
        }
        val samplingLoads = configs.zipWithIndex.map {
            case (_, i) => StopWatch.get(method + "_" + i + "_" + CInterCFGBenchmarkMarks.TUNIT_LOAD)
        }
        val samplingSolvings = (samplingRuns, (samplingInits, samplingLoads).zipped.map(_ + _)).zipped.map(_ - _)

        val samplingRunTotal = samplingRuns.sum
        val samplingInitTotal = samplingInits.sum
        val samplingLoadTotal = samplingLoads.sum
        val samplingSolvingTotal = samplingRunTotal - (samplingInitTotal + samplingLoadTotal)
        val samplingSolvingMax = if (samplingSolvings.nonEmpty) samplingSolvings.max else 0
        val samplingSolvingMin = if (samplingSolvings.nonEmpty) samplingSolvings.min else 0

        writer.write("#SAMPLINGTOTALRUNTIME:\t" + samplingRunTotal + "\n")
        writer.write("#SAMPLINGTOTALSOLVE:\t" + samplingSolvingTotal + "\n")
        writer.write("#SAMPLINGTOTALINIT:\t" + samplingInitTotal + "\n")
        writer.write("#SAMPLINGTOTALLOAD:\t" + samplingLoadTotal + "\n")
        writer.write("#SOLVINGMAX:\t" + samplingSolvingMax + "\n")
        writer.write("#SOLVINGMIN:\t" + samplingSolvingMin + "\n")

        writer.close()
    }
}