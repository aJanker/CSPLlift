package de.fosd.typechef.spllift.evaluation

import de.fosd.typechef.commons.StopWatch
import de.fosd.typechef.crewrite.ProductDerivation
import de.fosd.typechef.featureexpr.FeatureModel
import de.fosd.typechef.featureexpr.bdd.{BDDFeatureExpr, BDDFeatureModel}
import de.fosd.typechef.parser.c.{PrettyPrinter, TranslationUnit}
import de.fosd.typechef.spllift.cifdsproblem.{CFlowFact, CIFDSProblem, InformationFlow, InformationFlowProblem}
import de.fosd.typechef.spllift.commons.ConditionTools
import de.fosd.typechef.spllift.options.CSPLliftOptions
import de.fosd.typechef.spllift.{CInterCFG, CSPLlift, DefaultCInterCFGOptions, _}
import soot.spl.ifds.Constraint

class CSPLliftEvalFrontend(ast: TranslationUnit, fm: FeatureModel = BDDFeatureModel.empty) extends ConditionTools {

    def checkAgainstSampling(opt: CSPLliftOptions) : Boolean = {
        var successful = true

        if (opt.liftTaintAnalysis)
            successful = runSampling[InformationFlow, InformationFlowProblem](classOf[InformationFlowProblem], opt: CSPLliftOptions) && successful

        successful
    }

    def checkAgainstErrorConfiguration(opt: CSPLliftOptions) : Boolean  = {
        var successful = true

        if (opt.liftTaintAnalysis)
            successful = runErrorConfiguration[InformationFlow, InformationFlowProblem](classOf[InformationFlowProblem], opt: CSPLliftOptions) && successful

        successful
    }


    private def runSampling[D <: CFlowFact, T <: CIFDSProblem[D]](ifdsProblem: java.lang.Class[T], opt: CSPLliftOptions) : Boolean = {

        // 1. Step -> Run VAA first in order to detect all linked files for codecoverageconfiguration generation
        val cInterCFGOptions = new DefaultCInterCFGOptions(opt.getCLinkingInterfacePath)
        val (vaaWallTime, (vaaSolution, icfg)) = runSPLLift[D, T](ifdsProblem, cInterCFGOptions, "vaa")
        val vaaFacts = vaaSolution.flatMap(_.toList)

        // 2. Generate Code Coverage Configurations for all referenced files
        val sampling = new Sampling(icfg.cInterCFGElementsCacheEnv.getAllKnownTUnitsAsSingleTUnit, fm)
        val configs = sampling.codeConfigurationCoverage()

        // 3. Run Code Coverage
        val coverageResults = configs.map(config => {
            val cInterCFGOptions = new ConfigurationBasedCInterCFGOptions(config, opt.getCLinkingInterfacePath)
            val (wallTime, (solution, icfg)) = runSPLLift[D, T](ifdsProblem, cInterCFGOptions, "coverage")
            (solution, config, wallTime)
        })

        // 4. Compare
        var matchedVaaFacts = scala.collection.mutable.Map[(D, Constraint), Int]()
        vaaFacts.foreach(fact => matchedVaaFacts += (fact -> 0))

        val unmatchedCoverageFacts = coverageResults.flatMap(c => {
            val covFacts = c._1.flatMap(_.toList)
            val configuration = c._2

            // Check if for every coverage based result a corresponding vaa based result was found.
            val unmatched = covFacts.filterNot(cr => {
                vaaFacts.exists(vr => {
                    val condition = vr._2

                    if (isSatisfiableInConfiguration(condition, configuration) && vr._1.isEquivalent(cr._1, configuration)  ) {
                        // is match, increase vaa match counter
                        matchedVaaFacts += (vr -> (matchedVaaFacts.getOrElse(vr, 0) + 1))
                        true
                    } else false

                })
            })

            unmatched.map(um => (um, configuration))
        })

        println("### Generated " + configs.size + " unique variants.")

        val unmatchedVAAFacts = matchedVaaFacts.toList.collect { case ((x, 0)) => x }

        if (unmatchedVAAFacts.nonEmpty) {
            println("\n### Following results were not covered by the coverage approach: ")
            println("Size:\t" +  unmatchedVAAFacts.size)

            unmatchedVAAFacts.foreach(uc => println("Error:\n" + uc))
        }

        if (unmatchedCoverageFacts.nonEmpty) {
            println("\n### Following results were not covered by the lifted approach: ")
            println("Size:\t" + unmatchedCoverageFacts.size)

            unmatchedCoverageFacts.foreach(uc => {
                println("Configuration:\t" + uc._2)
                println("Error:\n" + uc._1)
            })
        }

        unmatchedCoverageFacts.isEmpty
    }

    private def runErrorConfiguration[D <: CFlowFact, T <: CIFDSProblem[D]](ifdsProblem: Class[T], opt: CSPLliftOptions) : Boolean = {
        // 1. Step -> Run VAA first in order to detect all affected features
        val cInterCFGOptions = new DefaultCInterCFGOptions(opt.getCLinkingInterfacePath)
        val (vaaWallTime, (vaaSolution, icfg)) = runSPLLift[D, T](ifdsProblem, cInterCFGOptions, "vaa")
        val vaaFacts = vaaSolution.flatMap(_.toList)

        // Collect distinct conditions
        val (cfgConditions, factConditions) = vaaFacts.foldLeft((Set[BDDFeatureExpr](), Set[BDDFeatureExpr]()))((x, fact) => {
            val (cfgConds, factConds) = x

            val cfgCond = fact._2.getFeatureExpr
            val factCond = fact._1.getConditions

            (cfgConds + cfgCond, factConds ++ factCond)
        })


        cfgConditions.map(f => {
            println(f)
            val satConf = f.getSatisfiableAssignment(fm, f.collectDistinctFeatureObjects, false)
            println(satConf)

            println(PrettyPrinter.print(ProductDerivation.deriveProduct(ast, satConf.get._1.map(_.feature).toSet)))

            println("dbg")
        })

        println(cfgConditions)
        println(cfgConditions.size)


        false
    }


    private def runSPLLift[D <: CFlowFact, T <: CIFDSProblem[D]](ifdsProblem: Class[T], cInterCFGOptions: CInterCFGOptions, stopWatchMark: String = "None"): (Long, (List[Map[D, Constraint]], CInterCFG)) =
        StopWatch.measureUserTime(stopWatchMark, {
            val cInterCFG = new CInterCFG(ast, fm, cInterCFGOptions)
            val problem = getCIFDSProblemInstance[D, T](ifdsProblem)(cInterCFG)

            (CSPLlift.solve[D](problem), cInterCFG)
        })
}
