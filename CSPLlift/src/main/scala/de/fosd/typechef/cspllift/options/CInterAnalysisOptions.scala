package de.fosd.typechef.cspllift.options

import de.fosd.typechef.options.{FrontendOptionsWithConfigFiles, OptionException, Options}
import gnu.getopt.{Getopt, LongOpt}

class CInterAnalysisOptions extends FrontendOptionsWithConfigFiles with CSPLliftOptions {

    private var cLinkingInterfaceMergeDir, cLinkingInterfacePath: Option[String] = None

    private var lift, liftBenchmark, liftEvalSampling, liftEvalSingle: Boolean = false
    private var printVariants, printCallGraph, mergeCLinkingInterfaces, noFunctionPointerComputation: Boolean = false

    private val F_MERGELINKINTERFACE: Char = Options.genOptionId()
    private val F_LINKINTERFACE: Char = Options.genOptionId()
    private val F_SPLLIFT: Char = Options.genOptionId
    private val F_NOFUNCTIONPOINTER: Char = Options.genOptionId()

    private val SPLLIFT_Taint = SecurityOption("TAINT", "Issues a warning when a potential taint memory leak is found.", dflt = false)

    private val liftopts: List[SecurityOption] = List(
        SPLLIFT_Taint
    )

    override def resolveFunctionPointer: Boolean = !noFunctionPointerComputation

    override def getCLinkingInterfacePath: Option[String] = cLinkingInterfacePath

    override def getCModuleInterfaceMergeDir: String = cLinkingInterfaceMergeDir.getOrElse(getOutputStem)

    override def getInformationFlowGraphExtension: String = ".ifg.dot"

    override def getInformationFLowGraphFilename: String = getOutputStem + getInformationFlowGraphExtension

    override def getInformationFlowGraphsOutputDir: String = getOutputStem + "_ifg"

    override def getVariantsOutputDir: String = getOutputStem + "_variants"

    override def isLiftEvaluationModeEnabled: Boolean = liftEvalSampling || liftEvalSingle

    override def isLiftAnalysisEnabled: Boolean = lift

    override def isLiftBenchmarkEnabled: Boolean = liftBenchmark

    override def isMergeLinkingInterfacesEnabled: Boolean = mergeCLinkingInterfaces

    override def isLiftSamplingEvaluationEnabled: Boolean = liftEvalSampling

    override def isLiftSingleEvaluationEnabled: Boolean = liftEvalSingle

    override def isLiftPrintExplodedSuperCallGraphEnabled: Boolean = printCallGraph

    override def liftTaintAnalysis: Boolean = SPLLIFT_Taint.isSelected

    override def writeVariants: Boolean = printVariants

    override def getOptionGroups = {
        val r = super.getOptionGroups

        r.add(new Options.OptionGroup("General options for interprocedural analysis with SPLLift", 100,
            new Options.Option("spllift", LongOpt.REQUIRED_ARGUMENT, F_SPLLIFT, "type",
                "Enables the lifted analysis class: \n" +
                  opts.map(o => " * " + o.param + (if (o.dflt) "*" else "") + ": " + o.expl).mkString("\n") +
                  "\n(Analyses with * are activated by default)."
            ),
            new Options.Option("noFP", LongOpt.NO_ARGUMENT, F_NOFUNCTIONPOINTER, null, "Disable function pointer computation for call graph."),
            new Options.Option("linkingInterface", LongOpt.REQUIRED_ARGUMENT, F_LINKINTERFACE, "file", "Linking interface for all externally exported functions."),
            new Options.Option("mergeLinkingInterface", LongOpt.REQUIRED_ARGUMENT, F_MERGELINKINTERFACE, "file", "Merges all sinkle file linking interfaces into a global file linking interface in a given directory.")
        ))

        r
    }

    protected override def interpretOption(c: Int, g: Getopt): Boolean = {
        def interpretLiftOpts(): Unit = {
            val arg: String = g.getOptarg.replace('_', '-')

            if (arg.equalsIgnoreCase("ALL")) liftopts.foreach(_.isSelected = true)
            else if (arg.equalsIgnoreCase("BENCHMARK")) liftBenchmark = true
            else if (arg.equalsIgnoreCase("EVALCOVERAGE")) liftEvalSampling = true
            else if (arg.equalsIgnoreCase("EVALSINGLE")) liftEvalSingle = true
            else if (arg.equalsIgnoreCase("PRINTVARIANTS")) printVariants = true
            else if (arg.equalsIgnoreCase("CALLGRAPH")) printCallGraph = true
            else {
                val opt = liftopts.find(_.param.toUpperCase equalsIgnoreCase arg)

                if (opt.isEmpty) throw new OptionException("Analysis " + arg + " unknown. Known analyses: " + opts.map(_.param).mkString(", "))

                opt.foreach(_.isSelected = true)
            }
        }

        if (c == F_LINKINTERFACE) {
            checkFileExists(g.getOptarg)
            cLinkingInterfacePath = Some(g.getOptarg)
        } else if (c == F_MERGELINKINTERFACE) {
            checkDirectoryExists(g.getOptarg)
            cLinkingInterfaceMergeDir = Some(g.getOptarg)
            mergeCLinkingInterfaces = true
        } else if (c == F_SPLLIFT) {
            lift = true
            interpretLiftOpts()
        } else if (c == F_NOFUNCTIONPOINTER) {
            noFunctionPointerComputation = true
        } else return super.interpretOption(c, g)

        true
    }
}