package de.fosd.typechef.spllift

import java.io._
import java.util.zip.GZIPInputStream

import de.fosd.typechef.StopWatch
import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.featureexpr.bdd.BDDFeatureModel
import de.fosd.typechef.featureexpr.{FeatureExpr, FeatureExprFactory}
import de.fosd.typechef.parser.c._
import de.fosd.typechef.spllift.analysis.Taint
import de.fosd.typechef.spllift.ifdsproblem.{InformationFlowProblem, Reach}
import org.scalatest.Matchers
import soot.spl.ifds.Constraint

trait SPLLiftTestHelper extends TestHelper with EnforceTreeHelper with Matchers {

    de.fosd.typechef.featureexpr.FeatureExprFactory.setDefault(de.fosd.typechef.featureexpr.FeatureExprFactory.bdd)

    val dbg = true

    override val fa = FeatureExprFactory.createDefinedExternal("A")
    override val fb = FeatureExprFactory.createDefinedExternal("B")
    override val fc = FeatureExprFactory.createDefinedExternal("C")
    override val fd = FeatureExprFactory.createDefinedExternal("D")
    override val fe = FeatureExprFactory.createDefinedExternal("E")
    override val ff = FeatureExprFactory.createDefinedExternal("F")
    override val fg = FeatureExprFactory.createDefinedExternal("G")
    override val fx = FeatureExprFactory.createDefinedExternal("X")
    override val fy = FeatureExprFactory.createDefinedExternal("Y")

    val testfileDir = "testfiles/"

    def defaultTestInit(filename: String, isSink: Reach => Boolean, cModuleInterfacePath : Option[String] = None) = {
        StopWatch.reset()

        val tunit = parseTUnitFromFile(filename)

        val cInterCFG = new CInterCFG(tunit, BDDFeatureModel.empty, new DefaultCInterCFGOptions(cModuleInterfacePath))
        val problem = new InformationFlowProblem(cInterCFG)
        val solution = CSPLliftFrontend.solve(problem)


        val sinks = Taint.findSinks[String](solution, isSink)

        // dbg print
        if (dbg) println(Taint.prettyPrintSinks(sinks))

        println(StopWatch.toString)

        (tunit, problem, solution, sinks)
    }

    def defaultSingleSinkTest(filename: String, sinkStmt : Statement, expectedReaches : List[(FeatureExpr, List[Opt[Id]])]) : Boolean = {
        def isSink(r: Reach): Boolean = {
            r.to.entry match {
                case `sinkStmt` => true
                case _ => false
            }
        }

        val (tunit, _, _, sinks) = defaultTestInit(filename, isSink)

        var successful = true

        successful = successful && sinks.size == 1 // only one sink location should be found

        val sink = sinks.head

        successful = successful && (sink._1 match {
            case `sinkStmt` => true // correct sink statement should be found
            case _ => false
        })

        successful && allReachesMatch(sink._2, expectedReaches)
    }

    def parseTUnitFromFile(filename: String): TranslationUnit = {
        val inStream: InputStream = getClass.getResourceAsStream("/" + testfileDir + filename)
        val includeDir = new File(getClass.getResource("/" + testfileDir + filename).getFile).getParent

        if (inStream == null)
            throw new FileNotFoundException("Input file not found!")

        val tunit: TranslationUnit = parseFile(inStream, filename, includeDir)
        assert(tunit != null, "AST is null")
        prepareAST(tunit)
    }

    def loadTUnitFromFile(filename: String): TranslationUnit = {
        val inStream: InputStream = getClass.getResourceAsStream("/" + testfileDir + filename)

        if (inStream == null)
            throw new FileNotFoundException("Input file not found!")

        val fr = new ObjectInputStream(new GZIPInputStream(inStream)) {
            override protected def resolveClass(desc: ObjectStreamClass) = super.resolveClass(desc)
        }

        val tunit: TranslationUnit = fr.readObject().asInstanceOf[TranslationUnit]
        fr.close()

        assert(tunit != null, "AST is null")
        prepareAST(tunit)

    }

    def allSinks(r: Reach): Boolean = true

    def allReachesMatch(reaches: List[(Constraint[_], Reach)], exectedConditonsAndSources: List[(FeatureExpr, List[Opt[Id]])]): Boolean =
        reaches.forall(reach => exectedConditonsAndSources.exists {
            case (condition, sources) => isReachMatch(reach._2, condition, sources)
            case _ => false
        })

    def isReachMatch(r: Reach, condition: FeatureExpr, reachingIds: List[Opt[Id]]): Boolean =
        r.to.condition.equivalentTo(condition) && r.from.forall(reachingIds contains)

}
