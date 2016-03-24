package de.fosd.typechef.spllift

import java.io._
import java.util.zip.GZIPInputStream

import de.fosd.typechef.conditional.Opt
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

    private val testfileDir = "testfiles/"

    def defaultInit(filename: String, isSink: Reach => Boolean) = {
        val tunit = parseTUnitFromFile(filename)

        val cInterCFG = new CInterCFG(tunit)
        val problem = new InformationFlowProblem(cInterCFG)
        val solution = CSPLliftFrontend.solve(problem)

        val sinks = Taint.findSinks[String](solution, isSink)

        // dbg print
        if (dbg) sinks.foreach(sink => {
            println("Sink at:\t" + PrettyPrinter.print(sink._1))
            sink._2.foreach(ssink => println("CFGcondition " + ssink._1 + ":\t" + ssink._2))
        })

        (tunit, problem, solution, sinks)
    }

    def parseTUnitFromFile(filename: String): TranslationUnit = {
        val inStream: InputStream = getClass.getResourceAsStream("/" + testfileDir + filename)

        if (inStream == null)
            throw new FileNotFoundException("Input file not found!")

        val tunit: TranslationUnit = parseFile(inStream, testfileDir, filename)
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

    def allReachesMatch(reaches: List[(Constraint[_], Reach)], exectedConditonsAndSources: List[(FeatureExpr, List[Opt[Id]])]): Boolean =
        reaches.forall(reach => exectedConditonsAndSources.exists {
            case (condition, sources) => isReachMatch(reach._2, condition, sources)
            case _ => false
        })

    def isReachMatch(r: Reach, condition: FeatureExpr, reachingIds: List[Opt[Id]]): Boolean =
        r.to.condition.equivalentTo(condition) && r.from.forall(reachingIds contains)

}
