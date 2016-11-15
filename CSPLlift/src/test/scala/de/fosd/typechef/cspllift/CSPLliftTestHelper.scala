package de.fosd.typechef.cspllift

import java.io._
import java.util.zip.GZIPInputStream

import de.fosd.typechef.customization.parser.TestHelper
import de.fosd.typechef.featureexpr.FeatureExprFactory
import de.fosd.typechef.parser.c._
import org.scalatest.Matchers

trait CSPLliftTestHelper extends TestHelper with Matchers {

    de.fosd.typechef.featureexpr.FeatureExprFactory.setDefault(de.fosd.typechef.featureexpr.FeatureExprFactory.bdd)


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

    lazy val dbgWriterDir = "/Users/andi/Dropbox/Masterarbeit/ifg_tests/"

    lazy val dbg = false

    setDefaultLogging()

    def parseTUnitFromFile(filename: String): TranslationUnit = {
        val inStream: InputStream = getClass.getResourceAsStream("/" + testfileDir + filename)
        val includeDir = new File(getClass.getResource("/" + testfileDir + filename).getFile).getParent

        if (inStream == null)
            throw new FileNotFoundException("Input file not found!")

        val tunit: TranslationUnit = parseFile(inStream, filename, includeDir)
        assert(tunit != null, "AST is null")
        tunit
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
        tunit

    }

    private def setDefaultLogging() = if (System.getProperty(org.slf4j.impl.SimpleLogger.DEFAULT_LOG_LEVEL_KEY) == null) System.setProperty(org.slf4j.impl.SimpleLogger.DEFAULT_LOG_LEVEL_KEY, "INFO")
}
