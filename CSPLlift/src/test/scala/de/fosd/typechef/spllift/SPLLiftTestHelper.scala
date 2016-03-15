package de.fosd.typechef.spllift

import java.io._
import java.util.zip.GZIPInputStream

import de.fosd.typechef.featureexpr.FeatureExprFactory
import de.fosd.typechef.parser.c.{EnforceTreeHelper, TestHelper, TranslationUnit}
import org.scalatest.Matchers

trait SPLLiftTestHelper extends TestHelper with EnforceTreeHelper with Matchers {

    de.fosd.typechef.featureexpr.FeatureExprFactory.setDefault(de.fosd.typechef.featureexpr.FeatureExprFactory.bdd)

    override val fa = FeatureExprFactory.createDefinedExternal("A")
    override val fb = FeatureExprFactory.createDefinedExternal("B")
    override val fc = FeatureExprFactory.createDefinedExternal("C")
    override val fx = FeatureExprFactory.createDefinedExternal("X")
    override val fy = FeatureExprFactory.createDefinedExternal("Y")

    private val testfileDir = "testfiles/"

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

}
