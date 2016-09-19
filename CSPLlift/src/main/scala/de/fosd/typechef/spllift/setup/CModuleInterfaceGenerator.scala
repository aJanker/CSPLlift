package de.fosd.typechef.spllift.setup

import java.io.File

import de.fosd.typechef.commons.clinking.CInterfaceWriter
import de.fosd.typechef.featureexpr.{FeatureExprFactory, FeatureModel}
import de.fosd.typechef.typesystem.linker._


/**
  * Interface for generating linking information of a whole project
  */
object CModuleInterfaceGenerator extends App with CInterfaceWriter {

    val startDir = args(0)
    val featureModel_DIMACS: String = if (args.length > 3) args(1) else " "

    val fm =
        if (new File(featureModel_DIMACS).isFile) FeatureExprFactory.default.featureModelFactory.createFromDimacsFilePrefix(featureModel_DIMACS, "")
        else FeatureExprFactory.default.featureModelFactory.empty

    mergeAndWriteInterfaces(startDir, fm)

    def mergeInterfaces(dir : String, fm : FeatureModel = FeatureExprFactory.default.featureModelFactory.empty, strictness: Strictness = LINK_RELAXED) : CInterface = {
        val fileList = getFileTree(new File(dir)).filter(f => f.isFile && f.getPath.endsWith(".interface"))

        val interfaces = fileList.par.map(f => {
            val interface = SystemLinker.linkStdLib(readInterface(f))
            // interface.exports.map(sig => sig.copy(p))
            interface
        }).toList

        println("#Loaded interfaces:\t" + interfaces.size)

        val finalInterface = linkInterfaces(interfaces, strictness) //.packWithOutElimination //.andFM(fm_constraints)

        println("#Linked interface is well-formed:\t" + finalInterface.isWellformed)

        finalInterface
    }

    def mergeAndWriteInterfaces(dir : String, fm : FeatureModel = FeatureExprFactory.default.featureModelFactory.empty, strictness: Strictness = LINK_RELAXED, name : String = "CModuleInterface.interface") : Unit = {
        val interface = mergeInterfaces(dir, fm, strictness)
        val out = new File(dir + "/" + name)

        writeExportInterface(interface, out)

        println("#Linked interface written to:\t" + out.getAbsolutePath)
    }

    private def getFileTree(f: File): Stream[File] = f #:: (if (f.isDirectory) f.listFiles().toStream.flatMap(getFileTree) else Stream.empty)

    private def linkInterfaces(l: List[CInterface], strictness: Strictness = LINK_RELAXED): CInterface =
        l.reduceLeft { (left, right) =>
            val conflicts = left getConflicts right

            for (c <- conflicts; if !c._2.isTautology(fm)) yield println(c + " is not a tautology in feature model.")

            left.linkWithOutElimination(right, strictness)
        }
}