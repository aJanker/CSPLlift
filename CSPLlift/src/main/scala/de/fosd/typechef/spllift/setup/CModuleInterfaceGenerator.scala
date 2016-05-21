package de.fosd.typechef.spllift.setup

import java.io.File

import de.fosd.typechef.featureexpr.FeatureExprFactory
import de.fosd.typechef.typesystem.linker.{CInterface, InterfaceWriter, SystemLinker}


/**
  * Interface for generating linking information of a whole project
  */
object CModuleInterfaceGenerator extends App with InterfaceWriter {

    val startDir = args(0)
    val out: String = args(0) + "/CModuleInterface.interface"
    val featureModel_DIMACS: String = if (args.length > 3) args(1) else " "

    private def getFileTree(f: File): Stream[File] =
        f #:: (if (f.isDirectory) f.listFiles().toStream.flatMap(getFileTree)
        else Stream.empty)

    val fileList = getFileTree(new File(startDir)).filter(f => f.isFile && f.getPath.endsWith(".interface"))

    val fm =
        if (new File(featureModel_DIMACS).isFile) FeatureExprFactory.default.featureModelFactory.createFromDimacsFilePrefix(featureModel_DIMACS, "")
        else FeatureExprFactory.default.featureModelFactory.empty

    val interfaces = fileList.par.map(f => {
        val interface = SystemLinker.linkStdLib(readInterface(f))
        // interface.exports.map(sig => sig.copy(p))
        interface
    }).toList

    println("#Loaded interfaces:\t" + interfaces.size)

    val finalInterface = linkInterfaces(interfaces) //.packWithOutElimination //.andFM(fm_constraints)

    println("#Linked interface is well-formed:\t" + finalInterface.isWellformed)

    writeExportInterface(finalInterface, new File(out))

    println("#Linked interface written to:\t" + new File(out).getAbsolutePath)

    private def linkInterfaces(l: List[CInterface]): CInterface =
        l.reduceLeft { (left, right) =>
            /*val conflicts = left getConflicts right

            for (c <- conflicts; if !c._2.isTautology(fm)) yield println(c + " is not a tautology in feature model.")

             if (!(left isCompatibleTo right)) {
                println(conflicts + " is not compatible with feature model.")
                // left
            } */

            left debug_join right
        }
}