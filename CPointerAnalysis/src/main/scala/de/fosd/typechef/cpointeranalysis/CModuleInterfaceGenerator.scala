package de.fosd.typechef.cpointeranalysis

import java.io.File

import de.fosd.typechef.featureexpr.FeatureExprFactory
import de.fosd.typechef.typesystem.linker.{CInterface, EmptyInterface, InterfaceWriter, SystemLinker}


/**
  * Interface for generating linking information of a whole  project
  */
object CModuleInterfaceGenerator extends App with InterfaceWriter {

  val interfaceFiles: String = "~/Dropbox/Masterarbeit/PA-OpenSSL/interfaceFiles"
  val featureModel_DIMACS: String = "~/Dropbox/Masterarbeit/PA-OpenSSL/openssl/OpenSSL.dimacs"
  val out: String = "~/Dropbox/Masterarbeit/PA-OpenSSL/CLinking.interface"

  val fileList = scala.io.Source.fromFile(interfaceFiles).getLines().toList

  val fm = FeatureExprFactory.default.featureModelFactory.createFromDimacsFilePrefix(featureModel_DIMACS, "")

  val interfaces = fileList.par.map(f => SystemLinker.linkStdLib(readInterface(new File(f)))).toList

  val finalInterface = linkInterfaces(interfaces) //.packWithOutElimination //.andFM(fm_constraints)

  println("Linked interface is well-formed:\t" + finalInterface.isWellformed)

  writeInterface(finalInterface, new File(out))

  private def linkInterfaces(l: List[CInterface]): CInterface =
    l.par.reduceLeft { (left, right) =>
      val conflicts = left getConflicts right

      for (c <- conflicts; if !c._2.isTautology(fm)) yield println(c + " is not a tautology in feature model.")

      if (!(left isCompatibleTo right)) {
        println(conflicts + " is not compatible with feature model.")
        left
      } else left linkWithOutElimination right
    }
}