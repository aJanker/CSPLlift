package de.fosd.typechef.cpointeranalysis

import java.io.File

import de.fosd.typechef.featureexpr.FeatureExprFactory
import de.fosd.typechef.typesystem.linker.{CInterface, EmptyInterface, InterfaceWriter, SystemLinker}


/**
  * Interface for generating linking information of a whole  project
  */
object CModuleInterfaceGenerator extends App with InterfaceWriter {

  val interfaceFiles: String = "~/Dropbox/Masterarbeit/PA-OpenSSL/interfaceFiles"
  val featureModel_DIMACS: String = "~/Dropbox/Masterarbeit/PA-OpenSSL/interfaceFiles"
  val out: String = "~/Dropbox/Masterarbeit/PA-OpenSSL/openssl/OpenSSL.dimacs"

  val filename = "CLinking"
  val linkExt = ".interface"
  val dbgLinkExt = ".dbginterface"

  val fileList = scala.io.Source.fromFile(interfaceFiles).getLines().toList

  val fm = FeatureExprFactory.default.featureModelFactory.createFromDimacsFilePrefix(featureModel_DIMACS, "")

  val interfaces = fileList.map(f => readInterface(new File(f))).map(SystemLinker.linkStdLib)

  def linkTreewise(l: List[CInterface]): CInterface = {
    if (l.size > 2) {
      val m: Int = l.size / 2
      val left = l.take(m)
      val right = l.drop(m)
      linkTreewise(List(linkTreewise(left), linkTreewise(right)))
    } else if (l.size == 2) {
      val left = l(0)
      val right = l(1)
      val conflicts = left getConflicts right

      for (c <- conflicts)
        if (!c._2.isTautology(fm))
          println(c + " is not a tautology in feature model.")

      if (!(left isCompatibleTo right)) {
        println(conflicts + " is not compatible with feature model.")
        left
      } else left linkWithOutElimination right
    } else if (l.size == 1) l(0)
    else {
      assert(false, l)
      EmptyInterface
    }
  }

  val finalInterface = linkTreewise(interfaces) //.packWithOutElimination //.andFM(fm_constraints)

  writeInterface(finalInterface, new File(out + filename + linkExt))
  //debugInterface(finalInterface, dbgInterfacePath)
}