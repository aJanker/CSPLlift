package de.fosd.typechef.cpointeranalysis

import java.io.File
import java.util

import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.error.Position
import de.fosd.typechef.featureexpr.FeatureModel
import de.fosd.typechef.typesystem.linker.{CSignature, InterfaceWriter}

import scala.collection.parallel.mutable

class CLinking(linkPath: String) {

  val reader = new InterfaceWriter {}
  val linkFile = new File(linkPath)
  val interface = reader.interfaceFromXML(xml.XML.loadFile(linkFile))

  var blackList = new mutable.ParHashSet[String]()

  val idLinkExpMap: util.HashMap[String, List[CSignature]] = new util.HashMap()
  val idLinkPosMap: util.HashMap[String, List[Position]] = new util.HashMap()

  val fileImportsFrom: util.HashMap[String, List[String]] = new util.HashMap()
  val fileExportsTo: util.HashMap[String, List[String]] = new util.HashMap()

  interface.exports.foreach(addToMaps)
  // interface.imports.foreach(addToMaps)

  interface.exports.foreach(addToFileExportsTo)
  interface.imports.foreach(addToFileImportsFrom)

  println(fileImportsFrom.size())
  println(fileExportsTo.size())

  println(fileExportsTo.get("file /scratch/janker/FOSD/extract/cRefactor-OpenSSLEvaluation1/openssl/crypto/pkcs7/pk7_doit.c"))
  println(fileImportsFrom.get("file /scratch/janker/FOSD/extract/cRefactor-OpenSSLEvaluation1/openssl/crypto/pkcs7/pk7_doit.c"))

  private def addToFileExportsTo(exp: CSignature) = {
    // all references to files which import this method
    val importsIt = interface.imports.par.filter(_.name.equalsIgnoreCase(exp.name)).flatMap(_.pos.toList).toList
    exp.pos.toList.foreach(pos =>
      importsIt.foreach(ref =>
        addToFileLinkingMap(pos, ref, fileExportsTo)))
  }

  private def addToFileLinkingMap(pos: Position, ref: Position, map : util.HashMap[String, List[String]]): List[String] = {
    if (map.containsKey(pos.getFile))
      map.put(pos.getFile, (ref.getFile :: map.get(pos.getFile)).distinct)
    else
      map.put(pos.getFile, List(ref.getFile))
  }

  private def addToFileImportsFrom(exp: CSignature) = {
    val exportsIt = interface.exports.par.filter(_.name.equalsIgnoreCase(exp.name)).flatMap(_.pos.toList).toList
    exp.pos.toList.foreach(pos =>
      exportsIt.foreach(ref =>
        addToFileLinkingMap(pos, ref, fileImportsFrom)))
  }

  private def addToMaps(exp: CSignature): List[Position] = {
    addToExpMap(exp.name, exp)
    addToPosMap(exp.name, exp.pos.toList)
  }

  private def addToExpMap(key: String, value: CSignature) =
    if (idLinkExpMap.containsKey(key))
      idLinkExpMap.put(key, value :: idLinkExpMap.get(key))
    else
      idLinkExpMap.put(key, List(value))

  private def addToPosMap(key: String, value: List[Position]) =
    if (idLinkPosMap.containsKey(key))
      idLinkPosMap.put(key, value ::: idLinkPosMap.get(key))
    else
      idLinkPosMap.put(key, value)

  def nameIsListed(name: String) = idLinkExpMap.containsKey(name) || idLinkPosMap.containsKey(name)

  def isListed(id: Opt[String], fm: FeatureModel): Boolean =
    if (idLinkExpMap.containsKey(id.entry)) idLinkExpMap.get(id.entry).exists(_.fexpr.implies(id.condition).isTautology(fm))
    else false

  def isBlackListed(id: String) = blackList.contains(id)

  def getSignatures(id: String) =
    idLinkExpMap.get(id) match {
      case null => None
      case res => Some(res)
    }

  def getPositions(id: String) = {
    val result = idLinkPosMap.get(id)
    if (result != null) result
    else List[Position]()
  }
}
