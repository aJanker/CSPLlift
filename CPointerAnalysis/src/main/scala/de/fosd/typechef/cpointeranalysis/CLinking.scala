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

  interface.exports.foreach(addToMaps)
  interface.imports.foreach(expr =>
      if (nameIsListed(expr.name)) addToMaps(expr)
      else blackList += expr.name)

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

  def getSignatures(id: String) = idLinkExpMap.get(id)

  def getPositions(id: String) = {
    val result = idLinkPosMap.get(id)
    if (result != null) result
    else List[Position]()
  }
}
