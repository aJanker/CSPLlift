package de.fosd.typechef.typesystem.linker

import java.io.File
import java.util

import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.error.Position
import de.fosd.typechef.featureexpr.FeatureModel

import scala.collection.parallel.mutable


class CModuleInterface(private val linkPath: String) {
  private val interface = new InterfaceWriter {}.readMergedInterfaceFromXML(xml.XML.loadFile(new File(linkPath)))

  private val idLinkExpCache: util.HashMap[String, List[CSignature]] = new util.HashMap()
  private val idLinkPosCache: util.HashMap[String, List[Position]] = new util.HashMap()
  private val incompleteLinks = new mutable.ParHashSet[String]()

  interface.exports.foreach(fillCache)
  interface.imports.foreach(expr =>
    if (isNameKnown(expr.name)) fillCache(expr)
    else incompleteLinks += expr.name)

  private def fillCache(exp: CSignature) = {
    addToExpMap(exp.name, exp)
    addToPosMap(exp.name, exp.pos.toList)
  }

  private def addToExpMap(key: String, value: CSignature) =
    if (idLinkExpCache.containsKey(key))
      idLinkExpCache.put(key, value :: idLinkExpCache.get(key))
    else
      idLinkExpCache.put(key, List(value))

  private def addToPosMap(key: String, value: List[Position]) =
    if (idLinkPosCache.containsKey(key))
      idLinkPosCache.put(key, value ::: idLinkPosCache.get(key))
    else
      idLinkPosCache.put(key, value)

  def isNameKnown(name: String) = idLinkExpCache.containsKey(name) || idLinkPosCache.containsKey(name)

  def isKnown(id: Opt[String], fm: FeatureModel): Boolean =
    if (idLinkExpCache.containsKey(id.entry)) idLinkExpCache.get(id.entry).exists(_.fexpr.implies(id.feature).isTautology(fm))
    else false

  def isBlackListed(id: String) = incompleteLinks.contains(id)

  def getSignatures(id: String): Option[List[CSignature]] =
    idLinkExpCache.get(id) match {
      case null => None
      case x => Some(x)
    }

  def getPositions(id: String) = {
    val result = idLinkPosCache.get(id)
    if (result != null) result
    else List[Position]()
  }
}
