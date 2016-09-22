package de.fosd.typechef.customization.clinking

import java.io.File
import java.util

import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.error.Position
import de.fosd.typechef.featureexpr.FeatureModel
import de.fosd.typechef.typesystem.linker.CSignature

import scala.collection.parallel.mutable

class CModuleInterface(private val linkPath: String) {
  private val interface = new CInterfaceWriter {}.readMergedInterfaceFromXML(xml.XML.loadFile(new File(linkPath)))

  private val idLinkExpCache: util.HashMap[String, List[CSignature]] = new util.HashMap()
  private val idLinkPosCache: util.HashMap[String, List[Position]] = new util.HashMap()
  private val incompleteLinks = new mutable.ParHashSet[String]()

  interface.exports.foreach(fillCache)
  /*interface.imports.foreach(expr =>
    if (isNameKnown(expr.name)) fillCache(expr)
    else incompleteLinks += expr.name) */

  def isNameKnown(name: String) = idLinkExpCache.containsKey(name) || idLinkPosCache.containsKey(name)

  def isKnown(id: Opt[String], fm: FeatureModel): Boolean =
    if (idLinkExpCache.containsKey(id.entry)) idLinkExpCache.get(id.entry).exists(_.fexpr.implies(id.condition).isTautology(fm))
    else false

  def isBlackListed(id: String) = incompleteLinks.contains(id)

  def getSignatures(id: String): Option[List[CSignature]] =
    idLinkExpCache.get(id) match {
      case null => None
      case x => Some(x)
    }

  def getPositions(id: String): Option[List[Position]] =
    idLinkPosCache.get(id) match {
      case null => None
      case result => Some(result)
    }

  private def addToCache[K, V](map: util.Map[K, List[V]], key: K, value: V) =
    if (map.containsKey(key)) map.put(key, value :: map.get(key))
    else map.put(key, List(value))

  private def addToListCache[K, V](map: util.Map[K, List[V]], key: K, value: List[V]) =
    if (map.containsKey(key)) map.put(key, value ::: map.get(key))
    else map.put(key, value)

  private def fillCache(exp: CSignature) = {
    addToCache(idLinkExpCache, exp.name, exp)
    addToListCache(idLinkPosCache, exp.name, exp.pos.toList)
  }
}