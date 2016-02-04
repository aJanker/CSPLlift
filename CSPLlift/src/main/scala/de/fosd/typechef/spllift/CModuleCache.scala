package de.fosd.typechef.spllift

import java.io.{FileInputStream, ObjectInputStream, ObjectStreamClass, ObjectStreamException}
import java.util
import java.util.zip.GZIPInputStream

import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.cpointeranalysis.CLinking
import de.fosd.typechef.parser.c._

import scala.collection.JavaConversions._


trait CModuleCache {

  def findEnv(node: AST, cache: CModuleCacheEnv): Option[ASTEnv] =
    cache.getEnvs.par.find(_.containsASTElem(node))


  def getTranslationUnit(node: AST, cache: CModuleCacheEnv): Option[TranslationUnit] =
    findEnv(node, cache) match {
      case Some(env) =>
        cache.getTunitForEnv(env) match {
          case null => None
          case tunit => Some(tunit)
        }
      case _ => None
    }

  def nameIsLinked(name: Opt[String], cache: CModuleCacheEnv): Boolean = false

  // TODO Implement
}

class CModuleCacheEnv(initialTUnit: TranslationUnit, cModuleInterfacePath: Option[String] = None, cPointerInterfacePath: Option[String] = None) {

  private val cModuleInterface =
    cModuleInterfacePath match {
      case Some(path) => new CLinking(path)
      case _ => None
    }

  private val cPointerInterface =
    cPointerInterfacePath match {
      case Some(path) => None //TODO
      case _ => None
    }

  private val envToTunit: util.IdentityHashMap[ASTEnv, TranslationUnit] = new util.IdentityHashMap()

  add(initialTUnit)

  def add(tunit: TranslationUnit) = {
    val env = CASTEnv.createASTEnv(tunit)
    envToTunit.put(env, tunit)
  }

  def getTunitForEnv(env: ASTEnv) = envToTunit.get(env)

  def getEnvs: List[ASTEnv] = envToTunit.keySet().toList

  def loadTUnit(filename: String): Option[TranslationUnit] =
    try {
      val fr = new ObjectInputStream(new GZIPInputStream(new FileInputStream(filename))) {
        override protected def resolveClass(desc: ObjectStreamClass) = super.resolveClass(desc)
      }

      val tunit = fr.readObject().asInstanceOf[TranslationUnit]
      fr.close()

      Some(tunit)
    } catch {
      case e: ObjectStreamException => System.err.println("failed loading serialized AST: " + e.getMessage); None
    }
}
