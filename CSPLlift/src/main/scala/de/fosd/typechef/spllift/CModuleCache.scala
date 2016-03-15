package de.fosd.typechef.spllift

import java.io._
import java.util
import java.util.zip.GZIPInputStream

import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.featureexpr.FeatureModel
import de.fosd.typechef.featureexpr.bdd.BDDFeatureModel
import de.fosd.typechef.parser.c._
import de.fosd.typechef.typesystem.linker.CModuleInterface
import de.fosd.typechef.typesystem.{CDeclUse, CTypeCache, CTypeSystemFrontend}

import scala.collection.JavaConversions._


trait CModuleCache {

  def findEnv(node: AST, cache: CModuleCacheEnv): Option[ASTEnv] =
    cache.getEnvs.par.find { _.containsASTElem(node) }


  def getTranslationUnit(node: AST, cache: CModuleCacheEnv): Option[TranslationUnit] =
    findEnv(node, cache) match {
      case Some(env) =>
        cache.getTunitForEnv(env) match {
          case null => None
          case tunit => Some(tunit)
        }
      case _ => None
    }

  def getTypeSystem(node: AST, cache: CModuleCacheEnv): Option[CTypeSystemFrontend with CTypeCache with CDeclUse] =
    findEnv(node, cache) match {
      case Some(env) =>
        cache.getTSForEnv(env) match {
          case null => None
          case ts => Some(ts)
        }
      case _ => None
    }


  def isNameLinked(name: Opt[String], cache: CModuleCacheEnv): Boolean =
    cache.isNameKnown(name)

  def getExternalDefinitions(name: Opt[String], cache: CModuleCacheEnv): List[Opt[FunctionDef]] =
    cache.getNameLocations(name).getOrElse(List()).foldLeft(List[Opt[FunctionDef]]())((res, path) => {
      val tUnit =
        cache.getTunitForFile(path) match {
          case Some(t) => t
          case None => throw new FileNotFoundException(path)
        }

      val foundDefs = tUnit.defs.flatMap {
        case o@Opt(ft, f@FunctionDef(_, decl, _, _)) if decl.getName.equalsIgnoreCase(name.entry) && ft.and(name.condition).isTautology(/* TODO FM */) =>
          Some(Opt(ft, f))
      }
      res ::: foundDefs
    })
}

class CModuleCacheEnv private(initialTUnit: TranslationUnit, fm: FeatureModel, cModuleInterfacePath: Option[String], cPointerInterfacePath: Option[String]) extends EnforceTreeHelper {

  def this(initialTUnit: TranslationUnit, fm: FeatureModel = BDDFeatureModel.empty, options: CInterCFGOptions = DefaultCInterCFGOptions) =
    this(initialTUnit, fm, options.getModuleInterface, options.getPointerInterface)

  private val envToTUnit: util.IdentityHashMap[ASTEnv, TranslationUnit] = new util.IdentityHashMap()
  private val envToTS: util.IdentityHashMap[ASTEnv, CTypeSystemFrontend with CTypeCache with CDeclUse] = new util.IdentityHashMap()
  private val fileToTUnit: util.IdentityHashMap[String, TranslationUnit] = new util.IdentityHashMap()

  private val cModuleInterface: Option[CModuleInterface] =
    cModuleInterfacePath match {
      case Some(path) => Some(new CModuleInterface(path))
      case _ => None
    }

  private val cPointerInterface =
    cPointerInterfacePath match {
      case Some(path) => None //TODO
      case _ => None
    }

  add(initialTUnit)

  override def prepareAST[T <: Product](ast: T): T = {
    // TODO Rewrite Nested function calls (outter(inner(x))
    val tunit = super.prepareAST(ast)
    tunit
  }

  def add(tunit: TranslationUnit) = {
    val env = CASTEnv.createASTEnv(tunit)
    val ts = new CTypeSystemFrontend(tunit, fm) with CTypeCache with CDeclUse
    ts.checkAST()

    envToTUnit.put(env, tunit)
    envToTS.put(env, ts)
    fileToTUnit.put(tunit.defs.last.entry.getPositionFrom.getFile, tunit) // Workaround as usually the first definitions are external includes
  }

  def getAllKnownTranslationunits: List[TranslationUnit] = envToTUnit.values.toList

  def getTunitForEnv(env: ASTEnv) = envToTUnit.get(env)

  def getTSForEnv(env: ASTEnv) = envToTS.get(env)

  def getTunitForFile(file: String): Option[TranslationUnit] =
    if (fileToTUnit.containsKey(file)) Some(fileToTUnit.get(file))
    else loadTUnit(file)

  def getEnvs: List[ASTEnv] = envToTUnit.keySet.toList

  def isNameKnown(name: Opt[String]): Boolean =
    cModuleInterface match {
      case None => false
      case Some(interface) => interface.isNameKnown(name.entry)
    }

  def getNameLocations(name: Opt[String]): Option[List[String]] =
    cModuleInterface match {
      case None => None
      case Some(interface) => {
        interface.getPositions(name.entry) match {
          case None => None
          case Some(pos) => Some(pos.map(_.getFile))
        }
      }
    }

  def loadTUnit(filename: String): Option[TranslationUnit] =
    try {
      val fr = new ObjectInputStream(new GZIPInputStream(new FileInputStream(filename))) {
        override protected def resolveClass(desc: ObjectStreamClass) = super.resolveClass(desc)
      }

      val tunit = prepareAST(fr.readObject().asInstanceOf[TranslationUnit])
      fr.close()

      add(tunit)

      Some(tunit)
    } catch {
      case e: ObjectStreamException => System.err.println("failed loading serialized AST: " + e.getMessage); None
    }
}
