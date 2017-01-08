package de.fosd.typechef.cmodule.cmodulecache

import java.io.{FileInputStream, ObjectInputStream}
import java.util
import java.util.zip.GZIPInputStream

import de.fosd.typechef.cmodule.{CModuleCommons, CModuleOperations}
import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.crewrite.ProductDerivation
import de.fosd.typechef.customization.StopWatch
import de.fosd.typechef.customization.conditional.SimpleConfiguration
import de.fosd.typechef.featureexpr.FeatureModel
import de.fosd.typechef.featureexpr.bdd.BDDFeatureModel
import de.fosd.typechef.parser.c._
import de.fosd.typechef.typesystem.CTypeSystemFrontend
import de.fosd.typechef.typesystem.modulelinking.CLinkingExtractor
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.JavaConverters._
import scala.collection.immutable.HashMap
import scala.collection.mutable

/**
  * Infrastructure for caching several distinct tunit objects and corresponding elements of a hole case study.
  */
abstract class CModuleCache(fm: FeatureModel = BDDFeatureModel.empty,
                            resolveLinkedFiles: Boolean = true,
                            rewriteTasks: List[((TranslationUnit, FeatureModel) => TranslationUnit)] = List(),
                            productConfiguration: Option[SimpleConfiguration] = None)
  extends CModuleOperations with CModuleCacheOptions with CModuleCacheMediator[TranslationUnit] with CModuleCommons {

    private lazy val logger: Logger = LoggerFactory.getLogger(getClass)

    private val localDebug = true

    /**
      * Caching data structures.
      */
    private val reverseTUnitToOrgTUnit: mutable.HashMap[TranslationUnit, TranslationUnit] = new mutable.HashMap
    private val envToTUnit: util.IdentityHashMap[ASTEnv, TranslationUnit] = new util.IdentityHashMap()
    private val envToTS: util.IdentityHashMap[ASTEnv, CTypeSystemFrontend with CLinkingExtractor] = new util.IdentityHashMap()
    private val fileToTUnit: mutable.HashMap[String, TranslationUnit] = new mutable.HashMap()
    private val nameToFunctionDef: mutable.HashMap[String, Map[String, List[Opt[FunctionDef]]]] = new mutable.HashMap()
    private val tunitToPseudoCall: util.IdentityHashMap[AST, Opt[FunctionDef]] = new util.IdentityHashMap()
    private val astToEnv: util.IdentityHashMap[AST, ASTEnv] = new util.IdentityHashMap[AST, ASTEnv]()

    /**
      * Adds a file to the cache from a string path.
      *
      * Note: currently it is requiered that this file is already parsed and serialized as AST.
      */
    def addFile(file: String): TranslationUnit = {
        val inputfile = removeFilePrefix(file)

        if (isFileKnown(inputfile)) return fileToTUnit(inputfile)

        logger.info("Adding the following source code file:\t" + inputfile)

        val tunit = StopWatch.measureProcessCPUTime({
            val inputStream = new ObjectInputStream(new GZIPInputStream(new FileInputStream(getASTSourceFilename(inputfile))))
            val loaded = inputStream.readObject()
            inputStream.close()

            loaded.asInstanceOf[TranslationUnit]
        })._2

        addTUnit(tunit)
    }

    /**
      * Adds an alreadey existing translation unit to the cache.
      */
    def addTUnit(inputTUnit: TranslationUnit): TranslationUnit = {
        val fileName = removeFilePrefix(replaceExtensionWithC(inputTUnit.defs.last.entry.getFile.getOrElse(NO_FILENAME)))

        val (runtime, (tunit, astEnv, ts)) = StopWatch.measureThreadUserTime({
            val (rewriteRuntime, preparedTUnit) = StopWatch.measureThreadUserTime({
                val pTunit = prepareAST(inputTUnit)
                val rTunit = rewriteTasks.foldLeft(pTunit)((currTUnit, rewriteTask) => rewriteTask(currTUnit, fm))

                if (productConfiguration.isDefined) ProductDerivation.deriveProduct(ProductDerivation.deriveProduct(rTunit, productConfiguration.get.getTrueFeatures), productConfiguration.get.getTrueFeatures)
                else rTunit
            })

            logger.info("Prepared file " + fileName + " in " + rewriteRuntime + "ms.")

            val (typeCheckTime, (astEnv, ts)) = StopWatch.measureThreadUserTime({
                val astEnv = CASTEnv.createASTEnv(preparedTUnit)

                val ts = new CTypeSystemFrontend(preparedTUnit, fm) with CLinkingExtractor
                if (silentTypeCheck) ts.makeSilent()
                ts.checkAST()

                (astEnv, ts)
            })

            logger.info("TypeChecked file " + fileName + " in " + typeCheckTime + "ms.")

            (preparedTUnit, astEnv, ts)
        })

        updateCache(inputTUnit, tunit, fileName, astEnv, ts)

        if (resolveLinkedFiles)
            loadImports(tunit)

        logger.info("Added file " + fileName + " in " + runtime + "ms to cache.")

        tunit
    }

    /**
      * Retrieves the type system environment for a given AST node.
      */
    override def getTypeSystem(node: AST): CTypeSystemFrontend with CLinkingExtractor =
        getTypeSystem(getASTEnv(node))

    private[cmodule] def getTypeSystem(e: ASTEnv): CTypeSystemFrontend with CLinkingExtractor = envToTS.get(e)

    /**
      * Retrieves the surrounding translation unit of a given AST node.
      */
    override def getTranslationUnit(node: AST): TranslationUnit = envToTUnit.get(getASTEnv(node))

    /**
      * Retrieves the corresponding AST environment for a given AST node
      */
    override def getASTEnv(node: AST): ASTEnv =
        if (astToEnv.containsKey(node)) astToEnv.get(node)
        else {
            val astEnv = envToTUnit.keySet().asScala.find {
                _.containsASTElem(node)
            }.get
            astToEnv.put(node, astEnv)
            astEnv
        }

    /**
      * Retrieves the untouched version of a translation unit with no performed rewrite operations.
      */
    override def getUntouchedVersion(tunit: TranslationUnit): TranslationUnit = reverseTUnitToOrgTUnit(tunit)

    /**
      * Returns all currently loaded files.
      */
    def getAllFiles: List[String] = fileToTUnit.keys.toList

    /**
      * Returns all currently loaded files maped tunits.
      */
    def getAllFileTunitPairs: List[(String, TranslationUnit)] = fileToTUnit.toList

    /**
      * Merges all cached tunit's into a single translation unit.
      */
    def getAllKnownTUnitsInSingleTUnit: TranslationUnit =
        TranslationUnit(getAllKnownTUnits.foldLeft(List[Opt[ExternalDef]]()) { (l, ast) => l ::: ast.defs })

    /**
      * Returns all currently cached translation units.
      */
    def getAllKnownTUnits: List[TranslationUnit] = envToTUnit.values.asScala.toList

    /**
      * Returns true if a given file is already loaded
      */
    def isFileKnown(file: String): Boolean = fileToTUnit.contains(removeFilePrefix(file))

    /**
      * Retrieves all known function definitions with this name
      */
    def getFunctionDef(name: String): Map[String, List[Opt[FunctionDef]]] = nameToFunctionDef.getOrElse(name, emptyFDefMap)

    /**
      * Retrieves all known non-emtpy function definitions with this name
      */
    def getNonEmptyFunctionDef(name: String): Map[String, List[Opt[FunctionDef]]] = getFunctionDef(name) map {
        case (file, fDefs) => (file, fDefs filter (_.entry.stmt.innerStatements.nonEmpty))
    }

    /**
      * Retrieves all known function definitions with this name
      */
    def getFlatFunctionDef(name: String): List[Opt[FunctionDef]] = nameToFunctionDef.getOrElse(name, emptyFDefMap).values.flatten.toList

    /**
      * Retrieves all known non-emtpy function definitions with this name
      */
    def getFlatNonEmptyFunctionDef(name: String): List[Opt[FunctionDef]] = getFlatFunctionDef(name) filter (_.entry.stmt.innerStatements.nonEmpty)

    override protected def initialMediation(): Iterable[TranslationUnit] = getAllKnownTUnits

    private def emptyFDefMap = new HashMap[String, List[Opt[FunctionDef]]]()

    private def updateCache(orgTUnit: TranslationUnit, tunit: TranslationUnit, file: String, env: ASTEnv,
                            ts: CTypeSystemFrontend with CLinkingExtractor,
                            pseudoSystemFunctionCall: Option[Opt[FunctionDef]] = None): TranslationUnit = {
        envToTUnit.put(env, tunit)
        envToTS.put(env, ts)
        fileToTUnit.put(file, tunit)
        reverseTUnitToOrgTUnit.put(tunit, orgTUnit)

        if (pseudoSystemFunctionCall.isDefined)
            tunitToPseudoCall.put(tunit, pseudoSystemFunctionCall.get)

        updateFunctionNameMapping(tunit, file)
        mediate(List(tunit))

        tunit
    }

    private def updateFunctionNameMapping(translationUnit: TranslationUnit, file: String) = {
        val functionDefs = filterAllASTElems[FunctionDef](translationUnit) map {
            fDef => parentOpt(fDef, getASTEnv(fDef)).asInstanceOf[Opt[FunctionDef]]
        }

        for (fDef <- functionDefs if fDef.entry.stmt.innerStatements.nonEmpty) {
            val oldMap = nameToFunctionDef.getOrElse(fDef.entry.getName, emptyFDefMap)
            val updatedMap = oldMap + (file -> (fDef :: oldMap.getOrElse(file, List())))

            nameToFunctionDef.put(fDef.entry.getName, updatedMap)
        }
    }

    private def getASTSourceFilename(inputfile: String) = {
        val fileExtension = if (inputfile.endsWith(PI_FILE_EXT)) PI_FILE_EXT else C_FILE_EXT
        val filename = removeFilePrefix(inputfile)
        val dbgName = if (localDebug) filename.replace("/home/janker/Masterarbeit", "/Users/andi/Masterarbeit") else filename

        if (logger.isInfoEnabled) logger.info("Loading:\t" + dbgName)

        val (source, _) = dbgName.splitAt(dbgName.lastIndexOf(fileExtension))

        source + ".ast"
    }

    private def loadImports(tunit: TranslationUnit): Unit = {
        if (getLinkingMap.isEmpty) return

        val fileName = removeFilePrefix(replaceExtensionWithC(tunit.defs.last.entry.getFile.getOrElse(NO_FILENAME)))

        val linkingMap = getLinkingMap.get
        val ts = getTypeSystem(tunit)
        val importedVarNames = ts.getImportedNames.flatMap(_.declNames)
        val importedFunctionNames = ts.getImportedFunctions.flatMap(_.declNames)
        val files =
            (importedVarNames.flatMap(linkingMap.findVarExportLocations).map(_.entry.getFile) ++
              importedFunctionNames.flatMap(linkingMap.findFDefsExportLocations).map(_.entry.getFile)).distinct

        logger.info(fileName + " imports the following files:\t" + files.map(removeFilePrefix).mkString(", "))

        files.foreach(addFile)
    }
}