package de.fosd.typechef.spllift

import java.io._
import java.util
import java.util.zip.GZIPInputStream

import de.fosd.typechef.commons.StopWatch
import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.cpointeranalysis._
import de.fosd.typechef.crewrite.ProductDerivation
import de.fosd.typechef.featureexpr.FeatureModel
import de.fosd.typechef.featureexpr.bdd.BDDFeatureModel
import de.fosd.typechef.parser.c._
import de.fosd.typechef.spllift.commons.{CInterCFGCommons, WarningsCache}
import de.fosd.typechef.typesystem.linker.CModuleInterface
import de.fosd.typechef.typesystem.{CDeclUse, CTypeCache, _}

import scala.collection.JavaConversions._


trait CInterCFGElementsCache {

    val cInterCFGElementsCacheEnv: CInterCFGElementsCacheEnv

    def getEnv(node: AST): Option[ASTEnv] =
        cInterCFGElementsCacheEnv.getEnv(node)

    def getTranslationUnit(node: AST): Option[TranslationUnit] =
        getEnv(node) match {
            case Some(env) =>
                cInterCFGElementsCacheEnv.getTunitForEnv(env) match {
                    case null => None
                    case tunit => Some(tunit)
                }
            case _ => None
        }

    def getTypeSystem(node: AST): Option[CTypeSystemFrontend with CTypeCache with CDeclUse] =
        getEnv(node) match {
            case Some(env) =>
                cInterCFGElementsCacheEnv.getTSForEnv(env) match {
                    case null => None
                    case ts => Some(ts)
                }
            case _ => None
        }

    def isNameLinked(name: Opt[String]): Boolean =
        cInterCFGElementsCacheEnv.isNameKnown(name)

    def getExternalDefinitions(name: Opt[String]): List[Opt[FunctionDef]] = {
        cInterCFGElementsCacheEnv.getNameLocations(name).getOrElse(List()).foldLeft(List[Opt[FunctionDef]]())((res, path) => {
            val tUnit =
                cInterCFGElementsCacheEnv.getTunitForFile(path) match {
                    case Some(t) => t
                    case None => throw new FileNotFoundException(path)
                }

            val foundDefs = tUnit.defs.flatMap {
                case o@Opt(ft, f@FunctionDef(_, decl, _, _)) if decl.getName.equalsIgnoreCase(name.entry) && ft.and(name.condition).isSatisfiable(/* TODO FM */) =>
                    Some(Opt(ft.and(name.condition), f))
                case _ => None
            }
            res ::: foundDefs
        })
    }
}

class CInterCFGElementsCacheEnv private(initialTUnit: TranslationUnit, fm: FeatureModel, cModuleInterfacePath: Option[String], options: CInterCFGConfiguration) extends EnforceTreeHelper with CInterCFGPseudoVistingSystemLibFunctions with CInterCFGCommons with PointerContext {

    def this(initialTUnit: TranslationUnit, fm: FeatureModel = BDDFeatureModel.empty, options: CInterCFGConfiguration = new DefaultCInterCFGConfiguration) =
        this(initialTUnit, fm, options.getModuleInterfacePath, options)

    private val envToTUnit: util.IdentityHashMap[ASTEnv, TranslationUnit] = new util.IdentityHashMap()
    private val envToTS: util.IdentityHashMap[ASTEnv, CTypeSystemFrontend with CTypeCache with CDeclUse] = new util.IdentityHashMap()
    private val fileToTUnit: util.HashMap[String, TranslationUnit] = new util.HashMap()

    private val cFunctionPointerAnalysis = new CPointerAnalysisFrontend(cModuleInterfacePath, fm)
    var cFunctionPointerEQRelation: CPointerAnalysisContext = _

    private val cModuleInterface: Option[CModuleInterface] =
        cModuleInterfacePath match {
            case Some(path) => Some(new CModuleInterface(path))
            case _ => None
        }

    val startTUnit = addToCache(initialTUnit)

    override def prepareAST[T <: Product](t: T): T = {
        var tunit = super.prepareAST(t.asInstanceOf[TranslationUnit])
        if (options.getConfiguration.isDefined)
            tunit = ProductDerivation.deriveProduct(tunit, options.getTrueSet.get)

        tunit = rewriteFunctionCallsInReturnStmts(tunit, fm)
        tunit = rewriteNestedFunctionCalls(tunit, fm)
        copyPositions(t.asInstanceOf[TranslationUnit], tunit)

        // if pseudo visiting system functions is enabled, add the pseudo function to the tunit
        tunit = if (options.pseudoVisitingSystemLibFunctions) tunit.copy(defs = SPLLIFT_PSEUDO_SYSTEM_FUNCTION_CALL :: tunit.defs) else tunit
        tunit.asInstanceOf[T]
    }

    private def addToCache(_tunit: TranslationUnit): TranslationUnit = {
        println("#preparation tasks for newly loaded translation unit started... ")

        var tunit: TranslationUnit = _tunit

        val (time, _) = StopWatch.measureWallTime(options.getStopWatchPrefix + "tunit_completePreparation", {
            StopWatch.measureUserTime(options.getStopWatchPrefix + "tunit_rewriting", {
                println("#Rewriting AST...")
                tunit = prepareAST(_tunit)
            })

            val env = CASTEnv.createASTEnv(tunit)
            val ts = new CTypeSystemFrontend(tunit, fm) with CTypeCache with CDeclUse

            println("#Typecheck")
            StopWatch.measureUserTime(options.getStopWatchPrefix + "typecheck", {
                ts.checkAST(printResults = !options.silentTypeCheck)
            })

            updateCaches(tunit, env, ts)
            println("#Calculating Pointer Equivalence Realations...")
            calculatePointerEquivalenceRelations
        })

        println("#preparation tasks for newly loaded translation unit finished in " + time + "ms")

        tunit
    }

    private def updateCaches(tunit: TranslationUnit, env: ASTEnv, ts: CTypeSystemFrontend with CTypeCache with CDeclUse) = {
        envToTUnit.put(env, tunit)
        envToTS.put(env, ts)
        fileToTUnit.put(tunit.defs.last.entry.getPositionFrom.getFile, tunit) // Workaround as usually the first definitions are external includes
    }
    private def calculatePointerEquivalenceRelations = {
        StopWatch.measureUserTime(options.getStopWatchPrefix + "pointsToAnalysis", {
            cFunctionPointerEQRelation = cFunctionPointerAnalysis.calculatePointerEquivalenceRelation(getAllKnownTUnitsAsSingleTUnit, (getEnvs, envToTS))
        })
    }
    def getAllKnownTUnits: List[TranslationUnit] = envToTUnit.values.toList

    def getAllKnownTUnitsAsSingleTUnit : TranslationUnit = TranslationUnit(getAllKnownTUnits.foldLeft(List[Opt[ExternalDef]]()) { (l, ast) => l ::: ast.defs })

    def getTunitForEnv(env: ASTEnv) = envToTUnit.get(env)

    def getTSForEnv(env: ASTEnv) = envToTS.get(env)

    def getTunitForFile(file: String): Option[TranslationUnit] = {
        val dbgFile = file /*.replace("/Users/andi/Dropbox", "/home/janker") */
        if (fileToTUnit.containsKey(dbgFile)) Some(fileToTUnit.get(dbgFile))
        else loadTUnit(file)
    }

    def getEnv(node: AST): Option[ASTEnv] =
        getEnvs.find {_.containsASTElem(node)}

    def getEnvs: List[ASTEnv] = envToTUnit.keySet.toList

    def isNameKnown(name: Opt[String]): Boolean =
        cModuleInterface match {
            case None => false
            case Some(interface) => interface.isNameKnown(name.entry)
        }

    def getNameLocations(name: Opt[String]): Option[List[String]] =
        cModuleInterface match {
            case None => None
            case Some(interface) =>
                interface.getPositions(name.entry) match {
                    case None => None
                    case Some(pos) => Some(pos.map(_.getFile))
                }
        }

    def loadTUnit(inputfile: String): Option[TranslationUnit] = {
        val filename = if (inputfile.startsWith("file ")) inputfile.substring("file ".length) else inputfile
        println("#loading:\t" + filename)

        val (source, extension) = filename.splitAt(filename.lastIndexOf(".c"))
        val inputStream = new ObjectInputStream(new GZIPInputStream(new FileInputStream(source + ".ast"))) {
            override protected def resolveClass(desc: ObjectStreamClass) = super.resolveClass(desc)
        }

        val tunit = inputStream.readObject().asInstanceOf[TranslationUnit]
        inputStream.close()

        Some(addToCache(tunit))
    }

    def getFPointerDestDefsNames(pointer: Opt[Expr], currFuncName: String) : List[Opt[String]] = {
        def removeEquivalenceClassScope(eqString : Opt[String]) : Opt[String] = {
            val nameOnly = eqString.entry.split('$')(1)
            val pointerExprRemoved = nameOnly.replaceAll("&", "").replaceAll("\\*", "")
            eqString.copy(entry = pointerExprRemoved)
        }

        val eqClass = getPointerEquivalenceClass(pointer, currFuncName)

        val eqFunctionObjectNames = eqClass match {
            case Some(equivalenceClass) => equivalenceClass.objectNames.toOptList().filter(_.entry.contains("§GLOBAL$"))
            case _ => List()
        }

        eqFunctionObjectNames.map(removeEquivalenceClassScope)
    }

    private def buildEquivalenceClassLookupQuery(pointer: Expr, scope: String): String = {
        def genObjectName(expr: Expr): String =
            expr match {
                case PostfixExpr(p: PostfixExpr, s) => "(" + genObjectName(p) + ")" +  PrettyPrinter.print(s)
                case PointerDerefExpr(p) => genObjectName(p)
                case p: PostfixExpr => PrettyPrinter.print(p)
                case x =>
                    WarningsCache.add("No equivalence class lookup query rule for:\t" + x)
                    PrettyPrinter.print(x)
            }

        getPlainFileName(pointer) + "§" + scope + "$" + genObjectName(pointer)
    }


    private def getPointerEquivalenceClass(pointer: Opt[Expr], currFuncName: String): Option[EquivalenceClass] = {
        val eqQuery = buildEquivalenceClassLookupQuery(pointer.entry, currFuncName)
        val eqRelation = cFunctionPointerEQRelation.find(eqQuery)

        if (eqRelation.isEmpty)
            WarningsCache.add("No pointer relation found for lookup: " + pointer + "\nQuery:\t" + eqQuery)

        eqRelation
    }
}
