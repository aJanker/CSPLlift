package de.fosd.typechef.cspllift.cintercfg

import java.io._
import java.util
import java.util.zip.GZIPInputStream

import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.cpointeranalysis._
import de.fosd.typechef.crewrite.ProductDerivation
import de.fosd.typechef.cspllift.cifdsproblem.CFlowConstants
import de.fosd.typechef.cspllift.commons.{CInterCFGCommons, RewritingRules}
import de.fosd.typechef.customization.StopWatch
import de.fosd.typechef.customization.clinking.CModuleInterface
import de.fosd.typechef.error.Position
import de.fosd.typechef.featureexpr.bdd.{BDDFeatureModel, True}
import de.fosd.typechef.featureexpr.{FeatureExprFactory, FeatureModel}
import de.fosd.typechef.parser.c._
import de.fosd.typechef.typesystem.{CDeclUse, CTypeCache, _}
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.JavaConversions._

object CInterCFGBenchmarkMarks {

    lazy val TUNIT_LOAD = "TUNIT_LOAD"

    lazy val TUNIT_INIT = "TUNIT_INIT"

    // The following values are subvalues of the init task.
    lazy val TUNIT_REWRITE = "TUNIT_REWRITE"

    lazy val TUNIT_TYPECHECK = "TUNIT_TYPECHECK"

    lazy val TUNIT_PRODUCTGENERATION = "TUNIT_TYPECHECK"

    lazy val POINTER_COMPUTATION = "POINTER_COMPUTATION"
}

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

    def getExternalDefinitions(name: Opt[String]): List[CICFGFDef] = {
        cInterCFGElementsCacheEnv.getNameLocations(name).getOrElse(List()).flatMap(path => {
            val tUnit =
                cInterCFGElementsCacheEnv.getTunitForFile(path) match {
                    case Some(t) => t
                    case None => throw new FileNotFoundException(path)
                }

            val foundDefs = tUnit.defs.flatMap {
                case o@Opt(ft, f@FunctionDef(_, decl, _, _)) if decl.getName.equalsIgnoreCase(name.entry) && ft.and(name.condition).isSatisfiable(/* TODO FM */) =>
                    Some(CICFGFDef(Opt(ft, f)))
                case _ => None
            }

            foundDefs
        })
    }
}

class CInterCFGElementsCacheEnv private(initialTUnit: TranslationUnit, fm: FeatureModel, cModuleInterfacePath: Option[String], options: CInterCFGConfiguration, benchmarkTag: Option[String]) extends RewritingRules with CFlowConstants with CInterCFGCommons with EnforceTreeHelper with PointerContext {

    def this(initialTUnit: TranslationUnit, fm: FeatureModel = BDDFeatureModel.empty, options: CInterCFGConfiguration = new DefaultCInterCFGConfiguration, benchmarkTag: Option[String] = None) =
        this(initialTUnit, fm, options.getModuleInterfacePath, options, benchmarkTag)

    private lazy val logger: Logger = LoggerFactory.getLogger(getClass)

    private val envToTUnit: util.IdentityHashMap[ASTEnv, TranslationUnit] = new util.IdentityHashMap()
    private val envToTS: util.IdentityHashMap[ASTEnv, CTypeSystemFrontend with CTypeCache with CDeclUse] = new util.IdentityHashMap()
    private val fileToTUnit: util.HashMap[String, TranslationUnit] = new util.HashMap()
    private val tunitToPseudoCall: util.IdentityHashMap[AST, Opt[FunctionDef]] = new util.IdentityHashMap()

    var cFunctionPointerEQRelation: Option[CPointerAnalysisContext] = None

    private val cModuleInterface: Option[CModuleInterface] =
        cModuleInterfacePath match {
            case Some(path) => Some(new CModuleInterface(path))
            case _ => None
        }

    val startTUnit = addToCache(initialTUnit)

    /**
      * Enforcing the compatibility of the used ast representation with the IFDS/IDE framework.
      */
    override def prepareAST[T <: Product](t: T): T = {
        var tUnit = t.asInstanceOf[TranslationUnit]
        val file = tUnit.defs.last.entry.getFile.get

        if (logger.isInfoEnabled) logger.info("Rewriting " + file)

        tUnit = super.prepareAST(tUnit)

        // Rewrite undisciplined variability
        tUnit = removeUndisciplinedVariability(tUnit, fm)
        // Rewrite combined call and return statements
        tUnit = rewriteCombinedCallAndExitStmts(tUnit, fm)
        // Rewrite nested function calls
        tUnit = rewriteNestedFunctionCalls(tUnit, fm)
        // Enforce single function entry point
        tUnit = enforceSingleFunctionEntryPoint(tUnit)
        // Enforce exit in void over return stmt
        tUnit = addReturnStmtsForNonReturnVoidExits(tUnit, fm)

        if (options.getConfiguration.isDefined)
            tUnit = ProductDerivation.deriveProduct(ProductDerivation.deriveProduct(tUnit, options.getTrueSet.get), options.getTrueSet.get)

        tUnit.asInstanceOf[T]
    }

    private def addToCache(tunit: TranslationUnit): TranslationUnit = {
        if (tunit.defs.isEmpty) {
            logger.warn("Empty tunit. did not add to cache")
            return tunit
        }

        val file = tunit.defs.last.entry.getFile.get

        /**
          * Perform upfront tunit init tasks like rewriting for IFDS/IDE and typechecking
          */
        val init = StopWatch.measureProcessCPUTime(benchmarkTag.getOrElse("") + CInterCFGBenchmarkMarks.TUNIT_INIT, {
            var res: TranslationUnit = tunit
            if (logger.isInfoEnabled) logger.info("Upfront computation of newly loaded translation unit started for " + file + ".")

            var pseudoSystemFunctionCall : Option[Opt[FunctionDef]] = None

            res = StopWatch.measureProcessCPUTime(benchmarkTag.getOrElse("") + CInterCFGBenchmarkMarks.TUNIT_REWRITE, {
                res = prepareAST(res)

                val pos = new TokenPosition(file, 0, 0, 0)
                pseudoSystemFunctionCall = Some(genPseudoSystemFunctionCall(Some(pos, pos)))

                // if pseudo visiting system functions is enabled, add the pseudo function to the tunit
                if (options.pseudoVisitingSystemLibFunctions) {
                    val tUnitPseudo = res.copy(defs = pseudoSystemFunctionCall.get :: res.defs)
                    tUnitPseudo.range = res.range
                    tUnitPseudo
                } else res
            })._2


            val ts = new CTypeSystemFrontend(res, fm) with CTypeCache with CDeclUse
            if (options.silentTypeCheck) ts.makeSilent()

            if (logger.isInfoEnabled) logger.info("Typechecking " + file)
            StopWatch.measureProcessCPUTime(benchmarkTag.getOrElse("") + CInterCFGBenchmarkMarks.TUNIT_TYPECHECK, {
                ts.checkAST(printResults = !options.silentTypeCheck)
            })

            val env = CASTEnv.createASTEnv(res)
            updateCaches(res, file, env, ts, pseudoSystemFunctionCall)

            if (logger.isInfoEnabled) logger.info("Calculating Pointer Equivalence Realations: " + file)
            calculatePointerEquivalenceRelations

            res
        })

        if (logger.isInfoEnabled) logger.info("Upfront computation of newly loaded translation unit finished in " + init._1 + "ms for " + file + ".")

        init._2
    }

    private def updateCaches(tunit: TranslationUnit, file: String, env: ASTEnv, ts: CTypeSystemFrontend with CTypeCache with CDeclUse, pseudoSystemFunctionCall: Option[Opt[FunctionDef]]) = {
        envToTUnit.put(env, tunit)
        envToTS.put(env, ts)
        fileToTUnit.put(file, tunit)
        if (pseudoSystemFunctionCall.isDefined) tunitToPseudoCall.put(tunit, pseudoSystemFunctionCall.get)
    }

    private def calculatePointerEquivalenceRelations =
        if (options.computePointer) StopWatch.measureProcessCPUTime(benchmarkTag.getOrElse("") + CInterCFGBenchmarkMarks.POINTER_COMPUTATION, {
            val cFunctionPointerAnalysis = new CPointerAnalysisFrontend(fm)
            val env = (getEnvs, envToTS)
            cFunctionPointerEQRelation = Some(cFunctionPointerAnalysis.calculatePointerEquivalenceRelation(getAllKnownTUnitsAsSingleTUnit, env))
        })

    private def genPseudoSystemFunctionCall(range: Option[(Position, Position)]): Opt[FunctionDef] = {
        val call = Opt(True, FunctionDef(List(Opt(FeatureExprFactory.True, VoidSpecifier())), AtomicNamedDeclarator(List(), Id(SPLLIFT_PSEUDO_SYSTEM_FUNCTION_CALL_NAME), List(Opt(FeatureExprFactory.True, DeclIdentifierList(List())))), List(), CompoundStatement(List(Opt(FeatureExprFactory.True, ReturnStatement(None))))))
        val addRange = everywherebu(query[Product] { case a: AST => if (!a.hasPosition) a.range = range })

        addRange(call)
        call
    }

    def getAllKnownTUnits: List[TranslationUnit] = envToTUnit.values.toList

    def getAllFiles = fileToTUnit.toList

    def getAllKnownTUnitsAsSingleTUnit: TranslationUnit = TranslationUnit(getAllKnownTUnits.foldLeft(List[Opt[ExternalDef]]()) { (l, ast) => l ::: ast.defs }.distinct)

    def getTunitForEnv(env: ASTEnv) = envToTUnit.get(env)

    def getTSForEnv(env: ASTEnv) = envToTS.get(env)

    def getTunitForFile(file: String): Option[TranslationUnit] = {
        if (fileToTUnit.containsKey(file)) Some(fileToTUnit.get(file))
        else loadTUnit(file)
    }

    def getEnv(node: AST): Option[ASTEnv] =
        getEnvs.find {
            _.containsASTElem(node)
        }

    def getEnvs: List[ASTEnv] = envToTUnit.keySet.toList

    def getPseudoSystemFunctionCall(tunit: TranslationUnit): Opt[FunctionDef] = tunitToPseudoCall.get(tunit)

    def isNameKnown(name: Opt[String]): Boolean =
        cModuleInterface match {
            case Some(interface) => interface.isNameKnown(name.entry)
            case _ => false
        }

    def getNameLocations(name: Opt[String]): Option[List[String]] =
        cModuleInterface match {
            case Some(interface) =>
                interface.getPositions(name.entry) match {
                    case None => None
                    case Some(pos) => Some(pos.map(_.getFile))
                }
            case _ => None

        }

    def loadTUnit(inputfile: String): Option[TranslationUnit] = {
        val tunit = StopWatch.measureProcessCPUTime(benchmarkTag.getOrElse("") + CInterCFGBenchmarkMarks.TUNIT_LOAD, {
            val fileExtension = if (inputfile.endsWith(".pi")) ".pi" else ".c"
            val filename = if (inputfile.startsWith("file ")) inputfile.substring("file ".length) else inputfile
            val dbgName = filename//.replace("/home/janker/Masterarbeit", "/Users/andi/Masterarbeit")

            if (logger.isInfoEnabled) logger.info("Loading:\t" + dbgName)

            val (source, _) = dbgName.splitAt(dbgName.lastIndexOf(fileExtension))

            val inputStream = new ObjectInputStream(new GZIPInputStream(new FileInputStream(source + ".ast")))
            val loaded = inputStream.readObject().asInstanceOf[TranslationUnit]
            inputStream.close()

            loaded
        })._2

        Some(addToCache(tunit))
    }

    def getFPointerDestDefsNames(pointer: Opt[Expr], currFuncName: String): List[Opt[String]] = {
        def removeEquivalenceClassScope(eqString: Opt[String]): Opt[String] = {
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
                case PostfixExpr(p: PostfixExpr, s) => "(" + genObjectName(p) + ")" + PrettyPrinter.print(s)
                case PointerDerefExpr(p) => genObjectName(p)
                case p: PostfixExpr => PrettyPrinter.print(p)
                case x =>
                    if (logger.isDebugEnabled) logger.debug("No equivalence class lookup query rule for:\t" + x)
                    PrettyPrinter.print(x)
            }

        getPlainFileName(pointer) + "§" + scope + "$" + genObjectName(pointer)
    }


    private def getPointerEquivalenceClass(pointer: Opt[Expr], currFuncName: String): Option[EquivalenceClass] = {
        val eqQuery = buildEquivalenceClassLookupQuery(pointer.entry, currFuncName)
        val eqRelation =
            if (cFunctionPointerEQRelation.isDefined) cFunctionPointerEQRelation.get.find(eqQuery)
            else None

        if (eqRelation.isEmpty && logger.isDebugEnabled) logger.debug("No pointer relation found for lookup: " + pointer + "\nQuery:\t" + eqQuery)

        eqRelation
    }
}