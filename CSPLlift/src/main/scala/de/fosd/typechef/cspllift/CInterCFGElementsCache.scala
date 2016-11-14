package de.fosd.typechef.cspllift

import java.io._
import java.util
import java.util.zip.GZIPInputStream

import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.cpointeranalysis._
import de.fosd.typechef.crewrite.ProductDerivation
import de.fosd.typechef.cspllift.cifdsproblem.CFlowConstants
import de.fosd.typechef.cspllift.commons.{CInterCFGCommons, RewritingRules, SolverNotifications}
import de.fosd.typechef.customization.StopWatch
import de.fosd.typechef.customization.clinking.CModuleInterface
import de.fosd.typechef.featureexpr.FeatureModel
import de.fosd.typechef.featureexpr.bdd.BDDFeatureModel
import de.fosd.typechef.parser.c._
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

class CInterCFGElementsCacheEnv private(initialTUnit: TranslationUnit, fm: FeatureModel, cModuleInterfacePath: Option[String], options: CInterCFGConfiguration) extends RewritingRules with CFlowConstants with CInterCFGCommons with EnforceTreeHelper with PointerContext {

    def this(initialTUnit: TranslationUnit, fm: FeatureModel = BDDFeatureModel.empty, options: CInterCFGConfiguration = new DefaultCInterCFGConfiguration) =
        this(initialTUnit, fm, options.getModuleInterfacePath, options)

    private val envToTUnit: util.IdentityHashMap[ASTEnv, TranslationUnit] = new util.IdentityHashMap()
    private val envToTS: util.IdentityHashMap[ASTEnv, CTypeSystemFrontend with CTypeCache with CDeclUse] = new util.IdentityHashMap()
    private val fileToTUnit: util.HashMap[String, TranslationUnit] = new util.HashMap()
    private val tunitToPseudoCall: util.IdentityHashMap[AST, Opt[FunctionDef]] = new util.IdentityHashMap()

    private var cFunctionPointerAnalysis = new CPointerAnalysisFrontend(fm)
    var cFunctionPointerEQRelation: CPointerAnalysisContext = _

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
        println("\t#Rewriting...")
        var tUnit = t.asInstanceOf[TranslationUnit]
        tUnit = super.prepareAST(tUnit)

        // Rewrite undisciplined variability
        tUnit = removeUndisciplinedVariability(tUnit, fm)
        // Rewrite combined call and return statements
        tUnit = rewriteFunctionCallsInReturnStmts(tUnit, fm)
        // Rewrite nested function calls
        tUnit = rewriteNestedFunctionCalls(tUnit, fm)
        // Enforce single function entry point
        tUnit = enforceSingleFunctionEntryPoint(tUnit)
        // Enforce exit in void over return stmt
        tUnit = addReturnStmtsForNonReturnExits(tUnit, fm)

        if (options.getConfiguration.isDefined)
            tUnit = ProductDerivation.deriveProduct(ProductDerivation.deriveProduct(tUnit, options.getTrueSet.get), options.getTrueSet.get)

        checkPositionInformation(tUnit)

        tUnit.asInstanceOf[T]
    }

    private def addToCache(tUnit: TranslationUnit): TranslationUnit = {
        if (tUnit.defs.isEmpty) {
            println("#empty tunit. did not add to cache")
            return tUnit
        }
        println("#upfront computation of newly loaded translation unit started... ")

        var _tUnit: TranslationUnit = tUnit

        val file = tUnit.defs.last.entry.getFile.get

        val (time, _) = StopWatch.measureWallTime(options.getStopWatchPrefix + "tunit_completePreparation", {
            StopWatch.measureUserTime(options.getStopWatchPrefix + "tunit_rewriting", _tUnit = prepareAST(_tUnit))

            val pos = new TokenPosition(file, 0, 0, 0)
            val pseudoSystemFunctionCall = genPseudoSystemFunctionCall(Some(pos, pos))

            // if pseudo visiting system functions is enabled, add the pseudo function to the tunit
            _tUnit =
              if (options.pseudoVisitingSystemLibFunctions) {
                  val _tUnitPseudo = _tUnit.copy(defs = pseudoSystemFunctionCall :: _tUnit.defs)
                  _tUnitPseudo.range = _tUnit.range
                  _tUnitPseudo
              } else _tUnit

            checkPositionInformation(_tUnit)

            val ts = new CTypeSystemFrontend(_tUnit, fm) with CTypeCache with CDeclUse
            if (options.silentTypeCheck) ts.makeSilent()

            println("\t#Typechecking...")
            StopWatch.measureUserTime(options.getStopWatchPrefix + "typecheck", {
                ts.checkAST(printResults = !options.silentTypeCheck)
            })

            val env = CASTEnv.createASTEnv(_tUnit)
            updateCaches(_tUnit, file, env, ts, pseudoSystemFunctionCall)
            println("\t#Calculating Pointer Equivalence Realations...")
            calculatePointerEquivalenceRelations
        })

        println("#upfront computation of newly loaded translation unit finished in " + time + "ms")

        _tUnit
    }

    private def updateCaches(tunit: TranslationUnit, file: String, env: ASTEnv, ts: CTypeSystemFrontend with CTypeCache with CDeclUse, pseudoSystemFunctionCall: Opt[FunctionDef]) = {
        envToTUnit.put(env, tunit)
        envToTS.put(env, ts)
        fileToTUnit.put(file, tunit)
        tunitToPseudoCall.put(tunit, pseudoSystemFunctionCall)
    }

    private def calculatePointerEquivalenceRelations = {
        StopWatch.measureUserTime(options.getStopWatchPrefix + "pointsToAnalysis", {
            cFunctionPointerAnalysis = new CPointerAnalysisFrontend(fm)
            cFunctionPointerEQRelation = cFunctionPointerAnalysis.calculatePointerEquivalenceRelation(getAllKnownTUnitsAsSingleTUnit, (getEnvs, envToTS))
        })
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
        val fileExtension = if (inputfile.endsWith(".pi")) ".pi" else ".c"
        val filename = if (inputfile.startsWith("file ")) inputfile.substring("file ".length) else inputfile
        val dbgName = filename //filename.replace("/home/janker/Masterarbeit", "/Users/andi/Masterarbeit")
        println("#loading:\t" + dbgName)

        val (source, _) = dbgName.splitAt(dbgName.lastIndexOf(fileExtension))
        val inputStream = new ObjectInputStream(new GZIPInputStream(new FileInputStream(source + ".ast"))) {
            override protected def resolveClass(desc: ObjectStreamClass) = super.resolveClass(desc)
        }

        val tunit = inputStream.readObject().asInstanceOf[TranslationUnit]
        inputStream.close()

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
                    SolverNotifications.add("No equivalence class lookup query rule for:\t" + x)
                    PrettyPrinter.print(x)
            }

        getPlainFileName(pointer) + "§" + scope + "$" + genObjectName(pointer)
    }


    private def getPointerEquivalenceClass(pointer: Opt[Expr], currFuncName: String): Option[EquivalenceClass] = {
        val eqQuery = buildEquivalenceClassLookupQuery(pointer.entry, currFuncName)
        val eqRelation = cFunctionPointerEQRelation.find(eqQuery)

        if (eqRelation.isEmpty)
            SolverNotifications.add("No pointer relation found for lookup: " + pointer + "\nQuery:\t" + eqQuery)

        None
    }
}