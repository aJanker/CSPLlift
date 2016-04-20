package de.fosd.typechef.spllift

import java.io._
import java.util
import java.util.zip.GZIPInputStream

import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.cpointeranalysis._
import de.fosd.typechef.featureexpr.FeatureModel
import de.fosd.typechef.featureexpr.bdd.BDDFeatureModel
import de.fosd.typechef.parser.c._
import de.fosd.typechef.typesystem.linker.CModuleInterface
import de.fosd.typechef.typesystem.{CDeclUse, CTypeCache, CTypeSystemFrontend}

import scala.collection.JavaConversions._


trait CInterCFGElementsCache {

    val cInterCFGElementsCacheEnv: CInterCFGElementsCacheEnv

    def findEnv(node: AST): Option[ASTEnv] =
        cInterCFGElementsCacheEnv.getEnvs.find {_.containsASTElem(node)}

    def getTranslationUnit(node: AST): Option[TranslationUnit] =
        findEnv(node) match {
            case Some(env) =>
                cInterCFGElementsCacheEnv.getTunitForEnv(env) match {
                    case null => None
                    case tunit => Some(tunit)
                }
            case _ => None
        }

    def getTypeSystem(node: AST): Option[CTypeSystemFrontend with CTypeCache with CDeclUse] =
        findEnv(node) match {
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
                        Some(Opt(ft, f))
                    case _ => None
                }
                res ::: foundDefs
            })
        }
}

class CInterCFGElementsCacheEnv private(initialTUnit: TranslationUnit, fm: FeatureModel, cModuleInterfacePath: Option[String], options: CInterCFGOptions) extends EnforceTreeHelper with CInterCFGPseudoVistingSystemLibFunctions with CInterCFGCommons with PointerContext {

    def this(initialTUnit: TranslationUnit, fm: FeatureModel = BDDFeatureModel.empty, options: CInterCFGOptions = new DefaultCInterCFGOptions) =
        this(initialTUnit, fm, options.getModuleInterfacePath, options)

    private val envToTUnit: util.IdentityHashMap[ASTEnv, TranslationUnit] = new util.IdentityHashMap()
    private val envToTS: util.IdentityHashMap[ASTEnv, CTypeSystemFrontend with CTypeCache with CDeclUse] = new util.IdentityHashMap()
    private val fileToTUnit: util.HashMap[String, TranslationUnit] = new util.HashMap()

    private val cPointerEQAnalysis = new CPointerAnalysisFrontend(cModuleInterfacePath)
    private var cPointerEqRelation : CPointerAnalysisContext = null

    private val cModuleInterface: Option[CModuleInterface] =
        cModuleInterfacePath match {
            case Some(path) => Some(new CModuleInterface(path))
            case _ => None
        }

    val startTUnit = add(initialTUnit)

    override def prepareAST[T <: Product](t: T): T = {
        // TODO Rewrite Nested function calls (outter(inner(x))
        val tunit = super.prepareAST(t)
        tunit
    }

    def add(_tunit: TranslationUnit): TranslationUnit = {
        // if pseudo visiting system functions is enabled, add the pseudo function to the tunit
        val pTunit = prepareAST(_tunit)
        val tunit = if (options.pseudoVisitingSystemLibFunctions) pTunit.copy(defs = PSEUDO_SYSTEM_FUNCTION_CALL :: pTunit.defs) else pTunit

        val env = CASTEnv.createASTEnv(tunit)
        val ts = new CTypeSystemFrontend(tunit, fm) with CTypeCache with CDeclUse
        ts.checkAST()

        updateCaches(tunit, env, ts)
        updatePointerEquivalenceRelations

        tunit
    }

    private def updateCaches(tunit: TranslationUnit, env: ASTEnv, ts: CTypeSystemFrontend with CTypeCache with CDeclUse) = {
        envToTUnit.put(env, tunit)
        envToTS.put(env, ts)
        fileToTUnit.put(tunit.defs.last.entry.getPositionFrom.getFile, tunit) // Workaround as usually the first definitions are external includes
    }
    private def updatePointerEquivalenceRelations = {
        val allDefs = getAllKnownTUnits.foldLeft(List[Opt[ExternalDef]]()) { (l, ast) => l ::: ast.defs }
        cPointerEqRelation = cPointerEQAnalysis.calculatePointerEquivalenceRelation(TranslationUnit(allDefs), getPlainFileName(allDefs.last.entry))
    }
    def getAllKnownTUnits: List[TranslationUnit] = envToTUnit.values.toList

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
            case Some(interface) =>
                interface.getPositions(name.entry) match {
                    case None => None
                    case Some(pos) => Some(pos.map(_.getFile))
                }
        }

    def loadTUnit(inputfile: String): Option[TranslationUnit] = {
        val filename = if (inputfile.startsWith("file ")) inputfile.substring("file ".length) else inputfile

        try {
            val (source, extension) = filename.splitAt(filename.lastIndexOf(".c"))
            val fr = new ObjectInputStream(new GZIPInputStream(new FileInputStream(source + ".ast"))) {
                override protected def resolveClass(desc: ObjectStreamClass) = super.resolveClass(desc)
            }

            val tunit = fr.readObject().asInstanceOf[TranslationUnit]
            fr.close()

            Some(add(tunit))
        } catch {
            case e: ObjectStreamException => System.err.println("failed loading serialized AST: " + e.getMessage); None
        }
    }

    def buildEquivalenceClassLookup(pointer: Expr, scope: String): String = {
        def genObjectName(expr: Expr): String =
            expr match {
                case p: PostfixExpr => PrettyPrinter.print(p)
                case PointerDerefExpr(pExpr) => ObjectNameOperator.PointerDereference + "(" + genObjectName(pExpr) + ")"
            }

        getPlainFileName(pointer) + "§" + scope + "$" + genObjectName(pointer)
    }


    def getPointerEquivalenceClass(pointer: Opt[Expr], cfg: CInterCFG): Option[EquivalenceClass] = {
        val lookup = buildEquivalenceClassLookup(pointer.entry, cfg.getMethodOf(pointer).entry.getName)
        cPointerEqRelation.find(lookup)
    }
}
