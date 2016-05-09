package de.fosd.typechef.spllift

import java.io._
import java.util
import java.util.zip.GZIPInputStream

import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.cpointeranalysis._
import de.fosd.typechef.featureexpr.bdd.BDDFeatureModel
import de.fosd.typechef.featureexpr.{FeatureExpr, FeatureExprFactory, FeatureModel}
import de.fosd.typechef.parser.c._
import de.fosd.typechef.spllift.commons.CInterCFGCommons
import de.fosd.typechef.typesystem._
import de.fosd.typechef.typesystem.linker.CModuleInterface

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
                    Some(Opt(ft.and(name.condition), f))
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
    private var cPointerEqRelation: CPointerAnalysisContext = null

    private val cModuleInterface: Option[CModuleInterface] =
        cModuleInterfacePath match {
            case Some(path) => Some(new CModuleInterface(path))
            case _ => None
        }

    val startTUnit = add(initialTUnit)

    override def prepareAST[T <: Product](t: T): T = {
        var tunit = super.prepareAST(t).asInstanceOf[AST]
        tunit = rewriteFunctionCallsInReturns(tunit)
        println("insert")
        println(PrettyPrinter.print(tunit))
        rewriteNestedFunctionCalls(tunit).asInstanceOf[T]
    }

    /**
      * Rewrites all function calls nested in return statements from:
      *     return foo(x);
      * to:
      *     type tmp = foo(x);
      *     return tmp;
      */
    private def rewriteFunctionCallsInReturns(tunit: AST): AST = {
        val allReturnStatementsWithFunctionCall =
            filterAllASTElems[ReturnStatement](tunit).filter {
                filterAllASTElems[de.fosd.typechef.parser.c.FunctionCall](_).nonEmpty
            }

        if (allReturnStatementsWithFunctionCall.isEmpty) return tunit

        var tmpCount = 0

        val ts = new CTypeSystemFrontend(tunit.asInstanceOf[TranslationUnit], fm) with CTypeCache with CDeclUse
        ts.checkASTSilent

        def extractExprFromReturnStatement(r: Opt[ReturnStatement]): List[Opt[Statement]] = {
            val tmpSpecifiers = getExprTypeSpecifiers(Opt(r.condition, r.entry.expr.get), ts)
            val tmpName = "__SPLLIFT" + tmpCount
            val tmpNameDeclarator = AtomicNamedDeclarator(List(), Id(tmpName), List())
            val tmpInitializer = Some(Initializer(None, r.entry.expr.get))
            val tmpInitDeclarator = List(Opt(r.condition, InitDeclaratorI(tmpNameDeclarator, List(), tmpInitializer)))

            val tmpDeclartion = Opt(r.condition, DeclarationStatement(Declaration(tmpSpecifiers, tmpInitDeclarator)))

            tmpCount += 1
            List(tmpDeclartion, r.copy(entry = r.entry.copy(expr = Some(Id(tmpName)))))
        }

        allReturnStatementsWithFunctionCall.foldLeft(tunit) {
            (currentTUnit, returnStatementWithFCall) => {
                val env = CASTEnv.createASTEnv(currentTUnit)
                returnStatementWithFCall match {
                    case r@ReturnStatement(Some(expr)) => {
                        val cc = findPriorASTElem[CompoundStatement](r, env)
                        if (cc.isEmpty) return currentTUnit // return not part of a compound statement -> can not rewrite

                        val parent = parentOpt(r, env).asInstanceOf[Opt[ReturnStatement]]
                        val returnReplacement = extractExprFromReturnStatement(parent)
                        val ccReplacement = replaceStmtWithStmtsListInCCStmt(cc.get, parent, returnReplacement)

                        replace(currentTUnit, cc.get, ccReplacement)
                    }
                    case _ => currentTUnit
                }
            }
        }
    }

    private def getExprTypeSpecifiers(expr: Opt[Expr], ts: CTypeSystemFrontend with CTypeCache with CDeclUse) = {
        def aTypeToASTTypeSpecifier(a: AType, condition: FeatureExpr = FeatureExprFactory.True): List[Opt[TypeSpecifier]] =
            a match {
                case CSigned(b) => Opt(condition, SignedSpecifier()) :: basicTypeToASTTypeSpecifier(b, condition)
                case CUnsigned(b) => Opt(condition, UnsignedSpecifier()) :: basicTypeToASTTypeSpecifier(b, condition)
                case CSignUnspecified(b) => basicTypeToASTTypeSpecifier(b, condition)
                case CBool() => List(Opt(condition, IntSpecifier()))
                case CDouble() => List(Opt(condition, DoubleSpecifier()))
                case CFloat() => List(Opt(condition, FloatSpecifier()))
                case CLongDouble() => List(Opt(condition, LongSpecifier()), Opt(condition, DoubleSpecifier()))
                case CPointer(t) => aTypeToASTTypeSpecifier(t, condition)
                case CStruct(name, isUnion) => List(Opt(condition, StructOrUnionSpecifier(isUnion, Some(Id(name)), None, List(), List())))
                case CVoid() | CZero() => List(Opt(condition, VoidSpecifier()))
                case missed =>
                    scala.Console.err.println("No atype definiton found for " + missed + "!")
                    List(Opt(condition, VoidSpecifier()))
            }

        def basicTypeToASTTypeSpecifier(b: CBasicType, condition: FeatureExpr = FeatureExprFactory.True): List[Opt[TypeSpecifier]] =
            b match {
                case CChar() => List(Opt(condition, CharSpecifier()))
                case CLongLong() => List(Opt(condition, LongSpecifier()), Opt(condition, LongSpecifier()))
                case CLong() => List(Opt(condition, LongSpecifier()))
                case CInt128() => List(Opt(condition, Int128Specifier()))
                case CInt() => List(Opt(condition, IntSpecifier()))
                case CShort() => List(Opt(condition, ShortSpecifier()))
            }

        ts.lookupExprType(expr.entry).toOptList.flatMap(t => aTypeToASTTypeSpecifier(t.entry.atype, t.condition.and(expr.condition)))
    }

    private def rewriteNestedFunctionCalls[T <: Product](tunit: T): T = {
        val nestedFunctionCalls = getNestedFunctionCalls(tunit)

        if (nestedFunctionCalls.isEmpty) return tunit

        var tmpCounter = 0;

        println(PrettyPrinter.print(tunit.asInstanceOf[TranslationUnit]))

        val ts = new CTypeSystemFrontend(tunit.asInstanceOf[TranslationUnit], fm) with CTypeCache with CDeclUse
        val env = CASTEnv.createASTEnv(tunit)
        ts.checkASTSilent

        nestedFunctionCalls.map(f => {
            val nestedOptCalls = filterAllASTElems[PostfixExpr](f).reverse.map(parentOpt(_, env))
            val optOfCall = parentOpt(f, env)
            nestedOptCalls.foreach(l =>
                println("dbg: " + getExprTypeSpecifiers(l.asInstanceOf[Opt[Expr]], ts) + " " + PrettyPrinter.print(l.entry.asInstanceOf[AST])))
        })

        println(tunit)

        tunit

    }

    private def getNestedFunctionCalls[T <: Product](tunit: T): List[de.fosd.typechef.parser.c.FunctionCall] = {
        val fCalls = filterASTElems[de.fosd.typechef.parser.c.FunctionCall](tunit).filter(fCall => {
            filterAllASTElems[de.fosd.typechef.parser.c.FunctionCall](fCall.params).nonEmpty
        })

        fCalls
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

    def getTunitForFile(file: String): Option[TranslationUnit] = {
        val dbgFile = file.replace("/Users/andi/Dropbox", "/home/janker")
        if (fileToTUnit.containsKey(dbgFile)) Some(fileToTUnit.get(dbgFile))
        else loadTUnit(file)
    }

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
