package de.fosd.typechef.cspllift.commons

import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.crewrite.{IntraCFG, ProductDerivation}
import de.fosd.typechef.cspllift.CICFGConcreteStmt
import de.fosd.typechef.cspllift.evaluation.Sampling
import de.fosd.typechef.error.Position
import de.fosd.typechef.featureexpr.bdd.{BDDFeatureModel, BDDNoFeatureModel}
import de.fosd.typechef.featureexpr.{FeatureExpr, FeatureExprFactory, FeatureModel}
import de.fosd.typechef.parser.c._
import de.fosd.typechef.typesystem.{CInt, CShort, _}

trait ASTRewritingRules extends ASTRewriting with ASTNavigation with ConditionalNavigation with EnforceTreeHelper {

    def replace[T <: Product, U](t: T, e: U, n: U): T = {
        val r = manytd(rule[Any] {
            case i if i.asInstanceOf[AnyRef] eq e.asInstanceOf[AnyRef] => n
        })
        r(t).getOrElse(t).asInstanceOf[T]
    }

    def insertStmtListBeforeStmt(c: CompoundStatement, e: Opt[Statement], n: List[Opt[Statement]]): CompoundStatement = {
        val r = oncetd(rule[Any] {
            case l: List[_] =>
                l.flatMap(x =>
                    if (x.asInstanceOf[AnyRef] eq e) n ::: List(x)
                    else x :: Nil)
        })
        r(c).getOrElse(c).asInstanceOf[CompoundStatement]
    }

    def replaceStmtWithStmtList[T <: Product](t: T, e: Statement, n: List[Opt[Statement]]): T = {
        val currASTEnv = CASTEnv.createASTEnv(t)
        val cc = findPriorASTElem[CompoundStatement](e, currASTEnv)

        if (cc.isEmpty) return t // statement not part of a compound statement -> can not rewrite

        val parentStmt = parentOpt(e, currASTEnv).asInstanceOf[Opt[Statement]]
        val ccReplacement = replaceStmtWithStmtsListInCCStmt(cc.get, parentStmt, n)

        replace(t, cc.get, ccReplacement)
    }

    def replaceStmtWithStmtsListInCCStmt(c: CompoundStatement, e: Opt[Statement], n: List[Opt[Statement]]): CompoundStatement = {
        val r = oncetd(rule[Any] {
            case l: List[_] =>
                l.flatMap(x =>
                    if (x.asInstanceOf[AnyRef] eq e) n
                    else x :: Nil)
        })
        r(c).getOrElse(c).asInstanceOf[CompoundStatement]
    }
}

trait TUnitRewriteEngine extends ASTNavigation with ConditionalNavigation with ASTRewritingRules {

    private var tmpVariablesCount = 0

    def checkForDuplicates[T <: Product](ast : T): Boolean = {
        val astEnv = CASTEnv.createASTEnv(ast)
        val cFGStmt = getCFGStatements(ast).map(stmt => CICFGConcreteStmt(parentOpt(stmt, astEnv).asInstanceOf[Opt[AST]], stmt.getPositionFrom))
        val diff = cFGStmt.diff(cFGStmt.distinct)

        println(cFGStmt.diff(cFGStmt.distinct))

        diff.nonEmpty
    }

    /**
      * Adds a return statement for all function exit points which are no return statements (e.g. only applicable in void function).
      */
    def addReturnStmtsForNonReturnExits[T <: Product](ast: T, fm: FeatureModel = BDDFeatureModel.empty): T = {
        val cfg = new Object with IntraCFG
        val astEnv = CASTEnv.createASTEnv(ast)

        val voidFunctions = filterAllASTElems[FunctionDef](ast).filter(fDef => fDef.specifiers.exists(_.entry match {
            case v: VoidSpecifier => true
            case _ => false
        }))

        val replacements = voidFunctions.flatMap(func => {
            lazy val condition = astEnv.featureExpr(func)
            lazy val ret = Opt(condition, ReturnStatement(None))
            ret.entry.range = func.range

            func.stmt.innerStatements.lastOption match {
                case Some(Opt(_, r: ReturnStatement)) => None
                case None => None
                case _ => Some((func.stmt.innerStatements, func.stmt.innerStatements :+ ret))
            }

        })

        replacements.foldLeft(ast)((currAST, replacement) => replace(currAST, replacement._1, replacement._2))
    }

    /**
      * Moves variability nested in CFG-Statements up to the CFG Statement by code duplication.
      * We are otherwise unable use CSPLlift as CSPLlift is only able to resolve variability on statement level but not below.
      */
    def removeStmtVariability[T <: Product](ast: T, fm: FeatureModel = BDDFeatureModel.empty): T = {
        assert(ast != null, "ast should not be null")

        val astEnv = CASTEnv.createASTEnv(ast)

        val cfgStmts = getCFGStatements(ast)
        val replacements = cfgStmts.flatMap {

            case c: CFGStmt if isVariable(c) =>
                val parent = parentOpt(c, astEnv)
                val parentCondition = astEnv.featureExpr(c)
                val stmtConditions = filterAllFeatureExpr(c)
                val allStmtConditions = stmtConditions.foldLeft(FeatureExprFactory.True)(_ and _)

                if (!allStmtConditions.equivalentTo(parentCondition, fm)) {
                    val sampling = new Sampling(c, BDDNoFeatureModel)
                    val configs = sampling.conditionConfigurationCoverage(stmtConditions.toSet)
                    val products = configs.flatMap(config => {

                        val trueCond = config.getTrueSet.foldLeft(FeatureExprFactory.True)(_ and _)
                        val falseCond = config.getFalseSet.foldLeft(FeatureExprFactory.True)(_ and _).not()
                        val finalCond = if (falseCond.isSatisfiable(fm)) trueCond.and(falseCond) else trueCond

                        val product = ProductDerivation.deriveProduct(c, config.getTrueFeatures, finalCond)
                        Some(Opt(finalCond, product))
                    })
                    Some((c, products))
                } else None

            case _ => None
        }

        replacements.foldLeft(ast)((currAST, r) => replaceStmtWithStmtList(currAST, r._1, r._2))
    }

    /**
      * Rewrites all function calls nested in return statements from:
      * return foo(x);
      * to:
      * returnTypeOf(foo(x)) tmp = foo(x);
      * return tmp;
      */
    def rewriteFunctionCallsInReturnStmts(tunit: TranslationUnit, fm: FeatureModel = BDDFeatureModel.empty): TranslationUnit = {
        val allReturnStatementsWithFunctionCall =
            filterAllASTElems[ReturnStatement](tunit).filter {
                filterAllASTElems[FunctionCall](_).nonEmpty
            }

        if (allReturnStatementsWithFunctionCall.isEmpty) return tunit

        val ts = new CTypeSystemFrontend(tunit, fm) with CTypeCache with CDeclUse
        ts.checkASTSilent

        def extractExprFromReturnStatement(r: Opt[ReturnStatement]): List[Opt[Statement]] = {
            val (tmpName, tmpDeclaration) = makeTmpDeclarationFromExpr(Opt(r.condition, r.entry.expr.get), ts)
            List(tmpDeclaration, r.copy(entry = r.entry.copy(expr = Some(Id(tmpName)))))
        }

        def replaceSingleNestedFCallInReturnStmt(currentTUnit: TranslationUnit, returnStmt: ReturnStatement): TranslationUnit =
            returnStmt match {
                case r@ReturnStatement(Some(expr)) =>
                    val env = CASTEnv.createASTEnv(currentTUnit)

                    val cc = findPriorASTElem[CompoundStatement](r, env)

                    if (cc.isEmpty) return currentTUnit // return not part of a compound statement -> can not rewrite

                    val parent = parentOpt(r, env).asInstanceOf[Opt[ReturnStatement]]
                    val returnReplacement = extractExprFromReturnStatement(parent)
                    addPreviousRange(r.range, returnReplacement)
                    val ccReplacement = replaceStmtWithStmtsListInCCStmt(cc.get, parent, returnReplacement)

                    replace(currentTUnit, cc.get, ccReplacement)
                case _ => currentTUnit
            }


        allReturnStatementsWithFunctionCall.foldLeft(tunit) {
            (currentTUnit, returnStatementWithFCall) => replaceSingleNestedFCallInReturnStmt(currentTUnit, returnStatementWithFCall)
        }
    }

    /**
      * Rewrites all function calls nested in another function call e.g. int foo =  outer(inner(x)) to:
      * returnTypeOf(inner(x)) tmp = inner(x);
      * int foo = outer(tmp);
      */
    def rewriteNestedFunctionCalls[T <: Product](tunit: T, fm: FeatureModel = BDDFeatureModel.empty): T = {
        def rewrite(nestedFCalls : List[FunctionCall], t : T) : T =
            if (nestedFCalls.isEmpty) t
            else {
                val replacement = rewriteNestedFCalls(t, fm, nestedFCalls.head)
                val res = replaceFCallInTunit(t, replacement)
                rewrite(getNestedFunctionCalls(res), res)
            }

        rewrite(getNestedFunctionCalls(tunit), tunit)
    }

    private def replaceFCallInTunit[T <: Product](tunit: T, replacement: (FunctionCall, FunctionCall, List[Opt[Statement]])): T = {
        val env = CASTEnv.createASTEnv(tunit)

        val cc = findPriorASTElem[CompoundStatement](replacement._1, env)
        val stmt = findPriorASTElem[Statement](replacement._1, env)

        if (cc.isEmpty || stmt.isEmpty) {
            Console.err.println("Warning: function rewrite rule may not by exhaustive for:\t" + replacement._1)
            return tunit
        } // return not part of a compound statement -> can not rewrite

        val parent = parentOpt(stmt.get, env).asInstanceOf[Opt[Statement]]

        addPreviousRange(replacement._1.range, replacement._2)
        addPreviousRange(stmt.get.range, replacement._3)

        val ccReplacement = insertStmtListBeforeStmt(cc.get, parent, replacement._3)
        replace(replace(tunit, cc.get, ccReplacement), replacement._1, replacement._2)
    }

    private def rewriteNestedFCalls[T <: Product](tunit: T, fm: FeatureModel = BDDFeatureModel.empty, orgFCall: FunctionCall): (FunctionCall, FunctionCall, List[Opt[Statement]]) = {
        val ts = new CTypeSystemFrontend(tunit.asInstanceOf[TranslationUnit], fm) with CTypeCache with CDeclUse
        val env = CASTEnv.createASTEnv(tunit)
        ts.checkASTSilent

        val nestedOptCalls = filterAllASTElems[PostfixExpr](orgFCall).reverse.map(parentOpt(_, env))

        def rewriteSingleNestedFCall(x: (FunctionCall, List[Opt[Statement]]), curr: Opt[Expr]): (FunctionCall, List[Opt[Statement]]) = {
            val (f, newDecls) = x
            val (name, declaration) = makeTmpDeclarationFromExpr(curr, ts)
            val replacedExpr = curr.copy(entry = Id(name))
            val newCall = replace(f, curr, replacedExpr)

            (newCall, declaration :: newDecls)
        }

        val res = nestedOptCalls.foldLeft((orgFCall, List[Opt[Statement]]()))((fCall_newDecls, o) =>
            o match {
                case Opt(_, e: Expr) => rewriteSingleNestedFCall(fCall_newDecls, o.asInstanceOf[Opt[Expr]])
                case _ => fCall_newDecls
            })

        (orgFCall, res._1, res._2.reverse)
    }


    private def addPreviousRange(range: Option[(Position, Position)], p: Product) = filterAllASTElems[AST](p).foreach {
        case ast if !ast.hasPosition => ast.range = range
        case _ =>
    }


    private def makeTmpDeclarationFromExpr(expr: Opt[Expr], ts: CTypeSystemFrontend with CTypeCache with CDeclUse, namePrefix: String = "__SPLLIFT_TMP"): (String, Opt[DeclarationStatement]) = {
        val tmpSpecifiers = getExprTypeSpecifiers(expr, ts)
        val tmpName = namePrefix + tmpVariablesCount
        val tmpNameDeclarator = AtomicNamedDeclarator(List(), Id(tmpName), List())
        val tmpInitializer = Some(Initializer(None, expr.entry))
        val tmpInitDeclarator = List(Opt(expr.condition, InitDeclaratorI(tmpNameDeclarator, List(), tmpInitializer)))
        tmpVariablesCount += 1

        val decl = DeclarationStatement(Declaration(tmpSpecifiers, tmpInitDeclarator))

        addPreviousRange(expr.entry.range, decl)

        (tmpName, Opt(expr.condition, decl))
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

    private def getCFGStatements[T <: Product](ast: T) = filterAllASTElems[CFGStmt](ast).flatMap(filterAllASTElems[Statement]).distinct

    private def getNestedFunctionCalls[T <: Product](tunit: T): List[FunctionCall] =
        filterASTElems[FunctionCall](tunit).filter(fCall => {
            filterAllASTElems[FunctionCall](fCall.params).nonEmpty
        })
}