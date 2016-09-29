package de.fosd.typechef.cspllift.commons

import de.fosd.typechef.conditional.{Choice, Opt}
import de.fosd.typechef.crewrite.IntraCFG
import de.fosd.typechef.cspllift.evaluation.Sampling
import de.fosd.typechef.featureexpr.bdd.{BDDFeatureExprFactory, BDDFeatureModel, BDDNoFeatureModel}
import de.fosd.typechef.featureexpr.{FeatureExpr, FeatureExprFactory, FeatureModel}
import de.fosd.typechef.parser.c._
import de.fosd.typechef.typesystem.{CInt, CShort, _}
import org.kiama.rewriting.Rewriter._

trait KiamaRewritingRules extends EnforceTreeHelper {

    def replace[T <: Product, U](t: T, e: U, n: U): T = {
        val r = manybu(rule[Any] {
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
        r(c).get.asInstanceOf[CompoundStatement]
    }

    def replaceStmtWithStmtsListInCCStmt(c: CompoundStatement, e: Opt[Statement], n: List[Opt[Statement]]): CompoundStatement = {
        val r = oncetd(rule[Any] {
            case l: List[_] =>
                l.flatMap(x =>
                    if (x.asInstanceOf[AnyRef] eq e) n
                    else x :: Nil)
        })
        r(c).get.asInstanceOf[CompoundStatement]
    }

    def deriveProductWithCondition[T <: Product](ast: T, selectedFeatures: Set[String], condition: FeatureExpr = BDDFeatureExprFactory.TrueB): T = {
        assert(ast != null)

        val prod = manytd(rule[Product] {
            case l: List[_] if l.forall(_.isInstanceOf[Opt[_]]) => {
                var res: List[Opt[_]] = List()
                // use l.reverse here to omit later reverse on res or use += or ++= in the thenBranch
                for (o <- l.reverse.asInstanceOf[List[Opt[_]]])
                    if (o.condition.evaluate(selectedFeatures)) {
                        val copy = o.copy(condition = condition)
                        copyPositions(o, copy)
                        res ::= copy
                    }
                res
            }
            case Choice(feature, thenBranch, elseBranch) => {
                if (feature.evaluate(selectedFeatures)) thenBranch
                else elseBranch
            }
            case a: AST =>
                val copy = a.clone()
                copyPositions(a, copy)
                a

        })
        val cast = prod(ast).get.asInstanceOf[T]
        copyPositions(ast, cast)
        cast
    }
}

trait TUnitRewriteEngine extends ASTNavigation with ConditionalNavigation with KiamaRewritingRules {

    private var tmpVariablesCount = 0

    /**
      * Moves variability nested in CFG-Statements up to the CFG Statement by code duplication.
      * We are otherwise unable use CSPLlift as CSPLlift is only able to resolve variability on statement level but not below.
      */
    def removeInStatementVariability[T <: Product](ast: T, fm: FeatureModel = BDDFeatureModel.empty): T = {
        assert(ast != null, "ast should not be null")

        val astEnv = CASTEnv.createASTEnv(ast)

        val cfgStmts = filterAllASTElems[CFGStmt](ast).flatMap(filterAllASTElems[Statement]).distinct
        val replacements = cfgStmts.flatMap {

            case c: CFGStmt if isVariable(c) =>
                val parentCondition = parentOpt(c, astEnv).condition
                val stmtConditions = filterAllFeatureExpr(c)
                val allStmtConditions = stmtConditions.foldLeft(FeatureExprFactory.True)(_ and _)

                if (!allStmtConditions.equivalentTo(parentCondition, fm)) {
                    val sampling = new Sampling(c, BDDNoFeatureModel)
                    val configs = sampling.conditionConfigurationCoverage(stmtConditions.toSet)
                    val products = configs.flatMap(config => {

                        val trueCond = config.getTrueSet.foldLeft(FeatureExprFactory.True)(_ and _)
                        val falseCond = config.getFalseSet.foldLeft(FeatureExprFactory.True)(_ and _).not()
                        val finalCond = if (falseCond.isSatisfiable(fm)) trueCond.and(falseCond) else trueCond

                        val product = deriveProductWithCondition(c, config.getTrueFeatures, finalCond)
                        Some(Opt(finalCond, product))
                    })
                    Some((c, products))
                } else None

            case _ => None
        }

        replacements.foldLeft(ast)((currAST, r) => {
            val currASTEnv = CASTEnv.createASTEnv(currAST)
            val cc = findPriorASTElem[CompoundStatement](r._1, currASTEnv)

            if (cc.isEmpty)
                return currAST // return not part of a compound statement -> can not rewrite

            val ccReplacement =
                replaceStmtWithStmtsListInCCStmt(cc.get, parentOpt(r._1, currASTEnv).asInstanceOf[Opt[Statement]], r._2.asInstanceOf[List[Opt[Statement]]])

            replace(currAST, cc.get, ccReplacement)
        })
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
                    val ccReplacement = replaceStmtWithStmtsListInCCStmt(cc.get, parent, returnReplacement)

                    replace(currentTUnit, cc.get, ccReplacement)
                case _ => currentTUnit
            }


        allReturnStatementsWithFunctionCall.foldLeft(tunit) {
            (currentTUnit, returnStatementWithFCall) => replaceSingleNestedFCallInReturnStmt(currentTUnit, returnStatementWithFCall)
        }
    }


    def rewriteCallsAsExitStmts[T <: Product](tunit: T, fm: FeatureModel = BDDFeatureModel.empty): T = {
        /*val fCalls = filterAllASTElems[FunctionCall](tunit)
        val env = CASTEnv.createASTEnv(tunit)
        val cfg = new rewriteCFG

        val exitFCalls = fCalls.filter(call => cfg.succ(call, env).exists {
            case Opt(_, f: FunctionDef) => true
            case _ => false
        })

        if(exitFCalls.nonEmpty) {
            println(exitFCalls)
        } */

        tunit
    }

    /**
      * Rewrites all function calls nested in another function call e.g. int foo =  outer(inner(x)) to:
      * returnTypeOf(inner(x)) tmp = inner(x);
      * int foo = outer(tmp);
      */
    def rewriteNestedFunctionCalls[T <: Product](tunit: T, fm: FeatureModel = BDDFeatureModel.empty): T = {
        val nestedFunctionCalls = getNestedFunctionCalls(tunit)

        if (nestedFunctionCalls.isEmpty) return tunit

        val ts = new CTypeSystemFrontend(tunit.asInstanceOf[TranslationUnit], fm) with CTypeCache with CDeclUse
        val env = CASTEnv.createASTEnv(tunit)
        ts.checkASTSilent

        def rewriteAllNestedFCalls(orgFCall: FunctionCall): (FunctionCall, FunctionCall, List[Opt[Statement]]) = {

            var previousReplacements = List[(Opt[Expr], Opt[Expr])]()
            val nestedOptCalls = filterAllASTElems[PostfixExpr](orgFCall).reverse.map(parentOpt(_, env))

            def rewriteSingleNestedFCall(x: (FunctionCall, List[Opt[Statement]]), curr: Opt[Expr]): (FunctionCall, List[Opt[Statement]]) = {
                val (f, newDecls) = x
                val (tmpName, tmpDeclaration) = makeTmpDeclarationFromExpr(curr, ts)
                val replacedExpr = curr.copy(entry = Id(tmpName))

                // lookup if we have already replaced some parts of the extracted function call expr, if so replace with the cached value
                val previousReplacement = previousReplacements.filter(prev => filterAllOptElems(curr).exists(prev._1.eq(_)))

                val (currReplacement, currDeclaration) =
                    if (previousReplacement.nonEmpty) {
                        val correctedDeclaration = previousReplacement.foldLeft(tmpDeclaration) {
                            (tmp, prev) => {
                                tmp.copy(entry = replace(tmp.entry, prev._1, prev._2.copy()))
                            }
                        }

                        val tmpEnv = CASTEnv.createASTEnv(f)
                        val currCall = findPriorASTElem[PostfixExpr](previousReplacements.head._2, tmpEnv)

                        if (currCall.isEmpty) {
                            Console.err.println("Could not convert nested function parameter:\t" + f)
                            (curr, correctedDeclaration)
                        } else (parentOpt(currCall.get, tmpEnv).asInstanceOf[Opt[Expr]], correctedDeclaration)
                    } else (curr, tmpDeclaration)

                previousReplacements = (currReplacement, replacedExpr) :: previousReplacements
                (replace(f, currReplacement, replacedExpr), currDeclaration :: newDecls)


            }

            val res = nestedOptCalls.foldLeft((orgFCall, List[Opt[Statement]]()))((fCall_newDecls, o) =>
                o match {
                    case Opt(_, e: Expr) => rewriteSingleNestedFCall(fCall_newDecls, o.asInstanceOf[Opt[Expr]])
                    case _ => fCall_newDecls
                })

            (orgFCall, res._1, res._2.reverse)
        }

        val toReplace = nestedFunctionCalls.map(rewriteAllNestedFCalls)

        toReplace.foldLeft(tunit)((t, r) => {
            val env = CASTEnv.createASTEnv(t)

            val cc = findPriorASTElem[CompoundStatement](r._1, env)
            val stmt = findPriorASTElem[Statement](r._1, env)

            if (cc.isEmpty || stmt.isEmpty) {
                Console.err.println("Warning: function rewrite rule may not by exhaustive for:\t" + r._1)
                return t
            } // return not part of a compound statement -> can not rewrite

            val parent = parentOpt(stmt.get, env).asInstanceOf[Opt[Statement]]
            val ccReplacement = insertStmtListBeforeStmt(cc.get, parent, r._3)

            val tmp = replace(t, cc.get, ccReplacement)
            replace(tmp, r._1, r._2)

        })
    }

    private def makeTmpDeclarationFromExpr(expr: Opt[Expr], ts: CTypeSystemFrontend with CTypeCache with CDeclUse, namePrefix: String = "__SPLLIFT_TMP"): (String, Opt[DeclarationStatement]) = {
        val tmpSpecifiers = getExprTypeSpecifiers(expr, ts)
        val tmpName = namePrefix + tmpVariablesCount
        val tmpNameDeclarator = AtomicNamedDeclarator(List(), Id(tmpName), List())
        val tmpInitializer = Some(Initializer(None, expr.entry))
        val tmpInitDeclarator = List(Opt(expr.condition, InitDeclaratorI(tmpNameDeclarator, List(), tmpInitializer)))
        tmpVariablesCount += 1

        (tmpName, Opt(expr.condition, DeclarationStatement(Declaration(tmpSpecifiers, tmpInitDeclarator))))
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

    private def getNestedFunctionCalls[T <: Product](tunit: T): List[FunctionCall] =
        filterASTElems[FunctionCall](tunit).filter(fCall => {
            filterAllASTElems[FunctionCall](fCall.params).nonEmpty
        })

    private class rewriteCFG extends IntraCFG {}

}