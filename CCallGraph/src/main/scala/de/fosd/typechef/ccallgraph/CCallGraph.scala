package de.fosd.typechef.ccallgraph

import de.fosd.typechef.conditional.{Conditional, Opt}
import de.fosd.typechef.parser.c._
import de.fosd.typechef.typesystem.{CType, ConditionalTypeMap}

/**
 * Created by gferreir on 9/20/14.
 */
class CCallGraph(val lookupExprType: Expr => Conditional[CType], val structLookup: (String, Boolean) => Conditional[ConditionalTypeMap]) {

    var objectNames: Set[String] = Set()

    def showObjectNames() = {
        println(objectNames)
    }

    def extractObjectNames(ast: AST): Option[String] = {
        println(ast)
        ast match {
            case TranslationUnit(list: List[Opt[ExternalDef]]) => for (Opt(_, e) <- list) extractObjectNames(e); None
            case FunctionDef(_, _, _, compoundStmt: CompoundStatement) => extractObjectNames(compoundStmt)

            case CompoundStatement(list: List[Opt[Statement]]) => for (Opt(_, e) <- list) extractObjectNames(e); None
            case AtomicNamedDeclarator(_, _, _) => None
            case ExprStatement(exprStmt) => extractObjectNames(exprStmt)

            case Declaration(_, initList) => for (Opt(_, e) <- initList) extractObjectNames(e); None
            case InitDeclaratorI(decl, _, init) =>

                // typesystem lookup
                // println(decl.getId +" => "+ lookupExprType(decl.getId))
                // val ctype = lookupExprType(decl.getId)
                // if (ctype!=null)
                //      ctype.map(t=>t.atype match {
                //          case CStruct(name, isUnion) =>
                //              val fields = structLookup(name, isUnion)
                //              println(fields)
                //          case _ =>
                //      })

                val initNames = init.flatMap(i => extractObjectNames(i.expr))
                if (initNames.isDefined) {
                    objectNames += decl.getName
                    objectNames += initNames.get
                }
                initNames

            case DeclarationStatement(d) => extractObjectNames(d); None
            case ReturnStatement(expr) => {
                val exprStr = extractObjectNames(expr.get)
                exprStr
            }
            case LcurlyInitializer(list: List[Opt[Initializer]]) => for (Opt(_, e) <- list) extractObjectNames(e); None
            case _: ArrayAccess => Some("[]")

            case Initializer(_, expr) =>
                val exprStr = extractObjectNames(expr); exprStr

            case StringLit(list: List[Opt[String]]) => for (Opt(_, e) <- list) e; None

            case ForStatement(expr1, expr2, expr3, _) => {
                extractObjectNames(expr1.get)
                extractObjectNames(expr2.get)
                extractObjectNames(expr3.get)
                None
            }

            case FunctionCall(exprList: ExprList) => extractObjectNames(exprList); None
            case ExprList(list: List[Opt[Expr]]) => for (Opt(_, e) <- list) extractObjectNames(e); None

            case IfStatement(condition: Conditional[Expr], thenBranch: Conditional[Statement], elifs: List[Opt[ElifStatement]], elseBranch: Option[Conditional[Statement]]) => {
                condition.map(e => extractObjectNames(e));
                None
                thenBranch.map(stmt => extractObjectNames(stmt));
                None
                if (elseBranch.isDefined) elseBranch.get.map(stmt => extractObjectNames(stmt));
                None
                for (Opt(_, e) <- elifs) extractObjectNames(e);
                None
            }

            case AssignExpr(target, operation, source) => {
                val exprStr1 = extractObjectNames(target)
                val exprStr2 = extractObjectNames(source)
                if (exprStr1.isDefined && exprStr2.isDefined) {
                    objectNames += exprStr1.get
                    objectNames += exprStr2.get
                }
                exprStr2
            };
            case Id(value) => Some(value)
            case PointerCreationExpr(expr) => {
                val exprStr = extractObjectNames(expr)
                if (exprStr.isDefined) {
                    objectNames += exprStr.get
                    objectNames += ("&" + exprStr.get)
                }
                exprStr.map("&" + _)
            }
            case PostfixExpr(expr, suffixExpr) => {
                val exprStr1 = extractObjectNames(expr)
                val exprStr2 = extractObjectNames(suffixExpr)

                if (exprStr1.isDefined && exprStr2.isDefined) {
                    if (exprStr2.get startsWith "->") {
                        objectNames += exprStr1.get
                        objectNames += ("*" + exprStr1.get)
                        objectNames += (exprStr1.get + exprStr2.get)
                    } else if (exprStr2.get startsWith ".") {
                        objectNames += exprStr1.get
                        objectNames += (exprStr1.get + exprStr2.get)
                    }
                }
                exprStr1.flatMap(e1 => exprStr2.map(e2 => e1 + e2))
            };
            case PointerPostfixSuffix(operator: String, expr) => {
                val str = extractObjectNames(expr)
                str.map(operator + _)
            }
            case PointerDerefExpr(expr) => {
                val exprStr = extractObjectNames(expr)
                exprStr.map(exprStr => {
                    objectNames = objectNames + exprStr
                    objectNames = objectNames + ("*" + exprStr)
                })
                exprStr.map("*" + _)
            }
            case _: NAryExpr => None
            case _: Constant => None
            case _: SimplePostfixSuffix => None
            case _: CastExpr => None
            case e: Expr => throw new NotImplementedError("Not implemented!!! %s".format(e))
            case s: Statement => throw new NotImplementedError("Not implemented!!! %s".format(s))
            case d: Declarator => throw new NotImplementedError("Not implemented!!! %s".format(d))
        }
    }
}
