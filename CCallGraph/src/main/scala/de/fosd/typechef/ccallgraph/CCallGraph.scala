package de.fosd.typechef.ccallgraph

import de.fosd.typechef.conditional.{Conditional, Opt}
import de.fosd.typechef.parser.c._
import de.fosd.typechef.typesystem.{CType, ConditionalTypeMap}

/**
 * Created by gferreir on 9/20/14.
 *
 */
class CCallGraph {

    var objectNames: Set[String] = Set()

    def showObjectNames() = {
        println(objectNames)
    }

    def extractExpr(expr: Expr): Option[String] = {
        println(expr)
        expr match {
            case Id(name: String) => Some(name)
            case Constant(value: String) => None
            case StringLit(name: List[Opt[String]]) => None
            case UnaryExpr(kind: String, e: Expr) => extractExpr(e)
            case SizeOfExprT(typeName: TypeName) => None
            case SizeOfExprU(expr: Expr) => extractExpr(expr); None
            case CastExpr(typeName: TypeName, expr: Expr) => extractExpr(expr); None
            case PointerDerefExpr(castExpr: Expr) => {
                val exprStr = extractExpr(castExpr)
                exprStr.map(exprStr => {
                    objectNames += exprStr
                    objectNames += ("*" + exprStr)
                })
                exprStr.map("*" + _)
            }
            case PointerCreationExpr(castExpr: Expr) => {
                val exprStr = extractExpr(castExpr)
                if (exprStr.isDefined) {
                    objectNames += exprStr.get
                    objectNames += ("&" + exprStr.get)
                }
                exprStr.map("&" + _)
            }
            case UnaryOpExpr(kind: String, castExpr: Expr) => extractExpr(castExpr)

            // pointer arithmetic is ignored by analysis ('others' list is not relevant)
            case NAryExpr(expr: Expr, others: List[Opt[NArySubExpr]]) => {
                val exprStr = extractExpr(expr)
                exprStr
            }
            case ConditionalExpr(condition: Expr, thenExpr: Option[Expr], elseExpr: Expr) => extractExpr(condition)
            case AssignExpr(target: Expr, operation: String, source: Expr) => {
                val exprStr1 = extractExpr(target)
                val exprStr2 = extractExpr(source)
                if (exprStr1.isDefined && exprStr2.isDefined) {
                    objectNames += exprStr1.get
                    objectNames += exprStr2.get
                }
                exprStr2
            }
            case PostfixExpr(expr, suffixExpr) => {
                val exprStr1 = extractExpr(expr)
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
            case ExprList(exprs: List[Opt[Expr]]) => for (Opt(_, e) <- exprs) extractExpr(e); None
        }
    }

    def extractStmt(stmt: Statement): Option[String] = {
        println(stmt)
        stmt match {
            case CompoundStatement(innerStatements: List[Opt[Statement]]) => for (Opt(_, e) <- innerStatements) extractStmt(e); None
            case EmptyStatement() => None
            case ExprStatement(expr: Expr) => extractExpr(expr)
            case WhileStatement(expr: Expr, s: Conditional[Statement]) => extractExpr(expr); s.map(stmt => extractStmt(stmt)); None
            case DoStatement(expr: Expr, s: Conditional[Statement]) => extractExpr(expr); s.map(stmt => extractStmt(stmt)); None
            case ForStatement(expr1: Option[Expr], expr2: Option[Expr], expr3: Option[Expr], s: Conditional[Statement]) => {
                extractExpr(expr1.get)
                extractExpr(expr2.get)
                extractExpr(expr3.get)
                None
            }
            case GotoStatement(target: Expr) => extractExpr(target); None
            case ContinueStatement() => None
            case BreakStatement() => None
            case ReturnStatement(expr: Option[Expr]) => None
            case LabelStatement(id: Id, attribute: Option[AttributeSpecifier]) => None
            case CaseStatement(c: Expr) => None
            case DefaultStatement() => None
            case IfStatement(condition: Conditional[Expr], thenBranch: Conditional[Statement], elifs: List[Opt[ElifStatement]], elseBranch: Option[Conditional[Statement]]) => None
            case SwitchStatement(expr: Expr, s: Conditional[Statement]) => None
            case DeclarationStatement(decl: Declaration) => extractObjectNames(decl)
        }
    }

    def extractDecl(decl: Declarator): Option[String] = {
        None
    }

    def extractObjectNames(ast: AST): Option[String] = {
        ast match {
            case TranslationUnit(list: List[Opt[ExternalDef]]) => for (Opt(_, e) <- list) extractObjectNames(e); None
            case FunctionDef(_, _, _, compoundStmt: Statement) => extractStmt(compoundStmt)
            case Declaration(declSpecs: List[Opt[Specifier]], init: List[Opt[InitDeclarator]]) => for (Opt(_, e) <- init) extractObjectNames(e); None
            case FunctionCall(exprList: ExprList) => extractObjectNames(exprList); None
            case PointerPostfixSuffix(operator: String, expr) => {
                val str = extractExpr(expr)
                str.map(operator + _)
            }
            case _: SimplePostfixSuffix => None
            case ArrayAccess(expr: Expr) => Some("[]")
            case NArySubExpr(op: String, e: Expr) => None
            case e: Expr => extractExpr(e)
            case s: Statement => extractStmt(s)
            case InitDeclaratorI(decl: Declarator, _, init: Option[Initializer]) => {
                val initNames = init.flatMap(i => extractObjectNames(i.expr))
                if (initNames.isDefined) {
                    objectNames += decl.getName
                    objectNames += initNames.get
                }
                initNames
            }
        }
    }
}
