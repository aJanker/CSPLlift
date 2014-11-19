package de.fosd.typechef.ccallgraph

import de.fosd.typechef.conditional.{Conditional, Opt}
import de.fosd.typechef.parser.c._

import scalax.collection.edge.LDiEdge
import scalax.collection.mutable.Graph

/**
 * Created by gferreir on 9/20/14.
 *
 */
class CCallGraph {

    var extractedObjectNames: Set[String] = Set()
    var callGraph = Graph[EquivalenceClass, LDiEdge]()


    def calculatePERelation(program: AST): Graph[EquivalenceClass, LDiEdge] = {
        // extract object names from the AST
        //extractObjectNames(program)
        extractedObjectNames = Set("a", "&a", "*a", "a.c")

        // create initial equivalance class for each object name
        initEquivalanceClasses

        // create edge between initia; equivalence classes
        constructPrefixSets

        println(callGraph.nodes)
        println(callGraph.edges)

        callGraph
    }

    private def initEquivalanceClasses() = {
        for (o <- extractedObjectNames) {
            insertEquivalenceClass(new EquivalenceClass(Set(o)))
        }
    }

    def find(objectName: String): Option[EquivalenceClass] = {
        for (node: EquivalenceClass <- callGraph.nodes.toOuter) {
            if (node.objectNames contains objectName) {
                return Some(node)
            }
        }
        None
    }

    private def constructPrefixSets = {
        val objectNamesCrossProduct = extractedObjectNames flatMap { x => extractedObjectNames map { y => (x, y)}}

        for ((o, o1) <- objectNamesCrossProduct) {
            val eqClassObjectO = find(o)
            val eqClassObjectO1 = find(o1)

            if (o.equals("&%s".format(o1)) && eqClassObjectO.isDefined && eqClassObjectO1.isDefined) {
                // add * edge from o1 to o
                callGraph += LDiEdge(eqClassObjectO.get, eqClassObjectO1.get)("*")

            } else if (o.equals("*%s".format(o1)) && eqClassObjectO.isDefined && eqClassObjectO1.isDefined) {
                // add * edge from o to o1
                callGraph += LDiEdge(eqClassObjectO1.get, eqClassObjectO.get)("*")

            } else if (o.startsWith("%s.".format(o1)) && eqClassObjectO.isDefined && eqClassObjectO1.isDefined) {
                val objectNameFields = o.split('.')
                if (objectNameFields.size == 2) {
                    val eqClassObjectOPartial = find(objectNameFields(0))

                    if (eqClassObjectOPartial.isDefined) {
                        // add field edge from o to o1
                        callGraph += LDiEdge(eqClassObjectOPartial.get, eqClassObjectO1.get)(objectNameFields(1))
                    }
                }
            }
        }
    }

    private def apply(source: EquivalenceClass, target: EquivalenceClass, operator: String): Unit = {
        callGraph += LDiEdge(source, target)(operator)
    }

    def insertEquivalenceClass(e: EquivalenceClass) = {
        callGraph += e
    }

    // Equivalence class of object names
    class EquivalenceClass(initialSet: Set[String]) {

        private var objectNamesSet: Set[String] = initialSet

        def objectNames(): Set[String] = objectNamesSet

        def addObjectName(objectName: String) = {
            objectNamesSet += objectName
        }

        def union(other: EquivalenceClass): EquivalenceClass = {
            new EquivalenceClass(this.objectNames().union(other.objectNames()))
        }

        override def toString: String = objectNamesSet.mkString("{", ",", "}")
    }

    def showExtractedObjectNames() = {
        println(extractedObjectNames)
    }

    private def extractExpr(expr: Expr): Option[String] = {
        //println(expr)
        expr match {
            case Id(name: String) => Some(name)
            case Constant(value: String) => None
            case StringLit(name: List[Opt[String]]) => None
            case UnaryExpr(kind: String, e: Expr) => extractExpr(e)
            case SizeOfExprT(typeName: TypeName) => None
            case SizeOfExprU(expr: Expr) => extractExpr(expr); None
            case CastExpr(typeName: TypeName, expr: Expr) => extractExpr(expr); None
            case ExprList(exprs: List[Opt[Expr]]) => for (Opt(_, e) <- exprs) extractExpr(e); None

            case PointerDerefExpr(castExpr: Expr) => {
                val exprStr = extractExpr(castExpr)
                exprStr.map(exprStr => {
                    extractedObjectNames += exprStr
                    extractedObjectNames += ("*" + exprStr)
                })
                exprStr.map("*" + _)
            }
            case PointerCreationExpr(castExpr: Expr) => {
                val exprStr = extractExpr(castExpr)
                if (exprStr.isDefined) {
                    extractedObjectNames += exprStr.get
                    extractedObjectNames += ("&" + exprStr.get)
                }
                exprStr.map("&" + _)
            }
            case UnaryOpExpr(kind: String, castExpr: Expr) => extractExpr(castExpr)

            // pointer arithmetic is ignored by analysis ('others' is not relevant)
            case NAryExpr(expr: Expr, others: List[Opt[NArySubExpr]]) => {
                val exprStr = extractExpr(expr)
                exprStr
            }
            case ConditionalExpr(condition: Expr, thenExpr: Option[Expr], elseExpr: Expr) => extractExpr(condition)
            case AssignExpr(target: Expr, operation: String, source: Expr) => {
                val exprStr1 = extractExpr(target)
                val exprStr2 = extractExpr(source)
                if (exprStr1.isDefined && exprStr2.isDefined) {
                    extractedObjectNames += exprStr1.get
                    extractedObjectNames += exprStr2.get
                }
                exprStr2
            }
            case PostfixExpr(expr, suffixExpr) => {
                val exprStr1 = extractExpr(expr)
                val exprStr2 = extractObjectNames(suffixExpr)

                // member access operators
                if (exprStr1.isDefined && exprStr2.isDefined) {
                    // -> operator
                    if (exprStr2.get startsWith "->") {
                        extractedObjectNames += exprStr1.get
                        extractedObjectNames += ("*" + exprStr1.get)
                        extractedObjectNames += (exprStr1.get + exprStr2.get)
                        // . (dot) operator
                    } else if (exprStr2.get startsWith ".") {
                        extractedObjectNames += exprStr1.get
                        extractedObjectNames += (exprStr1.get + exprStr2.get)
                    }
                }
                exprStr1.flatMap(e1 => exprStr2.map(e2 => e1 + e2))
            };

        }
    }

    private def extractStmt(stmt: Statement): Option[String] = {
        //println(stmt)
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
                s.map(stmt => extractStmt(stmt));
                None
            }
            case GotoStatement(target: Expr) => extractExpr(target); None
            case ContinueStatement() => None
            case BreakStatement() => None
            case ReturnStatement(expr: Option[Expr]) => extractExpr(expr.get); None
            case LabelStatement(id: Id, attribute: Option[AttributeSpecifier]) => None
            case CaseStatement(c: Expr) => extractExpr(c); None
            case DefaultStatement() => None
            case IfStatement(condition: Conditional[Expr], thenBranch: Conditional[Statement], elifs: List[Opt[ElifStatement]], elseBranch: Option[Conditional[Statement]]) => {
                condition.map(expr => extractExpr(expr))
                thenBranch.map(stmt => extractStmt(stmt))
                for (Opt(_, s) <- elifs) extractObjectNames(s)
                if (elseBranch.isDefined) elseBranch.get.map(stmt => extractStmt(stmt))
                None
            }
            case SwitchStatement(expr: Expr, s: Conditional[Statement]) => {
                extractExpr(expr)
                s.map(stmt => extractStmt(stmt))
                None
            }
            case DeclarationStatement(decl: Declaration) => extractObjectNames(decl)
        }
    }

    private def extractDecl(decl: Declarator): Option[String] = {
        None
    }

    def extractObjectNames(ast: AST): Option[String] = {
        //println(ast)
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
                    extractedObjectNames += decl.getName
                    extractedObjectNames += initNames.get
                }
                initNames
            }
        }
    }
}

object Main {
    def main(args: Array[String]) {
        val c: CCallGraph = new CCallGraph
        c.calculatePERelation(null)
    }
}
