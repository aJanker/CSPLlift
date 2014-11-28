package de.fosd.typechef.ccallgraph

import de.fosd.typechef.conditional.{Conditional, Opt}
import de.fosd.typechef.parser.c._

/**
 * Created by gferreir on 9/20/14.
 *
 */
class CCallGraph {

    var extractedObjectNames: Set[String] = Set()
    var equivalenceClasses: Set[EquivalenceClass] = Set()


    def calculatePERelation(program: AST): Set[EquivalenceClass] = {
        // extract object names from the AST
        extractObjectNames(program)
        //extractedObjectNames = Set("a", "&a", "*a", "a.c")

        // create initial equivalance class for each object name
        initEquivalanceClasses()

        // create edges between initial equivalence classes
        createInitialPrefixSets()

        // merge equivalence classes
        val assignmentsSet = extractProgramAssignments(program)

        // for each assignment, merge if sets are different
        for ((assignee, assignor) <- assignmentsSet) {
            mergeIfDifferent(assignee, assignor)
        }

        equivalenceClasses
    }

    def initEquivalanceClasses() = {
        for (o <- extractedObjectNames) {
            equivalenceClasses += new EquivalenceClass(Set(o), Set())
        }
    }

    def find(objectName: String): Option[EquivalenceClass] = {
        for (node: EquivalenceClass <- equivalenceClasses) {
            if (node.objectNames contains objectName) {
                return Some(node)
            }
        }
        None
    }

    def createInitialPrefixSets() = {
        val objectNamesCrossProduct = extractedObjectNames flatMap {
            x => extractedObjectNames map {
                y => (x, y)
            }
        }

        for ((o, o1) <- objectNamesCrossProduct) {
            val eqClassObjectO = find(o)
            val eqClassObjectO1 = find(o1)

            // pointer cretion operator
            if (o.equals("&%s".format(o1))) {
                // add * edge from o1 to o
                eqClassObjectO.get.addPrefix(("*", o1))
                //callGraph += LDiEdge(eqClassObjectO, eqClassObjectO1)("*")


                // pointer dereference operator
            } else if (o.equals("*%s".format(o1))) {
                // add * edge from o to o1
                eqClassObjectO1.get.addPrefix(("*", o))
                //callGraph += LDiEdge(eqClassObjectO1, eqClassObjectO)("*")

                // struct dot access operator
            } else if (o.startsWith("%s.".format(o1))) {
                val objectNameFields = o.split('.')
                if (objectNameFields.size == 2) {
                    val eqClassObjectOPartial = find(objectNameFields(0))

                    // add field edge from o to o1
                    eqClassObjectO1.get.addPrefix((objectNameFields(1), o))
                    // callGraph += LDiEdge(eqClassObjectO1, eqClassObjectO)(objectNameFields(1))
                }

                /*
                 *  FIXME: should we treat the -> before constructing the graph?
                 *  FIXME: maybe make all structure's access uniform (reduce all to use the . operator)
                 */
                // struct pointer access operator  (dereference + dot)
            } else if (o.startsWith("%s->".format(o1))) {
                val objectNameFields = o.split("->")
                if (objectNameFields.size == 2) {
                    val eqClassObjectOPartial = find("*%s".format(objectNameFields(0)))

                    // add field edge from o to o1
                    eqClassObjectOPartial.get.addPrefix((objectNameFields(1), o))
                    //callGraph += LDiEdge(eqClassObjectOPartial, eqClassObjectO)(objectNameFields(1))
                }
            }
        }
    }

    def extractProgramAssignments(ast: AST): Set[(String, String)] = {
        Set(("", ""))
    }

    def mergeIfDifferent(assignee: String, assignor: String) {
        val eqClassAssignee = find(assignee).get
        val eqClassAssignor = find(assignor).get

        if (!eqClassAssignee.equals(eqClassAssignor)) {
            merge(eqClassAssignee, eqClassAssignor)
        }
    }

    def merge(e1: EquivalenceClass, e2: EquivalenceClass) {
        val newObjectNamesSet: Set[String] = e1.objectNames().union(e2.objectNames())
        var newPrefixSet: Set[(String, String)] = e1.prefixes()

        // loop both prefix sets
        for ((a, o) <- e2.prefixes()) {
            val commonPrefix = newPrefixSet.filter({ case ((a1, o1)) => a.equals(a1)})

            if (!commonPrefix.isEmpty) {
                commonPrefix.map({ case ((_, o1)) =>

                    val eqClassO = find(o).get
                    val eqClassO1 = find(o1).get
                    //                // if any two eq classes have the same prefix relation, merge them recursevely
                    if (!eqClassO.equals(eqClassO1)) merge(eqClassO, eqClassO1);
                })
            } else newPrefixSet += ((a, o))
        }
        equivalenceClasses += new EquivalenceClass(newObjectNamesSet, newPrefixSet)
        equivalenceClasses -= (e1, e2)

    }


    def showExtractedObjectNames() = {
        println(extractedObjectNames)
    }

    def showCallGraph() = {
        equivalenceClasses.map(println)
    }

    private def extractExpr(expr: Expr): Option[String] = {
        //println(expr)
        expr match {
            case Id(name: String) => Some(name)
            case Constant(value: String) => None
            case StringLit(name: List[Opt[String]]) => None
            case UnaryExpr(kind: String, e: Expr) => extractExpr(e)
            case SizeOfExprT(typeName: TypeName) => None
            case SizeOfExprU(expr: Expr) => extractExpr(expr);
                None
            case CastExpr(typeName: TypeName, expr: Expr) => extractExpr(expr);
                None
            case ExprList(exprs: List[Opt[Expr]]) => for (Opt(_, e) <- exprs) extractExpr(e);
                None

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
            case CompoundStatement(innerStatements: List[Opt[Statement]]) => for (Opt(_, e) <- innerStatements) extractStmt(e);
                None
            case EmptyStatement() => None
            case ExprStatement(expr: Expr) => extractExpr(expr)
            case WhileStatement(expr: Expr, s: Conditional[Statement]) => extractExpr(expr);
                s.map(stmt => extractStmt(stmt));
                None
            case DoStatement(expr: Expr, s: Conditional[Statement]) => extractExpr(expr);
                s.map(stmt => extractStmt(stmt));
                None
            case ForStatement(expr1: Option[Expr], expr2: Option[Expr], expr3: Option[Expr], s: Conditional[Statement]) => {
                extractExpr(expr1.get)
                extractExpr(expr2.get)
                extractExpr(expr3.get)
                s.map(stmt => extractStmt(stmt));
                None
            }
            case GotoStatement(target: Expr) => extractExpr(target);
                None
            case ContinueStatement() => None
            case BreakStatement() => None
            case ReturnStatement(expr: Option[Expr]) => extractExpr(expr.get);
                None
            case LabelStatement(id: Id, attribute: Option[AttributeSpecifier]) => None
            case CaseStatement(c: Expr) => extractExpr(c);
                None
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

    def extractObjectNames(ast: AST): Option[String] = {
        //println(ast)
        ast match {
            case TranslationUnit(list: List[Opt[ExternalDef]]) => for (Opt(_, e) <- list) extractObjectNames(e);
                None
            case FunctionDef(_, _, _, compoundStmt: Statement) => extractStmt(compoundStmt)
            case Declaration(declSpecs: List[Opt[Specifier]], init: List[Opt[InitDeclarator]]) => for (Opt(_, e) <- init) extractObjectNames(e);
                None
            case FunctionCall(exprList: ExprList) => extractObjectNames(exprList);
                None
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
        //c.extractObjectNames(null)
        c.calculatePERelation(null)
    }
}
