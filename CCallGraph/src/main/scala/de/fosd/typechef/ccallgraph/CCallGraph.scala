package de.fosd.typechef.ccallgraph

import de.fosd.typechef.conditional.{Conditional, Opt}
import de.fosd.typechef.parser.c._

/**
 * Created by gferreir on 9/20/14.
 *
 */
class CCallGraph {

    var extractedObjectNames: Set[String] = Set()
    var pointerRelatedAssignments : Set[(String,String)] = Set()
    var equivalenceClasses: Set[EquivalenceClass] = Set()

    // constants
    val StructPointerAccessOp = "->"
    val PointerDereferenceOp = "*"
    val PointerCreationOp = "&"
    val StructAccessOp = "."


    def calculatePERelation(program: AST): Set[EquivalenceClass] = {

        // extract objectNames and their assignments from the AST
        extractObjectNames(program)

        // create initial equivalance class for each object name
        initEquivalanceClasses()

        // create edges between initial equivalence classes
        createInitialPrefixSets()

        // for each pointer related assignment, merge prefixes if  different
        for ((assignee, assignor) <- pointerRelatedAssignments) {
            mergeEquivalenceClasses(assignee, assignor)
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
            if ((node.objectNames contains objectName) ||  (node.objectNames contains parenthesize(objectName))) {
                return Some(node)
            }
        }
        None
    }

    def createInitialPrefixSets() = {
        // generate cross product of all object names
        val objectNamesCrossProduct = extractedObjectNames flatMap {
            x => extractedObjectNames map {
                y => (x, y)
            }
        }

        for ((o, o1) <- objectNamesCrossProduct) {
            val eqClassObjectO = find(o)
            val eqClassObjectO1 = find(o1)

            // pointer cretion operator
            if (o.equals(apply(PointerCreationOp, o1)) || o.equals(apply(PointerCreationOp, parenthesize(o1)))) {
                // add * edge from o1 to o
                eqClassObjectO.get.addPrefix((PointerDereferenceOp, o1))

                // pointer dereference operator
            } else if (o.equals(apply(PointerDereferenceOp, o1)) || o.equals(apply(PointerDereferenceOp, parenthesize(o1)))) {
                // add * edge from o to o1
                eqClassObjectO1.get.addPrefix((PointerDereferenceOp, o))

                // struct dot access operator
            } else if (o.startsWith(apply(StructAccessOp, o1)) || o.startsWith(apply(StructAccessOp, parenthesize(o1)))) {
                val objectNameFields = (o take (o.lastIndexOf(StructAccessOp)), o drop (o.lastIndexOf(StructAccessOp) + StructAccessOp.length))
                val eqClassObjectOPartial = find(objectNameFields._1)

                // add field edge from o to o1
                eqClassObjectO1.get.addPrefix((objectNameFields._2, o))

                // struct pointer access operator  (dereference + dot)
            } else if (o.startsWith(apply(StructPointerAccessOp, o1)) || o.startsWith(apply(StructPointerAccessOp, parenthesize(o1)))) {
                val objectNameFields = (o take (o.lastIndexOf(StructPointerAccessOp)), o drop (o.lastIndexOf(StructPointerAccessOp) + StructPointerAccessOp.length))
                val eqClassObjectOPartial = find("%s%s".format(PointerDereferenceOp, objectNameFields._1))

                // add field edge from o to o1
                eqClassObjectOPartial.get.addPrefix((objectNameFields._2, o))
            }
        }
    }

    def apply(operator: String, objectName: String): String = {
        operator match {
            // suffix operators
            case StructAccessOp | StructPointerAccessOp => "%s%s".format(objectName, operator)
            // prefix operators
            case PointerCreationOp | PointerDereferenceOp =>  "%s%s".format(operator, objectName)
        }
    }

    def extractProgramAssignments(ast: AST) = {
        pointerRelatedAssignments
    }

    def mergeEquivalenceClasses(assignee: String, assignor: String) {
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
            val commonPrefix = newPrefixSet.filter({ case ((a1, o1)) => a.equals(a1) || a.equals() })

            if (!commonPrefix.isEmpty) {
                commonPrefix.map({ case ((_, o1)) =>

                    val eqClassO = find(o).get
                    val eqClassO1 = find(o1).get

                    // if any two eq classes have the same prefix relation, merge them recursevely
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

    def parenthesize(objName : String) = {
        if (objName.contains("->") || objName.contains(".") || objName.contains("*") || objName.contains("&")) {
            "(%s)".format(objName)
        }
        else objName
    }

    private def extractExpr(expr: Expr): Option[String] = {
        println(expr)
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
                    extractedObjectNames += ("*" + parenthesize(exprStr))
                })
                exprStr.map("*" + parenthesize(_))
            }
            case PointerCreationExpr(castExpr: Expr) => {
                val exprStr = extractExpr(castExpr)
                if (exprStr.isDefined) {
                    extractedObjectNames += exprStr.get
                    extractedObjectNames += ("&" + parenthesize(exprStr.get))
                }
                exprStr.map("&" + parenthesize(_))
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
                    pointerRelatedAssignments += ((exprStr1.get, exprStr2.get))
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
                        extractedObjectNames += ("*" + parenthesize(exprStr1.get))
                        extractedObjectNames += (parenthesize(exprStr1.get) + exprStr2.get)
                        // . (dot) operator
                    } else if (exprStr2.get startsWith ".") {
                        extractedObjectNames += exprStr1.get
                        extractedObjectNames += (parenthesize(exprStr1.get) + exprStr2.get)
                    }
                }
                exprStr1.flatMap(e1 => exprStr2.map(e2 => parenthesize(e1) + e2))
            };

        }
    }

    private def extractStmt(stmt: Statement): Option[String] = {
        println(stmt)
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
        println(ast)
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