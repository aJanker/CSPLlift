package de.fosd.typechef.ccallgraph

import de.fosd.typechef.conditional.{Conditional, Opt}
import de.fosd.typechef.parser.c._

/**
 * Created by gferreir on 9/20/14.
 *
 */

class CCallGraph() {

    var extractedObjectNames: Set[String] = Set()
    var pointerRelatedAssignments : Set[(String,String)] = Set()
    var equivalenceClasses: Set[EquivalenceClass] = Set()

    // map of function defs (specif != void) and all returns
    var functionDefReturns : Map[String, Set[String]] = Map()
    var currentFunction : String = ""
    var isDeclarationStatement = false
    var isPointer = false
    var isFunction = false

    // constants
    val StructPointerAccessOp = "->"
    val PointerDereferenceOp = "*"
    val PointerCreationOp = "&"
    val StructAccessOp = "."
    val FunctionCallOp = "()"
    val ArrayAccessOp = "[]"

    def calculatePERelation(program: TranslationUnit): Set[EquivalenceClass] = {

        // extract objectNames and their assignments from the AST
        extractObjectNames(program)
        println(extractedObjectNames)

        // extract program assignments considering function returns
        replaceFunctionReturnsValuesForAssignments()

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

    def initEquivalanceClasses(): Unit = {
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

    def createInitialPrefixSets():Unit = {
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
                val objectNameFields = (o.take(o.lastIndexOf(StructAccessOp)), o.drop(o.lastIndexOf(StructAccessOp) + StructAccessOp.length))
                val eqClassObjectOPartial = find(objectNameFields._1)

                // add field edge from o to o1
                eqClassObjectO1.get.addPrefix((objectNameFields._2, o))

                // struct pointer access operator  (dereference + dot)
            } else if (o.startsWith(apply(StructPointerAccessOp, o1)) || o.startsWith(apply(StructPointerAccessOp, parenthesize(o1)))) {
                val objectNameFields = (o.take(o.lastIndexOf(StructPointerAccessOp)), o.drop(o.lastIndexOf(StructPointerAccessOp) + StructPointerAccessOp.length))
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

    // replace each auxiliary function call reference for its real return values 
    def replaceFunctionReturnsValuesForAssignments() = {
        val assignmentsPartition = pointerRelatedAssignments.partition({ a : ((String, String)) => !a._1.contains(FunctionCallOp) && !a._2.contains(FunctionCallOp)  })
        var normalizedAssignments : Set[(String,String)] = assignmentsPartition._1

        // replace function call by the all return values
        assignmentsPartition._2.foreach({ assign =>
            if (assign._1 contains FunctionCallOp) {
                functionDefReturns.getOrElse(assign._1, Set()).foreach({
                    x => normalizedAssignments += ((x, assign._2))
                })
            } else if (assign._2 contains FunctionCallOp) {
                functionDefReturns.getOrElse(assign._2, Set()).foreach({
                    x => normalizedAssignments += ((x, assign._1))
                })
            }
        })
        pointerRelatedAssignments = normalizedAssignments
    }

    def mergeEquivalenceClasses(assignee: String, assignor: String) {
        val eqClassAssignor = find(assignor)
        val eqClassAssignee = find(assignee)

        if (eqClassAssignee.isDefined && eqClassAssignor.isDefined && !eqClassAssignee.equals(eqClassAssignor)) {
            merge(eqClassAssignee.get, eqClassAssignor.get)
        }
    }

    def merge(e1: EquivalenceClass, e2: EquivalenceClass) {
        val newObjectNamesSet: Set[String] = e1.objectNames().union(e2.objectNames())
        var newPrefixSet: Set[(String, String)] = e1.prefixes()

        // loop both prefix sets
        for ((a, o) <- e2.prefixes()) {
            val sharedPrefix = newPrefixSet.filter({ case ((a1, o1)) => a.equals(a1) })

            if (sharedPrefix.nonEmpty) {
                // if equivalence classes share the same prefix (i.e., if they have edges to the same object name)
                sharedPrefix.map({ case ((_, o1)) =>

                    val eqClassO = find(o).get
                    val eqClassO1 = find(o1).get

                    // if any two eq classes have the same prefix relation, merge them recursevely
                    if (!eqClassO.equals(eqClassO1)) merge(eqClassO, eqClassO1);
                })
            } else newPrefixSet += ((a, o))
        }
        // add new equivalence class and delete merged ones
        equivalenceClasses += new EquivalenceClass(newObjectNamesSet, newPrefixSet)
        equivalenceClasses -= (e1, e2)

    }


    def showExtractedObjectNames() = {
        println(extractedObjectNames)
    }

    def showFunctionDefReturns() = {
        println(functionDefReturns)
    }

    def showCallGraph() = {
        equivalenceClasses.map(println)
    }

    def parenthesize(objName : String) = {
        val operators  = List(StructAccessOp, StructPointerAccessOp, PointerCreationOp, PointerDereferenceOp)
        if (operators.exists(objName.contains)) {
            "(%s)".format(objName)
        }
        else objName
    }

    private def extractExpr(expr: Expr): Option[String] = {
        println(expr)
        expr match {
            case Id(name: String) => Some(name)
            // constants are no important, variables are (changing this may affect the whole algorithm)
            case Constant(value: String) => None
            case StringLit(name: List[Opt[String]]) => None
            case UnaryExpr(kind: String, e: Expr) => extractExpr(e)
            case SizeOfExprT(typeName: TypeName) => None
            case SizeOfExprU(expr: Expr) => extractExpr(expr); None
            case CastExpr(typeName: TypeName, expr: Expr) => val exprStr = extractExpr(expr); exprStr
            case ExprList(exprs: List[Opt[Expr]]) => for (Opt(_, e) <- exprs) extractExpr(e); None
            case PointerDerefExpr(castExpr: Expr) => {
                val exprStr = extractExpr(castExpr)
                exprStr.map(exprStr => {
                    extractedObjectNames += exprStr
                    extractedObjectNames += (PointerDereferenceOp + parenthesize(exprStr))
                })
                exprStr.map(PointerDereferenceOp + parenthesize(_))
            }
            case PointerCreationExpr(castExpr: Expr) => {
                val exprStr = extractExpr(castExpr)
                if (exprStr.isDefined) {
                    extractedObjectNames += exprStr.get
                    extractedObjectNames += (PointerCreationOp + parenthesize(exprStr.get))
                }
                exprStr.map(PointerCreationOp + parenthesize(_))
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
            case PostfixExpr(pExpr: Expr, suffixExpr: PostfixSuffix)  => {
                val exprStr1 = extractExpr(pExpr)
                val exprStr2 = extractObjectNames(suffixExpr)

                // member access operators
                if (exprStr1.isDefined && exprStr2.isDefined) {
                    // -> (sctruct pointer access operator)
                    if (exprStr2.get startsWith StructPointerAccessOp) {
                        extractedObjectNames += exprStr1.get
                        extractedObjectNames += (PointerDereferenceOp + parenthesize(exprStr1.get))
                        extractedObjectNames += (parenthesize(exprStr1.get) + exprStr2.get)

                        // . (struct access operator)
                    } else if (exprStr2.get startsWith StructAccessOp) {
                        extractedObjectNames += exprStr1.get
                        extractedObjectNames += (parenthesize(exprStr1.get) + exprStr2.get)
                    }

                    // array access
                    else if (exprStr2.get equals ArrayAccessOp) {
                        extractedObjectNames += parenthesize(exprStr1.get) + exprStr2.get
                    }

                    // is a declaration followed by an assignment?
                    else if (isDeclarationStatement && (exprStr2.get equals FunctionCallOp)) {
                        extractedObjectNames += parenthesize(exprStr1.get) + FunctionCallOp
                    }
                }
                exprStr1.flatMap(e1 => exprStr2.map(e2 => parenthesize(e1) + e2))
            };

        }
    }

    private def extractStmt(stmt: Statement): Option[String] = {
        println(stmt)
        stmt match {
            case CompoundStatement(innerStatements: List[Opt[Statement]]) => {
                for (Opt(_, e) <- innerStatements) extractStmt(e)
                None
            }
            case EmptyStatement() => None
            case ExprStatement(expr: Expr) => extractExpr(expr)
            case WhileStatement(expr: Expr, s: Conditional[Statement]) => {
                extractExpr(expr)
                s.map(stmt => extractStmt(stmt))
                None
            }
            case DoStatement(expr: Expr, s: Conditional[Statement]) => {
                extractExpr(expr)
                s.map(stmt => extractStmt(stmt))
                None
            }
            case ForStatement(expr1: Option[Expr], expr2: Option[Expr], expr3: Option[Expr], s: Conditional[Statement]) => {
                extractExpr(expr1.get)
                extractExpr(expr2.get)
                extractExpr(expr3.get)
                s.map(stmt => extractStmt(stmt))
                None
            }
            case GotoStatement(target: Expr) => extractExpr(target); None
            case ContinueStatement() => None
            case BreakStatement() => None
            case ReturnStatement(expr: Option[Expr]) => {
                val exprStr = extractExpr(expr.get)
                if (exprStr.isDefined)
                    functionDefReturns = functionDefReturns.updated(currentFunction,  functionDefReturns.getOrElse(currentFunction, Set()) + exprStr.get)
                None
            }
            case LabelStatement(id: Id, attribute: Option[AttributeSpecifier]) => None
            case CaseStatement(c: Expr) => extractExpr(c)
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
            case DeclarationStatement(decl: Declaration) => {
                isDeclarationStatement = true
                var declStr = extractObjectNames(decl)
                isDeclarationStatement = false
                declStr
            }
        }
    }

    def extractObjectNames(ast: AST): Option[String] = {
        println(ast)
        ast match {
            case TranslationUnit(list: List[Opt[ExternalDef]]) => for (Opt(_, e) <- list) extractObjectNames(e); None
            case Declaration(declSpecs: List[Opt[Specifier]], init: List[Opt[InitDeclarator]]) => {
                for (Opt(_, e) <- declSpecs) extractObjectNames(e)
                for (Opt(_, e) <- init) extractObjectNames(e)
                None
            }
            case FunctionCall(exprList: ExprList) => extractObjectNames(exprList); Some(FunctionCallOp)
            /*
             * TODO: analyze function defs and calls to relate parameters
             *
             */
            case PointerPostfixSuffix(operator: String, expr) => {
                val str = extractExpr(expr)
                str.map(operator + _)
            }

            case specif : Specifier => None
            case _: SimplePostfixSuffix => None
            case ArrayAccess(expr: Expr) => Some(ArrayAccessOp)
            case NArySubExpr(op: String, e: Expr) => None

            case FunctionDef(specifiers: List[Opt[Specifier]], decl: Declarator, params: List[Opt[OldParameterDeclaration]], stmt: CompoundStatement) => {
                isFunction = true
                val declStr = extractObjectNames(decl)

                // if function returns something, save it for later replacements of return values
                specifiers.last match {
                    case Opt(_, s : VoidSpecifier) => ;
                    case _ => {
                        currentFunction = declStr.get + FunctionCallOp
                        functionDefReturns += (currentFunction -> Set())
                    }
                }
                for (Opt(_, e) <- params) extractObjectNames(e)
                isFunction = false

                extractStmt(stmt)
                None
            }

            case InitDeclaratorI(decl: Declarator, _, init: Option[Initializer]) => {
                val declStr = extractObjectNames(decl)
                val initNames = init.flatMap(i => extractObjectNames(i))

                if (isDeclarationStatement && declStr.isDefined) {
                    extractedObjectNames += declStr.get

                    if (initNames.isDefined) {
                        extractedObjectNames += initNames.get
                        pointerRelatedAssignments += ((declStr.get, initNames.get))
                    }
                    initNames
                } else None
            }

            case DeclIdentifierList(idList: List[Opt[Id]]) => for (Opt(_, e) <- idList) extractObjectNames(e); None
            case DeclParameterDeclList(parameterDecls: List[Opt[ParameterDeclaration]]) => for (Opt(_, e) <- parameterDecls) extractObjectNames(e); None
            case DeclArrayAccess(expr: Option[Expr]) => if (expr.isDefined) { val exprStr = extractExpr(expr.get); exprStr }; else None

            case AtomicNamedDeclarator(pointers: List[Opt[Pointer]], id: Id, extensions: List[Opt[DeclaratorExtension]]) => {
                for (Opt(_, e) <- pointers) extractObjectNames(e); None
                for (Opt(_, e) <- extensions) extractObjectNames(e); None

                if (isPointer && isFunction) {
                    isPointer = false
                    Some(PointerDereferenceOp + id.name)
                } else { Some(id.name) }
            }
            case NestedNamedDeclarator(pointers: List[Opt[Pointer]], nestedDecl: Declarator, extensions: List[Opt[DeclaratorExtension]], attr: List[Opt[AttributeSpecifier]]) => {
                for (Opt(_, e) <- pointers) extractObjectNames(e)
                for (Opt(_, e) <- extensions) extractObjectNames(e)
                for (Opt(_, e) <- attr) extractObjectNames(e)
                val strDecl = extractObjectNames(nestedDecl)
                strDecl
            }
            case AtomicAbstractDeclarator(pointers: List[Opt[Pointer]], extensions: List[Opt[DeclaratorAbstrExtension]]) => {
                for (Opt(_, e) <- pointers) extractObjectNames(e)
                for (Opt(_, e) <- extensions) extractObjectNames(e)
                None
            }

            case NestedAbstractDeclarator(pointers: List[Opt[Pointer]], nestedDecl: AbstractDeclarator, extensions: List[Opt[DeclaratorAbstrExtension]], attr: List[Opt[AttributeSpecifier]]) => {
                for (Opt(_, e) <- pointers) extractObjectNames(e)
                for (Opt(_, e) <- extensions) extractObjectNames(e)
                for (Opt(_, e) <- attr) extractObjectNames(e)
                val strDecl = extractObjectNames(nestedDecl)
                strDecl
            }

            case PlainParameterDeclaration(specifiers: List[Opt[Specifier]], attr: List[Opt[AttributeSpecifier]])  => {
                for (Opt(_, e) <- specifiers) extractObjectNames(e)
                for (Opt(_, e) <- attr) extractObjectNames(e)
                None
            }

            case ParameterDeclarationD(specifiers: List[Opt[Specifier]], decl: Declarator, attr: List[Opt[AttributeSpecifier]]) => {
                for (Opt(_, e) <- specifiers) extractObjectNames(e)
                for (Opt(_, e) <- attr) extractObjectNames(e)
                extractObjectNames(decl)
                None
            }

            case ParameterDeclarationAD(specifiers: List[Opt[Specifier]], decl: AbstractDeclarator, attr: List[Opt[AttributeSpecifier]]) => {
                for (Opt(_, e) <- specifiers) extractObjectNames(e)
                for (Opt(_, e) <- attr) extractObjectNames(e)
                extractObjectNames(decl)
                None
            }

            case Initializer(initializerElementLabel: Option[InitializerElementLabel], expr: Expr) => {
                val exprStr = extractExpr(expr)
                exprStr
            }

            case Pointer(specifier: List[Opt[Specifier]]) => {
                isPointer = true
                None
            }

            case AtomicAttribute(n: String) =>{
                Some(n)
            }

            case AttributeSequence(attributes: List[Opt[Attribute]]) =>{
                for (Opt(_, e) <- attributes) extractObjectNames(e)
                None
            }

            case CompoundAttribute(inner: List[Opt[AttributeSequence]]) => {
                for (Opt(_, e) <- inner) extractObjectNames(e)
                None
            }

            case e: Expr => extractExpr(e)
            case s: Statement => extractStmt(s)
            case z => throw new Exception("%s is not supported".format(z.getClass))
        }
    }
}