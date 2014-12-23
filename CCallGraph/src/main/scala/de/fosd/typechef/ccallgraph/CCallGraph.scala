package de.fosd.typechef.ccallgraph

import de.fosd.typechef.conditional.{Conditional, One, Opt}
import de.fosd.typechef.parser.c._

import scala.collection.mutable.Stack

/**
 * Created by gferreir on 9/20/14.
 *
 */

class CCallGraph() {

    var extractedObjectNames: Set[String] = Set()
    var objectNameAssignments: Set[(String, String)] = Set()
    var equivalenceClasses: Set[EquivalenceClass] = Set()

    // map of function defs and all possible return values
    var functionDefReturns: Map[String, Set[String]] = Map()
    var functionDefParameters: Map[String, List[String]] = Map()

    // control object names scope (handle nested expressions)
    var objectNamesScope: Map[String, Set[String]] = Map()
    var objectNamesScopeStack: Stack[String] = Stack()

    // context variables
    var currentFunction: String = "GLOBAL"
    var isDeclarationStatement = false
    var isPointer = false
    var isFunctionDef = false
    var isNotTypedefSpecifier = true

    // constants
    val StructPointerAccessOp = "->"
    val PointerDereferenceOp = "*"
    val PointerCreationOp = "&"
    val StructAccessOp = "."
    val FunctionCallOp = "()"
    val ArrayAccessOp = "[]"

    def calculatePERelation(program: TranslationUnit): Set[EquivalenceClass] = {

        // extract objectNames and their assignments from the AST
        // TODO: fix assignments extraction (they are not only pointer assignments)
        /* TODO: fix object names (currently object names include also non-pointer variables)
         *     How?  - consider typedefs and analyze if they are pointer redefinitions
         *           - extract only pointers of primitive variables, but keep ignoring typedef variables
         */
        extractObjectNames(program)

        // extract program assignments considering function returns
        replaceFunctionCallByReturnValues()

        // create initial equivalance class for each object name
        initEquivalanceClasses()

        // create edges between initial equivalence classes
        createInitialPrefixSets()

        // for each pointer related assignment, merge prefixes if  different
        for ((assignee, assignor) <- objectNameAssignments) {
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
            if ((node.objectNames contains objectName) || (node.objectNames contains parenthesize(objectName))) {
                return Some(node)
            }
        }
        None
    }

    def createInitialPrefixSets(): Unit = {
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
            case PointerCreationOp | PointerDereferenceOp => "%s%s".format(operator, objectName)
        }
    }

    def objectNameScope(rawObjectName: String): String = {
       ""
    }

    // replace each auxiliary function call reference for its real return values
    def replaceFunctionCallByReturnValues() = {
        val assignmentsPartition = objectNameAssignments.partition({ a: ((String, String)) => !a._1.contains(FunctionCallOp) && !a._2.contains(FunctionCallOp)})
        var normalizedAssignments: Set[(String, String)] = assignmentsPartition._1

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
        objectNameAssignments = normalizedAssignments
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
            val sharedPrefix = newPrefixSet.filter({ case ((a1, o1)) => a.equals(a1)})

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
        equivalenceClasses -=(e1, e2)

    }

    def addObjectName(objectName: String, scope: String): String = {


        val scopedObjectName = "%s$%s".format(scope, objectName)
        extractedObjectNames = extractedObjectNames ++ List(scopedObjectName)
        scopedObjectName
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

    def parenthesize(objName: String) = {
        val operators = List(StructAccessOp, StructPointerAccessOp, PointerCreationOp, PointerDereferenceOp)
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
                if (exprStr.isDefined) {
                    addObjectName(exprStr.get, currentFunction)
                    addObjectName((PointerDereferenceOp + parenthesize(exprStr.get)), currentFunction)
                }
                exprStr.map(PointerDereferenceOp + parenthesize(_))
            }
            case PointerCreationExpr(castExpr: Expr) => {
                val exprStr = extractExpr(castExpr)
                if (exprStr.isDefined) {
                    addObjectName(exprStr.get, currentFunction)
                    addObjectName((PointerCreationOp + parenthesize(exprStr.get)), currentFunction)
                }
                exprStr.map(PointerCreationOp + parenthesize(_))

            }
            case UnaryOpExpr(kind: String, castExpr: Expr) => extractExpr(castExpr)

            // any kind of pointer arithmetic or comparison is ignored by analysis
            case NAryExpr(expr: Expr, others: List[Opt[NArySubExpr]]) => {
                val exprStr = extractExpr(expr)
                if (exprStr.isDefined) {
                    addObjectName(exprStr.get, currentFunction)
                }
                for (Opt(_, subExpr) <- others) extractObjectNames(subExpr)
                exprStr
            }

            case ConditionalExpr(condition: Expr, thenExpr: Option[Expr], elseExpr: Expr) => {
                val exprStr = extractExpr(condition)
                if (thenExpr.isDefined) extractExpr(thenExpr.get)
                extractExpr(elseExpr)
                exprStr
            }

            case AssignExpr(target: Expr, operation: String, source: Expr) => {
                val exprStr1 = extractExpr(target)
                val exprStr2 = extractExpr(source)
                if (exprStr1.isDefined && exprStr2.isDefined) {
                    val objName1 = addObjectName(exprStr1.get, currentFunction)
                    val objName2 = addObjectName(exprStr2.get, currentFunction)

                    objectNameAssignments += ((objName1, objName2))
                }
                exprStr2
            }
            case PostfixExpr(pExpr: Expr, suffixExpr: PostfixSuffix) => {
                val exprStr1 = extractExpr(pExpr)
                val exprStr2 = extractObjectNames(suffixExpr)

                // member access operators
                if (exprStr1.isDefined && exprStr2.isDefined) {
                    // -> (sctruct pointer access operator)
                    if (exprStr2.get startsWith StructPointerAccessOp) {
                        addObjectName(exprStr1.get, currentFunction)
                        addObjectName(PointerDereferenceOp + parenthesize(exprStr1.get), currentFunction)
                        addObjectName(parenthesize(exprStr1.get) + exprStr2.get, currentFunction)

                        // . (struct access operator)
                    } else if (exprStr2.get startsWith StructAccessOp) {
                        addObjectName(exprStr1.get, currentFunction)
                        addObjectName(parenthesize(exprStr1.get) + exprStr2.get, currentFunction)
                    }

                    // array access
                    else if (exprStr2.get equals ArrayAccessOp) {
                        addObjectName(parenthesize(exprStr1.get) + exprStr2.get, currentFunction)
                    }

                    // is a declaration followed by an assignment?
                    else if (isDeclarationStatement && (exprStr2.get equals FunctionCallOp)) {
                        addObjectName(parenthesize(exprStr1.get) + FunctionCallOp, currentFunction)
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
                val exprStr = extractExpr(expr)
                if (exprStr.isDefined) {addObjectName(exprStr.get, currentFunction)}
                s.map(stmt => extractStmt(stmt))
                None
            }
            case DoStatement(expr: Expr, s: Conditional[Statement]) => {
                val exprStr = extractExpr(expr)
                if (exprStr.isDefined) {addObjectName(exprStr.get, currentFunction)}
                s.map(stmt => extractStmt(stmt))
                None
            }
            case ForStatement(expr1: Option[Expr], expr2: Option[Expr], expr3: Option[Expr], s: Conditional[Statement]) => {
                val expr1Str = extractExpr(expr1.getOrElse(new Constant("")))
                val expr2Str = extractExpr(expr2.getOrElse(new Constant("")))
                val expr3Str = extractExpr(expr3.getOrElse(new Constant("")))

                if (expr1Str.isDefined) {addObjectName(expr1Str.get, currentFunction)}
                if (expr2Str.isDefined) {addObjectName(expr2Str.get, currentFunction)}
                if (expr3Str.isDefined) {addObjectName(expr3Str.get, currentFunction)}

                s.map(stmt => extractStmt(stmt))
                None
            }
            case GotoStatement(target: Expr) => extractExpr(target); None
            case ContinueStatement() => None
            case BreakStatement() => None
            case ReturnStatement(expr: Option[Expr]) => {
                val exprStr = extractExpr(expr.get)
                if (exprStr.isDefined)
                    functionDefReturns = functionDefReturns.updated(currentFunction, functionDefReturns.getOrElse(currentFunction, Set()) + exprStr.get)
                None
            }
            case LabelStatement(id: Id, attribute: Option[AttributeSpecifier]) => None
            case CaseStatement(c: Expr) => extractExpr(c)
                None
            case DefaultStatement() => None
            case IfStatement(condition: Conditional[Expr], thenBranch: Conditional[Statement], elifs: List[Opt[ElifStatement]], elseBranch: Option[Conditional[Statement]]) => {
                val exprStr = condition match {
                    case One(e) => extractExpr(e)
                    case c => throw new Exception("%s is not supported".format(c))
                }
                if (exprStr.isDefined) {addObjectName(exprStr.get, currentFunction)}

                thenBranch.map(stmt => extractStmt(stmt))
                for (Opt(_, s) <- elifs) extractObjectNames(s)
                if (elseBranch.isDefined) elseBranch.get.map(stmt => extractStmt(stmt))
                exprStr
            }

            case SwitchStatement(expr: Expr, s: Conditional[Statement]) => {
                extractExpr(expr)
                s.map(stmt => extractStmt(stmt))
                None
            }
            case DeclarationStatement(decl: Declaration) => {
                isDeclarationStatement = true
                val declStr = extractObjectNames(decl)
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
                isDeclarationStatement = true
                for (Opt(_, e) <- declSpecs) extractObjectNames(e)
                declSpecs.head match {
                    case Opt(_, ts: TypeDefTypeSpecifier) => isNotTypedefSpecifier = true;
                    case Opt(_, ps: PrimitiveTypeSpecifier) => isNotTypedefSpecifier = true;
                    case _ => isNotTypedefSpecifier = false;
                }
                for (Opt(_, e) <- init) extractObjectNames(e)
                isNotTypedefSpecifier = true;
                isDeclarationStatement = false;
                None
            }

            case TypelessDeclaration(declList: List[Opt[InitDeclarator]]) => {
                for (Opt(_, decl) <- declList) extractObjectNames(decl)
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

            case specif: Specifier => None
            case _: SimplePostfixSuffix => None
            case ArrayAccess(expr: Expr) => Some(ArrayAccessOp)

            // ignore any kind of subexpression (with exception of assingments)
            case NArySubExpr(op: String, e: Expr) => {
                e match {
                    case a: AssignExpr => extractExpr(a);
                    case _ => ;
                }
                None
            }

            case FunctionDef(specifiers: List[Opt[Specifier]], declarator: Declarator, parameters: List[Opt[OldParameterDeclaration]], stmt: CompoundStatement) => {
                isFunctionDef = true
                currentFunction = declarator.getName
                functionDefReturns += (currentFunction -> Set())
                functionDefParameters += (currentFunction -> List())

                extractObjectNames(declarator)
                extractStmt(stmt)
                for (Opt(_, p) <- parameters) extractObjectNames(p)

                isFunctionDef = false
                currentFunction = "GLOBAL"
                None
            }

            // variable declaration with initializer
            case InitDeclaratorI(decl: Declarator, _, init: Option[Initializer]) => {
                val declStr = extractObjectNames(decl)
                val initNames = init.flatMap(i => extractObjectNames(i))

                // variable declaration with initializer
                if (isNotTypedefSpecifier && declStr.isDefined) {
                    val objName1 = addObjectName(declStr.get, currentFunction)
                    objectNamesScope = objectNamesScope.updated(currentFunction, objectNamesScope.getOrElse(currentFunction, Set()) + objName1)

                    if (initNames.isDefined) {
                        val objName2 = addObjectName(initNames.get, currentFunction)
                        objectNameAssignments += ((objName1, objName2))
                    }
                    initNames
                } else None
            }

            case DeclIdentifierList(idList: List[Opt[Id]]) => for (Opt(_, e) <- idList) extractObjectNames(e);
                None
            case DeclParameterDeclList(parameterDecls: List[Opt[ParameterDeclaration]]) => {
                isDeclarationStatement = true
                for (Opt(_, e) <- parameterDecls) extractObjectNames(e);
                isDeclarationStatement = false
                None
            };
            case DeclArrayAccess(expr: Option[Expr]) => if (expr.isDefined) {val exprStr = extractExpr(expr.get); exprStr}; else None

            // variable declarator
            case AtomicNamedDeclarator(pointers: List[Opt[Pointer]], id: Id, extensions: List[Opt[DeclaratorExtension]]) => {
                for (Opt(_, e) <- pointers) extractObjectNames(e);
                for (Opt(_, e) <- extensions) extractObjectNames(e);

                if (isDeclarationStatement && isNotTypedefSpecifier) {
                    val objName1 = addObjectName(id.name, currentFunction)
                    objectNamesScope = objectNamesScope.updated(currentFunction, objectNamesScope.getOrElse(currentFunction, Set()) + objName1)

                    if (isPointer) {
                        val objName2 = addObjectName(PointerDereferenceOp + parenthesize(id.name), currentFunction)
                        objectNamesScope = objectNamesScope.updated(currentFunction, objectNamesScope.getOrElse(currentFunction, Set()) + objName2)
                        isPointer = false
                    }
                }
                Some(id.name)

            }

            case NestedNamedDeclarator(pointers: List[Opt[Pointer]], nestedDecl: Declarator, extensions: List[Opt[DeclaratorExtension]], attr: List[Opt[AttributeSpecifier]]) => {
                for (Opt(_, e) <- pointers) extractObjectNames(e)
                for (Opt(_, e) <- extensions) extractObjectNames(e)
                for (Opt(_, e) <- attr) extractObjectNames(e)
                val declStr = extractObjectNames(nestedDecl)
                declStr
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
                val declStr = extractObjectNames(nestedDecl)
                declStr
            }

            case PlainParameterDeclaration(specifiers: List[Opt[Specifier]], attr: List[Opt[AttributeSpecifier]]) => {
                for (Opt(_, e) <- specifiers) extractObjectNames(e)
                for (Opt(_, e) <- attr) extractObjectNames(e)
                None
            }

            case ParameterDeclarationD(specifiers: List[Opt[Specifier]], decl: Declarator, attr: List[Opt[AttributeSpecifier]]) => {
                for (Opt(_, e) <- specifiers) extractObjectNames(e)
                for (Opt(_, e) <- attr) extractObjectNames(e)
                val declStr = extractObjectNames(decl)
                if (isDeclarationStatement && declStr.isDefined) {
                    functionDefParameters = functionDefParameters.updated(currentFunction, functionDefParameters.getOrElse(currentFunction, List()) :+ declStr.get)
                }
                declStr
            }

            case ParameterDeclarationAD(specifiers: List[Opt[Specifier]], decl: AbstractDeclarator, attr: List[Opt[AttributeSpecifier]]) => {
                for (Opt(_, e) <- specifiers) extractObjectNames(e)
                for (Opt(_, e) <- attr) extractObjectNames(e)
                val declStr = extractObjectNames(decl)
                if (isDeclarationStatement && declStr.isDefined) {
                    functionDefParameters = functionDefParameters.updated(currentFunction, functionDefParameters.getOrElse(currentFunction, List()) :+ declStr.get)
                }
                declStr
            }

            case Initializer(initializerElementLabel: Option[InitializerElementLabel], expr: Expr) => {
                val exprStr = extractExpr(expr)
                exprStr
            }

            case Pointer(specifier: List[Opt[Specifier]]) => {
                isPointer = true
                None
            }

            case AtomicAttribute(n: String) => {
                Some(n)
            }

            case AttributeSequence(attributes: List[Opt[Attribute]]) => {
                for (Opt(_, e) <- attributes) extractObjectNames(e)
                None
            }

            case CompoundAttribute(inner: List[Opt[AttributeSequence]]) => {
                for (Opt(_, e) <- inner) extractObjectNames(e)
                None
            }

            case ElifStatement(condition: Conditional[Expr], thenBranch: Conditional[Statement]) => {
                val exprStr = condition match {
                    case One(e) => extractExpr(e)
                    case c => throw new Exception("%s is not supported".format(c))
                }
                if (exprStr.isDefined) {addObjectName(exprStr.get, currentFunction)}
                thenBranch.map(stmt => extractStmt(stmt))
                exprStr
            }

            case e: Expr => extractExpr(e)
            case s: Statement => extractStmt(s)
            case z => throw new Exception("%s is not supported".format(z.getClass))
        }
    }

}