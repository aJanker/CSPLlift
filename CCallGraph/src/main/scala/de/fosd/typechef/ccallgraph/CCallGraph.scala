package de.fosd.typechef.ccallgraph

import de.fosd.typechef.conditional.{Opt, Conditional, One}
import de.fosd.typechef.parser.c._

/**
 * Created by gferreir on 9/20/14.
 *
 */

class CCallGraph() {

    var extractedObjectNames: Set[String] = Set()
    var objectNameAssignments: Set[(String, String)] = Set()
    var equivalenceClasses: Set[EquivalenceClass] = Set()

    // map of function defs and [return values, parameters]
    var functionDefReturns: Map[String, Set[String]] = Map()
    var functionDefParameters: Map[String, List[String]] = Map()
    var functionCallParameters: List[(String, List[String])] = List()

    var functonCallExpr : String = ""
    var functionParamList : List[String] = List()
    var functionCalls : Set[(String, String)] = Set()

    //  variables scope (key = function OR GLOBAL, set = set of variables declared)
    //var objectNamesScope: Map[String, Set[String]] = Map()
    // object names scope (key =  object name, value = scope)
    var objectNamesScope: Map[String, String] = Map()



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


        /* TODO: fix assignments extraction (they are not only pointer assignments)
         *  fix object names (currently object names include also non-pointer variables)
         *     How?  - consider typedefs and analyze if they are pointer redefinitions
         *           - extract only pointers of primitive variables, but keep ignoring typedef variables
         */

        // extract objectNames and their assignments from the AST
        extractObjectNames(program)

        // extract program assignments (function call parameters and return values)
        addAssignmentsFromFunctionCallParameters()
        addAssignmentsFromFunctionCallReturnValues()
        println(functionCalls)

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

            val uo = unscope(o)
            val uo1 = unscope(o1)

            // pointer cretion operator
            if (uo.equals(apply(PointerCreationOp, uo1)) || uo.equals(apply(PointerCreationOp, parenthesize(uo1)))) {
                // add * edge from o1 to o
                eqClassObjectO.get.addPrefix((PointerDereferenceOp, o1))

                // pointer dereference operator
            } else if (uo.equals(apply(PointerDereferenceOp, uo1)) || uo.equals(apply(PointerDereferenceOp, parenthesize(uo1)))) {
                // add * edge from o to o1
                eqClassObjectO1.get.addPrefix((PointerDereferenceOp, o))

                // struct dot access operator
            } else if (uo.startsWith(apply(StructAccessOp, uo1)) || uo.startsWith(apply(StructAccessOp, parenthesize(uo1)))) {
                val objectNameFields = (uo.take(uo.lastIndexOf(StructAccessOp)), uo.drop(uo.lastIndexOf(StructAccessOp) + StructAccessOp.length))
                val eqClassObjectOPartial = find(objectNameFields._1)

                // add field edge from o to o1
                if (eqClassObjectOPartial.isDefined) {
                    eqClassObjectOPartial.get.addPrefix((objectNameFields._2, o))
                }

                // struct pointer access operator  (dereference + dot)
            } else if (uo.startsWith(apply(StructPointerAccessOp, uo1)) || uo.startsWith(apply(StructPointerAccessOp, parenthesize(uo1)))) {
                val objectNameFields = (uo.take(uo.lastIndexOf(StructPointerAccessOp)), uo.drop(uo.lastIndexOf(StructPointerAccessOp) + StructPointerAccessOp.length))
                val eqClassObjectOPartial = find(apply(PointerDereferenceOp, objectNameFields._1))

                // add field edge from o to o1
                if (eqClassObjectOPartial.isDefined) {
                    eqClassObjectOPartial.get.addPrefix((objectNameFields._2, o))
                }
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

    def unscope(scopedObjectName : String): String = {
        val rawObjectName = scopedObjectName.replaceFirst("[A-Za-z1-9]+?\\$", "")
        rawObjectName
    }


    def addAssignmentsFromFunctionCallParameters() = {
        for ((function, listParams) <- functionCallParameters) {
            val listParamsDecl = functionDefParameters.getOrElse(function, List())
            val assignments = listParams.zip(listParamsDecl)
            assignments.foreach({ a  => objectNameAssignments += a  })
        }
    }

    // replace each auxiliary function call reference for its real return values
    def addAssignmentsFromFunctionCallReturnValues() = {
        val assignmentsPartition = objectNameAssignments.partition({ a: ((String, String)) => !a._1.contains(FunctionCallOp) && !a._2.contains(FunctionCallOp)})
        var normalizedAssignments: Set[(String, String)] = assignmentsPartition._1

        // replace function call by the all return values
        assignmentsPartition._2.foreach({ assign =>
            if (assign._1 contains FunctionCallOp) {
                functionDefReturns.getOrElse(unscope(assign._1.replaceAll("\\(\\)", "")), Set()).foreach({
                    x => normalizedAssignments += ((x, assign._2))
                })
            } else if (assign._2 contains FunctionCallOp) {
                functionDefReturns.getOrElse(unscope(assign._2.replaceAll("\\(\\)", "")), Set()).foreach({
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

    def addObjectName(scopedObjectName: String): String = {
        // add object name with scope defined
        extractedObjectNames = extractedObjectNames + scopedObjectName

        scopedObjectName
    }

    def applyScope(objectName: String, currentScope: String) : String = {
        // format object name with scope
        val scopedObjectName = "%s$%s".format(currentScope, objectName)

        // include object name with scope (according to the variable declarations)
        objectNamesScope = objectNamesScope.updated(objectName, currentScope)

        scopedObjectName
    }

    def findScopeForObjectName(objectName: String, currentScope: String) : String = {

        // trivial scopes (current function or global)
        val scopedVariable = objectNamesScope.getOrElse(objectName, "GLOBAL")
        scopedVariable
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
            case SizeOfExprU(expr: Expr) => extractExpr(expr);
                None
            case CastExpr(typeName: TypeName, expr: Expr) => val exprStr = extractExpr(expr);
                exprStr
            case ExprList(exprs: List[Opt[Expr]]) => {
                functionParamList = List()
                for (Opt(_, e) <- exprs) {
                    val exprStr = extractObjectNames(e);
                    if (exprStr.isDefined) {
                        val scope = findScopeForObjectName(exprStr.get, currentFunction)
                        functionParamList = functionParamList :+ applyScope(exprStr.get, scope)
                    }
                }
                None
            };

            case PointerDerefExpr(castExpr: Expr) => {
                val exprStr = extractExpr(castExpr)
                if (exprStr.isDefined) {

                    val scope = findScopeForObjectName(exprStr.get, currentFunction)

                    addObjectName(applyScope(exprStr.get, scope))
                    addObjectName(applyScope((PointerDereferenceOp + parenthesize(exprStr.get)), scope))
                }
                exprStr.map(PointerDereferenceOp + parenthesize(_))
            }
            case PointerCreationExpr(castExpr: Expr) => {
                val exprStr = extractExpr(castExpr)
                if (exprStr.isDefined) {
                    val scope = findScopeForObjectName(exprStr.get, currentFunction)

                    addObjectName(applyScope(exprStr.get, scope))
                    addObjectName(applyScope((PointerCreationOp + parenthesize(exprStr.get)), scope))
                }
                exprStr.map(PointerCreationOp + parenthesize(_))

            }
            case UnaryOpExpr(kind: String, castExpr: Expr) => extractExpr(castExpr)

            // any kind of pointer arithmetic or comparison is ignored by analysis
            case NAryExpr(expr: Expr, others: List[Opt[NArySubExpr]]) => {
                val exprStr = extractExpr(expr)
                if (exprStr.isDefined) {
                    val scope = findScopeForObjectName(exprStr.get, currentFunction)
                    addObjectName(applyScope(exprStr.get, scope))
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
                    val scopeObj1 = findScopeForObjectName(exprStr1.get, currentFunction)
                    val scopeObj2 = findScopeForObjectName(exprStr2.get, currentFunction)

                    val objName1 = addObjectName(applyScope(exprStr1.get, scopeObj1))
                    val objName2 = addObjectName(applyScope(exprStr2.get, scopeObj2))

                    objectNameAssignments += ((objName1, objName2))
                }
                exprStr2
            }
            case PostfixExpr(pExpr: Expr, suffixExpr: PostfixSuffix) => {
                val exprStr1 = extractExpr(pExpr)
                val exprStr2 = extractObjectNames(suffixExpr)

                // member access operators
                if (exprStr1.isDefined && exprStr2.isDefined) {
                    // potential function call name
                    functonCallExpr = exprStr1.get
                    val scope = findScopeForObjectName(exprStr1.get, currentFunction)

                    // -> (sctruct pointer access operator)
                    if (exprStr2.get startsWith StructPointerAccessOp) {
                        addObjectName(applyScope(exprStr1.get, scope))
                        addObjectName(applyScope(PointerDereferenceOp + parenthesize(exprStr1.get), scope))
                        addObjectName(applyScope(parenthesize(exprStr1.get) + exprStr2.get, scope))

                        // . (struct access operator)
                    } else if (exprStr2.get startsWith StructAccessOp) {
                        addObjectName(applyScope(exprStr1.get, scope))
                        addObjectName(applyScope(parenthesize(exprStr1.get) + exprStr2.get, scope))
                    }

                    // array access
                    else if (exprStr2.get equals ArrayAccessOp) {
                        addObjectName(applyScope(parenthesize(exprStr1.get) + exprStr2.get, scope))
                    }

                    // is a function call?
                    else if (exprStr2.get equals FunctionCallOp) {
                        functionCallParameters +:= (exprStr1.get, functionParamList)
                        functionCalls += ((currentFunction, exprStr1.get))

                        // is a declaration followed by a function call assignment?
                        if (isDeclarationStatement) {
                            addObjectName(applyScope(parenthesize(exprStr1.get) + FunctionCallOp, currentFunction))
                        }
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
                if (exprStr.isDefined) {
                    val scope = findScopeForObjectName(exprStr.get, currentFunction)
                    addObjectName(applyScope(exprStr.get, scope))
                }
                s.map(stmt => extractStmt(stmt))
                None
            }
            case DoStatement(expr: Expr, s: Conditional[Statement]) => {
                val exprStr = extractExpr(expr)
                if (exprStr.isDefined) {
                    val scope = findScopeForObjectName(exprStr.get, currentFunction)
                    addObjectName(applyScope(exprStr.get, scope))
                }
                s.map(stmt => extractStmt(stmt))
                None
            }
            case ForStatement(expr1: Option[Expr], expr2: Option[Expr], expr3: Option[Expr], s: Conditional[Statement]) => {
                val expr1Str = extractExpr(expr1.getOrElse(new Constant("")))
                val expr2Str = extractExpr(expr2.getOrElse(new Constant("")))
                val expr3Str = extractExpr(expr3.getOrElse(new Constant("")))

                if (expr1Str.isDefined) {
                    val scope = findScopeForObjectName(expr1Str.get, currentFunction)
                    addObjectName(applyScope(expr1Str.get, scope))
                }
                if (expr2Str.isDefined) {
                    val scope = findScopeForObjectName(expr2Str.get, currentFunction)
                    addObjectName(applyScope(expr2Str.get, scope))
                }
                if (expr3Str.isDefined) {
                    val scope = findScopeForObjectName(expr3Str.get, currentFunction)
                    addObjectName(applyScope(expr3Str.get, scope))
                }

                s.map(stmt => extractStmt(stmt))
                None
            }
            case GotoStatement(target: Expr) => extractExpr(target);
                None
            case ContinueStatement() => None
            case BreakStatement() => None
            case ReturnStatement(expr: Option[Expr]) => {
                val exprStr = extractExpr(expr.get)
                if (exprStr.isDefined) {
                    val scope = findScopeForObjectName(exprStr.get, currentFunction)
                    functionDefReturns = functionDefReturns.updated(currentFunction, functionDefReturns.getOrElse(currentFunction, Set()) + applyScope(exprStr.get, scope))
                }
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
                if (exprStr.isDefined) {
                    val scope = findScopeForObjectName(exprStr.get, currentFunction)
                    addObjectName(applyScope(exprStr.get, scope))
                }

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
            case EmptyExternalDef() => None
            case TypelessDeclaration(declList: List[Opt[InitDeclarator]]) => for (Opt(_, e) <- declList) extractObjectNames(e); None
            case TypeName(specifiers: List[Opt[Specifier]], decl: Option[AbstractDeclarator])  => {
                for (Opt(_, s) <- specifiers) extractObjectNames(s);
                if (decl.isDefined) extractObjectNames(decl.get);
                None
            }
            case TranslationUnit(list: List[Opt[ExternalDef]]) => for (Opt(_, e) <- list) extractObjectNames(e); None

            case Declaration(declSpecs: List[Opt[Specifier]], init: List[Opt[InitDeclarator]]) => {
                isDeclarationStatement = true
                for (Opt(_, e) <- declSpecs) extractObjectNames(e)
                declSpecs.map {
                    case Opt(_, t : TypedefSpecifier) => isNotTypedefSpecifier = false;
                    case _ =>
                }
                for (Opt(_, e) <- init) extractObjectNames(e)
                isDeclarationStatement = false;
                isNotTypedefSpecifier = true;
                None
            }

            case TypelessDeclaration(declList: List[Opt[InitDeclarator]]) => {
                for (Opt(_, decl) <- declList) extractObjectNames(decl)
                None
            }

            case FunctionCall(exprList: ExprList) => {
                extractExpr(exprList)

                Some(FunctionCallOp)
            };
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
                val oldObjectNamesScope = objectNamesScope

                extractObjectNames(declarator)
                for (Opt(_, p) <- parameters) extractObjectNames(p)
                extractStmt(stmt)

                objectNamesScope = oldObjectNamesScope
                currentFunction = "GLOBAL"
                isFunctionDef = false
                None
            }

            // variable declaration with initializer
            case InitDeclaratorI(decl: Declarator, _, init: Option[Initializer]) => {
                val declStr = extractObjectNames(decl)
                val initNames = init.flatMap(i => extractObjectNames(i))

                // variable declaration with initializer
                if (isNotTypedefSpecifier && declStr.isDefined) {
                    objectNamesScope = objectNamesScope.updated(declStr.get, currentFunction)
                    val objName1 = applyScope(declStr.get, currentFunction)

                    if (initNames.isDefined) {
                        objectNamesScope = objectNamesScope.updated(initNames.get, currentFunction)

                        val objName2 = applyScope(initNames.get, currentFunction)
                        objectNameAssignments += ((objName1, objName2))
                    }
                    initNames
                } else None
            }

            case DeclIdentifierList(idList: List[Opt[Id]]) => for (Opt(_, e) <- idList) extractObjectNames(e);
                None

            // function parameters declaration
            case DeclParameterDeclList(parameterDecls: List[Opt[ParameterDeclaration]]) => {
                isDeclarationStatement = true;
                for (Opt(_, e) <- parameterDecls) extractObjectNames(e);
                isDeclarationStatement = false;
                None
            };
            case DeclArrayAccess(expr: Option[Expr]) => if (expr.isDefined) {
                val exprStr = extractExpr(expr.get);
                exprStr
            };
            else None

            // variable declarator
            case AtomicNamedDeclarator(pointers: List[Opt[Pointer]], id: Id, extensions: List[Opt[DeclaratorExtension]]) => {
                for (Opt(_, e) <- pointers) extractObjectNames(e);
                for (Opt(_, e) <- extensions) extractObjectNames(e);

                if (isDeclarationStatement && isNotTypedefSpecifier) {
                    addObjectName(applyScope(id.name, currentFunction))

                    if (isPointer) {
                        addObjectName(applyScope(PointerDereferenceOp + parenthesize(id.name), currentFunction))
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

            case PlainParameterDeclaration(declSpecs: List[Opt[Specifier]], attr: List[Opt[AttributeSpecifier]]) => {
                for (Opt(_, e) <- declSpecs) extractObjectNames(e)
                declSpecs.map {
                    case Opt(_, t : TypedefSpecifier) => isNotTypedefSpecifier = false;
                    case _ =>
                }

                for (Opt(_, e) <- attr) extractObjectNames(e)
                isNotTypedefSpecifier = true
                None
            }

            case ParameterDeclarationD(declSpecs: List[Opt[Specifier]], decl: Declarator, attr: List[Opt[AttributeSpecifier]]) => {
                for (Opt(_, e) <- declSpecs) extractObjectNames(e)
                declSpecs.map {
                    case Opt(_, t : TypedefSpecifier) => isNotTypedefSpecifier = false;
                    case _ =>
                }

                for (Opt(_, e) <- attr) extractObjectNames(e)
                val declStr = extractObjectNames(decl)
                if (isDeclarationStatement && declStr.isDefined) {
                    objectNamesScope = objectNamesScope.updated(declStr.get, currentFunction)
                    functionDefParameters = functionDefParameters.updated(currentFunction, functionDefParameters.getOrElse(currentFunction, List()) :+ applyScope(declStr.get, currentFunction))
                }
                isNotTypedefSpecifier = true
                declStr
            }

            case ParameterDeclarationAD(declSpecs: List[Opt[Specifier]], decl: AbstractDeclarator, attr: List[Opt[AttributeSpecifier]]) => {
                for (Opt(_, e) <- declSpecs) extractObjectNames(e)
                declSpecs.map {
                    case Opt(_, t : TypedefSpecifier) => isNotTypedefSpecifier = false;
                    case _ =>
                }

                for (Opt(_, e) <- attr) extractObjectNames(e)
                val declStr = extractObjectNames(decl)
                if (isDeclarationStatement && declStr.isDefined) {
                    objectNamesScope = objectNamesScope.updated(declStr.get, currentFunction)
                    functionDefParameters = functionDefParameters.updated(currentFunction, functionDefParameters.getOrElse(currentFunction, List()) :+ applyScope(declStr.get, currentFunction))
                }
                isNotTypedefSpecifier = true
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
                if (exprStr.isDefined) {
                    val scope = findScopeForObjectName(exprStr.get, currentFunction)
                    addObjectName(applyScope(exprStr.get, scope))
                }
                thenBranch.map(stmt => extractStmt(stmt))
                exprStr
            }

            case e: Expr => extractExpr(e)
            case s: Statement => extractStmt(s)
            case z => throw new Exception("%s is not supported".format(z.getClass))
        }
    }

}