package de.fosd.typechef.ccallgraph

import de.fosd.typechef.conditional._
import de.fosd.typechef.featureexpr.FeatureExpr
import de.fosd.typechef.featureexpr.sat.{False, True}
import de.fosd.typechef.parser.c._

/**
 * Created by gferreir on 9/20/14.
 *
 */

class CCallGraph() {

    type ObjectName = String
    type Scope = String
    type Assignment = (ObjectName, ObjectName)

    var extractedObjectNames: ConditionalSet[ObjectName] = ConditionalSet()
    var objectNameAssignments: ConditionalSet[Assignment] = ConditionalSet()
    var equivalenceClasses: Set[EquivalenceClass] = Set()

    // map of function defs and [return values, parameters]
    var functionDefReturns: Map[String, Set[ObjectName]] = Map()
    var functionDefParameters: Map[String, List[ObjectName]] = Map()
    var functionCallParameters: List[(String, List[ObjectName])] = List()

    var functonCallExpr: String = ""
    var functionParamList: List[String] = List()
    var functionCalls: Set[(String, String)] = Set()

    // object names scope (key =  object name, value = scope)
    var objectNamesScope: Map[ObjectName, Scope] = Map()


    // context variables
    var currentFunction: Scope = "GLOBAL"
    var isDeclarationStatement: FeatureExpr = False
    var isPointer: FeatureExpr = False
    var isFunctionDef: FeatureExpr = False
    var isNotTypedefSpecifier: FeatureExpr = True

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
        extractObjectNames(program, True)

        // extract program assignments (function call parameters and return values)
        addAssignmentsFromFunctionCallParameters()
        addAssignmentsFromFunctionCallReturnValues()

        // create initial equivalance class for each object name
        initEquivalanceClasses()

        // create edges between initial equivalence classes
        createInitialPrefixSets()

        // for each pointer related assignment, merge prefixes if  different
        for ((assignee, assignor) <- objectNameAssignments.toPlainSet()) {
            mergeEquivalenceClasses(assignee, assignor)
        }

        equivalenceClasses
    }

    def initEquivalanceClasses(): Unit = {
        extractedObjectNames.keys.foreach({ k =>
            equivalenceClasses += new EquivalenceClass(ConditionalSet(k, extractedObjectNames.get(k)), ConditionalSet())
        })
    }

    def find(objectName: ObjectName): Option[EquivalenceClass] = {
        for (node: EquivalenceClass <- equivalenceClasses) {
            if ((node.objectNames().contains(objectName)).isSatisfiable() || (node.objectNames().contains(parenthesize(objectName))).isSatisfiable()) {
                return Some(node)
            }
        }
        None
    }

    def createInitialPrefixSets(): Unit = {
        // generate cross product of all object names
        val objectNamesCrossProduct = extractedObjectNames.toPlainSet.flatMap {
            x => extractedObjectNames.toPlainSet.map {
                y => (x, y)
            }
        }

        for ((o, o1) <- objectNamesCrossProduct) {
            val eqClassObjectO = find(o)
            val eqClassObjectO1 = find(o1)

            val eqClassObjectOFeatExpr = eqClassObjectO.get.objectNames().get(o);
            val eqClassObjectO1FeatExpr = eqClassObjectO1.get.objectNames().get(o1);

            val uo = unscope(o)
            val uo1 = unscope(o1)

            // pointer cretion operator
            if (((uo.equals(apply(PointerCreationOp, uo1))) || uo.equals(apply(PointerCreationOp, parenthesize(uo1)))) && (eqClassObjectOFeatExpr.equiv(eqClassObjectO1FeatExpr).isTautology())) {
                // add * edge from o1 to o
                eqClassObjectO.get.addPrefix((PointerDereferenceOp, o1), eqClassObjectO1.get.objectNames().get(o1))

                // pointer dereference operator
            } else if ((uo.equals(apply(PointerDereferenceOp, uo1)) || uo.equals(apply(PointerDereferenceOp, parenthesize(uo1))) && (eqClassObjectOFeatExpr.equiv(eqClassObjectO1FeatExpr).isTautology()))) {
                // add * edge from o to o1
                eqClassObjectO1.get.addPrefix((PointerDereferenceOp, o), eqClassObjectO.get.objectNames().get(o))

                // struct dot access operator
            }
            else if ((uo.startsWith(apply(StructAccessOp, uo1)) || uo.startsWith(apply(StructAccessOp, parenthesize(uo1))) && (eqClassObjectOFeatExpr.equiv(eqClassObjectO1FeatExpr).isTautology()))) {
                val objectNameFields = (uo.take(uo.lastIndexOf(StructAccessOp)), uo.drop(uo.lastIndexOf(StructAccessOp) + StructAccessOp.length))
                val eqClassObjectOPartial = find(objectNameFields._1)

                // add field edge from o to o1
                if (eqClassObjectOPartial.isDefined) {
                    eqClassObjectOPartial.get.addPrefix((objectNameFields._2, o), eqClassObjectO.get.objectNames().get(o))
                }

                // struct pointer access operator  (dereference + dot)
            } else if ((uo.startsWith(apply(StructPointerAccessOp, uo1)) || uo.startsWith(apply(StructPointerAccessOp, parenthesize(uo1))) && (eqClassObjectOFeatExpr.equiv(eqClassObjectO1FeatExpr).isTautology()))) {
                val objectNameFields = (uo.take(uo.lastIndexOf(StructPointerAccessOp)), uo.drop(uo.lastIndexOf(StructPointerAccessOp) + StructPointerAccessOp.length))
                val eqClassObjectOPartial = find(apply(PointerDereferenceOp, objectNameFields._1))

                // add field edge from o to o1
                if (eqClassObjectOPartial.isDefined) {
                    eqClassObjectOPartial.get.addPrefix((objectNameFields._2, o), eqClassObjectO.get.objectNames().get(o))
                }
            }
        }
    }

    def objectNamePresenceCondition(objectName : ObjectName) : FeatureExpr = {
        extractedObjectNames.get(objectName)
    }

    def apply(operator: String, objectName: String): String = {
        operator match {
            // suffix operators
            case StructAccessOp | StructPointerAccessOp => "%s%s".format(objectName, operator)
            // prefix operators
            case PointerCreationOp | PointerDereferenceOp => "%s%s".format(operator, objectName)
        }
    }

    def unscope(scopedObjectName: String): String = {
        val rawObjectName = scopedObjectName.replaceFirst("[A-Za-z1-9]+?\\$", "")
        rawObjectName
    }


    def addAssignmentsFromFunctionCallParameters() = {
        for ((function, listParams) <- functionCallParameters) {
            val listParamsDecl = functionDefParameters.getOrElse(function, List())
            val assignments = listParams.zip(listParamsDecl)
            // TODO: FIX
            // assignments.foreach({ a  => objectNameAssignments += a  })
        }
    }

    // replace each auxiliary function call reference for its real return values
    def addAssignmentsFromFunctionCallReturnValues() = {
        //        val assignmentsPartition = objectNameAssignments.partition({ a: ((String, String)) => !a._1.contains(FunctionCallOp) && !a._2.contains(FunctionCallOp)})
        //        var normalizedAssignments: Set[(String, String)] = assignmentsPartition._1
        //
        //        // replace function call by the all return values
        //        assignmentsPartition._2.foreach({ assign =>
        //            if (assign._1 contains FunctionCallOp) {
        //                functionDefReturns.getOrElse(unscope(assign._1.replaceAll("\\(\\)", "")), Set()).foreach({
        //                    x => normalizedAssignments += ((x, assign._2))
        //                })
        //            } else if (assign._2 contains FunctionCallOp) {
        //                functionDefReturns.getOrElse(unscope(assign._2.replaceAll("\\(\\)", "")), Set()).foreach({
        //                    x => normalizedAssignments += ((x, assign._1))
        //                })
        //            }
        //        })
        //        objectNameAssignments = normalizedAssignments
    }

        def mergeEquivalenceClasses(assignee: String, assignor: String) {
            val eqClassAssignor = find(assignor)
            val eqClassAssignee = find(assignee)

            if (eqClassAssignee.isDefined && eqClassAssignor.isDefined && !eqClassAssignee.equals(eqClassAssignor)) {
                merge(eqClassAssignee.get, eqClassAssignor.get)
            }
        }

        def merge(e1: EquivalenceClass, e2: EquivalenceClass) {
            val newObjectNamesSet: EquivalenceClass = e1.union(e2)
            var newPrefixSet: ConditionalSet[(String, String)] = e1.prefixes()

            // loop both prefix sets
            for ((a, o) <- e2.prefixes().toPlainSet()) {
                val sharedPrefix = newPrefixSet.toPlainSet().filter({ case (a1, o1) => a.equals(a1) })

                if (sharedPrefix.nonEmpty) {
                    // if equivalence classes share the same prefix (i.e., if they have edges to the same object name)
                    sharedPrefix.map({ case ((_, o1)) =>

                        val eqClassO = find(o).get
                        val eqClassO1 = find(o1).get

                        // if any two eq classes have the same prefix relation, merge them recursevely
                        if (!eqClassO.equals(eqClassO1)) merge(eqClassO, eqClassO1);
                    })
                } else newPrefixSet += ((a, o), e2.prefixes().get((a,o)))
            }
            // add new equivalence class and delete merged ones
            equivalenceClasses += new EquivalenceClass(newObjectNamesSet.objectNames(), newPrefixSet)
            equivalenceClasses -=(e1, e2)

        }

    def addObjectName(scopedObjectName: ObjectName, ctx: FeatureExpr): (ObjectName, FeatureExpr) = {
        // add object name with scope defined
        extractedObjectNames = extractedObjectNames +(scopedObjectName, ctx)
        (scopedObjectName, ctx)
    }

    def applyScope(objectName: ObjectName, currentScope: Scope): ObjectName = {
        // format object name with scope
        val scopedObjectName = "%s$%s".format(currentScope, objectName)

        // include object name with scope (according to the variable declarations)
        objectNamesScope = objectNamesScope.updated(objectName, currentScope)
        scopedObjectName
    }

    def findScopeForObjectName(rawObjectName: ObjectName, currentScope: Scope): ObjectName = {

        // trivial scopes (current function or global)
        val scopedVariable = objectNamesScope.getOrElse(rawObjectName, "GLOBAL")
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

    private def extractExpr(expr: Expr, ctx: FeatureExpr): Option[String] = {
        println(expr)

        expr match {
            case Id(name: String) => Some(name)
            // constants are no important, variables are (changing this may affect the whole algorithm)
            case Constant(value: String) => None
            case StringLit(name: List[Opt[String]]) => None
            case UnaryExpr(kind: String, e: Expr) => extractExpr(e, ctx)
            case SizeOfExprT(typeName: TypeName) => None
            case SizeOfExprU(expr: Expr) => extractExpr(expr, ctx);
                None
            case CastExpr(typeName: TypeName, expr: Expr) => val exprStr = extractExpr(expr, ctx);
                exprStr
            case ExprList(exprs: List[Opt[Expr]]) => {
                functionParamList = List()
                for (Opt(fExpr, e) <- exprs) {
                    val exprStr = extractObjectNames(e, ctx and fExpr);
                    if (exprStr.isDefined) {
                        val scope = findScopeForObjectName(exprStr.get, currentFunction)
                        functionParamList = functionParamList :+ applyScope(exprStr.get, scope)
                    }
                }
                None
            };

            case PointerDerefExpr(castExpr: Expr) => {
                val exprStr = extractExpr(castExpr, ctx)
                if (exprStr.isDefined) {

                    val scope = findScopeForObjectName(exprStr.get, currentFunction)

                    addObjectName(applyScope(exprStr.get, scope), ctx)
                    addObjectName(applyScope((PointerDereferenceOp + parenthesize(exprStr.get)), scope), ctx)
                }
                exprStr.map(PointerDereferenceOp + parenthesize(_))
            }
            case PointerCreationExpr(castExpr: Expr) => {
                val exprStr = extractExpr(castExpr, ctx)
                if (exprStr.isDefined) {
                    val scope = findScopeForObjectName(exprStr.get, currentFunction)

                    addObjectName(applyScope(exprStr.get, scope), ctx)
                    addObjectName(applyScope((PointerCreationOp + parenthesize(exprStr.get)), scope), ctx)
                }
                exprStr.map(PointerCreationOp + parenthesize(_))

            }
            case UnaryOpExpr(kind: String, castExpr: Expr) => extractExpr(castExpr, ctx)

            // any kind of pointer arithmetic or comparison is ignored by analysis
            case NAryExpr(expr: Expr, others: List[Opt[NArySubExpr]]) => {
                val exprStr = extractExpr(expr, ctx)
                if (exprStr.isDefined) {
                    val scope = findScopeForObjectName(exprStr.get, currentFunction)
                    addObjectName(applyScope(exprStr.get, scope), ctx)
                }
                for (Opt(fExpr, subExpr) <- others) extractObjectNames(subExpr, ctx and fExpr)
                exprStr
            }

            case ConditionalExpr(condition: Expr, thenExpr: Option[Expr], elseExpr: Expr) => {
                val exprStr = extractExpr(condition, ctx)
                if (thenExpr.isDefined) extractExpr(thenExpr.get, ctx)
                extractExpr(elseExpr, ctx)
                exprStr
            }

            case AssignExpr(target: Expr, operation: String, source: Expr) => {
                val exprStr1 = extractExpr(target, ctx)
                val exprStr2 = extractExpr(source, ctx)
                if (exprStr1.isDefined && exprStr2.isDefined) {
                    val scopeObj1 = findScopeForObjectName(exprStr1.get, currentFunction)
                    val scopeObj2 = findScopeForObjectName(exprStr2.get, currentFunction)

                    val objName1 = addObjectName(applyScope(exprStr1.get, scopeObj1), ctx)
                    val objName2 = addObjectName(applyScope(exprStr2.get, scopeObj2), ctx)

                    // object names and context (disjunction of both subexpressions)
                    objectNameAssignments +=((objName1._1, objName2._1), objName1._2 and objName2._2)
                }
                exprStr2
            }
            case PostfixExpr(postFixExpr: Expr, suffixExpr: PostfixSuffix) => {
                val exprStr1 = extractExpr(postFixExpr, ctx)
                val exprStr2 = extractObjectNames(suffixExpr, ctx)

                // member access operators
                if (exprStr1.isDefined && exprStr2.isDefined) {
                    // potential function call name
                    functonCallExpr = exprStr1.get
                    val scope = findScopeForObjectName(exprStr1.get, currentFunction)

                    // -> (sctruct pointer access operator)
                    if (exprStr2.get startsWith StructPointerAccessOp) {
                        addObjectName(applyScope(exprStr1.get, scope), ctx)
                        addObjectName(applyScope(PointerDereferenceOp + parenthesize(exprStr1.get), scope), ctx)
                        addObjectName(applyScope(parenthesize(exprStr1.get) + exprStr2.get, scope), ctx)

                        // . (struct access operator)
                    } else if (exprStr2.get startsWith StructAccessOp) {
                        addObjectName(applyScope(exprStr1.get, scope), ctx)
                        addObjectName(applyScope(parenthesize(exprStr1.get) + exprStr2.get, scope), ctx)
                    }

                    // array access
                    else if (exprStr2.get equals ArrayAccessOp) {
                        addObjectName(applyScope(parenthesize(exprStr1.get) + exprStr2.get, scope), ctx)
                    }

                    // is a function call?
                    else if (exprStr2.get equals FunctionCallOp) {
                        functionCallParameters +:=(exprStr1.get, functionParamList)
                        functionCalls += ((currentFunction, exprStr1.get))

                        // is a declaration followed by a function call assignment?
                        if (isDeclarationStatement.isSatisfiable()) {
                            addObjectName(applyScope(parenthesize(exprStr1.get) + FunctionCallOp, currentFunction), ctx)
                        }
                    }
                }
                exprStr1.flatMap(e1 => exprStr2.map(e2 => parenthesize(e1) + e2))
            };

        }
    }

    private def extractStmt(stmt: Statement, ctx: FeatureExpr): Option[String] = {
        println(stmt)
        stmt match {
            case CompoundStatement(innerStatements: List[Opt[Statement]]) => {
                for (Opt(fExpr, e) <- innerStatements) extractStmt(e, ctx and fExpr)
                None
            }
            case EmptyStatement() => None
            case ExprStatement(expr: Expr) => extractExpr(expr, ctx)
            case WhileStatement(expr: Expr, s: Conditional[Statement]) => {
                val exprStr = extractExpr(expr, ctx)
                if (exprStr.isDefined) {
                    val scope = findScopeForObjectName(exprStr.get, currentFunction)
                    addObjectName(applyScope(exprStr.get, scope), ctx)
                }
                s.mapf(ctx, (c, stmt) => extractStmt(stmt, c))
                None
            }
            case DoStatement(expr: Expr, s: Conditional[Statement]) => {
                val exprStr = extractExpr(expr, ctx)
                if (exprStr.isDefined) {
                    val scope = findScopeForObjectName(exprStr.get, currentFunction)
                    addObjectName(applyScope(exprStr.get, scope), ctx)
                }
                s.mapf(ctx, (c, stmt) => extractStmt(stmt, c))
                None
            }
            case ForStatement(expr1: Option[Expr], expr2: Option[Expr], expr3: Option[Expr], s: Conditional[Statement]) => {
                val expr1Str = extractExpr(expr1.getOrElse(new Constant("")), ctx)
                val expr2Str = extractExpr(expr2.getOrElse(new Constant("")), ctx)
                val expr3Str = extractExpr(expr3.getOrElse(new Constant("")), ctx)

                if (expr1Str.isDefined) {
                    val scope = findScopeForObjectName(expr1Str.get, currentFunction)
                    addObjectName(applyScope(expr1Str.get, scope), ctx)
                }
                if (expr2Str.isDefined) {
                    val scope = findScopeForObjectName(expr2Str.get, currentFunction)
                    addObjectName(applyScope(expr2Str.get, scope), ctx)
                }
                if (expr3Str.isDefined) {
                    val scope = findScopeForObjectName(expr3Str.get, currentFunction)
                    addObjectName(applyScope(expr3Str.get, scope), ctx)
                }

                s.mapf(ctx, (c, stmt) => extractStmt(stmt, c))
                None
            }
            case GotoStatement(target: Expr) => extractExpr(target, ctx);
                None
            case ContinueStatement() => None
            case BreakStatement() => None
            case ReturnStatement(expr: Option[Expr]) => {
                val exprStr = extractExpr(expr.get, ctx)
                if (exprStr.isDefined) {
                    val scope = findScopeForObjectName(exprStr.get, currentFunction)
                    functionDefReturns = functionDefReturns.updated(currentFunction, functionDefReturns.getOrElse(currentFunction, Set()) + applyScope(exprStr.get, scope))
                }
                None
            }
            case LabelStatement(id: Id, attribute: Option[AttributeSpecifier]) => None
            case CaseStatement(c: Expr) => extractExpr(c, ctx)
                None
            case DefaultStatement() => None
            case IfStatement(condition: Conditional[Expr], thenBranch: Conditional[Statement], elifs: List[Opt[ElifStatement]], elseBranch: Option[Conditional[Statement]]) => {
                val exprStr = condition match {
                    case One(e) => extractExpr(e, ctx)
                    case c => throw new Exception("%s is not supported".format(c))
                }
                if (exprStr.isDefined) {
                    val scope = findScopeForObjectName(exprStr.get, currentFunction)
                    addObjectName(applyScope(exprStr.get, scope), ctx)
                }

                thenBranch.mapf(ctx, (c, stmt) => extractStmt(stmt, c))
                for (Opt(fExpr, s) <- elifs) extractObjectNames(s, ctx and fExpr)
                if (elseBranch.isDefined) elseBranch.get.mapf(ctx, (c, stmt) => extractStmt(stmt, c))
                exprStr
            }

            case SwitchStatement(expr: Expr, s: Conditional[Statement]) => {
                extractExpr(expr, ctx)
                s.mapf(ctx, (c, stmt) => extractStmt(stmt, c))
                None
            }
            case DeclarationStatement(decl: Declaration) => {
                isDeclarationStatement = True
                val declStr = extractObjectNames(decl, ctx)
                isDeclarationStatement = False
                declStr
            }
        }
    }

    def extractObjectNames(ast: AST, ctx: FeatureExpr): Option[String] = {
        println(ast)
        ast match {
            case EmptyExternalDef() => None
            //            case TypelessDeclaration(declList: List[Opt[InitDeclarator]]) => {
            //                for (Opt(featExpr, e) <- declList) { extractObjectNames(e, ctx and featExpr); }
            //                None
            //            }
            //            case TypeName(specifiers: List[Opt[Specifier]], decl: Option[AbstractDeclarator])  => {
            //                for (Opt(featExpr, s) <- specifiers) extractObjectNames(s, ctx and featExpr);
            //                if (decl.isDefined) extractObjectNames(decl.get, ctx);
            //                None
            //            }
            case TranslationUnit(list: List[Opt[ExternalDef]]) => for (Opt(featExpr, e) <- list) extractObjectNames(e, ctx and featExpr); None

            case Declaration(declSpecs: List[Opt[Specifier]], init: List[Opt[InitDeclarator]]) => {
                isDeclarationStatement = True
                for (Opt(featExpr, e) <- declSpecs) extractObjectNames(e, ctx and featExpr)
                declSpecs.map {
                    case Opt(featExpr, t: TypedefSpecifier) => isNotTypedefSpecifier = isNotTypedefSpecifier andNot featExpr;
                    case _ =>
                }
                for (Opt(featExpr, e) <- init) extractObjectNames(e, ctx and featExpr)
                isDeclarationStatement = False;
                isNotTypedefSpecifier = True;
                None
            }

            case TypelessDeclaration(declList: List[Opt[InitDeclarator]]) => {
                for (Opt(featExpr, decl) <- declList) extractObjectNames(decl, ctx and featExpr)
                None
            }

            case FunctionCall(exprList: ExprList) => {
                extractExpr(exprList, ctx)

                Some(FunctionCallOp)
            };
            /*
             * TODO: analyze function defs and calls to relate parameters
             *
             */
            case PointerPostfixSuffix(operator: String, expr) => {
                val str = extractExpr(expr, ctx)
                str.map(operator + _)
            }

            case specif: Specifier => None
            case _: SimplePostfixSuffix => None
            case ArrayAccess(expr: Expr) => Some(ArrayAccessOp)

            // ignore any kind of subexpression (with exception of assingments)
            case NArySubExpr(op: String, e: Expr) => {
                e match {
                    case a: AssignExpr => extractExpr(a, ctx);
                    case _ => ;
                }
                None
            }

            case FunctionDef(specifiers: List[Opt[Specifier]], declarator: Declarator, parameters: List[Opt[OldParameterDeclaration]], stmt: CompoundStatement) => {
                isFunctionDef = True
                currentFunction = declarator.getName
                functionDefReturns += (currentFunction -> Set())
                functionDefParameters += (currentFunction -> List())
                val oldObjectNamesScope = objectNamesScope

                extractObjectNames(declarator, ctx)
                extractStmt(stmt, ctx)
                for (Opt(featExpr, p) <- parameters) extractObjectNames(p, ctx and featExpr)

                objectNamesScope = oldObjectNamesScope
                currentFunction = "GLOBAL"
                isFunctionDef = False
                None
            }

            // variable declaration with initializer
            case InitDeclaratorI(decl: Declarator, _, init: Option[Initializer]) => {
                val declStr = extractObjectNames(decl, ctx)
                val initNames = init.flatMap(i => extractObjectNames(i, ctx))

                // variable declaration with initializer
                if (isNotTypedefSpecifier.isSatisfiable() && declStr.isDefined) {
                    objectNamesScope = objectNamesScope.updated(declStr.get, currentFunction)
                    val objName1 = applyScope(declStr.get, currentFunction)

                    if (initNames.isDefined) {
                        objectNamesScope = objectNamesScope.updated(initNames.get, currentFunction)
                        val objName2 = applyScope(initNames.get, currentFunction)

                        // assignment (no need to get a new presence condition, objet names already in the same context)
                        objectNameAssignments +=((objName1, objName2), ctx and objectNamePresenceCondition(objName2))
                    }
                    initNames
                } else None
            }

            case DeclIdentifierList(idList: List[Opt[Id]]) => for (Opt(fExpr, e) <- idList) extractObjectNames(e, fExpr);
                None

            // function parameters declaration
            case DeclParameterDeclList(parameterDecls: List[Opt[ParameterDeclaration]]) => {
                isDeclarationStatement = True;
                for (Opt(featExpr, e) <- parameterDecls) extractObjectNames(e, ctx and featExpr);
                isDeclarationStatement = False;
                None
            };
            case DeclArrayAccess(expr: Option[Expr]) => if (expr.isDefined) {
                val exprStr = extractExpr(expr.get, ctx);
                exprStr
            };
            else None

            // variable declarator
            case AtomicNamedDeclarator(pointers: List[Opt[Pointer]], id: Id, extensions: List[Opt[DeclaratorExtension]]) => {
                for (Opt(featExpr, e) <- pointers) extractObjectNames(e, ctx and featExpr);
                for (Opt(featExpr, e) <- extensions) extractObjectNames(e, ctx and featExpr);

                if (isDeclarationStatement.isSatisfiable() && isNotTypedefSpecifier.isSatisfiable()) {
                    addObjectName(applyScope(id.name, currentFunction), ctx)

                    if (isPointer.isSatisfiable()) {
                        addObjectName(applyScope(PointerDereferenceOp + parenthesize(id.name), currentFunction), ctx)
                        isPointer = False
                    }
                }
                Some(id.name)

            }

            case NestedNamedDeclarator(pointers: List[Opt[Pointer]], nestedDecl: Declarator, extensions: List[Opt[DeclaratorExtension]], attr: List[Opt[AttributeSpecifier]]) => {
                for (Opt(featExpr, e) <- pointers) extractObjectNames(e, ctx and featExpr)
                for (Opt(featExpr, e) <- extensions) extractObjectNames(e, ctx and featExpr)
                for (Opt(featExpr, e) <- attr) extractObjectNames(e, ctx and featExpr)
                val declStr = extractObjectNames(nestedDecl, ctx)
                declStr
            }
            case AtomicAbstractDeclarator(pointers: List[Opt[Pointer]], extensions: List[Opt[DeclaratorAbstrExtension]]) => {
                for (Opt(featExpr, e) <- pointers) extractObjectNames(e, ctx and featExpr)
                for (Opt(featExpr, e) <- extensions) extractObjectNames(e, ctx and featExpr)
                None
            }

            case NestedAbstractDeclarator(pointers: List[Opt[Pointer]], nestedDecl: AbstractDeclarator, extensions: List[Opt[DeclaratorAbstrExtension]], attr: List[Opt[AttributeSpecifier]]) => {
                for (Opt(featExpr, e) <- pointers) extractObjectNames(e, ctx and featExpr)
                for (Opt(featExpr, e) <- extensions) extractObjectNames(e, ctx and featExpr)
                for (Opt(featExpr, e) <- attr) extractObjectNames(e, ctx and featExpr)
                val declStr = extractObjectNames(nestedDecl, ctx)
                declStr
            }

            case PlainParameterDeclaration(declSpecs: List[Opt[Specifier]], attr: List[Opt[AttributeSpecifier]]) => {
                for (Opt(featExpr, e) <- declSpecs) extractObjectNames(e, ctx and featExpr)
                declSpecs.map {
                    case Opt(featExpr, ts: TypeDefTypeSpecifier) => isNotTypedefSpecifier = isNotTypedefSpecifier andNot featExpr;
                    case _ =>
                }

                for (Opt(_, e) <- attr) extractObjectNames(e, ctx)
                isNotTypedefSpecifier = True
                None
            }

            case ParameterDeclarationD(declSpecs: List[Opt[Specifier]], decl: Declarator, attr: List[Opt[AttributeSpecifier]]) => {
                for (Opt(featExpr, e) <- declSpecs) extractObjectNames(e, ctx and featExpr)
                declSpecs.map {
                    case Opt(featExpr, ts: TypeDefTypeSpecifier) => isNotTypedefSpecifier = isNotTypedefSpecifier andNot featExpr;
                    case _ =>
                }

                for (Opt(featExpr, e) <- attr) extractObjectNames(e, ctx and featExpr)
                val declStr = extractObjectNames(decl, ctx)
                if (isDeclarationStatement.isSatisfiable() && declStr.isDefined) {
                    objectNamesScope = objectNamesScope.updated(declStr.get, currentFunction)
                    functionDefParameters = functionDefParameters.updated(currentFunction, functionDefParameters.getOrElse(currentFunction, List()) :+ applyScope(declStr.get, currentFunction))
                }
                isNotTypedefSpecifier = True
                declStr
            }

            case ParameterDeclarationAD(declSpecs: List[Opt[Specifier]], decl: AbstractDeclarator, attr: List[Opt[AttributeSpecifier]]) => {
                for (Opt(featExpr, e) <- declSpecs) extractObjectNames(e, ctx and featExpr)
                declSpecs.map {
                    case Opt(featExpr, ts: TypeDefTypeSpecifier) => isNotTypedefSpecifier = isNotTypedefSpecifier andNot featExpr;
                    case _ =>
                }

                for (Opt(featExpr, e) <- attr) extractObjectNames(e, ctx and featExpr)
                val declStr = extractObjectNames(decl, ctx)
                if (isDeclarationStatement.isSatisfiable() && declStr.isDefined) {
                    objectNamesScope = objectNamesScope.updated(declStr.get, currentFunction)
                    functionDefParameters = functionDefParameters.updated(currentFunction, functionDefParameters.getOrElse(currentFunction, List()) :+ applyScope(declStr.get, currentFunction))
                }
                isNotTypedefSpecifier = True
                declStr
            }

            case Initializer(initializerElementLabel: Option[InitializerElementLabel], expr: Expr) => {
                val exprStr = extractExpr(expr, ctx)
                exprStr
            }

            case Pointer(specifier: List[Opt[Specifier]]) => {
                isPointer = True
                None
            }

            case AtomicAttribute(n: String) => {
                Some(n)
            }

            case AttributeSequence(attributes: List[Opt[Attribute]]) => {
                for (Opt(featExpr, e) <- attributes) extractObjectNames(e, ctx and featExpr)
                None
            }

            case CompoundAttribute(inner: List[Opt[AttributeSequence]]) => {
                for (Opt(featExpr, e) <- inner) extractObjectNames(e, ctx and featExpr)
                None
            }

            case ElifStatement(condition: Conditional[Expr], thenBranch: Conditional[Statement]) => {
                val exprStr = condition match {
                    case One(e) => extractExpr(e, ctx)
                    case c => throw new Exception("%s is not supported".format(c))
                }
                if (exprStr.isDefined) {
                    val scope = findScopeForObjectName(exprStr.get, currentFunction)
                    addObjectName(applyScope(exprStr.get, scope), ctx)
                }
                thenBranch.map(stmt => extractStmt(stmt, ctx))
                exprStr
            }

            case e: Expr => extractExpr(e, ctx)
            case s: Statement => extractStmt(s, ctx)
            case z => throw new Exception("%s is not supported".format(z.getClass))
        }
    }

}
