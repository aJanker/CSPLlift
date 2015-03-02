package de.fosd.typechef.ccallgraph

import de.fosd.typechef.conditional._
import de.fosd.typechef.featureexpr.FeatureExpr
import de.fosd.typechef.featureexpr.FeatureExprFactory.{False, True}
import de.fosd.typechef.parser.c._

/**
 * Created by gferreir on 9/20/14.
 *
 */

class CCallGraph {

    type ObjectName = String
    type Scope = String
    type Assignment = (ObjectName, ObjectName)

    var callGraphNodes: ConditionalSet[String] = ConditionalSet()
    var callGraphEdges: ConditionalSet[(String, String, String)] = ConditionalSet()
    var callGraphPointerEdges: ConditionalSet[(String, String)] = ConditionalSet()

    var extractedObjectNames: ConditionalSet[ObjectName] = ConditionalSet()
    var objectNameAssignments: ConditionalSet[Assignment] = ConditionalSet()
    var equivalenceClasses: Set[EquivalenceClass] = Set()
    var equivalenceClassesPrefixSets: Set[(ObjectName, ObjectName)] = Set()

    // map of function defs and [return values, parameters]
    var functionDefReturns: Map[String, ConditionalSet[ObjectName]] = Map()
    var functionDefParameters: Map[String, List[Opt[ObjectName]]] = Map()
    var functionCallParameters: List[(String, List[Opt[ObjectName]])] = List()
    var functionDefs: ConditionalSet[ObjectName] = ConditionalSet()

    var functonCallExpr: String = ""
    var functionParamList: List[Opt[ObjectName]] = List()

    var functionCalls: ConditionalSet[(String, String)] = ConditionalSet()

    // object names scope (key =  object name, value = scope)
    var objectNamesScope: Map[ObjectName, Scope] = Map()


    // context variables
    var currentFunction: Scope = "GLOBAL"
    var currentDeclarator: ObjectName = ""
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

    def extractCallGraph() = {
        extractFunctionDef() // nodes
        extractFunctionCalls() // edges
    }

    def extractFunctionDef() = {
        for (fdef <- functionDefParameters.keySet) {
            callGraphNodes = callGraphNodes.+(fdef, True)
        }
    }

    def extractFunctionCalls() = {
        var found: Boolean = false
        for (((source, target), condition) <- functionCalls.toPlainSetWithConditionals()) {

            // check if object name is not a function name
            if (!functionDefs.toPlainSet().contains(unscope(target))) {

                // search for equivalence class that contains the object name
                val pointerEquivalanceClass = equivalenceClasses.find({ e => e.plainObjectNames.contains(target)})

                // if no equivalence class is found, throw exception (this should never occur)
                // reason: an equivalence class is created for each object name in the beginning of the analysis
                if (!pointerEquivalanceClass.isDefined) {
                    callGraphEdges +=((source, target, "ECNF"), condition)
                } else {
                    // add an indirect function call for each function name found in the equivalence class
                    found = false
                    for ((objName, functionCondition) <- pointerEquivalanceClass.get.objectNames().toPlainSetWithConditionals()) {
                        if (functionDefs.toPlainSet().contains(unscope(objName)) || functionDefs.toPlainSet().contains("%s()".format(unscope(objName)))) {
                            callGraphEdges +=((source, unscope(objName), "I"), functionCondition)
                            found = true
                        }
                    }
                    // if no function name was found, insert an "unresolved" edge with the original object name
                    if (!found) {
                        callGraphEdges +=((source, target, "FNNF"), condition)
                    }
                }
            } else {
                // add direct function call
                callGraphEdges +=((source, unscope(target), "D"), condition)
            }
        }
    }

    def calculatePointerEquivalenceRelation(program: TranslationUnit): Set[EquivalenceClass] = {


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


    def addPrefixSet(o1: ObjectName, o2: ObjectName) = {
        equivalenceClassesPrefixSets += ((o1, o2))
    }

    def createInitialPrefixSets(): Unit = {
        // generate cross product of all object names
        for ((o, o1) <- equivalenceClassesPrefixSets) {
            val eqClassObjectO = find(o)
            val eqClassObjectO1 = find(o1)

            if (eqClassObjectO.isDefined && eqClassObjectO1.isDefined) {

                val eqClassObjectOFeatExpr = eqClassObjectO.getOrElse(EquivalenceClass()).objectNames().get(o);
                val eqClassObjectO1FeatExpr = eqClassObjectO1.getOrElse(EquivalenceClass()).objectNames().get(o1);

                val uo = unscope(o)
                val uo1 = unscope(o1)

                // pointer cretion operator
                if (((uo.equals(apply(PointerCreationOp, uo1))) || uo.equals(apply(PointerCreationOp, parenthesize(uo1)))) && (eqClassObjectOFeatExpr.equiv(eqClassObjectO1FeatExpr).isTautology())) {
                    // add * edge from o to o1 (removing the pointer creation effect with a dereferencing operator)
                    eqClassObjectO.get.addPrefix((PointerDereferenceOp, o1), eqClassObjectO1.get.objectNames().get(o1))

                    // pointer dereference operator
                } else if ((uo.equals(apply(PointerDereferenceOp, uo1)) || uo.equals(apply(PointerDereferenceOp, parenthesize(uo1))) && (eqClassObjectOFeatExpr.equiv(eqClassObjectO1FeatExpr).isTautology()))) {
                    // add * edge from o1 to o
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
            else {
                println(">>> One or both object names are not defined: %s / %s".format(o, o1))
            }
        }
    }

    def objectNamePresenceCondition(objectName: ObjectName): FeatureExpr = {
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

            val listParamsFuncDef: Conditional[List[ObjectName]] = ConditionalLib.explodeOptList(functionDefParameters.getOrElse(function, List()))
            val listParamsFuncCal: Conditional[List[ObjectName]] = ConditionalLib.explodeOptList(listParams)

            val assignments = ConditionalLib.zip(listParamsFuncCal, listParamsFuncDef).toList

            assignments.foreach({ t =>
                val expr = t._1
                val (list1, list2) = t._2
                val condAssignments = list1.zip(list2)

                condAssignments.foreach({ a => objectNameAssignments +=(a, expr)})
            })
        }
    }

    // replace each auxiliary function call reference for its real return values
    def addAssignmentsFromFunctionCallReturnValues() = {
        val assignmentsPartition = objectNameAssignments.partition({ a: ((String, String)) => !a._1.contains(FunctionCallOp) && !a._2.contains(FunctionCallOp)})
        var normalizedAssignments: ConditionalSet[Assignment] = assignmentsPartition._1

        // replace function call by the all return values
        assignmentsPartition._2.toPlainSetWithConditionals.foreach({ case (assign, featExpr) =>
            if (assign._1 contains FunctionCallOp) {
                functionDefReturns.getOrElse(unscope(assign._1.replaceAll("\\(\\)", "")), ConditionalSet()).toPlainSetWithConditionals().foreach({
                    x => normalizedAssignments +=((x._1, assign._2), featExpr and x._2)
                })
            } else if (assign._2 contains FunctionCallOp) {
                functionDefReturns.getOrElse(unscope(assign._2.replaceAll("\\(\\)", "")), ConditionalSet()).toPlainSetWithConditionals.foreach({
                    x => normalizedAssignments +=((x._1, assign._1), featExpr and x._2)
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
        val newObjectNamesSet: EquivalenceClass = e1.union(e2)
        var newPrefixSet: ConditionalSet[(String, String)] = e1.prefixes()

        // loop both prefix sets
        for ((a, o) <- e2.prefixes().toPlainSet()) {
            val sharedPrefix = newPrefixSet.toPlainSet().filter({ case (a1, o1) => a.equals(a1)})

            if (sharedPrefix.nonEmpty) {
                // if equivalence classes share the same prefix (i.e., if they have edges to the same object name)
                sharedPrefix.map({ case ((_, o1)) =>

                    val eqClassO = find(o).get
                    val eqClassO1 = find(o1).get

                    // if any two eq classes have the same prefix relation, merge them recursevely
                    if (!eqClassO.equals(eqClassO1)) merge(eqClassO, eqClassO1);
                })
            } else newPrefixSet +=((a, o), e2.prefixes().get((a, o)))
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

    def showAssignments() = {
        println(objectNameAssignments)
    }

    def showExtractedObjectNames() = {
        println(extractedObjectNames)
    }

    def showFunctionDefReturns() = {
        println(functionDefReturns)
    }

    def showFunctionDefs() = {
        println(functionDefs)
    }

    def showFunctionDefsParameters() = {
        println(functionDefParameters)
    }

    def showPointerEquivalenceClasses() = {
        equivalenceClasses.map(println)
    }

    def showFunctionCalls() = {
        println("Function calls (%d): %s".format(functionCalls.toPlainSet().size, functionCalls.toPlainSetWithConditionals()))
    }

    def showCallGraph() = {
        println("\nCall graph [N = %d, E = %d, PAE = %d]\nNodes: %s\nEdges: %s".format(callGraphNodes.toPlainSet().size, callGraphEdges.toPlainSet().size, callGraphPointerEdges.toPlainSet().size, callGraphNodes, callGraphEdges))
    }

    def writeCallGraph(fileName: String, writer: GraphWriter) = {
        callGraphNodes.toPlainSetWithConditionals().foreach({ case (v, expr) =>
            writer.writeNode(v, expr)
        })
        callGraphEdges.toPlainSetWithConditionals().foreach({ case ((s, t, etype), expr) =>
            writer.writeEdge(s, t, etype, expr)
        })
        writer.close()

    }

    def parenthesize(objName: String) = {
        val operators = List(StructAccessOp, StructPointerAccessOp, PointerCreationOp, PointerDereferenceOp)
        if (operators.exists(objName.contains)) {
            "(%s)".format(objName)
        }
        else objName
    }

    private def extractExpr(expr: Expr, ctx: FeatureExpr): Option[String] = {
        // println(expr)
        expr match {
            case Id(name: String) => Some(name)
            // constants are not important
            case Constant(value: String) => None
            case StringLit(name: List[Opt[String]]) => None
            case l: LcurlyInitializer => None
            case UnaryExpr(kind: String, e: Expr) => extractExpr(e, ctx)
            case SizeOfExprT(typeName: TypeName) => None
            case SizeOfExprU(expr: Expr) => extractExpr(expr, ctx);
                None
            case CastExpr(typeName: TypeName, expr: Expr) => {
                val exprStr = extractExpr(expr, ctx);
                exprStr
            }
            case ExprList(exprs: List[Opt[Expr]]) => {
                functionParamList = List()
                for (Opt(fExpr, e) <- exprs) {
                    val exprStr = extractObjectNames(e, ctx and fExpr);
                    if (exprStr.isDefined) {
                        val scope = findScopeForObjectName(exprStr.get, currentFunction)
                        functionParamList = functionParamList :+ Opt(ctx and fExpr, applyScope(exprStr.get, scope))
                    }
                }
                None
            };

            case PointerDerefExpr(castExpr: Expr) => {
                val exprStr = extractExpr(castExpr, ctx)
                if (exprStr.isDefined) {

                    val scope = findScopeForObjectName(exprStr.get, currentFunction)

                    val objectName1 = applyScope(exprStr.get, scope)
                    val objectName2 = applyScope((PointerDereferenceOp + parenthesize(exprStr.get)), scope)

                    addObjectName(objectName1, ctx)
                    addObjectName(objectName2, ctx)
                    addPrefixSet(objectName2, objectName1)
                }
                exprStr.map(PointerDereferenceOp + parenthesize(_))
            }
            case PointerCreationExpr(castExpr: Expr) => {
                val exprStr = extractExpr(castExpr, ctx)
                if (exprStr.isDefined) {
                    val scope = findScopeForObjectName(exprStr.get, currentFunction)

                    val objectName1 = applyScope(exprStr.get, scope)
                    val objectName2 = applyScope((PointerCreationOp + parenthesize(exprStr.get)), scope)

                    addObjectName(objectName1, ctx)
                    addObjectName(objectName2, ctx)
                    addPrefixSet(objectName2, objectName1)

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
                extractExpr(condition, ctx)
                var exprStr1: Option[String] = None
                val exprStr2 = extractExpr(elseExpr, ctx)

                if (thenExpr.isDefined) { exprStr1 = extractExpr(thenExpr.get, ctx) }


                Some(exprStr1.getOrElse("Empty") + "|" + exprStr2.getOrElse("Empty")) // TODO: FIX ME (should return two options)
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
                        val objectName1 = applyScope(exprStr1.get, scope)
                        val objectName2 = applyScope(PointerDereferenceOp + parenthesize(exprStr1.get), scope)
                        val objectName3 = applyScope(parenthesize(exprStr1.get) + exprStr2.get, scope)

                        addObjectName(objectName1, ctx)
                        addObjectName(objectName2, ctx)
                        addObjectName(objectName3, ctx)

                        addPrefixSet(objectName3, objectName2)

                        // . (struct access operator)
                    } else if (exprStr2.get startsWith StructAccessOp) {
                        val objectName1 = applyScope(exprStr1.get, scope)
                        val objectName2 = applyScope(parenthesize(exprStr1.get) + exprStr2.get, scope)

                        addObjectName(objectName1, ctx)
                        addObjectName(objectName2, ctx)

                        addPrefixSet(objectName2, objectName1)

                    }

                    // array access
                    else if (exprStr2.get equals ArrayAccessOp) {
                        addObjectName(applyScope(parenthesize(exprStr1.get) + exprStr2.get, scope), ctx)
                    }

                    // is a function call?
                    else if (exprStr2.get equals FunctionCallOp) {
                        val scope = findScopeForObjectName(exprStr1.get, currentFunction)

                        functionCallParameters +:=(exprStr1.get, functionParamList)
                        functionCalls +=((currentFunction, applyScope(parenthesize(exprStr1.get), scope)), ctx)

                        // is a declaration followed by a function call assignment?
                        //if (isDeclarationStatement.isSatisfiable()) {
                            addObjectName(applyScope(parenthesize(exprStr1.get) + FunctionCallOp, scope), ctx)
                        //}

                    }
                }
                exprStr1.flatMap(e1 => exprStr2.map(e2 => parenthesize(e1) + e2))
            };
            case CompoundStatementExpr(compoundStatement: CompoundStatement) => extractStmt(compoundStatement, ctx)
            case GnuAsmExpr(isVolatile: Boolean, isGoto: Boolean, expr: StringLit, stuff: Any) => None
        }

    }

    private def extractStmt(stmt: Statement, ctx: FeatureExpr): Option[String] = {
        // println(stmt)
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
                if (expr.isDefined) {
                    val exprStr = extractExpr(expr.get, ctx)
                    if (exprStr.isDefined) {
                        val scope = findScopeForObjectName(exprStr.get, currentFunction)
                        functionDefReturns = functionDefReturns.updated(currentFunction, functionDefReturns.getOrElse(currentFunction, ConditionalSet()) +(applyScope(exprStr.get, scope), ctx))
                    }
                }
                None
            }
            case LabelStatement(id: Id, attribute: Option[AttributeSpecifier]) => None
            case CaseStatement(c: Expr) => extractExpr(c, ctx)
                None
            case DefaultStatement() => None
            case IfStatement(condition: Conditional[Expr], thenBranch: Conditional[Statement], elifs: List[Opt[ElifStatement]], elseBranch: Option[Conditional[Statement]]) => {

                for (Opt(fExpr, e) <- condition.toOptList) {
                    val exprStr = extractExpr(e, ctx and fExpr)

                    if (exprStr.isDefined) {
                        val scope = findScopeForObjectName(exprStr.get, currentFunction)
                        addObjectName(applyScope(exprStr.get, scope), ctx and fExpr)
                    }
                }
                thenBranch.mapf(ctx, (c, stmt) => extractStmt(stmt, c))
                for (Opt(fExpr, s) <- elifs) extractObjectNames(s, ctx and fExpr)
                if (elseBranch.isDefined) elseBranch.get.mapf(ctx, (c, stmt) => extractStmt(stmt, c))
                None
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

    private def extractDeclarator(declarator: AST, ctx: FeatureExpr): Option[String] = {
        // println(declarator)
        declarator match {
            // variable declarator with initializer
            case InitDeclaratorI(decl: Declarator, _, init: Option[Initializer]) => {
                val declStr = extractObjectNames(decl, ctx)

                currentDeclarator = declStr.getOrElse("DECLARATOR_NOT_AVAILABLE")
                val initNames = init.flatMap(i => extractObjectNames(i, ctx))

                if (declStr.isDefined) {
                    if (isNotTypedefSpecifier.isSatisfiable()) {
                        val objName1 = applyScope(declStr.get, currentFunction)

                        if (initNames.isDefined) {
                            val scope = findScopeForObjectName(initNames.get, currentFunction)
                            val objName2 = applyScope(initNames.get, scope)

                            // assignment (no need to get a new presence condition, objet names already in the same context)
                            objectNameAssignments +=((objName1, objName2), ctx and objectNamePresenceCondition(objName2))
                        }
                    }
                    else {
                        currentFunction = declStr.get
                    }
                    initNames
                } else None
            }

            // variable declarator
            case AtomicNamedDeclarator(pointers: List[Opt[Pointer]], id: Id, extensions: List[Opt[DeclaratorExtension]]) => {
                currentDeclarator = id.name
                isPointer = False

                if (isDeclarationStatement.isSatisfiable() && isNotTypedefSpecifier.isSatisfiable()) {
                    addObjectName(applyScope(currentDeclarator, currentFunction), ctx)

                    if (isPointer.isSatisfiable()) {
                        currentDeclarator = PointerDereferenceOp + parenthesize(id.name)
                        addObjectName(applyScope(currentDeclarator, currentFunction), ctx)
                    }
                }
                for (Opt(featExpr, e) <- pointers) extractObjectNames(e, ctx and featExpr);
                for (Opt(featExpr, e) <- extensions) extractObjectNames(e, ctx and featExpr);

                Some(currentDeclarator)

            }

            case NestedNamedDeclarator(pointers: List[Opt[Pointer]], nestedDecl: Declarator, extensions: List[Opt[DeclaratorExtension]], attr: List[Opt[AttributeSpecifier]]) => {
                isPointer = False
                for (Opt(featExpr, e) <- pointers) extractObjectNames(e, ctx and featExpr)

                val declStr = extractObjectNames(nestedDecl, ctx)
                currentDeclarator = declStr.getOrElse("DECLARATOR_NOT_AVAILABLE")

                for (Opt(featExpr, e) <- extensions) extractObjectNames(e, ctx and featExpr)
                for (Opt(featExpr, e) <- attr) extractObjectNames(e, ctx and featExpr)
                declStr
            }
            case AtomicAbstractDeclarator(pointers: List[Opt[Pointer]], extensions: List[Opt[DeclaratorAbstrExtension]]) => {
                isPointer = False
                for (Opt(featExpr, e) <- pointers) extractObjectNames(e, ctx and featExpr)
                for (Opt(featExpr, e) <- extensions) extractObjectNames(e, ctx and featExpr)
                None
            }

            case NestedAbstractDeclarator(pointers: List[Opt[Pointer]], nestedDecl: AbstractDeclarator, extensions: List[Opt[DeclaratorAbstrExtension]], attr: List[Opt[AttributeSpecifier]]) => {
                isPointer = False
                for (Opt(featExpr, e) <- pointers) extractObjectNames(e, ctx and featExpr)
                for (Opt(featExpr, e) <- extensions) extractObjectNames(e, ctx and featExpr)
                for (Opt(featExpr, e) <- attr) extractObjectNames(e, ctx and featExpr)
                val declStr = extractObjectNames(nestedDecl, ctx)
                declStr
            }
        }

    }

    def extractObjectNames(ast: AST, ctx: FeatureExpr): Option[String] = {
     //   println(ast)
        ast match {
            case EmptyExternalDef() => None
            case TypelessDeclaration(declList: List[Opt[InitDeclarator]]) => {
                for (Opt(featExpr, e) <- declList) {extractObjectNames(e, ctx and featExpr);}
                None
            }
            case TypeName(specifiers: List[Opt[Specifier]], decl: Option[AbstractDeclarator]) => {
                for (Opt(featExpr, s) <- specifiers) extractObjectNames(s, ctx and featExpr);
                if (decl.isDefined) extractObjectNames(decl.get, ctx);
                None
            }
            case TranslationUnit(list: List[Opt[ExternalDef]]) => for (Opt(featExpr, e) <- list) extractObjectNames(e, ctx and featExpr); None

            case Declaration(declSpecs: List[Opt[Specifier]], init: List[Opt[InitDeclarator]]) => {
                isDeclarationStatement = True
                isNotTypedefSpecifier = True;
                for (Opt(featExpr, e) <- declSpecs) extractObjectNames(e, ctx and featExpr)
                declSpecs.map {
                    case Opt(featExpr, t: TypedefSpecifier) => isNotTypedefSpecifier = isNotTypedefSpecifier andNot featExpr;
                    case _ =>
                }
                for (Opt(featExpr, e) <- init) extractObjectNames(e, ctx and featExpr)
                isDeclarationStatement = False;
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
            case SimplePostfixSuffix(t: String) => None
            case PointerPostfixSuffix(operator: String, expr) => {
                val str = extractExpr(expr, ctx)
                str.map(operator + _)
            }

            case specif: Specifier => None
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

                val oldObjectNamesScope = objectNamesScope
                isDeclarationStatement = True;

                extractObjectNames(declarator, ctx)
                currentFunction = declarator.getName

                // initialize return and parameters list for function
                functionDefReturns += (currentFunction -> ConditionalSet())
                functionDefParameters += (currentFunction -> List[Opt[ObjectName]]())
                functionDefs +=(currentFunction, ctx)

                // process parameters and function body
                for (Opt(featExpr, p) <- parameters) extractObjectNames(p, ctx and featExpr)
                extractStmt(stmt, ctx)

                // end of function - restore global state
                isDeclarationStatement = False;
                isFunctionDef = False
                currentFunction = "GLOBAL"
                objectNamesScope = oldObjectNamesScope

                None
            }

            case DeclIdentifierList(idList: List[Opt[Id]]) => for (Opt(fExpr, e) <- idList) extractObjectNames(e, fExpr);
                None

            // function parameters declaration
            // captures parameters for FunctionDef, Extern declarations, Typedefs definitions
            case DeclParameterDeclList(parameterDecls: List[Opt[ParameterDeclaration]]) => {
                isDeclarationStatement = True;

                // add declarator name for current function
                functionDefs = functionDefs +(currentDeclarator, ctx)
                currentFunction = currentDeclarator

                // add function as object name
                val scopedFunctionName = applyScope(currentDeclarator, "GLOBAL")
                addObjectName(scopedFunctionName, ctx)

                // iterate over parameters extracting object names
                for (Opt(featExpr, e) <- parameterDecls) extractObjectNames(e, ctx and featExpr);

                isDeclarationStatement = False;
                None
            }

            case DeclArrayAccess(expr: Option[Expr]) => {
                if (expr.isDefined) {
                    val exprStr = extractExpr(expr.get, ctx);
                    exprStr
                }
                else None
            }

            case PlainParameterDeclaration(declSpecs: List[Opt[Specifier]], attr: List[Opt[AttributeSpecifier]]) => {
                for (Opt(featExpr, e) <- declSpecs) extractObjectNames(e, ctx and featExpr)
                isNotTypedefSpecifier = True
                declSpecs.map {
                    case Opt(featExpr, ts: TypedefSpecifier) => isNotTypedefSpecifier = isNotTypedefSpecifier andNot featExpr;
                    case _ =>
                }
                for (Opt(_, e) <- attr) extractObjectNames(e, ctx)

                None
            }

            case ParameterDeclarationD(declSpecs: List[Opt[Specifier]], decl: Declarator, attr: List[Opt[AttributeSpecifier]]) => {
                for (Opt(featExpr, e) <- declSpecs) extractObjectNames(e, ctx and featExpr)
                isNotTypedefSpecifier = True
                declSpecs.map {
                    case Opt(featExpr, ts: TypedefSpecifier) => isNotTypedefSpecifier = isNotTypedefSpecifier andNot featExpr;
                    case _ =>
                }

                val declStr = extractObjectNames(decl, ctx)
                if (isDeclarationStatement.isSatisfiable() && declStr.isDefined) {
                    objectNamesScope = objectNamesScope.updated(declStr.get, currentFunction)
                    functionDefParameters = functionDefParameters.updated(currentFunction, functionDefParameters.getOrElse(currentFunction, List[Opt[ObjectName]]()) :+ Opt(ctx, applyScope(declStr.get, currentFunction)))
                }

                for (Opt(featExpr, e) <- attr) extractObjectNames(e, ctx and featExpr)

                declStr
            }

            case ParameterDeclarationAD(declSpecs: List[Opt[Specifier]], decl: AbstractDeclarator, attr: List[Opt[AttributeSpecifier]]) => {
                for (Opt(featExpr, e) <- declSpecs) extractObjectNames(e, ctx and featExpr)
                isNotTypedefSpecifier = True
                declSpecs.map {
                    case Opt(featExpr, ts: TypedefSpecifier) => isNotTypedefSpecifier = isNotTypedefSpecifier andNot featExpr;
                    case _ =>
                }

                for (Opt(featExpr, e) <- attr) extractObjectNames(e, ctx and featExpr)
                val declStr = extractObjectNames(decl, ctx)
                if (isDeclarationStatement.isSatisfiable() && declStr.isDefined) {
                    objectNamesScope = objectNamesScope.updated(declStr.get, currentFunction)
                    functionDefParameters = functionDefParameters.updated(currentFunction, functionDefParameters.getOrElse(currentFunction, List[Opt[ObjectName]]()) :+ Opt(ctx, applyScope(declStr.get, currentFunction)))
                }
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
                    case Choice(e, thenBranch, elseBranch) => {
                        val thenStr = thenBranch.map(expr => extractExpr(expr, ctx and e))

                        val elseStr = elseBranch.map(expr => extractExpr(expr, ctx andNot e))
                        None
                    }
                }
                if (exprStr.isDefined) {
                    val scope = findScopeForObjectName(exprStr.get, currentFunction)
                    addObjectName(applyScope(exprStr.get, scope), ctx)
                }
                thenBranch.map(stmt => extractStmt(stmt, ctx))
                exprStr
            }

            case va: VarArgs => None
            case p: Pragma => None
            case e: Expr => extractExpr(e, ctx)
            case s: Statement => extractStmt(s, ctx)
            case d: Declarator => extractDeclarator(d, ctx)
            case ad: AbstractDeclarator => extractDeclarator(ad, ctx)
            case id: InitDeclarator => extractDeclarator(id, ctx)
            case z => throw new RuntimeException("%s is not supported".format(z.getClass))
        }
    }

}
