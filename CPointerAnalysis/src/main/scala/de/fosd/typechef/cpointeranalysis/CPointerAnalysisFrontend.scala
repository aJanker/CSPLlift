package de.fosd.typechef.cpointeranalysis

import java.util

import de.fosd.typechef.conditional._
import de.fosd.typechef.featureexpr.FeatureExprFactory._
import de.fosd.typechef.featureexpr.{FeatureExpr, FeatureExprFactory, FeatureModel}
import de.fosd.typechef.parser.c._
import de.fosd.typechef.typesystem.linker.CSignature
import de.fosd.typechef.typesystem.{CDeclUse, CFunction, CTypeCache, CTypeSystemFrontend}

import scala.collection.immutable.Map
import scala.collection.mutable


/**
  * Pointer Analysis for TypeChef. This is implementation is adapted from the CCallGraph Implementation of TypeChef by Gabriel Ferreira
  * Improved by Andreas Janker
  */
class CPointerAnalysisFrontend(linkingInterface: Option[String] = None,
                               featureModel: FeatureModel = FeatureExprFactory.default.featureModelFactory.empty,
                               DEBUG: Boolean = false) extends PointerContext with ASTNavigation with ConditionalNavigation {

    lazy val linking : Option[CLinking] = None
        /*if (Files.exists(Paths.get(linkingInterface.getOrElse(" ")))) Some(new CLinking(linkingInterface.get))
        else None */

    def calculatePointerEquivalenceRelation(tUnit: TranslationUnit, env : (List[ASTEnv], util.IdentityHashMap[ASTEnv, CTypeSystemFrontend with CTypeCache with CDeclUse])): CPointerAnalysisContext = {
        val context = extractObjectNames(tUnit, env)

        extractIntraproceduralAssignments(context)
        context.solve()

        /*extractInterproceduralFieldPointerAccesses(context)
        context.solve() */
    }

    private def extractInterproceduralFieldPointerAccesses(context: CPointerAnalysisContext): CPointerAnalysisContext = {
        val cachedEqFieldReferences = new mutable.HashMap[Opt[ObjectName], List[Opt[ObjectName]]]()
        val cachedEqParentFieldReferences = new mutable.HashMap[Opt[ObjectName], List[Opt[ObjectName]]]()
        val conditionalObjectNames = context.getObjectNames.toOptList()
        val eqClassesWithContent = context.getEquivalenceClasses.par.filter(_.getObjectNames.size() > 1).seq
        val objectNamesWithStructFieldPointerDereference = conditionalObjectNames.par.filter(objectName => ObjectNameOperator.containsFieldPointerAccess(objectName.entry)).seq

        def getField(objectName: ObjectName): String = objectName.substring(objectName.lastIndexOf(ObjectNameOperator.StructPointerAccess.toString) + ObjectNameOperator.StructPointerAccess.toString.length)

        def isFieldAccessOf(parent: String, child: String): Boolean = {
            def isFieldAccessOf(fieldAccess: String) : Boolean = {
                val defaultStructPointerAccess = parent + fieldAccess
                lazy val nestedStructPointerAccess = "(" + parent + fieldAccess
                lazy val doubleNestedStructPointerAccess = "(" + parent + ")" + fieldAccess

                child.startsWith(defaultStructPointerAccess) || child.startsWith(nestedStructPointerAccess) || child.startsWith(doubleNestedStructPointerAccess)
            }

            isFieldAccessOf(ObjectNameOperator.StructPointerAccess.toString) || isFieldAccessOf(ObjectNameOperator.StructAccess.toString)
        }

        def isFieldAccess(scope: String, name: String, access: Opt[ObjectName]): Boolean = {
            val otherScope = unscopeFileAndMethod(access.entry)
            val otherName = unscopeName(access.entry)

            scope.equalsIgnoreCase(otherScope) && isFieldAccessOf(name, otherName)
        }

        def findFieldAccesses(query: Opt[ObjectName]): List[Opt[ObjectName]] = {
            if (cachedEqFieldReferences.contains(query))
                return cachedEqFieldReferences.getOrElse(query, List())

            val scope = unscopeFileAndMethod(query.entry)
            val name = unscopeName(query.entry)

            val result = objectNamesWithStructFieldPointerDereference.par.filter(isFieldAccess(scope, name, _)).toList

            cachedEqFieldReferences.+=((query, result))
            result
        }

        def findParentFields(query: Opt[ObjectName]): List[Opt[ObjectName]] = {
            if (cachedEqParentFieldReferences.contains(query))
                return cachedEqParentFieldReferences.getOrElse(query, List())

            val name = unscopeName(query.entry)

            if (!name.contains(ObjectNameOperator.StructPointerAccess.toString))
                return List()

            val scope = unscopeFileAndMethod(query.entry)
            val parent = scope + ObjectNameOperator.removeBracesFromStructPointerAccess(name.substring(0, name.lastIndexOf(ObjectNameOperator.StructPointerAccess.toString)))

            val parentEqClass = context.find(parent) match {
                case Some(s) => s.getObjectNames.toOptList()
                case _ => List()
            }

            val result = parentEqClass.flatMap(findFieldAccesses)

            cachedEqParentFieldReferences.+=((query, result))
            result
        }

        def mergeFieldPointerAccessesOfEqClassEquivalence(eqClass: EquivalenceClass) = {
            val objectNames = eqClass.getObjectNames.toOptList()

            objectNames.foldLeft(objectNames)((workingNames, currEntry) => {
                val remainingWorkingNames = workingNames.tail

                val referencedFields = findFieldAccesses(currEntry) ++ findParentFields(currEntry)

                if (referencedFields.nonEmpty)
                    remainingWorkingNames.foreach(otherEntry => {
                        val otherReferencedFields = findFieldAccesses(otherEntry) ++ findParentFields(otherEntry)

                        referencedFields.foreach(currField => {
                            val currName = unscopeName(currField.entry)
                            val currFieldName = getField(currName)

                            otherReferencedFields.foreach(otherField => {
                                val otherName = unscopeName(otherField.entry)
                                val otherFieldName = getField(otherName)


                                if (currFieldName.equalsIgnoreCase(otherFieldName))
                                    context.addObjectNameAssignment((currField.entry, otherField.entry), currField.condition.and(otherField.condition))
                            })
                        })
                    })


                remainingWorkingNames
            })
        }

        eqClassesWithContent.foreach(mergeFieldPointerAccessesOfEqClassEquivalence)

        context
    }

    private def getExternalParamsFuncDef(function: String, context: CPointerAnalysisContext): List[Opt[ObjectName]] = {
        def findInInterface(interface: CLinking): List[Opt[context.ObjectName]] = {
            interface.getSignatures(function) match {
                case None => List[Opt[ObjectName]]()
                case signatures => signatures.get.flatMap(findInSignature)
            }
        }

        def findInSignature(signature: CSignature): List[Opt[ObjectName]] =
            signature.ctype.atype match {
                case CFunction(params, ret) => params.toList.zipWithIndex.map {
                    case (cType, value) => {
                        val filename = signature.pos.headOption match {
                            case Some(name) => extractFilenameS(name.getFile)
                            case None => "NOFILENAME"
                        }
                        val name = context.applyScope("ARGUMENT" + value, signature.name, filename) // TODO VAARGS
                        LinkedObjectNames.addName(name)
                        Opt(signature.fexpr, name)
                    }
                }
                case _ => List()
            }

        linking match {
            case None => List()
            case Some(interface) => findInInterface(interface)
        }
    }


    private def addAssignmentsFromFunctionCallParameters(context: CPointerAnalysisContext): CPointerAnalysisContext = {
        for ((function, listParams) <- context.functionCallParameters) {
            val externalFuncDef = getExternalParamsFuncDef(function, context)
            val listParamsFuncDef: Conditional[List[ObjectName]] = ConditionalLib.explodeOptList(context.functionDefParameters.getOrElse(function, externalFuncDef))
            val listParamsFuncCal: Conditional[List[ObjectName]] = ConditionalLib.explodeOptList(listParams)

            val assignments = ConditionalLib.explode(listParamsFuncCal, listParamsFuncDef).toList

            assignments.foreach({ t =>
                val expr = t._1
                val (list1, list2) = t._2
                val condAssignments = list1.zip(list2)

                condAssignments.foreach({ a => {
                    LinkedObjectNames.addObjectNameAssignment(a, expr) // will be only added if one of the object names is known to be declared externally
                    context.addObjectNameAssignment(a, expr)
                }
                })
            })
        }
        context
    }

    // replace each auxiliary function call reference for its real return values
    private def addAssignmentsFromFunctionCallReturnValues(context: CPointerAnalysisContext): CPointerAnalysisContext = {

        val assignmentsPartition = context.getObjectNamesAssignments.partition({ a: ((String, String)) => !a._1.contains(ObjectNameOperator.FunctionCall.toString) && !a._2.contains(ObjectNameOperator.FunctionCall.toString) })
        var normalizedAssignments: ConditionalSet[Assignment] = assignmentsPartition._1

        // replace function call by the all return values
        assignmentsPartition._2.toPlainSetWithConditionals().foreach({ case (assign, featExpr) =>
            if (assign._1 contains ObjectNameOperator.FunctionCall.toString) {
                context.functionDefReturns.getOrElse(ObjectNameOperator.unscopeName(assign._1.replaceAll("\\(\\)", "")), ConditionalSet()).toPlainSetWithConditionals().foreach({
                    x => normalizedAssignments +=((x._1, assign._2), featExpr and x._2)
                })
            } else if (assign._2 contains ObjectNameOperator.FunctionCall.toString) {
                context.functionDefReturns.getOrElse(ObjectNameOperator.unscopeName(assign._2.replaceAll("\\(\\)", "")), ConditionalSet()).toPlainSetWithConditionals().foreach({
                    x => normalizedAssignments +=((x._1, assign._1), featExpr and x._2)
                })
            }
        })
        context.setObjectNameAssignments(normalizedAssignments)
        context
    }

    private def extractObjectNames(ast: AST, env : (List[ASTEnv], util.IdentityHashMap[ASTEnv, CTypeSystemFrontend with CTypeCache with CDeclUse])): CPointerAnalysisContext = {

        // context variables
        var analyisContext: CPointerAnalysisContext = new CPointerAnalysisContext()
        var currentFile: String = "NOFILENAME"
        var currentFunction: Scope = "GLOBAL"
        var currentFunctionKind: String = "function"
        var currentDeclarator: ObjectName = ""
        var currentDeclaratorLine: Int = -1
        var isDeclarationStatement: FeatureExpr = False
        var isPointer: FeatureExpr = False
        var isFunctionDef: FeatureExpr = False
        var isNotTypedefSpecifier: FeatureExpr = True
        var isFunctionDeclarator: FeatureExpr = False

        // map of function defs and [return values, parameters]
        var functionDefs: ConditionalSet[FunctionDefinition] = ConditionalSet()
        var functionDefReturns: Map[FunctionName, ConditionalSet[ObjectName]] = Map()
        var functionDefParameters: Map[FunctionName, List[Opt[ObjectName]]] = Map()

        var functionCallParamList: List[Opt[ObjectName]] = List()

        // function calls and function call parameters
        var functionCalls: ConditionalSet[FunctionCall] = ConditionalSet()
        var functionCallParameters: List[(FunctionName, List[Opt[ObjectName]])] = List()

        //helper functions
        val cachedEnv : (List[ASTEnv], util.IdentityHashMap[ASTEnv, CTypeSystemFrontend with CTypeCache with CDeclUse]) = env
        def getEnv(node: AST): Option[ASTEnv] = cachedEnv._1.find {_.containsASTElem(node)}
        def getTypeSystem(node: AST): Option[CTypeSystemFrontend with CTypeCache with CDeclUse] =
            getEnv(node) match {
                case Some(env) =>
                    cachedEnv._2.get(env) match {
                        case null => None
                        case ts => Some(ts)
                    }
                case _ => None
            }

        // Recursive Extraction Functions
        def extractAST(ast: AST, ctx: FeatureExpr = True): Option[String] = {
            if (DEBUG) println(ast)

            currentFile = extractFilename(ast)

            ast match {
                case EmptyExternalDef() => None
                case TypeName(specifiers: List[Opt[Specifier]], decl: Option[AbstractDeclarator]) => {
                    for (Opt(featExpr, s) <- specifiers) extractAST(s, ctx and featExpr)
                    if (decl.isDefined) extractAST(decl.get, ctx)
                    None
                }
                case TranslationUnit(list: List[Opt[ExternalDef]]) => for (Opt(featExpr, e) <- list) extractAST(e, ctx and featExpr); None

                case Declaration(declSpecs: List[Opt[Specifier]], init: List[Opt[InitDeclarator]]) => {
                    isDeclarationStatement = True
                    isNotTypedefSpecifier = True
                    for (Opt(featExpr, e) <- declSpecs) extractAST(e, ctx and featExpr)
                    declSpecs.foreach {
                        case Opt(featExpr, t: TypedefSpecifier) => isNotTypedefSpecifier = isNotTypedefSpecifier andNot featExpr;
                        case _ =>
                    }

                    // select potential function kind for declaration
                    currentFunctionKind = functionKind(declSpecs)

                    for (Opt(featExpr, e) <- init) extractAST(e, ctx and featExpr)
                    isDeclarationStatement = False;
                    None
                }

                case AsmExpr(isVolatile: Boolean, expr: Expr) => {
                    extractExpr(expr, ctx)
                }

                case TypelessDeclaration(declList: List[Opt[InitDeclarator]]) => {
                    for (Opt(featExpr, decl) <- declList) extractAST(decl, ctx and featExpr)
                    None
                }

                case FunctionCall(exprList: ExprList) => {
                    extractExpr(exprList, ctx)

                    Some(ObjectNameOperator.FunctionCall.toString)
                };

                case SimplePostfixSuffix(t: String) => None
                case PointerPostfixSuffix(operator: String, expr) => {
                    val str = extractExpr(expr, ctx)
                    str.map(operator + _)
                }

                case specif: Specifier => None
                case ArrayAccess(expr: Expr) => Some(ObjectNameOperator.ArrayAccess.toString)

                // ignore any kind of subexpression (with exception of assignments)
                case NArySubExpr(op: String, e: Expr) => {
                    e match {
                        case a: AssignExpr => extractExpr(a, ctx);
                        case fc: PostfixExpr => extractExpr(fc, ctx);
                        case ne: NAryExpr => extractExpr(ne, ctx)
                        case _ => ;
                    }
                    None
                }

                case FunctionDef(specifiers: List[Opt[Specifier]], declarator: Declarator, parameters: List[Opt[OldParameterDeclaration]], stmt: CompoundStatement) => {
                    isFunctionDef = True

                    // initialize return and parameters list for function
                    functionDefReturns += (declarator.getName -> ConditionalSet())
                    functionDefParameters += (declarator.getName -> List[Opt[ObjectName]]())

                    val oldObjectNamesScope = analyisContext
                    isDeclarationStatement = True;

                    // extract function definition information
                    isFunctionDeclarator = True
                    extractAST(declarator, ctx)
                    isFunctionDeclarator = False

                    // update scope - current function
                    currentFunction = declarator.getName
                    currentFunctionKind = functionKind(specifiers)
                    currentDeclaratorLine = declarator.getPositionFrom.getLine

                    // replace old function declarations for complete definitions (with updated context)
                    functionDefs = functionDefs.filterNot({ case (name, kind, _) => name.equals(currentFunction) && kind.equals("declaration") })

                    // remove old function definition and use old function presence condition
                    val oldFunctionDef = functionDefs.toPlainSetWithConditionals().find({ case ((name, kind, _), _) => name.equals(currentFunction) && kind.equals("function") })
                    if (oldFunctionDef.isDefined) {
                        val oldCtx = oldFunctionDef.map({ case ((name, kind, _), featExpr) => featExpr }).getOrElse(True)
                        functionDefs = functionDefs.filterNot({ case (name, kind, _) => name.equals(currentFunction) && kind.equals("function") })
                        functionDefs +=((currentFunction, currentFunctionKind, currentDeclaratorLine), ctx or oldCtx)
                    } else {
                        functionDefs +=((currentFunction, currentFunctionKind, currentDeclaratorLine), ctx)
                    }

                    // extract function parameters and body statements
                    for (Opt(featExpr, p) <- parameters) extractAST(p, ctx and featExpr)
                    extractStmt(stmt, ctx)

                    // end of function - restore global state
                    isDeclarationStatement = False
                    isFunctionDef = False
                    currentFunction = "GLOBAL"
                    analyisContext = oldObjectNamesScope // TODO why?

                    None
                }

                case DeclIdentifierList(idList: List[Opt[Id]]) => {
                    // add declarator name for current function
                    val previousFunction = currentFunction
                    currentFunction = currentDeclarator

                    // if it is a function declaration
                    val funcDef = functionDefs.toPlainSet().filter({ case (name, kind, _) => name.equals(currentFunction) && (kind.equals("function") || kind.equals("declaration")) })
                    if (funcDef.isEmpty) {
                        functionDefs +=((currentFunction, "declaration", currentDeclaratorLine), ctx)
                    }
                    for (Opt(fExpr, e) <- idList) extractAST(e, fExpr)
                    currentFunction = previousFunction

                    None
                }

                // function parameters declaration
                // captures parameters for FunctionDef, Extern declarations, Typedefs definitions
                case DeclParameterDeclList(parameterDecls: List[Opt[ParameterDeclaration]]) => {
                    isDeclarationStatement = True

                    // add declarator name for current function
                    val previousFunction = currentFunction
                    currentFunction = currentDeclarator

                    // if it is a function declaration
                    val funcDef = functionDefs.toPlainSet().filter({ case (name, kind, _) => name.equals(currentFunction) && (kind.equals("function") || kind.equals("declaration")) })
                    if (funcDef.isEmpty) {
                        functionDefs +=((currentFunction, "declaration", currentDeclaratorLine), ctx)
                    }

                    // add function as object name
                    val scopedFunctionName = analyisContext.applyScope(currentDeclarator, "GLOBAL", currentFile)
                    analyisContext.addObjectName(scopedFunctionName, ctx)

                    isFunctionDeclarator = False
                    // iterate over parameters extracting object names
                    for (Opt(featExpr, e) <- parameterDecls) extractAST(e, ctx and featExpr);
                    isFunctionDeclarator = True
                    currentFunction = previousFunction
                    isDeclarationStatement = False
                    None
                }

                case DeclArrayAccess(expr: Option[Expr]) => {
                    if (expr.isDefined) {
                        val exprStr = extractExpr(expr.get, ctx)
                        exprStr
                    }
                    else None
                }

                case PlainParameterDeclaration(declSpecs: List[Opt[Specifier]], attr: List[Opt[AttributeSpecifier]]) => {
                    for (Opt(featExpr, e) <- declSpecs) extractAST(e, ctx and featExpr)
                    isNotTypedefSpecifier = True
                    declSpecs.map {
                        case Opt(featExpr, ts: TypedefSpecifier) => isNotTypedefSpecifier = isNotTypedefSpecifier andNot featExpr;
                        case _ =>
                    }
                    for (Opt(_, e) <- attr) extractAST(e, ctx)

                    None
                }

                case ParameterDeclarationD(declSpecs: List[Opt[Specifier]], decl: Declarator, attr: List[Opt[AttributeSpecifier]]) => {
                    for (Opt(featExpr, e) <- declSpecs) extractAST(e, ctx and featExpr)

                    isNotTypedefSpecifier = True
                    declSpecs.map {
                        case Opt(featExpr, ts: TypedefSpecifier) => isNotTypedefSpecifier = isNotTypedefSpecifier andNot featExpr;
                        case _ =>
                    }

                    val declStr = extractAST(decl, ctx)
                    if (isDeclarationStatement.isSatisfiable() && declStr.isDefined) {

                        analyisContext.updateScope(declStr.get, currentFunction)
                        functionDefParameters = functionDefParameters.updated(currentFunction, functionDefParameters.getOrElse(currentFunction, List[Opt[ObjectName]]()) :+ Opt(ctx, analyisContext.applyScope(declStr.get, currentFunction, currentFile)))

                        if (isPointer.isSatisfiable()) {
                            val pointerObjectName = ObjectNameOperator.PointerDereference.toString + ObjectNameOperator.parenthesize(declStr.get)
                            analyisContext.updateScope(pointerObjectName, currentFunction)
                        }
                    }

                    for (Opt(featExpr, e) <- attr) extractAST(e, ctx and featExpr)

                    declStr
                }

                case ParameterDeclarationAD(declSpecs: List[Opt[Specifier]], decl: AbstractDeclarator, attr: List[Opt[AttributeSpecifier]]) => {
                    for (Opt(featExpr, e) <- declSpecs) extractAST(e, ctx and featExpr)
                    isNotTypedefSpecifier = True
                    declSpecs.map {
                        case Opt(featExpr, ts: TypedefSpecifier) => isNotTypedefSpecifier = isNotTypedefSpecifier andNot featExpr;
                        case _ =>
                    }

                    for (Opt(featExpr, e) <- attr) extractAST(e, ctx and featExpr)
                    val declStr = extractAST(decl, ctx)
                    if (isDeclarationStatement.isSatisfiable() && declStr.isDefined) {
                        analyisContext.updateScope(declStr.get, currentFunction)
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
                    for (Opt(featExpr, e) <- attributes) extractAST(e, ctx and featExpr)
                    None
                }

                case CompoundAttribute(inner: List[Opt[AttributeSequence]]) => {
                    for (Opt(featExpr, e) <- inner) extractAST(e, ctx and featExpr)
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
                        val scope = analyisContext.findScopeForObjectName(exprStr.get, currentFunction)
                        analyisContext.addObjectName(analyisContext.applyScope(exprStr.get, scope, currentFile), ctx)
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

        def extractExpr(expr: Expr, ctx: FeatureExpr): Option[String] = {
            if (DEBUG) println(expr)

            expr match {
                case Id(name: String) => Some(name)
                // constants are not important
                case Constant(value: String) => None
                case StringLit(name: List[Opt[String]]) => None
                case UnaryExpr(kind: String, e: Expr) => extractExpr(e, ctx)
                case SizeOfExprT(typeName: TypeName) => None
                case SizeOfExprU(expr: Expr) => extractExpr(expr, ctx)
                    None
                case CastExpr(typeName: TypeName, expr: Expr) => {
                    val exprStr = extractExpr(expr, ctx)
                    exprStr
                }
                case ExprList(exprs: List[Opt[Expr]]) => {
                    functionCallParamList = List()
                    for (Opt(fExpr, e) <- exprs) {
                        val exprStr = extractAST(e, ctx and fExpr);
                        if (exprStr.isDefined) {
                            val scope = analyisContext.findScopeForObjectName(exprStr.get, currentFunction)
                            functionCallParamList = functionCallParamList :+ Opt(ctx and fExpr, analyisContext.applyScope(exprStr.get, scope, currentFile))
                        }
                    }
                    None
                };

                case PointerDerefExpr(castExpr: Expr) => {
                    val exprStr = extractExpr(castExpr, ctx)
                    if (exprStr.isDefined) {

                        val scope = analyisContext.findScopeForObjectName(exprStr.get, currentFunction)

                        val objectName1 = analyisContext.applyScope(exprStr.get, scope, currentFile)
                        val objectName2 = analyisContext.applyScope((ObjectNameOperator.PointerDereference.toString + ObjectNameOperator.parenthesize(exprStr.get)), scope, currentFile)

                        analyisContext.addObjectName(objectName1, ctx)
                        analyisContext.addObjectName(objectName2, ctx)
                        analyisContext.addPrefixSet(objectName2, objectName1)
                    }
                    exprStr.map(ObjectNameOperator.PointerDereference.toString + ObjectNameOperator.parenthesize(_))
                }
                case PointerCreationExpr(castExpr: Expr) => {
                    val exprStr = extractExpr(castExpr, ctx)
                    if (exprStr.isDefined) {
                        val scope = analyisContext.findScopeForObjectName(exprStr.get, currentFunction)

                        val objectName1 = analyisContext.applyScope(exprStr.get, scope, currentFile)
                        val objectName2 = analyisContext.applyScope((ObjectNameOperator.PointerCreation.toString + ObjectNameOperator.parenthesize(exprStr.get)), scope, currentFile)

                        analyisContext.addObjectName(objectName1, ctx)
                        analyisContext.addObjectName(objectName2, ctx)
                        analyisContext.addPrefixSet(objectName2, objectName1)


                    }
                    exprStr.map(ObjectNameOperator.PointerCreation.toString + ObjectNameOperator.parenthesize(_))

                }
                case UnaryOpExpr(kind: String, castExpr: Expr) => extractExpr(castExpr, ctx)

                // any kind of pointer arithmetic or comparison is ignored by analysis
                case NAryExpr(expr: Expr, others: List[Opt[NArySubExpr]]) => {
                    val exprStr = extractExpr(expr, ctx)
                    if (exprStr.isDefined) {
                        val scope = analyisContext.findScopeForObjectName(exprStr.get, currentFunction)
                        analyisContext.addObjectName(analyisContext.applyScope(exprStr.get, scope, currentFile), ctx)
                    }
                    for (Opt(fExpr, subExpr) <- others) extractAST(subExpr, ctx and fExpr)
                    exprStr
                }

                case ConditionalExpr(condition: Expr, thenExpr: Option[Expr], elseExpr: Expr) => {
                    extractExpr(condition, ctx)
                    var exprStr1: Option[String] = None
                    val exprStr2 = extractExpr(elseExpr, ctx)

                    if (thenExpr.isDefined) {
                        exprStr1 = extractExpr(thenExpr.get, ctx)
                    }
                    Some(exprStr1.getOrElse("Empty") + "|" + exprStr2.getOrElse("Empty"))
                }

                case AssignExpr(target: Expr, operation: String, source: Expr) => {
                    val exprStr1 = extractExpr(target, ctx)
                    val exprStr2 = extractExpr(source, ctx)
                    if (exprStr1.isDefined && exprStr2.isDefined) {
                        val scopeObj1 = analyisContext.findScopeForObjectName(exprStr1.get, currentFunction)
                        val scopeObj2 = analyisContext.findScopeForObjectName(exprStr2.get, currentFunction)

                        val objName1 = analyisContext.addObjectName(analyisContext.applyScope(exprStr1.get, scopeObj1, currentFile), ctx)
                        val objName2 = analyisContext.addObjectName(analyisContext.applyScope(exprStr2.get, scopeObj2, currentFile), ctx)

                        // object names and context (disjunction of both subexpressions)
                        analyisContext.addObjectNameAssignment((objName1, objName2), ctx)
                    }
                    exprStr2
                }
                case PostfixExpr(postFixExpr: Expr, suffixExpr: PostfixSuffix) => {
                    val exprStr1 = extractExpr(postFixExpr, ctx)
                    val exprStr2 = extractAST(suffixExpr, ctx)

                    // member access operators
                    if (exprStr1.isDefined && exprStr2.isDefined) {
                        val scope = analyisContext.findScopeForObjectName(exprStr1.get, currentFunction)

                        // -> (sctruct pointer access operator)
                        if (exprStr2.get startsWith ObjectNameOperator.StructPointerAccess.toString) {
                            val objectName1 = analyisContext.applyScope(exprStr1.get, scope, currentFile)
                            val objectName2 = analyisContext.applyScope(ObjectNameOperator.PointerDereference.toString + ObjectNameOperator.parenthesize(exprStr1.get), scope, currentFile)
                            val objectName3 = analyisContext.applyScope(ObjectNameOperator.parenthesize(exprStr1.get) + exprStr2.get, scope, currentFile)

                            analyisContext.addObjectName(objectName1, ctx)
                            analyisContext.addObjectName(objectName2, ctx)
                            analyisContext.addObjectName(objectName3, ctx)

                            analyisContext.addPrefixSet(objectName2, objectName1)
                            analyisContext.addPrefixSet(objectName3, objectName2)

                            // . (struct access operator)
                        } else if (exprStr2.get startsWith ObjectNameOperator.StructAccess.toString) {
                            val objectName1 = analyisContext.applyScope(exprStr1.get, scope, currentFile)
                            val objectName2 = analyisContext.applyScope(ObjectNameOperator.parenthesize(exprStr1.get) + exprStr2.get, scope, currentFile)

                            analyisContext.addObjectName(objectName1, ctx)
                            analyisContext.addObjectName(objectName2, ctx)

                            analyisContext.addPrefixSet(objectName2, objectName1)
                        }

                        // array access
                        else if (exprStr2.get equals ObjectNameOperator.ArrayAccess.toString) {
                            analyisContext.addObjectName(analyisContext.applyScope(ObjectNameOperator.parenthesize(exprStr1.get) + exprStr2.get, scope, currentFile), ctx)
                        }

                        // is a function call?
                        else if (exprStr2.get equals ObjectNameOperator.FunctionCall.toString) {
                            functionCallParameters +:=(exprStr1.get, functionCallParamList)
                            functionCalls = functionCalls.+((currentFunction, exprStr1.get), ctx)

                            analyisContext.addObjectName(analyisContext.applyScope(exprStr1.get + ObjectNameOperator.FunctionCall.toString, currentFunction, currentFile), ctx)
                        }
                    }
                    exprStr1.flatMap(e1 => exprStr2.map(e2 => ObjectNameOperator.parenthesize(e1) + e2))
                };
                case CompoundStatementExpr(compoundStatement: CompoundStatement) => extractStmt(compoundStatement, ctx)
                case BuiltinOffsetof(typeName: TypeName, offsetofMemberDesignator: List[Opt[OffsetofMemberDesignator]]) => None
                case BuiltinTypesCompatible(typeName1: TypeName, typeName2: TypeName) => None
                case BuiltinVaArgs(expr: Expr, typeName: TypeName) => None
                case curly@LcurlyInitializer(inits: List[Opt[Initializer]]) =>
                    extractCurlyInitializer(curly, ctx)
                    None
                case AlignOfExprT(typeName: TypeName) => extractAST(typeName, ctx);
                case AlignOfExprU(expr: Expr) => extractExpr(expr, ctx);
                case GnuAsmExpr(isVolatile: Boolean, isGoto: Boolean, expr: StringLit, stuff: Any) => extractExpr(expr, ctx);
                case RangeExpr(from: Expr, to: Expr) => extractExpr(from, ctx); extractExpr(to, ctx); None

            }
        }

        def extractCurlyInitializer(curly: LcurlyInitializer, ctx: FeatureExpr): Option[String] = {
            if (DEBUG) {
                println(curly)
            }

            println("curly")
            val env = getEnv(curly).get
            val ts = getTypeSystem(curly).get
            println(findPriorASTElem[StructOrUnionSpecifier](curly, env))
            None
        }

        def extractStmt(stmt: Statement, ctx: FeatureExpr): Option[String] = {
            if (DEBUG) {
                println(stmt)
            }
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
                        val scope = analyisContext.findScopeForObjectName(exprStr.get, currentFunction)
                        analyisContext.addObjectName(analyisContext.applyScope(exprStr.get, scope, currentFile), ctx)
                    }
                    s.vmap(ctx, (c, stmt) => extractStmt(stmt, c))
                    None
                }
                case DoStatement(expr: Expr, s: Conditional[Statement]) => {
                    val exprStr = extractExpr(expr, ctx)
                    if (exprStr.isDefined) {
                        val scope = analyisContext.findScopeForObjectName(exprStr.get, currentFunction)
                        analyisContext.addObjectName(analyisContext.applyScope(exprStr.get, scope, currentFile), ctx)
                    }
                    s.vmap(ctx, (c, stmt) => extractStmt(stmt, c))
                    None
                }
                case ForStatement(expr1: Option[Expr], expr2: Option[Expr], expr3: Option[Expr], s: Conditional[Statement]) => {
                    val expr1Str = extractExpr(expr1.getOrElse(new Constant("")), ctx)
                    val expr2Str = extractExpr(expr2.getOrElse(new Constant("")), ctx)
                    val expr3Str = extractExpr(expr3.getOrElse(new Constant("")), ctx)

                    if (expr1Str.isDefined) {
                        val scope = analyisContext.findScopeForObjectName(expr1Str.get, currentFunction)
                        analyisContext.addObjectName(analyisContext.applyScope(expr1Str.get, scope, currentFile), ctx)
                    }
                    if (expr2Str.isDefined) {
                        val scope = analyisContext.findScopeForObjectName(expr2Str.get, currentFunction)
                        analyisContext.addObjectName(analyisContext.applyScope(expr2Str.get, scope, currentFile), ctx)
                    }
                    if (expr3Str.isDefined) {
                        val scope = analyisContext.findScopeForObjectName(expr3Str.get, currentFunction)
                        analyisContext.addObjectName(analyisContext.applyScope(expr3Str.get, scope, currentFile), ctx)
                    }

                    s.vmap(ctx, (c, stmt) => extractStmt(stmt, c))
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
                            val scope = analyisContext.findScopeForObjectName(exprStr.get, currentFunction)
                            functionDefReturns = functionDefReturns.updated(currentFunction, functionDefReturns.getOrElse(currentFunction, ConditionalSet()) +(analyisContext.applyScope(exprStr.get, scope, currentFile), ctx))
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
                            val scope = analyisContext.findScopeForObjectName(exprStr.get, currentFunction)
                            analyisContext.addObjectName(analyisContext.applyScope(exprStr.get, scope, currentFile), ctx and fExpr)
                        }
                    }
                    thenBranch.vmap(ctx, (c, stmt) => extractStmt(stmt, c))
                    for (Opt(fExpr, s) <- elifs) extractAST(s, ctx and fExpr)
                    if (elseBranch.isDefined) elseBranch.get.vmap(ctx, (c, stmt) => extractStmt(stmt, c))
                    None
                }

                case SwitchStatement(expr: Expr, s: Conditional[Statement]) => {
                    extractExpr(expr, ctx)
                    s.vmap(ctx, (c, stmt) => extractStmt(stmt, c))
                    None
                }
                case DeclarationStatement(decl: Declaration) => {
                    isDeclarationStatement = True
                    val declStr = extractAST(decl, ctx)
                    isDeclarationStatement = False
                    declStr
                }
                case LocalLabelDeclaration(ids: List[Opt[Id]]) => {
                    for (Opt(fExpr, id) <- ids) extractExpr(id, ctx and fExpr)
                    None
                }

                case NestedFunctionDef(isAuto: Boolean, specifiers: List[Opt[Specifier]], declarator: Declarator, parameters: List[Opt[Declaration]], stmt: CompoundStatement) => {
                    isFunctionDef = True

                    // initialize return and parameters list for function
                    functionDefReturns += (declarator.getName -> ConditionalSet())
                    functionDefParameters += (declarator.getName -> List[Opt[ObjectName]]())

                    val oldObjectNamesScope = analyisContext
                    val oldCurrentFunction = currentFunction
                    isDeclarationStatement = True;

                    // extract function definition information
                    isFunctionDeclarator = True
                    extractAST(declarator, ctx)
                    isFunctionDeclarator = False

                    // update scope - current function
                    currentFunction = declarator.getName
                    currentFunctionKind = functionKind(specifiers)
                    currentDeclaratorLine = declarator.getPositionFrom.getLine

                    // replace old function declarations for complete definitions
                    functionDefs = functionDefs.filterNot({ case (name, kind, _) => name.equals(currentFunction) && kind.equals("declaration") })

                    // remove old function definition and use old function presence condition
                    val oldFunctionDef = functionDefs.toPlainSetWithConditionals().find({ case ((name, kind, _), _) => name.equals(currentFunction) && kind.equals("function") })
                    if (oldFunctionDef.isDefined) {
                        val oldCtx = oldFunctionDef.map({ case ((name, kind, _), featExpr) => featExpr }).getOrElse(True)
                        functionDefs = functionDefs.filterNot({ case (name, kind, _) => name.equals(currentFunction) && kind.equals("function") })
                        functionDefs +=((currentFunction, currentFunctionKind, currentDeclaratorLine), ctx or oldCtx)
                    } else {
                        functionDefs +=((currentFunction, currentFunctionKind, currentDeclaratorLine), ctx)
                    }
                    // extract function parameters and body statements
                    for (Opt(featExpr, p) <- parameters) extractAST(p, ctx and featExpr)
                    extractStmt(stmt, ctx)

                    // end of function - restore global state
                    isDeclarationStatement = False;
                    isFunctionDef = False
                    currentFunction = oldCurrentFunction
                    analyisContext = oldObjectNamesScope // TODO CLONE!

                    None
                }
            }
        }

        def extractDeclarator(declarator: AST, ctx: FeatureExpr): Option[String] = {
            if (DEBUG) {
                println(declarator)
            }
            declarator match {
                // variable declarator with initializer
                case InitDeclaratorI(decl: Declarator, _, init: Option[Initializer]) => {
                    val declStr = extractAST(decl, ctx)

                    currentDeclarator = declStr.getOrElse("DECLARATOR_NOT_AVAILABLE")
                    val initNames = init.flatMap(i => extractAST(i, ctx))

                    if (declStr.isDefined) {
                        if (isNotTypedefSpecifier.isSatisfiable()) {
                            val objName1 = analyisContext.addObjectName(analyisContext.applyScope(declStr.get, currentFunction, currentFile), ctx)

                            if (initNames.isDefined) {
                                val scope = analyisContext.findScopeForObjectName(initNames.get, currentFunction)
                                val objName2 = analyisContext.addObjectName(analyisContext.applyScope(initNames.get, scope, currentFile), ctx)

                                // assignment (no need to get a new presence condition, objet names already in the same context)
                                analyisContext.addObjectNameAssignment((objName1, objName2), ctx)
                            }
                        }
                        initNames
                    } else None
                }

                // variable declarator
                case AtomicNamedDeclarator(pointers: List[Opt[Pointer]], id: Id, extensions: List[Opt[DeclaratorExtension]]) => {
                    currentDeclarator = id.name
                    currentDeclaratorLine = id.getPositionFrom.getLine
                    isPointer = False

                    for (Opt(featExpr, e) <- pointers) extractAST(e, ctx and featExpr);

                    if (isDeclarationStatement.isSatisfiable() && isNotTypedefSpecifier.isSatisfiable()) {
                        analyisContext.addObjectName(analyisContext.applyScope(currentDeclarator, currentFunction, currentFile), ctx)

                        // consider pointers only on non-function declarators and function parameters
                        if (isPointer.isSatisfiable() && isFunctionDeclarator.isContradiction()) {
                            val scopeObjectName = analyisContext.addObjectName(analyisContext.applyScope(ObjectNameOperator.PointerDereference.toString + ObjectNameOperator.parenthesize(id.name), currentFunction, currentFile), ctx)
                        }
                    }
                    for (Opt(featExpr, e) <- extensions) extractAST(e, ctx and featExpr);

                    Some(currentDeclarator)

                }

                case NestedNamedDeclarator(pointers: List[Opt[Pointer]], nestedDecl: Declarator, extensions: List[Opt[DeclaratorExtension]], attr: List[Opt[AttributeSpecifier]]) => {
                    isPointer = False
                    for (Opt(featExpr, e) <- pointers) extractAST(e, ctx and featExpr)

                    val declStr = extractAST(nestedDecl, ctx)
                    currentDeclarator = declStr.getOrElse("DECLARATOR_NOT_AVAILABLE")

                    // for (Opt(featExpr, e) <- extensions) extractObjectNames(e, ctx and featExpr)
                    // for (Opt(featExpr, e) <- attr) extractObjectNames(e, ctx and featExpr)
                    declStr
                }
                case AtomicAbstractDeclarator(pointers: List[Opt[Pointer]], extensions: List[Opt[DeclaratorAbstrExtension]]) => {
                    isPointer = False
                    for (Opt(featExpr, e) <- pointers) extractAST(e, ctx and featExpr)
                    for (Opt(featExpr, e) <- extensions) extractAST(e, ctx and featExpr)
                    None
                }

                case NestedAbstractDeclarator(pointers: List[Opt[Pointer]], nestedDecl: AbstractDeclarator, extensions: List[Opt[DeclaratorAbstrExtension]], attr: List[Opt[AttributeSpecifier]]) => {
                    isPointer = False
                    for (Opt(featExpr, e) <- pointers) extractAST(e, ctx and featExpr)
                    val declStr = extractAST(nestedDecl, ctx)

                    // for (Opt(featExpr, e) <- extensions) extractObjectNames(e, ctx and featExpr)
                    // for (Opt(featExpr, e) <- attr) extractObjectNames(e, ctx and featExpr)
                    declStr
                }
            }

        }

        extractAST(ast)
        analyisContext.addFuncMappings(functionDefs, functionDefParameters, functionDefReturns, functionCalls, functionCallParameters)
        analyisContext
    }

    private def functionKind(specifiers: List[Opt[Specifier]]): String = {
        if (specifiers.map(_.entry).contains(InlineSpecifier())) "function-inline"
        else if (specifiers.map(_.entry).contains(StaticSpecifier())) "function-static"
        else "function"
    }

    // extract program assignments (function call parameters and return values)
    private def extractIntraproceduralAssignments(context: CPointerAnalysisContext): CPointerAnalysisContext = {
        val fCallContext = addAssignmentsFromFunctionCallParameters(context)
        val rValueContext = addAssignmentsFromFunctionCallReturnValues(fCallContext)
        rValueContext
    }
}

