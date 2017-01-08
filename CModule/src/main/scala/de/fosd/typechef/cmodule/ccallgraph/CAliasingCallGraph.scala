package de.fosd.typechef.cmodule.ccallgraph

import de.fosd.typechef.cmodule.CModule
import de.fosd.typechef.cmodule.ccallgraph.cpointeraliasing.CFunctionAliasingFactory
import de.fosd.typechef.cmodule.ccallgraph.cpointeraliasing.extractors.ObjectNameExtractor
import de.fosd.typechef.cmodule.ccallgraph.cpointeraliasing.types.ObjectName
import de.fosd.typechef.conditional.{Conditional, One, Opt}
import de.fosd.typechef.error.Position
import de.fosd.typechef.parser.c._
import org.slf4j.{Logger, LoggerFactory}

/**
  * Call graph implementation for variational C code.
  */
class CAliasingCallGraph(cModule: CModule, functionAliasing: Boolean = true) extends CCallGraph {

    private lazy val logger: Logger = LoggerFactory.getLogger(getClass)

    private val cFunctionAliasing =
        if (functionAliasing) Some(CFunctionAliasingFactory.getUpdateableInstance(cModule, cModule.getFeatureModel))
        else None

    /**
      * Retrieves a list of all potential entry points of a given program.
      */
    override def getEntryFunctions: List[Opt[FunctionDef]] = getEntryFunctionsNames.flatMap(name => {
        val allStartFDefs = cModule.getFlatNonEmptyFunctionDef(name.entry)
        val satFDefs = allStartFDefs.filter(_.condition.and(name.condition).isSatisfiable())
        satFDefs
    })

    /**
      * Checks whether a ast node is a function call or not
      */
    override def isFunctionCall(call: AST): Boolean =
        cModule.filterAllASTElems[PostfixExpr](call).exists {
            case PostfixExpr(_, FunctionCall(_)) => true
            case _ => false
        }

    /**
      * Checks if a input function call is a function call to a system function.
      */
    override def isSystemFunctionCall(call: AST): Boolean =
        cModule.filterAllASTElems[PostfixExpr](call).exists {
            case PostfixExpr(i: Id, FunctionCall(_)) => isSystemFunctionName(i)
            case _ => false
        }

    /**
      * Checks whether a ast node is a function pointer call or not
      */
    override def isFunctionPointerCall(call: AST): Conditional[Boolean] =
        call match {
            case PostfixExpr(i: Id, FunctionCall(_)) if functionAliasing => nameIsPointer(i).map(b => b && !isIgnoredSystemOrBuiltinFunctionName(i))
            case PostfixExpr(_, FunctionCall(_)) if functionAliasing => One(true)
            case _ => One(false)
        }

    /**
      * Extracts the function call names.
      */
    override def getFCallNames(call: AST): List[Opt[String]] =
        cModule.filterAllASTElems[PostfixExpr](call) flatMap {
            case PostfixExpr(i: Id, FunctionCall(_)) => Some(Opt(cModule.getCondition(i), i.name))
            case _ => None
        }

    /**
      * Extracts the function call pointer names.
      */
    override def getFCallPointerNames(call: AST): List[Opt[ObjectName]] =
        cModule.filterAllASTElems[PostfixExpr](call) flatMap {
            case PostfixExpr(i: Id, FunctionCall(_)) => None
            case PostfixExpr(pointer, FunctionCall(_)) if functionAliasing => ObjectNameExtractor.getTopObjectNames(pointer, cModule)
            case _ => None
        }

    /**
      * Retrieves if all possible target destinations of a function call are known in the currently loaded scope.
      */
    override def isTargetKnown(call: AST): Boolean =
        getFCallNames(call).forall(isTargetKnown)

    /**
      * Retrieves if all possible target destinations of a function call are known in the currently loaded scope.
      */
    override def isTargetKnown(call: Opt[String]): Boolean = cModule.getFlatNonEmptyFunctionDef(call.entry).exists {
        case Opt(condition, fDef) => condition.and(call.condition).isSatisfiable(cModule.getFeatureModel)
    }

    /**
      * Calculates the function pointer aliasing equivalence classes.
      */
    def solveFunctionPointerPointsToRelation(): Boolean = if (cFunctionAliasing.isDefined) cFunctionAliasing.get.solve() else false

    /**
      * Retrieves all possible target destinations of a function call.
      *
      * Note the return type is ConditionalPointsToFDef. The inner opt node, is the original opt node from the
      * surrounding TranslationUnit while the outer Opt node encodes the individual pointsTo condition.
      *
      * The optional arguments @param noFunctionPointer disable the resolving of function pointer while the
      * argument @param noUnresolvedFileLoading disables the internal mechanism of loading additional files into the enviornment.
      */
    override def getCalleesOfCallAt(call: AST, withAliasing: Boolean = true): Set[PointsToFDef] = {
        def r: (Iterable[PointsToFDef]) => Set[PointsToFDef] = PointsToFDef.reduceFDefs

        lazy val callExpr = getTopCallExpr(call)
        lazy val isFunctionPointer: Boolean = if (callExpr.isDefined) isFunctionPointerCall(callExpr.get).exists(_ == true) else false // TODO Currently ignores the corner case that a function call maybe a classic and pointer function call at the same time

        if (!isFunctionCall(call)) return Set()

        // Is functionpointer call but we either did not calculate function pointers or function pointers are disabled.
        if ((!functionAliasing || !withAliasing) && isFunctionPointer) return Set()

        if (isFunctionPointer) r(getCalleesOfPointerCall(call, withAliasing))
        else r(getCalleesOfClassicCall(call, withAliasing))
    }

    private def getCalleesOfClassicCall(call: AST, loadLinkedFiles: Boolean): Set[PointsToFDef] = {
        val names = getFCallNames(call)
        val callFile = removeFilePrefix(call.getPositionFrom.getFile)

        if (names.forall(isIgnoredSystemOrBuiltinFunctionName)) return Set() // ignore system function

        val localCallees = names.flatMap(findCalleesInFile(_, callFile))

        // At this point we assume an internal function may not be imported in any case.
        if (localCallees.nonEmpty) return localCallees.toSet
        if (cModule.getLinkingMap.isEmpty) return Set()

        val externalCallees = findLinkedCalleesOfCall(call, loadLinkedFiles)

        externalCallees.toSet
    }

    private def getCalleesOfPointerCall(call: AST, loadLinkedFiles: Boolean): Set[PointsToFDef] = {
        lazy val pointer = cModule.filterAllASTElems[PostfixExpr](call).find {
            case PostfixExpr(_, FunctionCall(_)) => true
            case _ => false
        }

        if (cFunctionAliasing.isEmpty || pointer.isEmpty) return Set()

        val pointsToDest = cFunctionAliasing.get.getPossibleDestinations(pointer.get, cModule)

        if (pointsToDest.isEmpty)
            logger.debug("Did not find function pinter destination for call:\t" + call)

        // No need to look for "external" definitions, since our function pointer analysis strategy is already aware of
        // the location of the target pointer, we can skip the step of querying the interface.
        pointsToDest.flatMap(pts => findCalleesInFile(pts.copy(entry = pts.entry.functionName), pts.entry.fileName)).toSet
    }

    /**
      * Looks up callees of an call which are externally defined.
      */
    private def findLinkedCalleesOfCall(call: AST, loadLinkedFiles: Boolean = true): List[PointsToFDef] = {
        val names = getFCallNames(call)

        val ts = cModule.getTypeSystem(call)
        val linkingMap = cModule.getLinkingMap.get
        val localMap = ts.getFDefLinkingMap

        val imports = names.flatMap(name => {
            val importFDef = localMap.getImportsByName(name.entry)

            if (importFDef.isEmpty) None
            else importFDef.get.flatMap(_._2.flatMap(sig => {
                val condition = sig.classicSignature.fexpr
                sig.declNames.toList.map(decl => Opt(condition, decl))
            }))
        })

        val positions = imports.flatMap(imp =>
            linkingMap.findFDefsExportLocations(imp.entry).
              filter(_.condition.and(imp.condition).isSatisfiable()).
              map(_.and(imp.condition)))


        val allFilesLoaded = positions.forall(pos => cModule.isFileKnown(removeFilePrefix(pos.entry.getFile)))

        if (allFilesLoaded) findCalleeByPosition(positions, names)
        else if (loadLinkedFiles) {
            val files = positions.map(pos => removeFilePrefix(pos.entry.getFile)).distinct
            files.foreach(cModule.addFile)
            findCalleeByPosition(positions, names)
        } else List()
    }

    private def findCalleeByPosition(positions: List[Opt[Position]], fDefNames: List[Opt[String]]): List[PointsToFDef] = positions.flatMap(pos => {
        val file = removeFilePrefix(pos.entry.getFile)

        fDefNames.flatMap(findCalleesInFile(_, file).flatMap(x => {
            val pointsToFDef = x.and(pos.condition)
            if (pointsToFDef.pointsToCondition.isSatisfiable(cModule.getFeatureModel)) Some(pointsToFDef)
            else None
        }))
    })


    private def findCalleesInFile(callName: Opt[String], calleeFile: String): List[PointsToFDef] = {
        val fDefsWithName = cModule.getNonEmptyFunctionDef(callName.entry).getOrElse(calleeFile, List())
        fDefsWithName.flatMap {
            case o@Opt(condition, functionDef) if callName.condition.and(condition).isSatisfiable(cModule.getFeatureModel) =>
                Some(PointsToFDef.apply(callName.condition.and(condition), o))
            case _ => None
        }
    }

    /**
      * Looks up in the TypeSystem if a name may be a pointer.
      */
    private def nameIsPointer(i: Id): Conditional[Boolean] = exprIsPointer(i, cModule.getTypeSystem(i))

    private def getTopCallExpr(call: AST): Option[AST] = cModule.filterASTElems[FunctionCall](call).headOption match {
        case None => None
        case Some(functionCall) => Some(cModule.parentAST(functionCall, cModule.getASTEnv(functionCall)))
    }

}
