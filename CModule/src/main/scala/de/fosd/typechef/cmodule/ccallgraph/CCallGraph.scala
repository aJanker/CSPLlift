package de.fosd.typechef.cmodule.ccallgraph

import de.fosd.typechef.cmodule.ccallgraph.cpointeraliasing.types.ObjectName
import de.fosd.typechef.conditional.{Conditional, Opt}
import de.fosd.typechef.parser.c._


trait CCallGraph extends EntryFunctions with CFunctionNameCheck {

    /**
      * Checks whether a ast node is a function call or not
      */
    def isFunctionCall(call: AST): Boolean

    /**
      * Checks if a input function call is a function call to a system function.
      */
    def isSystemFunctionCall(call: AST): Boolean

    /**
      * Checks whether a ast node is a function pointer call or not
      */
    def isFunctionPointerCall(call: AST): Conditional[Boolean]

    /**
      * Extracts the function call names.
      */
    def getFCallNames(call: AST): List[Opt[String]]

    /**
      * Extracts the function call pointer names.
      */
    def getFCallPointerNames(call: AST): List[Opt[ObjectName]]

    /**
      * Retrieves if all possible target destinations of a function call are known in the currently loaded scope.
      */
    def isTargetKnown(call: AST): Boolean

    /**
      * Retrieves if all possible target destinations of a function call are known in the currently loaded scope.
      */
    def isTargetKnown(call: Opt[String]): Boolean

    /**
      * Retrieves all possible target destinations of a function call.
      *
      * Note the return type is ConditionalPointsToFDef. The inner opt node, is the original opt node from the
      * surrounding TranslationUnit while the outer Opt node encodes the individual pointsTo condition.
      *
      * The optional arguments @param noFunctionPointer disable the resolving of function pointer while the
      * argument @param noUnresolvedFileLoading disables the internal mechanism of loading additional files into the enviornment.
      */
    def getCalleesOfCallAt(call: AST, aliasing: Boolean = true) : Set[PointsToFDef]
}



