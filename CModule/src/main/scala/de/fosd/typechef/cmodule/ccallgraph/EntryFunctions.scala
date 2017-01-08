package de.fosd.typechef.cmodule.ccallgraph

import de.fosd.typechef.cmodule.CModuleCommons
import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.featureexpr.FeatureExprFactory
import de.fosd.typechef.parser.c.FunctionDef

trait EntryFunctions extends CModuleCommons {

    /**
      * Set of possible entry function names.
      */
    private[ccallgraph] var ENTRY_FUNCTION_NAMES = Set(Opt(FeatureExprFactory.True, "main"))

    /**
      * Adds a function name to the set of possible entry functions.
      */
    final def addEntryFunctionName(name: Opt[String]): Unit = ENTRY_FUNCTION_NAMES += name

    /**
      * Removes a function name from the set of possible entry functions.
      */
    final def removeEntryFunctionName(name: Opt[String]): Unit = ENTRY_FUNCTION_NAMES -= name

    /**
      * Retrieves a list of all potential entry points of a given program.
      */
    def getEntryFunctions: List[Opt[FunctionDef]]

    /**
      * Retrieves a list of all potential entry points of a given program.
      */
    def getEntryFunctionsNames: List[Opt[String]] = ENTRY_FUNCTION_NAMES.toList

}
