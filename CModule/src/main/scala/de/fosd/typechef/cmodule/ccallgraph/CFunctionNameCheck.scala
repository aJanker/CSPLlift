package de.fosd.typechef.cmodule.ccallgraph

import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.parser.c.Id
import de.fosd.typechef.typesystem.linker.SystemLinker

/**
  * Collection of various C function name checks.
  */
trait CFunctionNameCheck {

    private lazy val unsupportedNames: Set[String] = Set("mbedtls_free", "mbedtls_calloc", "mbedtls_ripemd160_process")
    private lazy val unsupportPrefixes: Set[String] = Set("__builtin")

    def isIgnoredName(functionName: Id): Boolean = isIgnoredName(functionName.name)
    def isIgnoredName(functionName: Opt[String]): Boolean = isIgnoredName(functionName.entry)
    def isIgnoredName(functionName: String): Boolean = unsupportPrefixes.exists(functionName.startsWith) || unsupportedNames.contains(functionName)

    def isSystemFunctionName(i: Id): Boolean = isSystemFunctionName(i.name)
    def isSystemFunctionName(functionName: Opt[String]): Boolean = isSystemFunctionName(functionName.entry)
    def isSystemFunctionName(functionName: String): Boolean = SystemLinker.allLibs.contains(functionName)

    def isIgnoredSystemOrBuiltinFunctionName(functionName: Id): Boolean = isIgnoredSystemOrBuiltinFunctionName(functionName.name)
    def isIgnoredSystemOrBuiltinFunctionName(functionName: Opt[String]): Boolean = isIgnoredSystemOrBuiltinFunctionName(functionName.entry)
    def isIgnoredSystemOrBuiltinFunctionName(functionName: String): Boolean = isSystemFunctionName(functionName) || isIgnoredName(functionName)
}
