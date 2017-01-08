package de.fosd.typechef.cmodule

import java.io.File

import de.fosd.typechef.cmodule.ccallgraph.CCallGraph
import de.fosd.typechef.cmodule.ccallgraph.clinking.CModuleLinkMap
import de.fosd.typechef.customization.conditional.SimpleConfiguration
import de.fosd.typechef.featureexpr.{FeatureExpr, FeatureModel}
import de.fosd.typechef.parser.c._
import de.fosd.typechef.typesystem.CTypeSystemFrontend
import de.fosd.typechef.typesystem.modulelinking.CLinkingExtractor

trait CModuleOperations extends CModuleNamings with ASTNavigation with ConditionalNavigation with EnforceTreeHelper {

    /**
      * Retrieves the call graph of current module.
      */
    def getCallGraph: CCallGraph

    /**
      * Retrieves the current feature model.
      */
    def getFeatureModel: FeatureModel

    /**
      * Retrieves the linking map.
      */
    def getLinkingMap: Option[CModuleLinkMap]

    /**
      * Retrieves the corresponding AST environment for a given AST node.
      */
    def getASTEnv(node: AST): ASTEnv

    /**
      * Retrieves the type system environment for a given AST node.
      */
    def getTypeSystem(node: AST): CTypeSystemFrontend with CLinkingExtractor

    /**
      * Retrieves the surrounding translation unit of a given AST node.
      */
    def getTranslationUnit(node: AST): TranslationUnit

    /**
      * Retrieves the untouched version of a translation unit with no performed rewrite operations.
      */
    def getUntouchedVersion(tunit: TranslationUnit): TranslationUnit

    /**
      * Retrieves the presence condition of an ast node.
      */
    def getCondition(node: AST): FeatureExpr

    /**
      * Retrieves the function name of the surrounding function or the default value for
      * an globally defined name.
      */
    def getFunctionName(node: AST): String

    /**
      * Extract the filename of an ast node.
      */
    def getFilename(ast: AST, default: String = NO_FILENAME): String

    /**
      * Gets the environment for a product.
      */
    def getProductModule(config: SimpleConfiguration): CModule
}

trait CModuleNamings {
    lazy val GLOBAL_DEFINITION_NAME: String = "$GLOBAL§"

    lazy val NO_FILENAME: String = "$NOFILENAME§"

    lazy val PI_FILE_EXT: String = ".pi"

    lazy val AST_FILE_EXT: String = ".ast"

    lazy val C_FILE_EXT: String = ".c"

    lazy val TYPECHEF_FILE_PREFIX: String = "file "

    /**
      * Name of default storage directory for TypeChef Module Files
      */
    lazy val MODULE_DIR_NAME: String = "typechef_modules"

    /**
      * Filename of the global linking interface.
      */
    lazy val MODULE_INTERFACE_FILENAME: String = "CModules"

    /**
      * Default fileextension of the var linking map.
      */
    lazy val VAR_LINKMAP_FILE_EXTENSION: String = "_var.link"

    /**
      * Default fileextension of the function def linking map.
      */
    lazy val FDEF_LINKMAP_FILE_EXTENSION: String = "_fdef.link"

    private[cmodule] def _extractFilename(str: String, default: String = NO_FILENAME): String = {
        if (!str.equalsIgnoreCase(default)) {
            val prefixFreeFName =
                if (str.startsWith(TYPECHEF_FILE_PREFIX)) str.substring(TYPECHEF_FILE_PREFIX.length)
                else str

            val lastPathSeparator = prefixFreeFName.lastIndexOf(File.separatorChar)
            val fName =
                if (lastPathSeparator != -1) prefixFreeFName.substring(lastPathSeparator + 1)
                else prefixFreeFName

            val extensionIndex = fName.lastIndexOf('.')
            if (extensionIndex != -1) fName.substring(0, extensionIndex)
            else fName
        }
        else default
    }
}