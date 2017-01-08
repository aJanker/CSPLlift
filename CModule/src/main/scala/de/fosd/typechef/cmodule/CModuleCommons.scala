package de.fosd.typechef.cmodule

import java.io.File

import de.fosd.typechef.conditional.{Conditional, One, Opt}
import de.fosd.typechef.featureexpr.bdd.BDDNoFeatureModel
import de.fosd.typechef.featureexpr.{FeatureExpr, FeatureModel}
import de.fosd.typechef.parser.c.Expr
import de.fosd.typechef.typesystem.modulelinking.CLinkingExtractor
import de.fosd.typechef.typesystem.{CPointer, CTypeSystemFrontend}

/**
  * Workaround for disallowed multi-inheritence.
  */
object ModuleCommons extends CModuleCommons

/**
  * Collection of some utility functions and names.
  */
trait CModuleCommons extends CModuleNamings {

    /**
      * Gets the file object to the module directory.
      */
    def getModuleDir: File = new File(MODULE_DIR_NAME)

    /**
      * Checks if the default module directory is available and if not tries to generate this directory.
      */
    def checkIfModuleDirExistsElseCreate: Boolean = {
        val moduleDir = getModuleDir
        (moduleDir.isDirectory && moduleDir.canWrite) || (moduleDir.mkdirs() && moduleDir.canWrite)
    }

    /**
      * Groups a opt list into distinct feature groups.
      *
      * e.g. List(Opt(A), Opt(!A), Opt(A), Opt(!A)) => List(List(Opt(A), Opt(!A)), List(Opt(A), Opt(!A))
      */
    private[cmodule] def groupOptListVAware[T](l: Iterable[Opt[T]], fm: FeatureModel = BDDNoFeatureModel): List[List[Opt[T]]] = {
        if (l.isEmpty) return List()

        var group: List[Opt[T]] = List(l.head)
        var groups: List[List[Opt[T]]] = List()
        var combinedCondition: FeatureExpr = l.head.condition

        for (curr <- l.drop(1)) {
            val groupCondition = combinedCondition.and(curr.condition)

            if (groupCondition.isContradiction(fm)) group = curr :: group
            else if (combinedCondition.implies(curr.condition).isTautology(fm)) {
                groups = group.reverse :: groups
                group = List(curr)
            }
            else group = curr :: group

            // add current feature expression as it might influence the addition of selem for
            // the remaining elements of l
            combinedCondition = combinedCondition.or(curr.condition)
        }

        (group.reverse :: groups).reverse
    }

    /**
      * Looks up in the TypeSystem if a expr is a pointer.
      */
    private[cmodule] def exprIsPointer(expr: Expr, ts: CTypeSystemFrontend with CLinkingExtractor): Conditional[Boolean] = {
        val exprType = ts.lookupExprType(expr)
        if (exprType == null) One(false)
        else exprType.map(_.atype match {
            case CPointer(_) => true
            case _ => false
        })
    }

    /**
      * Looks up in the TypeSystem if a expr may be a pointer.
      */
    private[cmodule] def exprMayBePointer(expr: Expr, ts: CTypeSystemFrontend with CLinkingExtractor): Boolean =
        exprIsPointer(expr, ts).exists(_ == true)

    private[cmodule] def removeFilePrefix(str: String): String =
        if (str.startsWith(TYPECHEF_FILE_PREFIX)) str.substring(TYPECHEF_FILE_PREFIX.length)
        else str

    private[cmodule] def replaceExtensionWithC(s: String): String =
        if (s.endsWith(AST_FILE_EXT)) s.substring(0, s.lastIndexOf(AST_FILE_EXT)) + C_FILE_EXT
        else if (s.endsWith(PI_FILE_EXT)) s.substring(0, s.lastIndexOf(PI_FILE_EXT)) + C_FILE_EXT
        else s
}
