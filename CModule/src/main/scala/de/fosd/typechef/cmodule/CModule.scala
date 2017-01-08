package de.fosd.typechef.cmodule

import de.fosd.typechef.cmodule.ccallgraph._
import de.fosd.typechef.cmodule.ccallgraph.clinking.CModuleLinkMap
import de.fosd.typechef.cmodule.cmodulecache.CModuleCache
import de.fosd.typechef.customization.conditional.SimpleConfiguration
import de.fosd.typechef.featureexpr.bdd.BDDFeatureModel
import de.fosd.typechef.featureexpr.{FeatureExpr, FeatureModel}
import de.fosd.typechef.parser.c.{AST, FunctionDef, TranslationUnit}

/**
  * Implementation of a module interface which transparently resolves linked source files,
  * provides on-demand required elements for analysis strategies such as a call-graph, type-system etc.
  */
class CModule(fm: FeatureModel = BDDFeatureModel.empty,
              resolveLinkedFiles: Boolean = true,
              rewriteTasks: List[((TranslationUnit, FeatureModel) => TranslationUnit)] = List(),
              linkingMapDir: Option[String] = None,
              functionAliasing: Boolean = true,
              productConfiguration: Option[SimpleConfiguration] = None
             ) extends CModuleCache(fm, resolveLinkedFiles, rewriteTasks, productConfiguration) {

    private val cLinkingMap = linkingMapDir match {
        case Some(path) => Some(new CModuleLinkMap(path))
        case _ => None
    }

    private val callGraph = new CAliasingCallGraph(this, functionAliasing)

    /**
      * Retrieves the call graph of current module.
      */
    override def getCallGraph: CCallGraph = callGraph

    /**
      * Retrieves the presence condition of an ast node.
      */
    override def getCondition(node: AST): FeatureExpr = getASTEnv(node).featureExpr(node)

    /**
      * Extract the filename of an ast node.
      */
    override def getFilename(node: AST, default: String = NO_FILENAME): String = removeFilePrefix(node.getFile.getOrElse(default))

    /**
      * Retrieves the current feature model.
      */
    override def getFeatureModel: FeatureModel = fm

    /**
      * Retrieves the function name of the surrounding function or the default value for
      * an globally defined name.
      */
    override def getFunctionName(node: AST): String =
        findPriorASTElem[FunctionDef](node, getASTEnv(node)) match {
            case Some(fDef) => fDef.getName
            case _ => GLOBAL_DEFINITION_NAME
        }

    /**
      * Retrieves the linking map.
      */
    override def getLinkingMap: Option[CModuleLinkMap] = cLinkingMap

    /**
      * Gets the environment for a product.
      */
    override def getProductModule(config: SimpleConfiguration): CModule = {
        val productEnv = new CModule(fm, resolveLinkedFiles = false, rewriteTasks, linkingMapDir, functionAliasing, Some(config))
        getAllKnownTUnits.foreach(productEnv.addTUnit)

        if (functionAliasing) productEnv.solveAliasing()

        productEnv
    }

    /**
      * Computes the aliasing equivalence relationships.
      */
    def solveAliasing() : Boolean =
        if (functionAliasing) callGraph.solveFunctionPointerPointsToRelation() else false
}
