package de.fosd.typechef.cmodule.ccallgraph.cpointeraliasing

import de.fosd.typechef.cmodule.CModule
import de.fosd.typechef.cmodule.ccallgraph.cpointeraliasing.extractors.DefaultExtractor
import de.fosd.typechef.cmodule.cmodulecache.CModuleCache
import de.fosd.typechef.featureexpr.FeatureModel
import de.fosd.typechef.featureexpr.bdd.BDDNoFeatureModel
import de.fosd.typechef.parser.c._
import org.slf4j.{Logger, LoggerFactory}

/**
  * Factory for generating the function aliasing mapping.
  */
object CFunctionAliasingFactory {

    private lazy val logger: Logger = LoggerFactory.getLogger(getClass)

    /**
      * Retrieves a (Function-) PointerEquivalence classes for the input translation unit
      * and externally linked variables, but not functions.
      */
    def getStaticInstance(tunit: TranslationUnit, env: CModule, fm: FeatureModel = BDDNoFeatureModel): CPointerAliasing = {
        val (objectNames, assignments) = DefaultExtractor.extractAll(tunit, env)

        new CPointerAliasing(objectNames, assignments, fm)
    }

    /**
      * Retrieves a (Function-) PointerEquivalence classes for the input translation unit
      * and externally linked variables, but not functions which adaptes to occuring enviornment changes.
      */
    def getUpdateableInstance(env: CModuleCache, fm: FeatureModel = BDDNoFeatureModel): CPointerAliasing = {
        val facade = new CPointerAliasing(Set(), Set(), fm)
        env.register(facade)
        facade
    }

    /**
      * Retrieves a (Function-) PointerEquivalence classes for the input serialized objectnames and assignments.
      */
    def getSerializedInstance(file: String, fm: FeatureModel = BDDNoFeatureModel): CPointerAliasing = new CPointerAliasing(file, fm)
}
