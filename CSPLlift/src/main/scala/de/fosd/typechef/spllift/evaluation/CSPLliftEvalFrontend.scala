package de.fosd.typechef.spllift.evaluation

import de.fosd.typechef.crewrite.ProductDerivation
import de.fosd.typechef.featureexpr.FeatureModel
import de.fosd.typechef.featureexpr.bdd.BDDFeatureModel
import de.fosd.typechef.parser.c.TranslationUnit
import de.fosd.typechef.spllift.options.CSPLliftOptions

class CSPLliftEvalFrontend(startTunit: TranslationUnit, fm: FeatureModel = BDDFeatureModel.empty) {

    def checkAgainstSampling(opt: CSPLliftOptions) = {
        val product = ProductDerivation.deriveProduct(null, null)
    }

    def checkAgainstErrorConfiguration(opt: CSPLliftOptions) = {

    }

}
