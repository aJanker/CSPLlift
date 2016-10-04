package de.fosd.typechef.customization.crewrite

import de.fosd.typechef.error.WithPosition

trait CopyPosition {
    def copyPosition(source: Product, target: Product) {
        assert(source.getClass == target.getClass, "cloned tree should match exactly the original, typewise")

        source match {
            case position: WithPosition => target.asInstanceOf[WithPosition].range = position.range
            case _ =>
        }

        assert(source.productArity == target.productArity, "cloned tree should match exactly the original")
        for ((c1, c2) <- source.productIterator.zip(target.productIterator)) {
            // The following assert will cause failure, when some replacements in the TranslationUnit took place using kiama
            if ((c1.getClass == c2.getClass) && c1.isInstanceOf[Product] && c2.isInstanceOf[Product])
                copyPosition(c1.asInstanceOf[Product], c2.asInstanceOf[Product])
        }
    }
}
