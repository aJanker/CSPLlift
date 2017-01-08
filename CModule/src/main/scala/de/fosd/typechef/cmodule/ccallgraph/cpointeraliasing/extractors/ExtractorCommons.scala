package de.fosd.typechef.cmodule.ccallgraph.cpointeraliasing.extractors

import de.fosd.typechef.cmodule.{CModule, CModuleCommons}
import de.fosd.typechef.conditional.{One, Opt}
import de.fosd.typechef.parser.c._
import de.fosd.typechef.typesystem.{CUnknown, ConditionalTypeMap}
import org.slf4j.{Logger, LoggerFactory}

trait ExtractorCommons extends CModuleCommons {

    private lazy val logger: Logger = LoggerFactory.getLogger(getClass)

    private[extractors] def groupInitializerWithFields(lcurlyInitializer: LcurlyInitializer, cField: Opt[ConditionalTypeMap]) = {
        // In the ConditionalTypeMap all fields are unordered. To match correctly, we must sort them according to their lexical ordering in the source code.
        // However some fields are only availabe in certain configs, otherwise they are "Unknown Type"
        def sortByPosition(key1: String, key2: String): Boolean = {
            val fieldASTElement1 = cField.entry.getAstOrElse(key1, null)
            val filedASTElement2 = cField.entry.getAstOrElse(key2, null)

            (fieldASTElement1, filedASTElement2) match {
                case (One(null), One(null)) => false
                case (One(null), _) => true
                case (_, One(null)) => false
                case _ =>
                    fieldASTElement1.exists(field1 => filedASTElement2.exists(field2 =>
                        if (field1 != null && field2 != null && field1.hasPosition && field2.hasPosition) field1.getPositionFrom.<(field2.getPositionFrom)
                        else false)) // assumes that variability can not alter the ordering
            }
        }

        val sortedFieldKeys = cField.entry.keys.toList.sortWith(sortByPosition)
        val allTypedFields = sortedFieldKeys.flatMap(key =>
            cField.entry.apply(key).toOptList.flatMap(ct =>
                ct.entry.atype match {
                    case _: CUnknown => None
                    case _ => Some(ct.copy(entry = key))
                }))

        val allFields = groupOptListVAware(allTypedFields)
        val allInits = groupOptListVAware(lcurlyInitializer.inits)

        if (allInits.size != allFields.size)
            logger.debug("Fields and Inits do not match for: " + lcurlyInitializer)

        allFields.zip(allInits)
    }

    var callCount = 0

    private[extractors] def isPointerRelatedExpr(source: Expr, env: CModule): Boolean = {
        val astEnv = env.getASTEnv(source)
        val ts = env.getTypeSystem(source)
        lazy val pointerOperator = env.filterAllASTElems[PointerDerefExpr](source, astEnv).nonEmpty || env.filterAllASTElems[PointerPostfixSuffix](source, astEnv).nonEmpty || env.filterAllASTElems[PointerCreationExpr](source, astEnv).nonEmpty
        exprMayBePointer(source, ts) || pointerOperator
    }

}
