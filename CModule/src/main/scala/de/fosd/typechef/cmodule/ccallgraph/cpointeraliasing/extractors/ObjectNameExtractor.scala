package de.fosd.typechef.cmodule.ccallgraph.cpointeraliasing.extractors

import de.fosd.typechef.cmodule.ccallgraph.cpointeraliasing.types._
import de.fosd.typechef.cmodule.{CModule, CModuleCommons}
import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.parser.c._
import de.fosd.typechef.typesystem._
import org.slf4j.{Logger, LoggerFactory}

object ObjectNameExtractor extends CModuleCommons with ExtractorCommons {

    private lazy val logger: Logger = LoggerFactory.getLogger(getClass)

    /**
      * Extracts object names from a input ast node.
      *
      * @param node      the ast node
      * @param env       the module enviornment
      * @param recursive if true, nested objectnames, such as objectnames in compoundstatements in function definitions
      *                  are extracted as well
      * @return a list with all extract object names
      */
    def get(node: Any, env: CModule, recursive: Boolean = true): List[Opt[ObjectName]] = {
        val objectNames = node match {
            case _: LabelStatement | _: TypeName | _: Specifier => List() // Pointers can not be a part of specifiers.
            case i: Id => // add basic name
                val decls = env.getTypeSystem(i).getUseDeclMap.get(i)
                val ids = if (decls == null) List(i) else decls
                val scope = None
                // TODO Clean name scoping with TypeSystem
                val condition = env.getCondition(i)

                ids.map(id => Opt(condition, PlainObjectName(id.name, Some(env.getFunctionName(id)), Some(env.getFilename(id)), scope)))

            case a: AtomicNamedDeclarator =>
                val basicName = get(a.getId, env)
                val pointerObjectNames =
                    if (a.pointers.nonEmpty)
                        a.pointers.zipWithIndex.flatMap {
                            case (_, i) => basicName.flatMap { _ =>
                                (0 until (i + 1)).foldLeft(basicName) { case (l, _) => Opt(l.head.condition, PointerObjectName(l.head.entry, PointerDerefOperator())) :: l }
                            }
                        }
                    else List()

                pointerObjectNames ::: basicName

            case f: FunctionDef => // add function names
                val objectName = PlainObjectName(f.getName, Some(env.GLOBAL_DEFINITION_NAME), Some(env.getFilename(f)))
                val condition = env.getCondition(f)
                val fDefObjectName = Opt(condition, objectName)

                val recursiveNames =
                    if (recursive) get(f.stmt, env, recursive)
                    else List()

                val parameters = get(List(f.declarator.extensions, f.oldStyleParameters), env, recursive)

                fDefObjectName :: recursiveNames ::: parameters

            case p: PointerDerefExpr => getObjectNameWithOperator(p.castExpr, PointerDerefOperator(), env, recursive)
            case p: PointerCreationExpr => getObjectNameWithOperator(p.castExpr, PointerCreationOperator(), env, recursive)
            case l: LcurlyInitializer if env.findPriorASTElem[InitDeclarator](l, env.getASTEnv(l)).isDefined => List()
                val astEnv = env.getASTEnv(l)
                val priorDeclaration = env.findPriorASTElem[Declaration](l, astEnv).get
                val baseObjectNames = getTopObjectNames(priorDeclaration.init.map(_.entry.declarator.getId), env)
                val ts = env.getTypeSystem(l)

                val tsEnv =
                    try {
                        ts.lookupEnv(l)
                    } catch {
                        case e: Exception =>
                            // No typechecked env available - silently ignore this special corner case
                            logger.debug("No typechecked env available for: " + priorDeclaration)
                            return List()
                    }

                lazy val typeDefTypeSpecifiers = env.filterASTElems[TypeDefTypeSpecifier](priorDeclaration)
                lazy val structOrUnionSpecifiers = env.filterASTElems[StructOrUnionSpecifier](priorDeclaration)

                if (tsEnv == null) {
                    logger.debug("No typechecked env available for: " + priorDeclaration)
                    List()
                } else if (typeDefTypeSpecifiers.nonEmpty) {
                    typeDefTypeSpecifiers.flatMap(spec => {
                        val cTypes = tsEnv.typedefEnv.apply(spec.name.name)
                        cTypes.toOptList.flatMap {
                            _.entry match {
                                case CType(s: CAnonymousStruct, _, _, _) =>
                                    logger.debug("Missed CAnonymousStruct: " + s)
                                    List()
                                case CType(s: CStruct, _, _, _) =>
                                    val fields = tsEnv.structEnv.getFields(s.s, s.isUnion).toOptList
                                    matchStructFieldWithCurlyInits(baseObjectNames, l, fields)
                                case x =>
                                    logger.debug("Missed curly type of: " + x)
                                    List()
                            }
                        }
                    })
                } else if (structOrUnionSpecifiers.nonEmpty) {
                    structOrUnionSpecifiers.flatMap {
                        case spec if spec.id.isDefined =>
                            if (tsEnv.structEnv == null) {
                                logger.info("No struct env available for:\t" + spec.id)
                                None
                            } else {
                                val fields = tsEnv.structEnv.getFields(spec.id.get.name, spec.isUnion)
                                if (fields == null) None
                                else matchStructFieldWithCurlyInits(baseObjectNames, l, fields.toOptList)
                            }
                        case _ => None
                    }

                } else List()

            case p@PostfixExpr(pe, ps: PointerPostfixSuffix) =>
                val peNames = get(pe, env, recursive)
                val psNames = get(ps, env, recursive)

                val peName = extractTopObjectNames(peNames)
                val psName = extractTopObjectNames(psNames)

                val fieldObjectNames = peName.flatMap(pon => {
                    psName.map(psn =>
                        Opt(pon.condition.and(psn.condition),
                            FieldObjectName(pon.entry, ObjectName.removePreviousScopingFromObjectName(psn.entry))))
                })

                fieldObjectNames.toList
            case p: PostfixExpr => get(p.p, env, recursive) ::: get(p.s, env, recursive)
            case p: Product => p.productIterator.flatMap(get(_, env)).toList
            case _ => List()
        }
        objectNames.distinct
    }

    /**
      * Extracts the objectnames with the greatest amount of operators for an input node.
      */
    def getTopObjectNames(node: Any, env: CModule): Set[Opt[ObjectName]] = extractTopObjectNames(get(node, env))

    /**
      * Filters for the objectname with the greatest amount of operators.
      */
    def extractTopObjectNames(inputNames: Iterable[Opt[ObjectName]]): Set[Opt[ObjectName]] = {
        val uniqueNames = inputNames.groupBy(_.entry.getBaseName)
        uniqueNames.map(_._2.maxBy(_.entry.size())).toSet // Note: assumes no nested variability -> for spllift
    }

    private def getObjectNameWithOperator(castExpr: Expr, pointerOperator: CPointerOperator, env: CModule, recursive: Boolean = true): List[Opt[ObjectName]] = {
        val names = get(castExpr, env, recursive)
        val res = names.map(name => {
            val oldObjectName = name.entry
            val condition = env.getCondition(castExpr)
            Opt(condition, PointerObjectName(oldObjectName, pointerOperator))
        })

        names ::: res
    }

    private def matchStructFieldWithCurlyInits(baseObjectNames: Iterable[Opt[ObjectName]], lcurlyInitializer: LcurlyInitializer, fields: List[Opt[ConditionalTypeMap]]): List[Opt[ObjectName]] = {
        baseObjectNames.flatMap(baseObjectName => {
            val condition = baseObjectName.condition

            fields.flatMap(cField => {
                // group inits and fields together
                groupInitializerWithFields(lcurlyInitializer, cField).flatMap {
                    case (field, init) =>
                        field.map(f => Opt(condition.and(f.condition), FieldObjectName(baseObjectName.entry, PlainObjectName(f.entry, None))))
                }
            })
        }).toList
    }
}