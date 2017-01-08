package de.fosd.typechef.cmodule.ccallgraph.cpointeraliasing

import java.io.{FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}
import java.util
import java.util.zip.{GZIPInputStream, GZIPOutputStream}

import de.fosd.typechef.cmodule.ccallgraph.cpointeraliasing.extractors.{AssignmentExtractor, DefaultExtractor}
import de.fosd.typechef.cmodule.ccallgraph.cpointeraliasing.types.{Assignment, _}
import de.fosd.typechef.cmodule.cmodulecache.{CModuleCacheMediation, CModuleCacheUser}
import de.fosd.typechef.cmodule.{CModule, CModuleCommons}
import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.customization.StopWatch
import de.fosd.typechef.featureexpr.bdd.BDDNoFeatureModel
import de.fosd.typechef.featureexpr.{FeatureExpr, FeatureExprFactory, FeatureModel}
import de.fosd.typechef.parser.c.{AST, TranslationUnit}
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.mutable

/**
  * Frontend class of our (function-) pointer aliasing analysis.
  */
class CPointerAliasing(private var objectNames: Set[Opt[ObjectName]], private var assignments: Set[Opt[Assignment]], fm: FeatureModel) extends CModuleCacheUser[TranslationUnit] with CModuleCommons {

    private lazy val logger: Logger = LoggerFactory.getLogger(getClass)

    private val equivClassesMap: EQClassesMap = new EQClassesMap

    private var visitedTUnits: List[TranslationUnit] = List()

    private var solved: Boolean = false
    private var updated: Boolean = false

    def this(file: String, fm: FeatureModel = BDDNoFeatureModel) {
        this(Set(), Set(), fm)
        update(file)
    }

    private def getFunctions(objectName: ObjectName, condition: FeatureExpr = FeatureExprFactory.True, cModuleEnv: CModule): Option[Opt[FileFunctionName]] = objectName match {
        case objectName: PlainObjectName if objectName.isKnownAsFunction(cModuleEnv) =>
            Some(Opt(condition, FileFunctionName(objectName.getFile.getOrElse(""), objectName.name)))
        case p: PointerObjectName => getFunctions(p.getPlainObjectName, condition, cModuleEnv)
        case _ => None
    }

    /**
      * Retrieves the possible points-to functions of a function pointer.
      */
    def getPossibleDestinations(pointer: AST, cModuleEnv: CModule): List[Opt[FileFunctionName]] = {
        val eq = equivClassesMap.find(pointer, cModuleEnv)
        eq.flatMap(eqClass => eqClass.entry.objectNames.flatMap(getFunctions(_, eqClass.condition, cModuleEnv)))
    }

    /**
      * Computes the PointerEquivalence classes for input set of objectNames and assignments.
      */
    private def update(file: String): Unit = {
        val (objectNames, assignments) = load(file)
        update(objectNames, assignments)
    }

    /**
      * Computes the PointerEquivalence classes for input set of objectNames and assignments.
      */
    private def update(objectNames: Set[Opt[ObjectName]], assignments: Set[Opt[Assignment]]): Unit = {
        this.objectNames = this.objectNames ++ objectNames
        this.assignments = this.assignments ++ assignments

        updated = true

        if (solved) {
            solved = false
            solve()
        }
    }

    /**
      * Saves the current objectnames and assignments to an external file.
      */
    def save(file: String): Unit = {
        val fw = new ObjectOutputStream(new GZIPOutputStream(new FileOutputStream(file)))
        fw.writeObject((objectNames, assignments))
        fw.close()
    }

    /**
      * Computes the final equivalence classes based on the extracted assignments.
      */
    def solve(): Boolean = {
        if (this.objectNames.isEmpty) return false

        val pointerSolvingTime = StopWatch.measureProcessCPUTime({
            initEQClassMap()
            logger.info("Assignments:\t" + assignments.size)
            logger.info("ObjectNames:\t" + objectNames.size)
            val step = assignments.size / 10
            assignments.toList.zipWithIndex.foreach { case (assignment, i) =>
                val leftEQClass = equivClassesMap.find(assignment.entry.left)
                val rightEQClass = equivClassesMap.find(assignment.entry.right)
                if (step != 0 && (i % step == 0))
                    logger.info("Solved: " + i + " of " + assignments.size + " assignments")
                merge(leftEQClass, rightEQClass, assignment.condition)
            }
        })._1

        solved = true
        updated = false

        logger.info("Solved PointerEquiv in " + pointerSolvingTime + "ms.")
        logger.debug("EQClasses:\t" + equivClassesMap.getSize)

        true
    }

    private def initEQClassMap() = {
        equivClassesMap.clear()
        equivClassesMap.addObjectNames(objectNames)
        equivClassesMap.addObjectNamesFromAssignments(assignments)
        equivClassesMap.initializePrefixSets()
    }

    /**
      * Two EQClasses are mergeable if their combined presence condition is satisfiable and their entries are not equal
      */
    private def canMerge(e1: Opt[EQClass], e2: Opt[EQClass], mergeCondition: FeatureExpr = FeatureExprFactory.True): Boolean = {
        lazy val eqEQClasses = e1.entry.equals(e2.entry)
        lazy val beingMerged = mergedEQs.contains(e1) && mergedEQs.contains(e2)
        lazy val sat = mergeCondition.isSatisfiable(fm)

        !beingMerged && !eqEQClasses && sat
    }

    /**
      * Merges two equivalence classes variational according to the algorithm of Zhang.
      *
      * Note: The implementation is very feature sensitive but the strategy of Zhang may cause exponential explosion
      * feature-wise: currently at a threshold of 75 distinct variants we are forced to reduce the precision of
      * the pointer analysis strategy since otherwise we are not able to compute all pointer equality relationships.
      * This is NOT sound, but in practice does not alter the result of our function points-to target computation strategy,
      * since we are later simplify the feature condition to the minimum presence condition.
      */
    private val mergedEQs: util.Set[Opt[EQClass]] = java.util.Collections.newSetFromMap[Opt[EQClass]](new java.util.IdentityHashMap())
    private def merge(eqClass1: List[Opt[EQClass]], eqClass2: List[Opt[EQClass]], mergeCondition: FeatureExpr, prefixMerge: Boolean = false): Unit = {
        def getPrefixSets(e1: Opt[EQClass], e2: Opt[EQClass]) = (equivClassesMap.getPrefix(e1), equivClassesMap.getPrefix(e2))

        val merges = determineSolveableUniqueMergeConditions(eqClass1, eqClass2, mergeCondition, prefixMerge)

        for ((localMergeCondition, eqs) <- merges if canMerge(eqs.head._1, eqs.head._2, localMergeCondition)) {
            val (e1L, e2L) = (eqs.map(_._1), eqs.map(_._2))
            val (e1, e2) = (e1L.head, e2L.head)

            // avoid duplicate merges
            e1L.foreach(mergedEQs.add)
            e2L.foreach(mergedEQs.add)

            val unionEQClass = equivClassesMap.addEQ(Opt(localMergeCondition, e1.entry.union(e2.entry)), e1L, e2L, fm)
            mergedEQs.add(unionEQClass)

            /**
              * Recursive prefix merge strategy according to the algorithm.
              */
            val (prefixSet1, prefixSet2) = getPrefixSets(e1, e2)
            var newPrefixSet = prefixSet1

            for (prefix2 <- prefixSet2) {
                var prefixSetMerged = false

                def prefixMerge(on1: ObjectName, on2: ObjectName): Unit = {
                    prefixSetMerged = true
                    merge(equivClassesMap.find(on1), equivClassesMap.find(on2), localMergeCondition, prefixMerge = true)
                }

                for (prefix1 <- prefixSet1)
                    (prefix1, prefix2) match {
                        case (o1: OperatorPrefix, o2: OperatorPrefix) if o1.pointerOperator == o2.pointerOperator =>
                            prefixMerge(o1.objectName, o2.objectName)
                        case (f1: FieldPrefix, f2: FieldPrefix) if f1.fieldPrefix.getBaseName == f2.fieldPrefix.getBaseName =>
                            prefixMerge(f1.objectName, f2.objectName)
                        case _ =>
                    }

                if (!prefixSetMerged)
                    newPrefixSet = newPrefixSet + prefix2
            }

            equivClassesMap.updatePrefix(unionEQClass, newPrefixSet)

        }

        if (!prefixMerge)
            mergedEQs.clear()
    }

    private val reducedObjectNames = new mutable.HashSet[ObjectName]
    private def determineSolveableUniqueMergeConditions(eqClass1: List[Opt[EQClass]], eqClass2: List[Opt[EQClass]], mergeCondition: FeatureExpr, prefixMerge: Boolean = false) = {
        def and(f1 : FeatureExpr, f2 : FeatureExpr) = f1 and f2
        def or(f1 : FeatureExpr, f2 : FeatureExpr) = f1 or f2
        def f(f1 : FeatureExpr, f2 : FeatureExpr) = f1
        def t(f1 : FeatureExpr, f2 : FeatureExpr) = FeatureExprFactory.True

        def getUniqueMerges(condition : FeatureExpr = FeatureExprFactory.True, op: (FeatureExpr, FeatureExpr) => FeatureExpr = and) = {
            val featurePairs = for {e1 <- eqClass1; e2 <- eqClass2} yield (op (e1.condition, e2.condition) and mergeCondition, (e1, e2))
            featurePairs.groupBy(_._1).toList.map(x => (x._1, x._2.map(_._2)))
        }

        lazy val reducedMerges = getUniqueMerges(mergeCondition, op = if (prefixMerge) t else t)
        val previouslyReduced = eqClass1.exists(_.entry.objectNames.exists(reducedObjectNames.contains)) || eqClass2.exists(_.entry.objectNames.exists(reducedObjectNames.contains))

        val uniqueMerges = if (!previouslyReduced) getUniqueMerges(mergeCondition) else reducedMerges

        if (uniqueMerges.size > 150) { // Note: we are currently forced to reduce the precision if we are merging more than 100 variants.
        val reduced = reducedMerges
            if (!prefixMerge) {
                logger.warn("Forced to reduce pointer aliasing precision because of conditional explosion.")
                logger.info("Old merges:\t" + uniqueMerges.size)
                logger.debug("New merges:\t" + reduced.size)
            }
            eqClass1.foreach(_.entry.objectNames.foreach(reducedObjectNames.add))
            eqClass2.foreach(_.entry.objectNames.foreach(reducedObjectNames.add))
            reduced
        } else uniqueMerges
    }

    private def load(file: String): (Set[Opt[ObjectName]], Set[Opt[Assignment]]) = {
        val fr = new ObjectInputStream(new GZIPInputStream(new FileInputStream(file)))
        val ro = fr.readObject()
        fr.close()
        ro.asInstanceOf[(Set[Opt[ObjectName]], Set[Opt[Assignment]])]
    }

    override def mediation(cModuleEnv: CModuleCacheMediation[TranslationUnit], t: Iterable[TranslationUnit]): Boolean = {
        def emptyObjectNames: Set[Opt[ObjectName]] = Set[Opt[ObjectName]]()

        def emptyAssignments: Set[Opt[Assignment]] = Set[Opt[Assignment]]()

        val (newObjectNames, newAssignments) = t.foldLeft((emptyObjectNames, emptyAssignments)) {
            case ((o, a), tunit) => cModuleEnv match {
                case env: CModule =>
                    val (no, na) = DefaultExtractor.extractAll(tunit, env)
                    (o union no, a union na)
                case _ => (o, a)
            }
        }

        visitedTUnits :::= t.toList

        val newCallCalleeAssignments = visitedTUnits.foldLeft(emptyAssignments)((ass, t) =>
            cModuleEnv match {
                case env: CModule => ass union AssignmentExtractor.getCallCalleeAssignments(t, env)
                case _ => ass
            })

        update(newObjectNames, newAssignments union newCallCalleeAssignments)

        true
    }
}