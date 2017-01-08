package de.fosd.typechef.cmodule.ccallgraph.cpointeraliasing.types

import java.io.{StringWriter, Writer}
import java.util

import de.fosd.typechef.cmodule.CModule
import de.fosd.typechef.cmodule.ccallgraph.cpointeraliasing.extractors.ObjectNameExtractor
import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.featureexpr.bdd.BDDNoFeatureModel
import de.fosd.typechef.featureexpr.{FeatureExprFactory, FeatureModel}
import de.fosd.typechef.parser.c.AST
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.mutable

case class EQClass(objectNames: Set[ObjectName]) {
    def union(other: EQClass): EQClass = EQClass(objectNames.union(other.objectNames))
}

class EQClassesMap() {

    private lazy val logger: Logger = LoggerFactory.getLogger(getClass)

    private lazy val EMPTY_PREFIX_SET: Set[Prefix] = Set(EmptyPrefix())

    /**
      * Internal storage data-structures.
      * Note: we use for the prefixset map an IdentityHashMap since equals on the key is very expensive and requires sat checks.
      */
    private val eqClassMap: mutable.Map[ObjectName, List[Opt[EQClass]]] = new mutable.HashMap[ObjectName, List[Opt[EQClass]]]()
    private val prefixMap: util.IdentityHashMap[Opt[EQClass], Set[Prefix]] = new util.IdentityHashMap[Opt[EQClass], Set[Prefix]]()

    /**
      * Adds all objectnames of a collection to the EQClassesMap.
      */
    def addObjectNames(objectNames: Iterable[Opt[ObjectName]]): Unit = objectNames.foreach(addObjectName)

    /**
      * Adds all objectnames nested in assignments to the EQClassesMap.
      */
    def addObjectNamesFromAssignments(assignments: Set[Opt[Assignment]]): Unit =
        assignments.foreach(assignment => {
            addObjectName(Opt(assignment.condition, assignment.entry.left))
            addObjectName(Opt(assignment.condition, assignment.entry.right))
        })

    /**
      * Adds a single obectname to the EQClassesMap.
      */
    def addObjectName(objectName: Opt[ObjectName]): Unit =
        if (!(eqClassMap.contains(objectName.entry)
          && find(objectName.entry).exists(_.condition.equivalentTo(objectName.condition)))) // add object name only if not present in its unique condition
        {
            putObjectName(objectName)

            // add nested object names as well
            objectName.entry match {
                case f: FieldObjectName => addObjectName(objectName.copy(entry = f.name))
                case p: PointerObjectName => addObjectName(objectName.copy(entry = p.name))
                case _ =>
            }
        }

    private def putObjectName(objectName: Opt[ObjectName]): Unit = {
        val prev = eqClassMap.getOrElse(objectName.entry, List())
        val updated = if (prev.nonEmpty) Opt(prev.head.condition or objectName.condition, EQClass(Set(objectName.entry))) else Opt(FeatureExprFactory.True, EQClass(Set(objectName.entry)))
        eqClassMap += (objectName.entry -> List(updated))
    }

    /**
      * Retrieves all related equivalence classes for a ast node.
      */
    def find(node: AST, env: CModule): List[Opt[EQClass]] =
        ObjectNameExtractor.getTopObjectNames(node, env).flatMap(objectName => {
            find(objectName.entry).filter(_.condition.and(objectName.condition).isSatisfiable())
        }).toList

    /**
      * Reset all entries of the map.
      */
    def clear(): Unit = {
        eqClassMap.clear()
        prefixMap.clear()
    }

    /**
      * Gets the total amount of stored eq classes.
      */
    def getSize: Int = eqClassMap.values.map(_.size).sum

    /**
      * Retrieves all related equivalence classes for a given objectname.
      */
    def find(objectName: ObjectName): List[Opt[EQClass]] = eqClassMap.getOrElse(objectName, List())

    /**
      * Line 4 -> 10 of the algorithm.
      */
    def initializePrefixSets(): Unit = {
        prefixMap.clear()
        eqClassMap.foreach { case (objectName, variationalEQClass) =>
            objectName match {
                case _: PlainObjectName => // no prefix
                case PointerObjectName(baseObjectname, PointerDerefOperator()) => // (*a) -> (*,*a) -> EQ(a)
                    find(baseObjectname).foreach(addToPrefixMap(_, OperatorPrefix(objectName, PointerDerefOperator())))
                case PointerObjectName(baseObjectname, PointerCreationOperator()) => // (&a) -> (*,a) -> EQ(&a)
                    variationalEQClass.foreach(addToPrefixMap(_, OperatorPrefix(baseObjectname, PointerDerefOperator())))
                //find(baseObjectname).foreach(addToPrefixMap(_, OperatorPrefix(objectName, PointerDerefOperator())))
                case FieldObjectName(baseObjectname, field) => // field
                    find(PointerObjectName(baseObjectname, PointerCreationOperator())).foreach(addToPrefixMap(_, FieldPrefix(objectName, field)))
                    find(baseObjectname).foreach(addToPrefixMap(_, FieldPrefix(objectName, field)))
                case _ =>
            }
        }
    }

    /**
      * Updates the prefix set of an equivalence class.
      */
    def updatePrefix(eqClass: Opt[EQClass], prefix: Set[Prefix]): Unit =
        if (!prefix.equals(EMPTY_PREFIX_SET)) prefixMap.put(eqClass, getPrefixOrElse(eqClass) union prefix)

    /**
      * Adds a new equivalence class.
      */
    def addEQClass(updatedValue: Opt[EQClass], oldE1: List[Opt[EQClass]], oldE2: List[Opt[EQClass]], fm: FeatureModel = BDDNoFeatureModel): Opt[EQClass] = {
        /**
          * Reduce the amount of variability by collecting all EQClasses which are exactly the same.
          */
        def cleanEqualEQClasses(objectName: ObjectName): Option[Opt[EQClass]] = {
            val oldMapEntry = eqClassMap(objectName)
            val cleanMap = oldMapEntry.filterNot(cEQ => (oldE1.exists(cEQ.eq) || oldE2.exists(cEQ.eq)) && updatedValue.condition.equivalentTo(cEQ.condition))
            val (equalEQClasses, cleanedEQ) = cleanMap.partition(x => x.entry.objectNames == updatedValue.entry.objectNames && x.condition.and(updatedValue.condition).isSatisfiable(fm))
            val sameOrCond = equalEQClasses.foldLeft(updatedValue.condition)((condition, eqClass) => if (condition eq eqClass.condition) condition else condition or eqClass.condition)
            val distinctEQClass = updatedValue.copy(condition = sameOrCond)

            eqClassMap += (objectName -> cleanedEQ)
            Some(distinctEQClass)
        }

        val eqClassObjectNames = updatedValue.entry.objectNames.toList
        val distinctEQClass = eqClassObjectNames.flatMap(cleanEqualEQClasses).foldLeft(updatedValue)((u, v) =>
            if (!v.condition eq u.condition) u.copy(condition = v.condition or u.condition)
            else u
        )

        eqClassObjectNames.foreach(o => eqClassMap += (o -> (distinctEQClass :: eqClassMap(o))))

        distinctEQClass
    }

    /**
      * Gets the prefix set of an equivalence class.
      */
    def getPrefix(eqClass: Opt[EQClass]): Set[Prefix] = getPrefixOrElse(eqClass)

    private def getPrefixOrElse(key: Opt[EQClass], other: Set[Prefix] = Set[Prefix]()) = {
        val query = prefixMap.get(key)
        if (query == null) other else query
    }

    private def addToPrefixMap(key: Opt[EQClass], value: Prefix): Unit = prefixMap.put(key, getPrefixOrElse(key) + value)

    def eqClassesToString(): String = writeEQ(new StringWriter()).toString

    def writeEQ(writer: Writer): Writer = {
        eqClassMap.foreach {
            case (objectName, entry) =>
                writer.write(objectName.toString + " {\n")
                entry.foreach(eqClass => writer.write("\t" + eqClass.toString + "\n"))
                writer.write("}\n")
        }
        writer
    }

    def eqClassesAndPrefixSetsToString(): String = writeEQAndPrefixSets(new StringWriter()).toString

    def writeEQAndPrefixSets(writer: Writer): Writer = {
        eqClassMap.foreach {
            case (objectName, entry) =>
                writer.write(objectName.toString + " {\n")
                entry.foreach(eqClass => writer.write("\t" + eqClass.toString + "\n\t\tPrefix" + getPrefix(eqClass) + "\n"))
                writer.write("}\n")
        }
        writer
    }

    private def idEQSet = java.util.Collections.newSetFromMap[Opt[EQClass]](new util.IdentityHashMap())
}