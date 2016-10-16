package de.fosd.typechef.cpointeranalysis

import java.io._

import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.customization.conditional.ConditionalSet
import de.fosd.typechef.featureexpr.FeatureExpr
import de.fosd.typechef.featureexpr.bdd.False

import scala.collection.immutable.Map
import scala.collection.mutable
import scala.pickling.Defaults._
import scala.pickling.json._

class CPointerAnalysisContext() extends PointerContext with ObjectNameContext with EquivalenceContext with FunctionContext {

    def solve(): CPointerAnalysisContext = {
        initEquivalenceClasses(this)
        createInitialPrefixSets(this)

        solveEquivalenceClasses(this)

        this
    }

}

trait EquivalenceContext extends PointerContext {
    private var equivalenceClassesPrefixSets: Set[(ObjectName, ObjectName)] = Set()
    private var equivalenceClassesMap: Map[ObjectName, EquivalenceClass] = Map()
    private var equivalenceClasses: Set[EquivalenceClass] = Set()

    def addPrefixSet(o1: ObjectName, o2: ObjectName) = equivalenceClassesPrefixSets += ((o1, o2))

    def find(objectName: ObjectName): Option[EquivalenceClass] = {
        val eqClass1: Option[EquivalenceClass] = equivalenceClassesMap.get(objectName)
        lazy val eqClass2: Option[EquivalenceClass] = equivalenceClassesMap.get(parenthesize(objectName))

        if (eqClass1.isDefined) eqClass1
        else if (eqClass2.isDefined) eqClass2
        else None
    }

    def getEquivalenceClassesPrefixSets: Set[(ObjectName, ObjectName)] = equivalenceClassesPrefixSets

    def getEquivalenceClassesMap: Map[ObjectName, EquivalenceClass] = equivalenceClassesMap

    def getEquivalenceClasses: Set[EquivalenceClass] = equivalenceClasses

    def showPointerEquivalenceClasses() = equivalenceClasses.foreach(println)

    def pointerEquivalenceClassesToString() = equivalenceClasses.foldLeft(new StringWriter())((w, e) => w.append(e.toString + "\n")).toString

    def initEquivalenceClasses(objectNameContext: ObjectNameContext): Unit = {
        clearEquivalenceClasses()

        val r = objectNameContext.getObjectNames.keys.foldLeft((getEquivalenceClassesMap, getEquivalenceClasses))((e, k) => {
            val ec = new EquivalenceClass(ConditionalSet(k, objectNameContext.getObjectNames.get(k)), ConditionalSet())

            (e._1 + (k -> ec), e._2 + ec)
        })

        equivalenceClassesMap = r._1
        equivalenceClasses = r._2
    }

    def clearEquivalenceClasses(): Unit = {
        equivalenceClassesMap = Map[ObjectName, EquivalenceClass]()
        equivalenceClasses = Set[EquivalenceClass]()
        // equivalenceClassesPrefixSets = Set[(ObjectName, ObjectName)]()
    }

    def createInitialPrefixSets(objectNameContext: ObjectNameContext, equivalenceClassesPrefixSets: Set[(ObjectName, ObjectName)] = equivalenceClassesPrefixSets, equivalenceClassesMap: Map[ObjectName, EquivalenceClass] = equivalenceClassesMap): Unit = {
        def lookup(objectName: ObjectName): Opt[Option[EquivalenceClass]] = {
            val eqClassLocal = find(objectName)
            val conditionLocal = objectNameContext.getObjectNames.get(objectName)

            lazy val globalObjectName = objectName.replace("§" + objectNameContext.unscopeMethodName(objectName) + "$", "§GLOBAL$")
            lazy val eqClassGlobal = find(globalObjectName)
            lazy val conditionGlobal = objectNameContext.getObjectNames.get(globalObjectName)

            val eqClass = if (eqClassLocal.isDefined) eqClassLocal else eqClassGlobal
            val condition = if (eqClassLocal.isDefined) conditionLocal else conditionGlobal

            Opt(condition, eqClass)
        }

        val objectNames = objectNameContext.getObjectNames.keys

        for (o <- objectNames) {
            val eqClassObjectO = equivalenceClassesMap.get(o)
            val oCondition = objectNameContext.getObjectNames.get(o)

            val uo = unscopeName(o)
            val scope = unscopeFileAndMethod(o)

            if (uo.startsWith(ObjectNameOperator.PointerCreation.toString)) {
                val uo1 = uo.replace(ObjectNameOperator.PointerCreation.toString, "")
                val o1Condition = lookup(scope + uo1).condition

                if (eqClassObjectO.isDefined)
                    if (o1Condition.equivalentTo(False)) eqClassObjectO.get.addPrefix((ObjectNameOperator.PointerDereference.toString, scope + uo1), oCondition.and(o1Condition))
                    else eqClassObjectO.get.addPrefix((ObjectNameOperator.PointerDereference.toString, scope + uo1), oCondition)
            } else if (uo.startsWith(ObjectNameOperator.PointerDereference.toString)) {
                val uo1 = uo.replace(ObjectNameOperator.PointerDereference.toString, "")

                val query = scope + uo1
                val query2 = scope + ObjectNameOperator.unparenthesize(uo1)

                val condEqClass = if (lookup(query).entry.isDefined) lookup(query) else lookup(query2)

                if (condEqClass.entry.isDefined) condEqClass.entry.get.addPrefix((ObjectNameOperator.PointerDereference.toString, o), oCondition.and(condEqClass.condition))

            } else if (uo.contains(ObjectNameOperator.StructAccess.toString) || uo.contains(ObjectNameOperator.StructPointerAccess.toString)) {
                val split = ObjectNameOperator.splitParentAndField(uo)

                if (split.nonEmpty) {
                    val (uo1, field) = split.get

                    val query = scope + uo1
                    val condEqClass = lookup(query)

                    if (condEqClass.entry.isDefined) condEqClass.entry.get.addPrefix((field, o), oCondition.and(condEqClass.condition))
                }
            }
        }
    }

    def solveEquivalenceClasses(context: ObjectNameContext) =
        context.getObjectNamesAssignments.toPlainSet().foreach {
            case (assignee, assignor) => mergeEquivalenceClasses(assignee, assignor)
        }

    private def mergeEquivalenceClasses(assignee: ObjectName, assignor: ObjectName) {
        val eqClassAssignee = find(assignee)
        val eqClassAssignor = find(assignor)

        if (eqClassAssignee.isDefined && eqClassAssignor.isDefined && !eqClassAssignee.equals(eqClassAssignor)) {
            eqClassAssignee.get.beingMerged = true
            eqClassAssignor.get.beingMerged = true
            merge(eqClassAssignee.get, eqClassAssignor.get)
            eqClassAssignee.get.beingMerged = false
            eqClassAssignor.get.beingMerged = false
        }
    }

    private def merge(e1: EquivalenceClass, e2: EquivalenceClass) {
        val newObjectNamesSet: EquivalenceClass = e1.union(e2)
        var newPrefixSet: ConditionalSet[(String, String)] = e1.prefixes()

        // loop both prefix sets
        for ((a, o) <- e2.prefixes().toPlainSet()) {
            // filter prefixes shared by the two eq classes
            val sharedPrefix = newPrefixSet.toPlainSet().filter({ case (a1, o1) => a.equals(a1) })

            // if equivalence classes share the same prefix (i.e., if they have edges to the same object name)
            if (sharedPrefix.nonEmpty) {
                sharedPrefix.foreach({ case ((_, o1)) =>
                    val eqClassO = find(o)
                    val eqClassO1 = find(o1)

                    // if any two eq classes have the same prefix relation, merge them recursively
                    if (eqClassO.isDefined && eqClassO1.isDefined && !eqClassO.get.beingMerged && !eqClassO1.get.beingMerged && !eqClassO.equals(eqClassO1) ) {
                        merge(eqClassO.get, eqClassO1.get)
                    }
                })
            } else newPrefixSet += ((a, o), e2.prefixes().get((a, o)))
        }
        // add new equivalence class and delete merged ones
        equivalenceClasses -= (e1, e2)
        val e = new EquivalenceClass(newObjectNamesSet.objectNames, newPrefixSet)
        equivalenceClasses += e
        e1.objectNames.toPlainSet().foreach({ o => equivalenceClassesMap += (o -> e) })
        e2.objectNames.toPlainSet().foreach({ o => equivalenceClassesMap += (o -> e) })
    }

}

trait ObjectNameContext extends PointerContext {

    private var _objectNames: Set[Opt[ObjectName]] = Set()
    private var objectNames: ConditionalSet[ObjectName] = ConditionalSet()
    private var objectNamesScope: Map[ObjectName, Scope] = Map()
    private var objectNameAssignments: ConditionalSet[Assignment] = ConditionalSet()

    def getObjectNamesAssignments = objectNameAssignments

    def setObjectNameAssignments(oAssignments: ConditionalSet[Assignment]) = objectNameAssignments = oAssignments

    def addObjectName(scopedObjectName: ObjectName, ctx: FeatureExpr): ObjectName = {
        _objectNames = _objectNames + Opt(ctx, scopedObjectName)
        // add object name with scope defined
        objectNames = objectNames + (scopedObjectName, ctx)
        scopedObjectName
    }

    def addObjectNameAssignment(assignment: Assignment, ctx: FeatureExpr) = {
        objectNameAssignments += (assignment, ctx)
    }

    def applyScope(objectName: ObjectName, currentScope: Scope, currentFile: String): ObjectName = {
        // format object name with scope
        val scopedObjectName = currentFile + "§" + currentScope + "$" + objectName

        // include object name with scope (according to the variable declarations)
        objectNamesScope = objectNamesScope.updated(objectName, currentScope)
        scopedObjectName
    }

    def findScopeForObjectName(rawObjectName: ObjectName, currentScope: Scope): ObjectName = {
        // trivial scopes (current function or global)
        // TypeSystem?
        objectNamesScope.getOrElse(rawObjectName, "GLOBAL")
    }

    def getObjectNames = objectNames

    def getObjectNamesScope = objectNamesScope

    def updateScope(key: ObjectName, value: Scope) = objectNamesScope = objectNamesScope.updated(key, value)

    def showAssignments() = println("*** Assignments: %s".format(objectNameAssignments))

    def showExtractedObjectNames() = println(objectNames)

    def getField(objectName: ObjectName): String = {
        val delimiter =
            if (objectName.contains(ObjectNameOperator.StructPointerAccess.toString)) Some(ObjectNameOperator.StructPointerAccess.toString)
            else if (objectName.contains(ObjectNameOperator.StructAccess.toString)) Some(ObjectNameOperator.StructAccess.toString)
            else None
        if (delimiter.isDefined) objectName.substring(objectName.indexOf(delimiter.get) + delimiter.get.length)
        else ""
    }

}

trait FunctionContext extends PointerContext {

    var functionDefs: ConditionalSet[FunctionDefinition] = ConditionalSet()
    var functionDefReturns: Map[FunctionName, ConditionalSet[ObjectName]] = Map()
    var functionDefParameters: Map[FunctionName, List[Opt[ObjectName]]] = Map()
    var functionCallParamList: List[Opt[ObjectName]] = List()

    // function calls and function call parameters
    var functionCalls: ConditionalSet[FunctionCall] = ConditionalSet()
    var functionCallParameters: List[(FunctionName, List[Opt[ObjectName]])] = List()


    def addFuncMappings(fDefs: ConditionalSet[FunctionDefinition], fDefParameters: Map[FunctionName, List[Opt[ObjectName]]], fReturns: Map[FunctionName, ConditionalSet[ObjectName]],
                        fCalls: ConditionalSet[FunctionCall], fCallParameters: List[(FunctionName, List[Opt[ObjectName]])]) = {
        functionDefs = fDefs
        functionDefParameters = fDefParameters
        functionCalls = fCalls
        functionCallParameters = fCallParameters
        functionDefReturns = fReturns
        this
    }

    def showFunctionDefReturns() = println(functionDefReturns)

    def showFunctionDefs() = println("*** Function defs are (%d): %s".format(functionDefs.toPlainSet().size, functionDefs.toPlainSetWithConditionals()))

    def showFunctionDefsParameters() = println(functionDefParameters)
}

object LinkedObjectNames extends PointerContext {

    private val paramLinks: mutable.HashMap[ObjectName, List[Opt[ObjectName]]] = new mutable.HashMap[ObjectName, List[Opt[ObjectName]]]()

    def addName(name: ObjectName) = if (!hasName(name)) paramLinks.put(name, List())

    def hasName(name: ObjectName) = paramLinks.contains(name)

    def addObjectNameAssignment(assignment: Assignment, ctx: FeatureExpr) = {
        val left = assignment._1
        val right = assignment._2

        if (hasName(left)) addLink(left, Opt(ctx, right))
        else if (hasName(right)) addLink(right, Opt(ctx, left))
    }

    def addLink(linkTo: ObjectName, linkFrom: Opt[ObjectName]) = {
        val put = get(linkTo) match {
            case Some(refs) => linkFrom :: refs
            case None => List(linkFrom)
        }
        paramLinks.put(linkTo, put)
    }

    def get(name: ObjectName) = paramLinks.get(name)

    def save(file: String) = new PrintWriter(file) {
        write(paramLinks.keySet.map(key => (key, paramLinks(key))).pickle.toString)
        close()
    }

    def load(file: String) =
        scala.io.Source.fromFile(file).mkString.unpickle[List[(ObjectName, List[Opt[ObjectName]])]].foreach(x => paramLinks.put(x._1, x._2))
}