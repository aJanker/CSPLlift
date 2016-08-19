package de.fosd.typechef.cpointeranalysis

import java.io._

import de.fosd.typechef.conditional.{ConditionalSet, Opt}
import de.fosd.typechef.featureexpr.FeatureExpr

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
        val eqClass2: Option[EquivalenceClass] = equivalenceClassesMap.get(parenthesize(objectName))

        if (eqClass1.isDefined)
            eqClass1
        else if (eqClass2.isDefined)
            eqClass2
        else None
    }

    def getEquivalenceClassesPrefixSets: Set[(ObjectName, ObjectName)] = equivalenceClassesPrefixSets

    def getEquivalenceClassesMap: Map[ObjectName, EquivalenceClass] = equivalenceClassesMap

    def getEquivalenceClasses: Set[EquivalenceClass] = equivalenceClasses

    def showPointerEquivalenceClasses() = equivalenceClasses.foreach(println)

    def pointerEquivalenceClassesToString() = equivalenceClasses.foldLeft(new StringWriter())((w, e) => w.append(e.toString + "\n")).toString

    def initEquivalenceClasses(objectNameContext: ObjectNameContext): Unit = {
        clearEquivalenceClasses

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

    def createInitialPrefixSets(objectNameContext: ObjectNameContext ,equivalenceClassesPrefixSets: Set[(ObjectName, ObjectName)] = equivalenceClassesPrefixSets, equivalenceClassesMap: Map[ObjectName, EquivalenceClass] = equivalenceClassesMap): Unit = {
        val objectNames = objectNameContext.getObjectNames.keys
        var eqClassObjectO: EquivalenceClass = EquivalenceClass()
        var eqClassObjectO1: EquivalenceClass = EquivalenceClass()


        // generate cross product of all object names
        // for ((o, o1) <- equivalenceClassesPrefixSets) {
        for { o <- objectNames; o1 <- objectNames } {
            val findObjectNameO = equivalenceClassesMap.get(o)
            val findObjectNameO1 = equivalenceClassesMap.get(o1)

            if (findObjectNameO.isDefined && findObjectNameO1.isDefined) {
                eqClassObjectO = findObjectNameO.get
                eqClassObjectO1 = findObjectNameO1.get

                val eqClassObjectOFeatExpr = eqClassObjectO.objectNames.get(o)
                val eqClassObjectO1FeatExpr = eqClassObjectO1.objectNames.get(o1)

                val uo = unscopeName(o)
                val uo1 = unscopeName(o1)

                // pointer creation operator
                if ((uo.equals(applyOperator(ObjectNameOperator.PointerCreation.toString, uo1))) || uo.equals(applyOperator(ObjectNameOperator.PointerCreation.toString, parenthesize(uo1)))) {
                    // add * edge from o to o1 (removing the pointer creation effect with a dereferencing operator)
                    eqClassObjectO.addPrefix((ObjectNameOperator.PointerDereference.toString, o1), eqClassObjectOFeatExpr and eqClassObjectO1FeatExpr)

                    // pointer dereference operator
                } else if ((uo.equals(applyOperator(ObjectNameOperator.PointerDereference.toString, uo1)) || uo.equals(applyOperator(ObjectNameOperator.PointerDereference.toString, parenthesize(uo1))))) {
                    // add * edge from o1 to o
                    eqClassObjectO1.addPrefix((ObjectNameOperator.PointerDereference.toString, o), eqClassObjectOFeatExpr and eqClassObjectO1FeatExpr)

                    // struct dot access operator
                } else if ((uo.startsWith(applyOperator(ObjectNameOperator.StructAccess.toString, uo1)) || uo.startsWith(applyOperator(ObjectNameOperator.StructAccess.toString, parenthesize(uo1))))) {
                    val objectNameFields = (o.take(o.lastIndexOf(ObjectNameOperator.StructAccess.toString)), uo.drop(uo.lastIndexOf(ObjectNameOperator.StructAccess.toString) + ObjectNameOperator.StructAccess.toString.length))
                    val eqClassObjectOPartial = equivalenceClassesMap.get(objectNameFields._1)

                    // add field edge from o to o1
                    if (eqClassObjectOPartial.isDefined) {
                        eqClassObjectOPartial.get.addPrefix((objectNameFields._2, o), eqClassObjectOFeatExpr and eqClassObjectO1FeatExpr)

                    }
                    // remove pointer dereference -> causes wrong results when applied on function calls
                } else if ((uo.startsWith(applyOperator(ObjectNameOperator.StructAccess.toString, unapplyPointer(uo1))) || uo.startsWith(applyOperator(ObjectNameOperator.StructAccess.toString, parenthesize(unapplyPointer(uo1)))))) {
                    val objectNameFields = (o.take(o.lastIndexOf(ObjectNameOperator.StructAccess.toString)), uo.drop(uo.lastIndexOf(ObjectNameOperator.StructAccess.toString) + ObjectNameOperator.StructAccess.toString.length))
                    val eqClassObjectOPartial = equivalenceClassesMap.get(objectNameFields._1)

                    // add field edge from o to o1
                    if (eqClassObjectOPartial.isDefined) {
                        eqClassObjectOPartial.get.addPrefix((objectNameFields._2, o), eqClassObjectOFeatExpr and eqClassObjectO1FeatExpr)

                    }
                    // struct pointer access operator  (dereference + dot)
                } else if ((uo.startsWith(applyOperator(ObjectNameOperator.StructPointerAccess.toString, uo1)) || uo.startsWith(applyOperator(ObjectNameOperator.StructPointerAccess.toString, parenthesize(uo1))))) {
                    val objectNameFields = (o.take(o.lastIndexOf(ObjectNameOperator.StructPointerAccess.toString)), uo.drop(uo.lastIndexOf(ObjectNameOperator.StructPointerAccess.toString) + ObjectNameOperator.StructPointerAccess.toString.length))
                    val eqClassObjectOPartial = equivalenceClassesMap.get(applyOperator(ObjectNameOperator.PointerDereference.toString, objectNameFields._1))

                    // add field edge from o to o1
                    if (eqClassObjectOPartial.isDefined) {
                        eqClassObjectOPartial.get.addPrefix((objectNameFields._2, o), eqClassObjectOFeatExpr and eqClassObjectO1FeatExpr)
                    }
                    // remove pointer dereference -> causes wrong results when applied on function calls
                } else if ((uo.startsWith(applyOperator(ObjectNameOperator.StructPointerAccess.toString, unapplyPointer(uo1))) || uo.startsWith(applyOperator(ObjectNameOperator.StructPointerAccess.toString, parenthesize(unapplyPointer(uo1)))))) {
                    val objectNameFields = (o.take(o.lastIndexOf(ObjectNameOperator.StructPointerAccess.toString)), uo.drop(uo.lastIndexOf(ObjectNameOperator.StructPointerAccess.toString) + ObjectNameOperator.StructPointerAccess.toString.length))
                    val eqClassObjectOPartial = equivalenceClassesMap.get(objectNameFields._1)

                    // add field edge from o to o1
                    if (eqClassObjectOPartial.isDefined) {
                        eqClassObjectOPartial.get.addPrefix((objectNameFields._2, o), eqClassObjectOFeatExpr and eqClassObjectO1FeatExpr)
                    }
                }
                eqClassObjectO.objectNames = eqClassObjectO.objectNames.and(o, eqClassObjectOFeatExpr and eqClassObjectO1FeatExpr)
                eqClassObjectO1.objectNames = eqClassObjectO1.objectNames.and(o1, eqClassObjectO1FeatExpr and eqClassObjectOFeatExpr)
            }
            else {
                if (!findObjectNameO.isDefined) {
                    println("Equivalence class not found for object name" + o)
                } else if (!findObjectNameO1.isDefined) {
                    println("Equivalence class not found for object name" + o1)
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
                    if (eqClassO.isDefined && eqClassO1.isDefined && !eqClassO.get.beingMerged && !eqClassO1.get.beingMerged && !eqClassO.equals(eqClassO1)) {
                        merge(eqClassO.get, eqClassO1.get);
                    } else {
                        if (!eqClassO.isDefined) {
                            println("Equivalence class not merged: " + eqClassO);
                        } else if (!eqClassO1.isDefined) {
                            println("Equivalence class not merged: " + eqClassO1);
                        }
                    }
                })
            } else newPrefixSet += ((a, o), e2.prefixes().get((a, o)))
        }
        // add new equivalence class and delete merged ones
        equivalenceClasses -= (e1, e2)
        val e = new EquivalenceClass(newObjectNamesSet.objectNames, newPrefixSet)
        equivalenceClasses += e
        e1.objectNames.toPlainSet().foreach({ o => equivalenceClassesMap += ((o -> e)) })
        e2.objectNames.toPlainSet().foreach({ o => equivalenceClassesMap += ((o -> e)) })
    }

}

trait ObjectNameContext extends PointerContext {

    private var objectNames: ConditionalSet[ObjectName] = ConditionalSet()
    private var objectNamesScope: Map[ObjectName, Scope] = Map()
    private var objectNameAssignments: ConditionalSet[Assignment] = ConditionalSet()

    def getObjectNamesAssignments = objectNameAssignments

    def setObjectNameAssignments(oAssignments: ConditionalSet[Assignment]) = objectNameAssignments = oAssignments

    def addObjectName(scopedObjectName: ObjectName, ctx: FeatureExpr): ObjectName = {
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
        write(paramLinks.keySet.map(key => (key, paramLinks.get(key).get)).pickle.toString)
        close
    }

    def load(file: String) =
        scala.io.Source.fromFile(file).mkString.unpickle[List[(ObjectName, List[Opt[ObjectName]])]].foreach(x => paramLinks.put(x._1, x._2))
}