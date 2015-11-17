package de.fosd.typechef.cpointeranalysis

import de.fosd.typechef.conditional.{Opt, ConditionalSet}
import de.fosd.typechef.featureexpr.FeatureExpr

import scala.collection.immutable.Map

// TODO Clean up and functional
class CPointerAnalysisContext() extends PointerContext with ObjectNameContext with EquivalenceContext with FunctionContext {

}

trait EquivalenceContext extends PointerContext {
  private var equivalenceClassesPrefixSets: Set[(ObjectName, ObjectName)] = Set()
  private var equivalenceClassesMap: Map[ObjectName, EquivalenceClass] = Map()
  private var equivalenceClasses: Set[EquivalenceClass] = Set()

  def addPrefixSet(o1: ObjectName, o2: ObjectName) = {
    equivalenceClassesPrefixSets += ((o1, o2))
  }

  def getEquivalenceClassesPrefixSets: Set[(ObjectName, ObjectName)] = equivalenceClassesPrefixSets

  def getEquivalenceClassesMap: Map[ObjectName, EquivalenceClass] = equivalenceClassesMap

  def getEquivalenceClasses: Set[EquivalenceClass] = equivalenceClasses


  def showPointerEquivalenceClasses() = {
    equivalenceClasses.map(println)
  }

  def initEquivalenceClasses(objectNames: ObjectNameContext): Unit = {
    val r = objectNames.getObjectNames.keys.foldLeft((getEquivalenceClassesMap, getEquivalenceClasses))( (e, k) => {
      val ec = new EquivalenceClass(ConditionalSet(k, objectNames.getObjectNames.get(k)), ConditionalSet())
      (e._1 + (k -> ec), e._2 + ec)
    })

    equivalenceClassesMap = r._1
    equivalenceClasses = r._2
  }

  def createInitialPrefixSets(equivalenceClassesPrefixSets : Set[(ObjectName, ObjectName)] = equivalenceClassesPrefixSets, equivalenceClassesMap : Map[ObjectName, EquivalenceClass] = equivalenceClassesMap): Unit = {
    var eqClassObjectO: EquivalenceClass = EquivalenceClass()
    var eqClassObjectO1: EquivalenceClass = EquivalenceClass()

    // generate cross product of all object names
    for ((o, o1) <- equivalenceClassesPrefixSets) {
      val findObjectNameO = equivalenceClassesMap.get(o)
      val findObjectNameO1 = equivalenceClassesMap.get(o1)

      if (findObjectNameO.isDefined && findObjectNameO1.isDefined) {
        eqClassObjectO = findObjectNameO.get
        eqClassObjectO1 = findObjectNameO1.get

        val eqClassObjectOFeatExpr = eqClassObjectO.objectNames.get(o)
        val eqClassObjectO1FeatExpr = eqClassObjectO1.objectNames.get(o1)

        val uo = ObjectNameOperator.unscope(o)
        val uo1 = ObjectNameOperator.unscope(o1)

        // pointer creation operator
        if ((uo.equals(ObjectNameOperator.applyOperator(ObjectNameOperator.PointerCreation.toString, uo1))) || uo.equals(ObjectNameOperator.applyOperator(ObjectNameOperator.PointerCreation.toString, ObjectNameOperator.parenthesize(uo1)))) {
          // add * edge from o to o1 (removing the pointer creation effect with a dereferencing operator)
          eqClassObjectO.addPrefix((ObjectNameOperator.PointerDereference.toString, o1), eqClassObjectOFeatExpr and eqClassObjectO1FeatExpr)

          // pointer dereference operator
        } else if ((uo.equals(ObjectNameOperator.applyOperator(ObjectNameOperator.PointerDereference.toString, uo1)) || uo.equals(ObjectNameOperator.applyOperator(ObjectNameOperator.PointerDereference.toString, ObjectNameOperator.parenthesize(uo1))))) {
          // add * edge from o1 to o
          eqClassObjectO1.addPrefix((ObjectNameOperator.PointerDereference.toString, o), eqClassObjectOFeatExpr and eqClassObjectO1FeatExpr)

          // struct dot access operator
        } else if ((uo.startsWith(ObjectNameOperator.applyOperator(ObjectNameOperator.StructAccess.toString, uo1)) || uo.startsWith(ObjectNameOperator.applyOperator(ObjectNameOperator.StructAccess.toString, ObjectNameOperator.parenthesize(uo1))))) {
          val objectNameFields = (uo.take(uo.lastIndexOf(ObjectNameOperator.StructAccess.toString)), uo.drop(uo.lastIndexOf(ObjectNameOperator.StructAccess.toString) + ObjectNameOperator.StructAccess.toString.length))
          val eqClassObjectOPartial = equivalenceClassesMap.get(objectNameFields._1)

          // add field edge from o to o1
          if (eqClassObjectOPartial.isDefined) {
            eqClassObjectOPartial.get.addPrefix((objectNameFields._2, o), eqClassObjectOFeatExpr and eqClassObjectO1FeatExpr)

          }
          // struct pointer access operator  (dereference + dot)
        } else if ((uo.startsWith(ObjectNameOperator.applyOperator(ObjectNameOperator.StructPointerAccess.toString, uo1)) || uo.startsWith(ObjectNameOperator.applyOperator(ObjectNameOperator.StructPointerAccess.toString, ObjectNameOperator.parenthesize(uo1))))) {
          val objectNameFields = (uo.take(uo.lastIndexOf(ObjectNameOperator.StructPointerAccess.toString)), uo.drop(uo.lastIndexOf(ObjectNameOperator.StructPointerAccess.toString) + ObjectNameOperator.StructPointerAccess.toString.length))
          val eqClassObjectOPartial = equivalenceClassesMap.get(ObjectNameOperator.applyOperator(ObjectNameOperator.PointerDereference.toString, objectNameFields._1))

          // add field edge from o to o1
          if (eqClassObjectOPartial.isDefined) {
            eqClassObjectOPartial.get.addPrefix((objectNameFields._2, o), eqClassObjectOPartial.get.objectNames.get(o) and eqClassObjectO1FeatExpr)
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

  def calculateLocalEquivalenceClasses(objectNames: ObjectNameContext) = {
    for ((assignee, assignor) <- objectNames.getObjectNamesAssignments.toPlainSet()) {
      mergeEquivalenceClasses(assignee, assignor)
    }
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
      } else newPrefixSet +=((a, o), e2.prefixes().get((a, o)))
    }
    // add new equivalence class and delete merged ones
    equivalenceClasses -=(e1, e2)
    val e = new EquivalenceClass(newObjectNamesSet.objectNames, newPrefixSet)
    equivalenceClasses += e
    e1.objectNames.toPlainSet().foreach({ o => equivalenceClassesMap += ((o -> e)) })
    e2.objectNames.toPlainSet().foreach({ o => equivalenceClassesMap += ((o -> e)) })
  }

  def find(objectName: ObjectName): Option[EquivalenceClass] = {
    val eqClass1: Option[EquivalenceClass] = equivalenceClassesMap.get(objectName)
    val eqClass2: Option[EquivalenceClass] = equivalenceClassesMap.get(ObjectNameOperator.parenthesize(objectName))

    if (eqClass1.isDefined) eqClass1 else if (eqClass2.isDefined) eqClass2 else None
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
    objectNames = objectNames +(scopedObjectName, ctx)
    scopedObjectName
  }

  def addObjectNameAssignment(assignment: Assignment, ctx: FeatureExpr) = {
    objectNameAssignments += (assignment, ctx)
  }

  def applyScope(objectName: ObjectName, currentScope: Scope): ObjectName = {
    // format object name with scope
    val scopedObjectName = currentScope + "$" + objectName

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

  def showAssignments() = {
    println("*** Assignments: %s".format(objectNameAssignments))
  }

  def showExtractedObjectNames() = {
    println(objectNames)
  }

  // TODO copy
}

trait FunctionContext extends PointerContext {

  var functionDefs: ConditionalSet[FunctionDef] = ConditionalSet()
  var functionDefReturns: Map[FunctionName, ConditionalSet[ObjectName]] = Map()
  var functionDefParameters: Map[FunctionName, List[Opt[ObjectName]]] = Map()
  var functionCallParamList: List[Opt[ObjectName]] = List()

  // function calls and function call parameters
  var functionCalls: ConditionalSet[FunctionCall] = ConditionalSet()
  var functionCallParameters: List[(FunctionName, List[Opt[ObjectName]])] = List()


  def addFuncMappings(fDefs: ConditionalSet[FunctionDef], fDefParameters: Map[FunctionName, List[Opt[ObjectName]]], fReturns : Map[FunctionName, ConditionalSet[ObjectName]],
                      fCalls: ConditionalSet[FunctionCall], fCallParameters: List[(FunctionName, List[Opt[ObjectName]])]) = {
    functionDefs = fDefs
    functionDefParameters = fDefParameters
    functionCalls = fCalls
    functionCallParameters = fCallParameters
    functionDefReturns = fReturns
  }

  def showFunctionDefReturns() = {
    println(functionDefReturns)
  }

  def showFunctionDefs() = {
    println("*** Function defs (%d): %s".format(functionDefs.toPlainSet().size, functionDefs.toPlainSetWithConditionals()))
  }

  def showFunctionDefsParameters() = {
    println(functionDefParameters)
  }

  //TODO copy

}