package de.fosd.typechef.cpointeranalysis

trait PointerContext {
  type ObjectName = String
  type Scope = String
  type Assignment = (ObjectName, ObjectName)
  type FunctionName = String

  // source, target
  type FunctionCall = (String, String)

  // name, kind, source code line
  type FunctionDef = (String, String, Int)
}

object ObjectNameOperator extends Enumeration with PointerContext {
  type Operator = Value
  val StructPointerAccess = Value("->")
  val PointerDereference = Value("*")
  val PointerCreation = Value("&")
  val StructAccess = Value(".")
  val FunctionCall = Value("()")
  val ArrayAccess = Value("[]")

  def applyOperator(operator: String, objectName: String): String = {
    ObjectNameOperator.withName(operator) match {
      // suffix operators
      case ObjectNameOperator.StructAccess | ObjectNameOperator.StructPointerAccess => objectName + operator
      // prefix operators
      case ObjectNameOperator.PointerCreation | ObjectNameOperator.PointerDereference => operator + objectName
    }
  }

  def unscope(scopedObjectName: String): String = {
    val unescopedObjectName = scopedObjectName.replaceFirst("[a-zA-Z0-9_]+?\\$", "")
    assert(!unescopedObjectName.contains("$"))
    unescopedObjectName
  }

  def getField(objectNameWithField: ObjectName): String = objectNameWithField.replaceFirst("[a-zA-Z0-9_$]+", "")


  def parenthesize(objName: String) : String = {
    if (ObjectNameOperator.values.toList.map({ op => op.toString }).exists(objName.contains)) "(" + objName + ")"
    else objName
  }
}
