package de.fosd.typechef.cpointeranalysis

import de.fosd.typechef.parser.c.AST

trait PointerContext extends Serializable {
  type ObjectName = String
  type Scope = String
  type Assignment = (ObjectName, ObjectName)
  type FunctionName = String

  // source, target
  type FunctionCall = (String, String)

  // name, kind, source code line
  type FunctionDefinition = (String, String, Int)

  def applyOperator(operator: String, objectName: String): String = {
    ObjectNameOperator.withName(operator) match {
      // suffix operators
      case ObjectNameOperator.StructAccess | ObjectNameOperator.StructPointerAccess => objectName + operator
      // prefix operators
      case ObjectNameOperator.PointerCreation | ObjectNameOperator.PointerDereference => operator + objectName
    }
  }

  def unscope(scopedObjectName: String): String = {
    val unescopedObjectName = scopedObjectName.replaceFirst("[a-zA-Z0-9_]+?\\§[a-zA-Z0-9_]+?\\$", "")
    assert(!unescopedObjectName.contains("$"))
    unescopedObjectName
  }

  def parenthesize(objName: String) : String = {
    if (ObjectNameOperator.values.toList.map({ op => op.toString }).exists(objName.contains)) "(" + objName + ")"
    else objName
  }

  def extractFilename(ast: AST): String = {
    val default = "NOFILENAME"
    val regex = """^(([^/]+/)*)(([^/.]+)\..+)""".r
    ast.getFile.getOrElse(default) match {
      case regex(m1, m2, m3, m4) => m4
      case _ => default
    }
  }
}

object ObjectNameOperator extends Enumeration with PointerContext {
  type Operator = Value
  val StructPointerAccess = Value("->")
  val PointerDereference = Value("*")
  val PointerCreation = Value("&")
  val StructAccess = Value(".")
  val FunctionCall = Value("()")
  val ArrayAccess = Value("[]")

  private val regex = "^[a-zA-Z0-9_$§]+"
  def getField(objectNameWithField: ObjectName): String = objectNameWithField.replaceFirst(regex, "")
  def removeFields(objectNameWithField: ObjectName) = regex.r.findPrefixOf(objectNameWithField)
}
