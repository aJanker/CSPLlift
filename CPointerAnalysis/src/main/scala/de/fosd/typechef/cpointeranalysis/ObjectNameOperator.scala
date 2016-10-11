package de.fosd.typechef.cpointeranalysis

import java.util.regex.Pattern

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

    def unapplyPointer(operator: String): String =
        operator.replaceAll(Pattern.quote(ObjectNameOperator.PointerCreation.toString), "").replaceAll(Pattern.quote(ObjectNameOperator.PointerDereference.toString), "")

    def unscopeMethodName(scopedObjectName: String): String = scopedObjectName.substring(scopedObjectName.indexOf('§') + 1, scopedObjectName.indexOf('$'))

    def unscopeName(scopedObjectName: String): String = {
        val unescopedObjectName = scopedObjectName.replaceFirst("[a-zA-Z0-9_]+?\\§[a-zA-Z0-9_]+?\\$", "")
        assert(!unescopedObjectName.contains("$"))
        unescopedObjectName
    }

    def haveSameScope(scopedObjectName: String, otherScopedObjectName: String) : Boolean = unscopeFileAndMethod(scopedObjectName).equalsIgnoreCase(unscopeFileAndMethod(otherScopedObjectName))

    def unscopeFileAndMethod(scopedObjectName: String): String = scopedObjectName.substring(0, scopedObjectName.indexOf('$') + 1)

    def parenthesize(objName: String): String = {
        if (ObjectNameOperator.values.toList.map({ op => op.toString }).exists(objName.contains)) "(" + objName + ")"
        else objName
    }

    def unparenthesize(objName: String): String = {
        val matchingGroup = "^.*\\((.*)\\).*$".r.findFirstMatchIn(objName)
        if (matchingGroup.isDefined) matchingGroup.get.group(1)
        else objName
    }

    def extractFilename(ast: AST, default: String = "NOFILENAME"): String = extractFilenameS(ast.getFile.getOrElse(default))

    def extractFilenameS(str: String, default: String = "NOFILENAME"): String = {
        val regex = """^(([^/]+/)*)(([^/.]+)\..+)""".r
        val filePrefix = "file "
        str match {
            case regex(m1, m2, m3, m4) => if (m4.startsWith(filePrefix)) m4.substring(filePrefix.length) else m4
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

    private lazy val regex = "^[a-zA-Z0-9_$§]+"
    private lazy val regexPattern = regex.r
    private lazy val regexRemoveBraces = "\\(([A-Za-z0-9_\\-\\>]+)\\)".r

    def splitParentAndField(objectNameWithField: ObjectName) : Option[(String, String)] = {
        val lastIndexOfSPA = objectNameWithField.lastIndexOf(StructPointerAccess.toString)
        val lastIndexOfSA = objectNameWithField.lastIndexOf(StructPointerAccess.toString)

        val lastIndex = math.min(lastIndexOfSA, lastIndexOfSPA)

        if (lastIndex == -1) return None

        Some(unparenthesize(unscopeName(objectNameWithField.substring(0, lastIndex))), objectNameWithField.substring(lastIndex, objectNameWithField.length))
    }
    def getField(objectNameWithField: ObjectName): String = objectNameWithField.replaceFirst(regex, "")
    def removeFields(objectNameWithField: ObjectName) = regexPattern.findPrefixOf(objectNameWithField)
    def containsFieldPointerAccess(name: ObjectName): Boolean = unscopeName(name).contains(StructPointerAccess.toString)
    def removeBracesFromStructPointerAccess(name: ObjectName): String =
        name match {
            case regexRemoveBraces(m) => m
            case _ => name
        }

}
