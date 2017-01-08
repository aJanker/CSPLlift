package de.fosd.typechef.cmodule.ccallgraph.cpointeraliasing.types

import de.fosd.typechef.cmodule.{CModule, CModuleNamings}

/**
  * The algorithm works on so called object names. This data-structure represents such object names.
  */
abstract class ObjectName extends Product with Serializable with CModuleNamings {

    /**
      * Returns true if objectname represents a function.
      */
    def isKnownAsFunction(cModuleEnv: CModule): Boolean = getPlainObjectName.isKnownAsFunction(cModuleEnv)

    /**
      * Retrieves the recursive size of an object name.
      */
    def size(): Int

    /**
      * Retrieves the low level base name.
      */
    def getBaseName: String

    /**
      * Retrieves the low level base objectname.
      */
    def getPlainObjectName: PlainObjectName
}

object ObjectName {
    /**
      * Removes all fields from the objectname except its name. Requiered for clean field matching.
      */
    def removePreviousScopingFromObjectName(objectName: ObjectName): ObjectName =
        objectName match {
            case p: PlainObjectName => p.removeScoping()
            case p: PointerObjectName => p.copy(name = removePreviousScopingFromObjectName(p.name))
            case f: FieldObjectName => f.copy(name = removePreviousScopingFromObjectName(f.name))
            case _ => objectName
        }
}

case class FieldObjectName(name: ObjectName, field: ObjectName) extends ObjectName {
    override def toString: String = {
        "(" + name.toString + "->" + field.toString + ")"
    }

    override def size(): Int = 1 + name.size()

    override def getBaseName: String = name.getBaseName

    override def getPlainObjectName: PlainObjectName = name.getPlainObjectName
}

case class PointerObjectName(name: ObjectName, pointerOperator: CPointerOperator) extends ObjectName {
    override def toString: String = {
        val prefix = pointerOperator match {
            case c: CPointerPrefixOperator => c.toText
            case _ => ""
        }
        val suffix = pointerOperator match {
            case c: CPointerSuffixOperator => c.toText
            case _ => ""
        }

        "(" + prefix + name.toString + suffix + ")"
    }

    override def size(): Int = 1 + name.size()

    override def getBaseName: String = name.getBaseName

    override def getPlainObjectName: PlainObjectName = name.getPlainObjectName
}

case class PlainObjectName private(name: String, method: Option[String]) extends ObjectName {
    // ignored by case equality
    def getFile: Option[String] = None

    // ignored by case equality
    def getScope: Option[Int] = None

    override def toString: String = "(" + (if (method.isDefined) method.get + "/" else "") + name + ")"

    override def size(): Int = 1

    override def getBaseName: String = name

    override def getPlainObjectName: PlainObjectName = this

    def removeScoping(): PlainObjectName = new PlainObjectName(name, None)

    /**
      * Returns true if objectname represents a function.
      */
    override def isKnownAsFunction(cModuleEnv: CModule): Boolean =
        if (!(method.getOrElse("").equalsIgnoreCase(GLOBAL_DEFINITION_NAME) || method.getOrElse("").equalsIgnoreCase(name))) false
        else cModuleEnv.getFunctionDef(name).contains(getFile.getOrElse(""))
}

object PlainObjectName {
    def apply(name: String, method: Option[String] = None, file: Option[String] = None, scope: Option[Int] = None): PlainObjectName = new PlainObjectName(name, method) {
        override def getFile: Option[String] = file

        override def getScope: Option[Int] = scope
    }
}