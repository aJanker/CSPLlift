package de.fosd.typechef.cmodule.ccallgraph.cpointeraliasing.types

/**
  * Default function aliasing operator.
  */
abstract class CPointerOperator(prefixOperator: Boolean) extends Product with Serializable {
    val toText: String

    def isPrefixOperator: Boolean = prefixOperator

    def isSuffixOperator: Boolean = !prefixOperator
}

/**
  * Pointer operator is a prefix of an objectname.
  */
abstract class CPointerPrefixOperator extends CPointerOperator(prefixOperator = true)

/**
  * Pointer operator is a suffix of an objectname.
  */
abstract class CPointerSuffixOperator extends CPointerOperator(prefixOperator = false)

/**
  * Pointer dereference operator.
  */
case class PointerDerefOperator() extends CPointerPrefixOperator {
    override val toText: String = "*"
}

/**
  * Struct pointer dereference operator.
  */
case class StructPointerDerefOperator() extends CPointerSuffixOperator {
    override val toText: String = "->"
}

/**
  * Pointer creation operator
  */
case class PointerCreationOperator() extends CPointerPrefixOperator {
    override val toText: String = "&"
}
