package de.fosd.typechef.cmodule.ccallgraph.cpointeraliasing.types

/**
  * The function pointer algorithm by Zhang uses a so called prefix set for maintaining field sensitivity.
  */
abstract class Prefix extends CFunctionAliasingStructure

case class EmptyPrefix() extends Prefix

case class OperatorPrefix(objectName: ObjectName, pointerOperator: CPointerOperator) extends Prefix

case class FieldPrefix(objectName: ObjectName, fieldPrefix: ObjectName) extends Prefix
