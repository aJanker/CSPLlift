package de.fosd.typechef.cmodule.ccallgraph.cpointeraliasing.types

/**
  * Symmetric representation of an assignment of two objectnames.
  */
case class Assignment(left: ObjectName, right: ObjectName) extends CFunctionAliasingStructure


