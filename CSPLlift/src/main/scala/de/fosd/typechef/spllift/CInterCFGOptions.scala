package de.fosd.typechef.spllift

/**
  *
  * Options for using SPLLift with TypeChef
  *
  */
trait CInterCFGOptions {

  def getModuleInterface: Option[String] // TODO Type

  def getPointerInterface: Option[String] // TODO Type

  def getGraphEntryFunctionNames: List[String] = List("main")

  def pseudoVisitingSystemLibFunctions : Boolean
}

object DefaultCInterCFGOptions extends CInterCFGOptions {
  override def getModuleInterface: Option[String] = None

  override def getPointerInterface: Option[String] = None

  override def pseudoVisitingSystemLibFunctions = true
}