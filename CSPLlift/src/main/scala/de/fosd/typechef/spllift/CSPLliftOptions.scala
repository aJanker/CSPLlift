package de.fosd.typechef.spllift

/**
  *
  * Options for using SPLLift with TypeChef
  *
  */
trait CSPLliftOptions {

  def getModuleInterface: Option[String] // TODO Type

  def getPointerInterface: Option[String] // TODO Type

  def getEntryNames: List[String] = List("main")
}

object DefaultCSPLliftOptions extends CSPLliftOptions {
  override def getModuleInterface: Option[String] = None

  override def getPointerInterface: Option[String] = None
}