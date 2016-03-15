package de.fosd.typechef.spllift

/**
  *
  * Options for using SPLLift with TypeChef
  *
  */
trait CInterCFGOptions {

  def getModuleInterface: Option[String] // TODO Type

  def getPointerInterface: Option[String] // TODO Type

  def getEntryNames: List[String] = List("main")
}

object DefaultCInterCFGOptions extends CInterCFGOptions {
  override def getModuleInterface: Option[String] = None

  override def getPointerInterface: Option[String] = None
}