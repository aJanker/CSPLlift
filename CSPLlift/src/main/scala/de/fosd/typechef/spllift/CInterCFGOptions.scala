package de.fosd.typechef.spllift

/**
  *
  * Options for using SPLLift with TypeChef
  *
  */
trait CInterCFGOptions {

  def getModuleInterfacePath: Option[String]

  def getGraphEntryFunctionNames: List[String] = List("main")

  /*
   * If enabled, now warnings for discovered type errors are shown.
   */
  def silentTypeCheck : Boolean = true

  /*
   * Sets the option if in our analysis system functions should be visited or ignored.
   */
  def pseudoVisitingSystemLibFunctions : Boolean
}

class DefaultCInterCFGOptions(moduleInterfacePath : Option[String] = None) extends CInterCFGOptions {
  override def getModuleInterfacePath: Option[String] = moduleInterfacePath

  override def pseudoVisitingSystemLibFunctions = true
}