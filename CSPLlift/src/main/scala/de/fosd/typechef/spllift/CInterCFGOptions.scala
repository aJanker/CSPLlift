package de.fosd.typechef.spllift

/**
  *
  * Options for using SPLLift with TypeChef
  *
  */
trait CInterCFGOptions {

  def getModuleInterfacePath: Option[String]

  def getGraphEntryFunctionNames: List[String] = List("main")

  def pseudoVisitingSystemLibFunctions : Boolean
}

class DefaultCInterCFGOptions(moduleInterfacePath : Option[String] = None) extends CInterCFGOptions {
  override def getModuleInterfacePath: Option[String] = moduleInterfacePath

  override def pseudoVisitingSystemLibFunctions = true
}