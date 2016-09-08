package de.fosd.typechef.spllift

import de.fosd.typechef.featureexpr.SingleFeatureExpr

/**
  *
  * Options for using SPLLift with TypeChef
  *
  */
trait CInterCFGOptions {

  def getModuleInterfacePath: Option[String]

  def getConfiguration: Option[Set[String]] = None

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

class ConfigurationBasedCInterCFGOptions(configuration : Set[SingleFeatureExpr], moduleInterfacePath : Option[String] = None) extends DefaultCInterCFGOptions(moduleInterfacePath) {
  override def getConfiguration: Option[Set[String]] = Some(configuration.map(_.feature))
}