package de.fosd.typechef.cpointeranalysis

/**
 * Case study specific configuration settings. Best way -> configuration file
 */
trait CPointerAnalysisOptions {
  val linkingInterface : String
  val mainListPath : String
  val linkedObjectNames: String
}

object DefaultOpenSSLPointerAnalysisOptions extends CPointerAnalysisOptions {
  override val linkingInterface: String = "../casestudy/CLinking.interface"
  override val mainListPath: String = "../casestudy/mainFunctions"
  override val linkedObjectNames: String = "../casestudy/linkedObjectNames"
}
