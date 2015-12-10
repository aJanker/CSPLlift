package de.fosd.typechef.cpointeranalysis

/**
  * Case study specific configuration settings. // TODO Config File Parser
 */
trait CPointerAnalysisOptions {
  val linkingInterface : String
  val mainListPath : String
  val linkedObjectNames: String
}

object DefaultOpenSSLPointerAnalysisOptions extends CPointerAnalysisOptions {
  override val linkingInterface: String = "../PA-OpenSSL/CLinking.interface"
  override val mainListPath: String = "../PA-OpenSSL/mainFunctions"
  override val linkedObjectNames: String = "../PA-OpenSSL/linkedObjectNames.json"
}
