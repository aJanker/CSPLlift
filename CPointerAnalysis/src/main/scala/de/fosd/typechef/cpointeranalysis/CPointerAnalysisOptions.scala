package de.fosd.typechef.cpointeranalysis

/**
 * Created by Andreas Janker on 27/10/15.
 */

/**
 * Case study specific configuration settings. Best way -> configuration file
 */
trait CPointerAnalysisOptions {
  val linkingInterface : String

  val eqClassFileEnding : String = ".eqc"
  val mainListPath : String


}

object DefaultOpenSSLPointerAnalysisOptions extends CPointerAnalysisOptions {
  override val linkingInterface: String = "/Users/andi/Desktop/CLinking.interface"
  override val mainListPath : String = "../mainFunctions"
}
