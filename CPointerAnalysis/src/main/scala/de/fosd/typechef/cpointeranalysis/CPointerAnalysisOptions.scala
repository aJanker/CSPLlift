package de.fosd.typechef.cpointeranalysis

/**
 * Created by Andreas Janker on 27/10/15.
 */

/**
 * Case study specific configuration settings. Best way -> configuration file
 */
trait CPointerAnalysisOptions {
  val linkingInterface : String

}

object DefaultOpenSSLOptions extends CPointerAnalysisOptions {
  override val linkingInterface: String = "not yet specified"
}
