package cern.jet.stat

import Utils._
//remove if not needed
import scala.collection.JavaConversions._

object Utils {

  /**
   * Similar to Math.ceil(value), but adjusts small numerical rounding errors
   * +- epsilon.
   */
  def epsilonCeiling(value: Double): Long = {
    val epsilon = 0.0000001
    Math.ceil(value - epsilon).toLong
  }
}

/**
 * Holds some utility methods shared by different quantile finding
 * implementations.
 */
class Utils protected () {

  throw new RuntimeException("Non instantiable")
}
