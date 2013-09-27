package cern.colt.matrix.tdouble.algo.solver

import cern.colt.matrix.tdouble.StrideMatrix1D
//remove if not needed
import scala.collection.JavaConversions._

/**
 * An iteration reporter which does nothing.
 */
class NoDoubleIterationReporter extends DoubleIterationReporter {

  def monitor(r: Double, i: Int) {
  }

  def monitor(r: Double, x: StrideMatrix1D, i: Int) {
  }
}
