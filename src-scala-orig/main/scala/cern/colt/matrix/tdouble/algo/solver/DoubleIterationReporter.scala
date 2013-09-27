package cern.colt.matrix.tdouble.algo.solver

import cern.colt.matrix.tdouble.StrideMatrix1D
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Reports on the progress of an iterative solver
 */
trait DoubleIterationReporter {

  /**
   * Registers current information
   *
   * @param r
   *            Current residual norm
   * @param x
   *            Current state vector
   * @param i
   *            Current iteration number
   */
  def monitor(r: Double, x: StrideMatrix1D, i: Int): Unit

  /**
   * Registers current information
   *
   * @param r
   *            Current residual norm
   * @param i
   *            Current iteration number
   */
  def monitor(r: Double, i: Int): Unit
}
