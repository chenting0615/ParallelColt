package cern.colt.matrix.tdouble.algo.solver

import cern.colt.matrix.Norm
import cern.colt.matrix.tdouble.StrideMatrix1D
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Monitors the iterative solution process for convergence and divergence. Can
 * also report the current progress.
 */
trait DoubleIterationMonitor {

  /**
   * Resets the iteration
   */
  def setFirst(): Unit

  /**
   * Returns true for the first iteration
   */
  def isFirst(): Boolean

  /**
   * Increases iteration counter
   */
  def next(): Unit

  /**
   * Number of iterations performed
   */
  def iterations(): Int

  /**
   * Returns current residual
   */
  def residual(): Double

  /**
   * Checks for convergence
   *
   * @param r
   *            Residual-vector
   * @param x
   *            State-vector
   * @return True if converged
   */
  def converged(r: StrideMatrix1D, x: StrideMatrix1D): Boolean

  /**
   * Checks for convergence
   *
   * @param r
   *            Residual-norm
   * @param x
   *            State-vector
   * @return True if converged
   */
  def converged(r: Double, x: StrideMatrix1D): Boolean

  /**
   * Checks for convergence
   *
   * @param r
   *            Residual-norm
   * @return True if converged
   */
  def converged(r: Double): Boolean

  /**
   * Checks for convergence
   *
   * @param r
   *            Residual-vector
   * @return True if converged
   */
  def converged(r: StrideMatrix1D): Boolean

  /**
   * Sets new iteration reporter
   */
  def setIterationReporter(monitor: DoubleIterationReporter): Unit

  /**
   * Returns current iteration reporter
   */
  def getIterationReporter(): DoubleIterationReporter

  /**
   * Sets the vector-norm to calculate with
   */
  def setNormType(normType: Norm): Unit

  /**
   * Returns the vector-norm in use
   */
  def getNormType(): Norm

  /**
   * Sets maximum number of iterations to permit
   *
   * @param maxIter
   *            Maximum number of iterations
   */
  def setMaxIterations(maxIter: Int): Unit

  /**
   * Returns the maximum number of iterations
   */
  def getMaxIterations(): Int
}
