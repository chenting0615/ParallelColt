package cern.colt.matrix.tdouble.algo.solver

import cern.colt.matrix.tdouble.StrideMatrix1D
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Default iteration monitor. This tester checks declares convergence if the
 * absolute value of the residual norm is sufficiently small, or if the relative
 * decrease is small. Divergence is decleared if too many iterations are spent,
 * or the residual has grown too much. NaNs will also cause divergence to be
 * flagged.
 */
class DefaultDoubleIterationMonitor(var maxIter: Int,
    var rtol: Double,
    var atol: Double,
    var dtol: Double) extends AbstractDoubleIterationMonitor {

  /**
   * Initial residual
   */
  var initR: Double = _

  /**
   * Constructor for DefaultIterationMonitor. Default is 100000 iterations at
   * most, relative tolerance of 1e-5, absolute tolerance of 1e-50 and a
   * divergence tolerance of 1e+5.
   */
  def this() {
    this()
    this.maxIter = 100000
    this.rtol = 1e-5
    this.atol = 1e-50
    this.dtol = 1e+5
  }

  /**
   * Sets maximum number of iterations to permit
   *
   * @param maxIter
   *            Maximum number of iterations
   */
  def setMaxIterations(maxIter: Int) {
    this.maxIter = maxIter
  }

  /**
   * Returns maximum number of iterations to permit
   */
  def getMaxIterations(): Int = this.maxIter

  /**
   * Sets the relative convergence tolerance
   *
   * @param rtol
   *            relative convergence tolerance (to initial residual)
   */
  def setRelativeTolerance(rtol: Double) {
    this.rtol = rtol
  }

  /**
   * Returns the relative convergence tolerance
   *
   * @return relative convergence tolerance (to initial residual)
   */
  def getRelativeTolerance(): Double = rtol

  /**
   * Sets the absolute convergence tolerance
   *
   * @param atol
   *            absolute convergence tolerance
   */
  def setAbsoluteTolerance(atol: Double) {
    this.atol = atol
  }

  /**
   * Returns the absolute convergence tolerance
   *
   * @return absolute convergence tolerance
   */
  def getAbsoluteTolerance(): Double = atol

  /**
   * Sets the relative divergence tolerance
   *
   * @param dtol
   *            relative divergence tolerance (to initial residual)
   */
  def setDivergenceTolerance(dtol: Double) {
    this.dtol = dtol
  }

  /**
   * Returns the relative divergence tolerance
   *
   * @return relative divergence tolerance (to initial residual)
   */
  def getDivergenceTolerance(): Double = dtol

  protected def convergedI(r: Double): Boolean = {
    if (isFirst) initR = r
    if (r < Math.max(rtol * initR, atol)) return true
    if (r > dtol * initR) throw new IterativeSolverDoubleNotConvergedException(DoubleNotConvergedException.Reason.Divergence,
      this)
    if (iter >= maxIter) throw new IterativeSolverDoubleNotConvergedException(DoubleNotConvergedException.Reason.Iterations,
      this)
    if (Double.isNaN(r)) throw new IterativeSolverDoubleNotConvergedException(DoubleNotConvergedException.Reason.Divergence,
      this)
    false
  }

  protected def convergedI(r: Double, x: StrideMatrix1D): Boolean = convergedI(r)
}
