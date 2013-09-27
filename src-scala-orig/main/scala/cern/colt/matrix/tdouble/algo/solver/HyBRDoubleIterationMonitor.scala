package cern.colt.matrix.tdouble.algo.solver

import cern.colt.matrix.tdouble.StrideMatrix1D
import HyBRDoubleIterationMonitor._
//remove if not needed
import scala.collection.JavaConversions._

object HyBRDoubleIterationMonitor {

  object HyBRStoppingCondition extends Enumeration {

    val FLAT_GCV_CURVE = new HyBRStoppingCondition()

    val MIN_OF_GCV_CURVE_WITHIN_WINDOW_OF_4_ITERATIONS = new HyBRStoppingCondition()

    val PERFORMED_MAX_NUMBER_OF_ITERATIONS = new HyBRStoppingCondition()

    class HyBRStoppingCondition extends Val

    implicit def convertValue(v: Value): HyBRStoppingCondition = v.asInstanceOf[HyBRStoppingCondition]
  }
}

class HyBRDoubleIterationMonitor extends AbstractDoubleIterationMonitor {

  protected var stoppingCondition: HyBRStoppingCondition = HyBRStoppingCondition.PERFORMED_MAX_NUMBER_OF_ITERATIONS

  protected var maxIter: Int = 100

  protected var dtol: Double = 1e+5

  protected var initR: Double = _

  protected var regularizationParameter: Double = _

  /**
   * Constructor for HyBRDoubleIterationMonitor
   *
   * @param maxIter
   *            Maximum number of iterations
   * @param dtol
   *            Relative divergence tolerance (to initial residual)
   */
  def this(maxIter: Int, dtol: Double) {
    this()
    this.maxIter = maxIter
    this.dtol = dtol
    this.stoppingCondition = HyBRStoppingCondition.PERFORMED_MAX_NUMBER_OF_ITERATIONS
  }

  def converged(r: Double, x: StrideMatrix1D): Boolean = {
    if (!isFirst) {
      reporter.monitor(r, x, iter)
    }
    this.residual = r
    convergedI(r, x)
  }

  def converged(r: Double): Boolean = {
    if (!isFirst) {
      reporter.monitor(r, iter)
    }
    this.residual = r
    convergedI(r)
  }

  protected def convergedI(r: Double): Boolean = {
    if (isFirst) initR = r
    if (initR != -1.0) {
      if (r > dtol * initR) throw new IterativeSolverDoubleNotConvergedException(DoubleNotConvergedException.Reason.Divergence,
        this)
    }
    if (iter >= (maxIter + 1)) throw new IterativeSolverDoubleNotConvergedException(DoubleNotConvergedException.Reason.Iterations,
      this)
    if (Double.isNaN(r)) throw new IterativeSolverDoubleNotConvergedException(DoubleNotConvergedException.Reason.Divergence,
      this)
    false
  }

  protected def convergedI(r: Double, x: StrideMatrix1D): Boolean = convergedI(r)

  def getMaxIterations(): Int = maxIter

  def setMaxIterations(maxIter: Int) {
    this.maxIter = maxIter
  }

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

  /**
   * Returns the regularization parameter
   *
   * @return regularization parameter
   */
  def getRegularizationParameter(): Double = regularizationParameter

  /**
   * Sets the regularization parameter
   *
   * @param regularizationParameter
   *            regularization parameter
   */
  def setRegularizationParameter(regularizationParameter: Double) {
    this.regularizationParameter = regularizationParameter
  }

  /**
   * Sets the stopping condition
   *
   * @param stoppingCondition
   *            stopping condition
   */
  def setStoppingCondition(stoppingCondition: HyBRStoppingCondition) {
    this.stoppingCondition = stoppingCondition
  }

  /**
   * Returns the stopping condition
   *
   * @return stopping condition
   */
  def getStoppingCondition(): HyBRStoppingCondition = stoppingCondition

  def iterations(): Int = Math.min(iter, maxIter)
}
