package cern.colt.matrix.tdouble.algo.solver

import cern.colt.matrix.Norm
import cern.colt.matrix.tdouble.StrideMatrix1D
import cern.colt.matrix.tdouble.algo.DenseDoubleAlgebra
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Partial implementation of an iteration reporter
 */
abstract class AbstractDoubleIterationMonitor extends DoubleIterationMonitor {

  /**
   * Iteration number
   */
  protected var iter: Int = _

  /**
   * Vector-norm
   */
  protected var normType: Norm = Norm.Two

  /**
   * Iteration reporter
   */
  protected var reporter: DoubleIterationReporter = new NoDoubleIterationReporter()

  /**
   * Current residual
   */
  protected var residual: Double = _

  def setFirst() {
    iter = 0
  }

  def isFirst(): Boolean = iter == 0

  def next() {
    iter += 1
  }

  def iterations(): Int = iter

  def converged(r: StrideMatrix1D, x: StrideMatrix1D): Boolean = {
    converged(DenseDoubleAlgebra.DEFAULT.norm(r, normType), x)
  }

  def converged(r: Double, x: StrideMatrix1D): Boolean = {
    reporter.monitor(r, x, iter)
    this.residual = r
    convergedI(r, x)
  }

  def converged(r: Double): Boolean = {
    reporter.monitor(r, iter)
    this.residual = r
    convergedI(r)
  }

  protected def convergedI(r: Double, x: StrideMatrix1D): Boolean

  protected def convergedI(r: Double): Boolean

  def converged(r: StrideMatrix1D): Boolean = {
    converged(DenseDoubleAlgebra.DEFAULT.norm(r, normType))
  }

  def getNormType(): Norm = normType

  def setNormType(normType: Norm) {
    this.normType = normType
  }

  def getIterationReporter(): DoubleIterationReporter = reporter

  def setIterationReporter(monitor: DoubleIterationReporter) {
    this.reporter = monitor
  }

  def residual(): Double = residual
}
