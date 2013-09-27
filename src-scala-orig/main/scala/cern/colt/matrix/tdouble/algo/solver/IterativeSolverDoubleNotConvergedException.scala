package cern.colt.matrix.tdouble.algo.solver

import scala.reflect.{BeanProperty, BooleanBeanProperty}
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Exception for lack of convergence in a linear problem. Contains the final
 * computed residual.
 */
@SerialVersionUID(1L)
class IterativeSolverDoubleNotConvergedException(reason: Reason, message: String, iter: DoubleIterationMonitor)
    extends DoubleNotConvergedException(reason, message) {

  /**
   * Iteration count when this exception was thrown
   */
  @BeanProperty
  var iterations: Int = iter.iterations()

  /**
   * Final residual
   */
  private var r: Double = iter.residual()

  /**
   * Constructor for IterativeSolverNotConvergedException
   *
   * @param reason
   *            Reason for this exception
   * @param iter
   *            Associated iteration monitor, for extracting residual and
   *            iteration number
   */
  def this(reason: Reason, iter: DoubleIterationMonitor) {
    super(reason)
    this.r = iter.residual()
    this.iterations = iter.iterations()
  }

  /**
   * Returns final computed residual
   */
  def getResidual(): Double = r
}
