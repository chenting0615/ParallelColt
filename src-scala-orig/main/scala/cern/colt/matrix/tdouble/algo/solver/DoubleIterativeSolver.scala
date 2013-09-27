package cern.colt.matrix.tdouble.algo.solver

import cern.colt.matrix.tdouble.StrideMatrix1D
import cern.colt.matrix.tdouble.StrideMatrix2D
import cern.colt.matrix.tdouble.algo.solver.preconditioner.DoublePreconditioner
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Iterative linear solver. Solves <code>Ax=b</code> for <code>x</code>, and it
 * supports preconditioning and convergence monitoring.
 */
trait DoubleIterativeSolver {

  /**
   * Solves the given problem, writing result into the vector.
   *
   * @param A
   *            Matrix of the problem
   * @param b
   *            Right hand side
   * @param x
   *            Solution is stored here. Also used as initial guess
   * @return The solution vector x
   */
  def solve(A: StrideMatrix2D, b: StrideMatrix1D, x: StrideMatrix1D): StrideMatrix1D

  /**
   * Sets preconditioner
   *
   * @param M
   *            Preconditioner to use
   */
  def setPreconditioner(M: DoublePreconditioner): Unit

  /**
   * Gets preconditioner
   *
   * @return Current preconditioner
   */
  def getPreconditioner(): DoublePreconditioner

  /**
   * Sets iteration monitor
   *
   * @param iter
   *            Iteration monitor
   */
  def setIterationMonitor(iter: DoubleIterationMonitor): Unit

  /**
   * Gets the iteration monitor
   *
   * @return Current iteration monitor
   */
  def getIterationMonitor(): DoubleIterationMonitor
}
