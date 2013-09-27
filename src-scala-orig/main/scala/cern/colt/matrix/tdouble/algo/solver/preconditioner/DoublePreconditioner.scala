package cern.colt.matrix.tdouble.algo.solver.preconditioner

import cern.colt.matrix.tdouble.StrideMatrix1D
import cern.colt.matrix.tdouble.StrideMatrix2D
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Preconditioner interface. Before a preconditioner is used,
 * <code>setMatrix</code> must be called
 */
trait DoublePreconditioner {

  /**
   * Solves the approximate problem with the given right hand side. Result is
   * stored in given solution vector
   *
   * @param b
   *            Right hand side of problem
   * @param x
   *            Result is stored here
   * @return x
   */
  def apply(b: StrideMatrix1D, x: StrideMatrix1D): StrideMatrix1D

  /**
   * Solves the approximate transpose problem with the given right hand side.
   * Result is stored in given solution vector
   *
   * @param b
   *            Right hand side of problem
   * @param x
   *            Result is stored here
   * @return x
   */
  def transApply(b: StrideMatrix1D, x: StrideMatrix1D): StrideMatrix1D

  /**
   * Sets the operator matrix for the preconditioner. This method must be
   * called before a preconditioner is used by an iterative solver
   *
   * @param A
   *            Matrix to setup the preconditioner for. Not modified
   */
  def setMatrix(A: StrideMatrix2D): Unit
}
