package cern.colt.matrix.tdouble.algo.solver

import java.util.Vector
import cern.colt.matrix.tdouble.StrideMatrix1D
import cern.colt.matrix.tdouble.StrideMatrix2D
import cern.colt.matrix.tdouble.algo.solver.preconditioner.DoubleIdentity
import cern.colt.matrix.tdouble.algo.solver.preconditioner.DoublePreconditioner
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Partial implementation of an iterative solver
 */
abstract class AbstractDoubleIterativeSolver extends DoubleIterativeSolver {

  /**
   * Preconditioner to use
   */
  protected var M: DoublePreconditioner = new DoubleIdentity()

  /**
   * Iteration monitor
   */
  protected var iter: DoubleIterationMonitor = new DefaultDoubleIterationMonitor()

  def setPreconditioner(M: DoublePreconditioner) {
    this.M = M
  }

  def getPreconditioner(): DoublePreconditioner = M

  def getIterationMonitor(): DoubleIterationMonitor = iter

  def setIterationMonitor(iter: DoubleIterationMonitor) {
    this.iter = iter
  }

  /**
   * Checks sizes of input data for {@link #solve(Matrix, Vector, Vector)}.
   * Throws an exception if the sizes does not match.
   */
  protected def checkSizes(A: StrideMatrix2D, b: StrideMatrix1D, x: StrideMatrix1D) {
    if (A.rows() != A.columns()) throw new IllegalArgumentException("A is not square")
    if (b.size != A.rows()) throw new IllegalArgumentException("b.size() != A.rows()")
    if (b.size != x.size) throw new IllegalArgumentException("b.size() != x.size()")
  }
}
