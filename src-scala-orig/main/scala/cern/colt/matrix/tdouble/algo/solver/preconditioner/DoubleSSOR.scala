package cern.colt.matrix.tdouble.algo.solver.preconditioner

import cern.colt.matrix.tdouble.StrideMatrix1D
import cern.colt.matrix.tdouble.StrideMatrix2D
import cern.colt.matrix.tdouble.impl.DenseMatrix1D
import cern.colt.matrix.tdouble.impl.SparseRCDoubleMatrix2D
//remove if not needed
import scala.collection.JavaConversions._

/**
 * SSOR preconditioner. Uses symmetrical sucessive overrelaxation as a
 * preconditioner. Meant for symmetrical, positive definite matrices. For best
 * performance, omega must be carefully chosen (between 0 and 2).
 */
class DoubleSSOR(val n: Int,
    val reverse: Boolean,
    omegaF: Double,
    omegaR: Double) extends DoublePreconditioner {

  /**
   * Overrelaxation parameter for the forward sweep
   */
  private var omegaF: Double = _

  /**
   * Overrelaxation parameter for the backwards sweep
   */
  private var omegaR: Double = _

  /**
   * Holds a copy of the matrix A in the compressed row format
   */
  private var F: SparseRCDoubleMatrix2D = _

  /**
   * indexes to the diagonal entries of the matrix
   */
  private val diagind = new Array[Int](n)

  /**
   * Temporary vector for holding the half-step state
   */
  private val xx = new Array[Double](n)

  setOmega(omegaF, omegaR)

  /**
   * Constructor for SSOR. Uses <code>omega=1</code> with a backwards sweep
   *
   * @param n
   *            Problem size (number of rows)
   */
  def this(n: Int) {
    this(n, true, 1, 1)
  }

  /**
   * Sets the overrelaxation parameters
   *
   * @param omegaF
   *            Overrelaxation parameter for the forward sweep. Between 0 and
   *            2.
   * @param omegaR
   *            Overrelaxation parameter for the backwards sweep. Between 0
   *            and 2.
   */
  def setOmega(omegaF: Double, omegaR: Double) {
    if (omegaF < 0 || omegaF > 2) throw new IllegalArgumentException("omegaF must be between 0 and 2")
    if (omegaR < 0 || omegaR > 2) throw new IllegalArgumentException("omegaR must be between 0 and 2")
    this.omegaF = omegaF
    this.omegaR = omegaR
  }

  def setMatrix(A: StrideMatrix2D) {
    if (A.rows() != n) {
      throw new IllegalArgumentException("A.rows() != n")
    }
    F = new SparseRCDoubleMatrix2D(n, n)
    F.assign(A)
    if (!F.hasColumnIndexesSorted()) {
      F.sortColumnIndexes()
    }
    val rowptr = F.getRowPointers
    val colind = F.getColumnIndexes
    for (k <- 0 until n) {
      diagind(k) = cern.colt.Sorting.binarySearchFromTo(colind, k, rowptr(k), rowptr(k + 1) - 1)
      if (diagind(k) < 0) throw new RuntimeException("Missing diagonal on row " + (k + 1))
    }
  }

  def apply(b: StrideMatrix1D, x: StrideMatrix1D): StrideMatrix1D = {
    if (x == null) {
      x = b.like()
    }
    if (!(b.isInstanceOf[DenseMatrix1D]) || !(x.isInstanceOf[DenseMatrix1D])) throw new IllegalArgumentException("b and x must be a DenseDoubleMatrix1D")
    val rowptr = F.getRowPointers
    val colind = F.getColumnIndexes
    val data = F.getValues
    val bd = b.asInstanceOf[DenseMatrix1D].elements()
    val xd = Array.ofDim[Double](x.size.toInt)
    val n = F.rows()
    System.arraycopy(xd, 0, xx, 0, n)
    for (i <- 0 until n) {
      var sigma = 0
      for (j <- rowptr(i) until diagind(i)) sigma += data(j) * xx(colind(j))
      for (j <- diagind(i) + 1 until rowptr(i + 1)) sigma += data(j) * xd(colind(j))
      sigma = (bd(i) - sigma) / data(diagind(i))
      xx(i) = xd(i) + omegaF * (sigma - xd(i))
    }
    if (!reverse) {
      System.arraycopy(xx, 0, xd, 0, n)
      x.assign(xd)
      return x
    }
    var i = n - 1
    while (i >= 0) {
      var sigma = 0
      for (j <- rowptr(i) until diagind(i)) sigma += data(j) * xx(colind(j))
      for (j <- diagind(i) + 1 until rowptr(i + 1)) sigma += data(j) * xd(colind(j))
      sigma = (bd(i) - sigma) / data(diagind(i))
      xd(i) = xx(i) + omegaR * (sigma - xx(i))
      i
    }
    x.assign(xd)
    x
  }

  def transApply(b: StrideMatrix1D, x: StrideMatrix1D): StrideMatrix1D = {
    if (x == null) {
      x = b.like()
    }
    apply(b, x)
  }
}
