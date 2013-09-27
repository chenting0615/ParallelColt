package cern.colt.matrix.tdouble.algo.solver.preconditioner

import java.util.Arrays
import cern.colt.matrix.tdouble.StrideMatrix1D
import cern.colt.matrix.tdouble.StrideMatrix2D
import cern.colt.matrix.tdouble.algo.DoubleProperty
import cern.colt.matrix.tdouble.impl.DenseMatrix1D
import cern.colt.matrix.tdouble.impl.SparseRCDoubleMatrix2D
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Incomplete Cholesky preconditioner without fill-in using a compressed row
 * matrix as internal storage
 */
class DoubleICC(val n: Int) extends DoublePreconditioner {

  /**
   * Factorisation matrix
   */
  private var R: SparseRCDoubleMatrix2D = _

  /**
   * Temporary vector for solving the factorised system
   */
  private val y = new DenseMatrix1D(n)

  private var diagind: Array[Int] = _

  def apply(b: StrideMatrix1D, x: StrideMatrix1D): StrideMatrix1D = {
    if (x == null) {
      x = b.like()
    }
    upperTransSolve(b, y)
    upperSolve(y, x)
  }

  def transApply(b: StrideMatrix1D, x: StrideMatrix1D): StrideMatrix1D = {
    if (x == null) {
      x = b.like()
    }
    apply(b, x)
  }

  def setMatrix(A: StrideMatrix2D) {
    DoubleProperty.DEFAULT.isSquare(A)
    if (A.rows() != n) {
      throw new IllegalArgumentException("A.rows() != n")
    }
    R = new SparseRCDoubleMatrix2D(n, n)
    R.assign(A)
    if (!R.hasColumnIndexesSorted()) {
      R.sortColumnIndexes()
    }
    factor()
  }

  private def factor() {
    val n = R.rows()
    val colind = R.getColumnIndexes
    val rowptr = R.getRowPointers
    val data = R.getValues
    val Rk = Array.ofDim[Double](n)
    diagind = findDiagonalIndexes(n, colind, rowptr)
    for (k <- 0 until n) {
      Arrays.fill(Rk, 0)
      for (i <- rowptr(k) until rowptr(k + 1)) Rk(colind(i)) = data(i)
      for (i <- 0 until k) {
        val Rii = data(diagind(i))
        if (Rii == 0) throw new RuntimeException("Zero pivot encountered on row " + (i + 1) + " during ICC process")
        val Rki = Rk(i) / Rii
        if (Rki == 0) //continue
        for (j <- diagind(i) + 1 until rowptr(i + 1)) Rk(colind(j)) -= Rki * data(j)
      }
      if (Rk(k) == 0) throw new RuntimeException("Zero diagonal entry encountered on row " + (k + 1) +
        " during ICC process")
      val sqRkk = Math.sqrt(Rk(k))
      for (i <- diagind(k) until rowptr(k + 1)) data(i) = Rk(colind(i)) / sqRkk
    }
  }

  private def findDiagonalIndexes(m: Int, colind: Array[Int], rowptr: Array[Int]): Array[Int] = {
    val diagind = Array.ofDim[Int](m)
    for (k <- 0 until m) {
      diagind(k) = cern.colt.Sorting.binarySearchFromTo(colind, k, rowptr(k), rowptr(k + 1) - 1)
      if (diagind(k) < 0) throw new RuntimeException("Missing diagonal entry on row " + (k + 1))
    }
    diagind
  }

  private def upperSolve(b: StrideMatrix1D, x: StrideMatrix1D): StrideMatrix1D = {
    val bd = b.asInstanceOf[DenseMatrix1D].elements()
    val xd = x.asInstanceOf[DenseMatrix1D].elements()
    val colind = R.getColumnIndexes
    val rowptr = R.getRowPointers
    val data = R.getValues
    val rows = R.rows()
    var i = rows - 1
    while (i >= 0) {
      var sum = 0
      for (j <- diagind(i) + 1 until rowptr(i + 1)) sum += data(j) * xd(colind(j))
      xd(i) = (bd(i) - sum) / data(diagind(i))
      i
    }
    x
  }

  private def upperTransSolve(b: StrideMatrix1D, x: StrideMatrix1D): StrideMatrix1D = {
    x.assign(b)
    val xd = x.asInstanceOf[DenseMatrix1D].elements()
    val colind = R.getColumnIndexes
    val rowptr = R.getRowPointers
    val data = R.getValues
    val rows = R.rows()
    for (i <- 0 until rows) {
      xd(i) /= data(diagind(i))
      for (j <- diagind(i) + 1 until rowptr(i + 1)) xd(colind(j)) -= data(j) * xd(i)
    }
    x
  }
}
