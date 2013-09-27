package cern.colt.matrix.tdouble.algo.solver.preconditioner

import cern.colt.matrix.tdouble.StrideMatrix1D
import cern.colt.matrix.tdouble.StrideMatrix2D
import cern.colt.matrix.tdouble.impl.DenseMatrix1D
import cern.colt.matrix.tdouble.impl.SparseRCDoubleMatrix2D
//remove if not needed
import scala.collection.JavaConversions._

/**
 * ILU(0) preconditioner using a compressed row matrix as internal storage
 */
class DoubleILU(val n: Int) extends DoublePreconditioner {

  /**
   * Factorisation matrix
   */
  private var LU: SparseRCDoubleMatrix2D = _

  /**
   * Temporary vector for solving the factorised system
   */
  private val y = new DenseMatrix1D(n)

  private var diagind: Array[Int] = _

  def apply(b: StrideMatrix1D, x: StrideMatrix1D): StrideMatrix1D = {
    if (x == null) {
      x = b.like()
    }
    lowerUnitSolve(b, y)
    upperSolve(y, x)
  }

  def transApply(b: StrideMatrix1D, x: StrideMatrix1D): StrideMatrix1D = {
    if (x == null) {
      x = b.like()
    }
    upperTransSolve(b, y)
    loverUnitTransSolve(y, x)
  }

  def setMatrix(A: StrideMatrix2D) {
    if (A.rows() != n) {
      throw new IllegalArgumentException("A.rows() != n")
    }
    LU = new SparseRCDoubleMatrix2D(n, n)
    LU.assign(A)
    if (!LU.hasColumnIndexesSorted()) {
      LU.sortColumnIndexes()
    }
    factor()
  }

  private def factor() {
    val colind = LU.getColumnIndexes
    val rowptr = LU.getRowPointers
    val data = LU.getValues
    diagind = findDiagonalIndexes(n, colind, rowptr)
    for (k <- 1 until n; i <- rowptr(k) until diagind(k)) {
      val index = colind(i)
      val LUii = data(diagind(index))
      if (LUii == 0) throw new RuntimeException("Zero pivot encountered on row " + (i + 1) + " during ILU process")
      val LUki = (data(i) /= LUii)
      for (j <- diagind(index) + 1 until rowptr(index + 1)) {
        while (l < rowptr(k + 1) && colind(l) < colind(j)) l += 1
        if (colind(l) == colind(j)) data(l) -= LUki * data(j)
      }
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

  private def lowerUnitSolve(b: StrideMatrix1D, x: StrideMatrix1D): StrideMatrix1D = {
    val bd = b.asInstanceOf[DenseMatrix1D].elements()
    val xd = x.asInstanceOf[DenseMatrix1D].elements()
    val colind = LU.getColumnIndexes
    val rowptr = LU.getRowPointers
    val data = LU.getValues
    val rows = LU.rows()
    for (i <- 0 until rows) {
      var sum = 0
      for (j <- rowptr(i) until diagind(i)) sum += data(j) * xd(colind(j))
      xd(i) = bd(i) - sum
    }
    x
  }

  private def loverUnitTransSolve(b: StrideMatrix1D, x: StrideMatrix1D): StrideMatrix1D = {
    x.assign(b)
    val xd = x.asInstanceOf[DenseMatrix1D].elements()
    val colind = LU.getColumnIndexes
    val rowptr = LU.getRowPointers
    val data = LU.getValues
    val rows = LU.rows()
    var i = rows - 1
    while (i >= 0) {for (j <- rowptr(i) until diagind(i)) xd(colind(j)) -= data(j) * xd(i)i
    }
    x
  }

  private def upperSolve(b: StrideMatrix1D, x: StrideMatrix1D): StrideMatrix1D = {
    val bd = b.asInstanceOf[DenseMatrix1D].elements()
    val xd = x.asInstanceOf[DenseMatrix1D].elements()
    val colind = LU.getColumnIndexes
    val rowptr = LU.getRowPointers
    val data = LU.getValues
    val rows = LU.rows()
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
    val colind = LU.getColumnIndexes
    val rowptr = LU.getRowPointers
    val data = LU.getValues
    val rows = LU.rows()
    for (i <- 0 until rows) {
      xd(i) /= data(diagind(i))
      for (j <- diagind(i) + 1 until rowptr(i + 1)) xd(colind(j)) -= data(j) * xd(i)
    }
    x
  }
}
