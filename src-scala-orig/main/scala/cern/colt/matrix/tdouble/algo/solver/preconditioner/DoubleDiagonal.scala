package cern.colt.matrix.tdouble.algo.solver.preconditioner

import cern.colt.matrix.tdouble.StrideMatrix1D
import cern.colt.matrix.tdouble.StrideMatrix2D
import cern.colt.matrix.tdouble.impl.DenseMatrix1D
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Diagonal preconditioner. Uses the inverse of the diagonal as preconditioner
 */
class DoubleDiagonal(n: Int) extends DoublePreconditioner {

  /**
   * This contains the inverse of the diagonal
   */
  private var invdiag: Array[Double] = new Array[Double](n)

  def apply(b: StrideMatrix1D, x: StrideMatrix1D): StrideMatrix1D = {
    if (x == null) {
      x = b.like()
    }
    if (!(x.isInstanceOf[DenseMatrix1D]) || !(b.isInstanceOf[DenseMatrix1D])) throw new IllegalArgumentException("a nad b must be dense vectors")
    val xd = x.asInstanceOf[DenseMatrix1D].elements()
    val bd = b.asInstanceOf[DenseMatrix1D].elements()
    for (i <- 0 until invdiag.length) xd(i) = bd(i) * invdiag(i)
    x
  }

  def transApply(b: StrideMatrix1D, x: StrideMatrix1D): StrideMatrix1D = {
    if (x == null) {
      x = b.like()
    }
    apply(b, x)
  }

  def setMatrix(A: StrideMatrix2D) {
    if (A.rows() != invdiag.length) throw new IllegalArgumentException("Matrix size differs from preconditioner size")
    for (i <- 0 until invdiag.length) {
      invdiag(i) = A.getQuick(i, i)
      if (invdiag(i) == 0) throw new RuntimeException("Zero diagonal on row " + (i + 1)) else invdiag(i) = 1 / invdiag(i)
    }
  }
}
