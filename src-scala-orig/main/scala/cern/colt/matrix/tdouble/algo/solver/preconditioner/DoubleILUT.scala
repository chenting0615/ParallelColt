package cern.colt.matrix.tdouble.algo.solver.preconditioner

import java.util.ArrayList
import java.util.Collections
import java.util.List
import cern.colt.matrix.Norm
import cern.colt.matrix.tdouble.StrideMatrix1D
import cern.colt.matrix.tdouble.StrideMatrix2D
import cern.colt.matrix.tdouble.algo.DenseDoubleAlgebra
import cern.colt.matrix.tdouble.algo.DoubleProperty
import cern.colt.matrix.tdouble.impl.DenseMatrix1D
import cern.colt.matrix.tdouble.impl.SparseDoubleMatrix1D
import cern.colt.matrix.tdouble.impl.SparseRCMDoubleMatrix2D
import DoubleILUT._
//remove if not needed
import scala.collection.JavaConversions._

object DoubleILUT {

  /**
   * Stores an integer/value pair, sorted by descending order according to the
   * value
   */
  private class IntDoubleEntry(var index: Int, var value: Double) extends Comparable[IntDoubleEntry] {

    def compareTo(o: IntDoubleEntry): Int = {
      if (Math.abs(value) < Math.abs(o.value)) 1 else if (Math.abs(value) == Math.abs(o.value)) 0 else -1
    }

    override def toString(): String = "(" + index + "=" + value + ")"
  }
}

/**
 * ILU preconditioner with fill-in. Uses the dual threshold approach of Saad.
 */
class DoubleILUT(val n: Int, val tau: Double, val p: Int) extends DoublePreconditioner {

  /**
   * Factorisation matrix
   */
  private var LU: SparseRCMDoubleMatrix2D = _

  /**
   * Temporary vector for solving the factorised system
   */
  private val y = new DenseMatrix1D(n)

  /**
   * Stores entries in the lower and upper part of the matrix. Used by the
   * dropping rule to determine the largest entries in the two parts of the
   * matrix
   */
  private val lower = new ArrayList[IntDoubleEntry](n)

  private val upper = new ArrayList[IntDoubleEntry](n)

  /**
   * Sets up the preconditioner for the given problem size. Uses a
   * drop-tolerance of 10<sup>-6</sup>, and keeps 25 entries on each row,
   * including the main diagonal and any previous entries in the matrix
   * structure
   *
   * @param n
   *            Problem size (number of rows)
   */
  def this(n: Int) {
    this(n, 1e-6, 25)
  }

  def apply(b: StrideMatrix1D, x: StrideMatrix1D): StrideMatrix1D = {
    if (x == null) {
      x = b.like()
    }
    unitLowerSolve(b, y)
    upperSolve(y, x)
  }

  def transApply(b: StrideMatrix1D, x: StrideMatrix1D): StrideMatrix1D = {
    if (x == null) {
      x = b.like()
    }
    upperTransSolve(b, y)
    unitLowerTransSolve(y, x)
  }

  def setMatrix(A: StrideMatrix2D) {
    DoubleProperty.DEFAULT.isSquare(A)
    if (A.rows() != n) {
      throw new IllegalArgumentException("A.rows() != n")
    }
    LU = new SparseRCMDoubleMatrix2D(n, n)
    LU.assign(A)
    LU.trimToSize()
    factor()
  }

  private def factor() {
    val n = LU.rows()
    for (i <- 1 until n) {
      val rowi = LU.viewRow(i)
      val taui = DenseDoubleAlgebra.DEFAULT.norm(rowi, Norm.Two) * tau
      for (k <- 0 until i) {
        val rowk = LU.viewRow(k)
        if (rowk.getQuick(k) == 0) throw new RuntimeException("Zero diagonal entry on row " + (k + 1) + " during ILU process")
        val LUik = rowi.getQuick(k) / rowk.getQuick(k)
        if (Math.abs(LUik) <= taui) //continue
        val rowUsed = rowk.size.toInt
        for (j <- k + 1 until rowUsed) rowi.setQuick(j, rowi.getQuick(j) - LUik * rowk.getQuick(j))
        rowi.setQuick(k, LUik)
      }
      gather(rowi, taui, i)
    }
  }

  /**
   * Copies the dense array back into the sparse vector, applying a numerical
   * dropping rule and keeping only a given number of entries
   */
  private def gather(v: SparseDoubleMatrix1D, taui: Double, d: Int) {
    var nl = 0
    var nu = 0
    val indexes = v.elements().keys.elements()
    for (i <- 0 until indexes.length) {
      if (indexes(i) < d) nl += 1 else if (indexes(i) > d) nu += 1
    }
    val z = v.toArray()
    v.assign(0)
    lower.clear()
    for (i <- 0 until d if Math.abs(z(i)) > taui) lower.add(new IntDoubleEntry(i, z(i)))
    upper.clear()
    for (i <- d + 1 until z.length if Math.abs(z(i)) > taui) upper.add(new IntDoubleEntry(i, z(i)))
    Collections.sort(lower)
    Collections.sort(upper)
    v.setQuick(d, z(d))
    for (i <- 0 until Math.min(nl + p, lower.size)) {
      val e = lower.get(i)
      v.setQuick(e.index, e.value)
    }
    for (i <- 0 until Math.min(nu + p, upper.size)) {
      val e = upper.get(i)
      v.setQuick(e.index, e.value)
    }
  }

  private def unitLowerSolve(b: StrideMatrix1D, x: StrideMatrix1D): StrideMatrix1D = {
    val bd = b.asInstanceOf[DenseMatrix1D].elements()
    val xd = x.asInstanceOf[DenseMatrix1D].elements()
    val rows = LU.rows()
    for (i <- 0 until rows) {
      val row = LU.viewRow(i)
      var sum = 0
      for (j <- 0 until i) sum += row.getQuick(j) * xd(j)
      xd(i) = bd(i) - sum
    }
    x
  }

  private def unitLowerTransSolve(b: StrideMatrix1D, x: StrideMatrix1D): StrideMatrix1D = {
    x.assign(b)
    val xd = x.asInstanceOf[DenseMatrix1D].elements()
    val rows = LU.rows()
    var i = rows - 1
    while (i >= 0) {
      val row = LU.viewRow(i)
      for (j <- 0 until i) xd(j) -= row.getQuick(j) * xd(i)
      i
    }
    x
  }

  private def upperSolve(b: StrideMatrix1D, x: StrideMatrix1D): StrideMatrix1D = {
    val bd = b.asInstanceOf[DenseMatrix1D].elements()
    val xd = x.asInstanceOf[DenseMatrix1D].elements()
    val rows = LU.rows()
    var i = rows - 1
    while (i >= 0) {
      val row = LU.viewRow(i)
      val used = row.size.toInt
      var sum = 0
      for (j <- i + 1 until used) sum += row.getQuick(j) * xd(j)
      xd(i) = (bd(i) - sum) / row.getQuick(i)
      i
    }
    x
  }

  private def upperTransSolve(b: StrideMatrix1D, x: StrideMatrix1D): StrideMatrix1D = {
    x.assign(b)
    val xd = x.asInstanceOf[DenseMatrix1D].elements()
    val rows = LU.rows()
    for (i <- 0 until rows) {
      val row = LU.viewRow(i)
      val used = row.size.toInt
      xd(i) /= row.getQuick(i)
      for (j <- i + 1 until used) xd(j) -= row.getQuick(j) * xd(i)
    }
    x
  }
}
