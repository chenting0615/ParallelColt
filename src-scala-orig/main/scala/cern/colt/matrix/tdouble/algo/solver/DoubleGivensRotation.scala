package cern.colt.matrix.tdouble.algo.solver

import cern.colt.matrix.tdouble.StrideMatrix1D
import cern.colt.matrix.tdouble.StrideMatrix2D
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Givens plane rotation
 */
class DoubleGivensRotation(x: Double, y: Double) {

  /**
   * Cosine and sine of the rotation angle. c = x / sqrt(x^2 + y^2), and s =
   * -y / sqrt(x^2 + y^2)
   */
  private val c: Double = _

  private val s: Double = _

  val roe = if (Math.abs(x) > Math.abs(y)) x else y

  val scale = Math.abs(x) + Math.abs(y)

  if (scale != 0) {
    val xs = x / scale
    val ys = y / scale
    var r = scale * Math.sqrt(xs * xs + ys * ys)
    if (roe < 0) r *= -1
    c = x / r
    s = y / r
  } else {
    c = 1
    s = 0
  }

  /**
   * Applies the Givens rotation to two elements in a matrix column
   *
   * @param H
   *            Matrix to apply to
   * @param column
   *            Column index
   * @param i1
   *            Row index of first element
   * @param i2
   *            Row index of second element
   */
  def apply(H: StrideMatrix2D,
      column: Int,
      i1: Int,
      i2: Int) {
    val temp = c * H.getQuick(i1, column) + s * H.getQuick(i2, column)
    H.setQuick(i2, column, -s * H.getQuick(i1, column) + c * H.getQuick(i2, column))
    H.setQuick(i1, column, temp)
  }

  /**
   * Applies the Givens rotation to two elements of a vector
   *
   * @param x
   *            Vector to apply to
   * @param i1
   *            Index of first element
   * @param i2
   *            Index of second element
   */
  def apply(x: StrideMatrix1D, i1: Int, i2: Int) {
    val temp = c * x.getQuick(i1) + s * x.getQuick(i2)
    x.setQuick(i2, -s * x.getQuick(i1) + c * x.getQuick(i2))
    x.setQuick(i1, temp)
  }
}
