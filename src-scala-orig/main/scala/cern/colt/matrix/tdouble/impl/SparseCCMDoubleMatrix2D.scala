package cern.colt.matrix.tdouble.impl

import cern.colt.matrix.tdouble.StrideMatrix2D
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Sparse column-compressed-modified 2-d matrix holding <tt>double</tt>
 * elements. Each column is stored as SparseDoubleMatrix1D.
 *
 * @author Piotr Wendykier (piotr.wendykier@gmail.com)
 *
 */
@SerialVersionUID(1L)
class SparseCCMDoubleMatrix2D(rows: Int, columns: Int) extends WrapperMatrix2D(null) {

  var elements: Array[SparseDoubleMatrix1D] = new Array[SparseDoubleMatrix1D](columns)

  try {
    setUp(rows, columns)
  } catch {
    case exc: IllegalArgumentException => if ("matrix too large" != exc.getMessage) throw exc
  }

  for (i <- 0 until columns) elements(i) = new SparseDoubleMatrix1D(rows)

  def getQuick(row: Int, column: Int): Double = elements(column).getQuick(row)

  def setQuick(row: Int, column: Int, value: Double) {
    elements(column).setQuick(row, value)
  }

  def trimToSize() {
    for (c <- 0 until columns) {
      elements(c).trimToSize()
    }
  }

  def viewColumn(column: Int): SparseDoubleMatrix1D = elements(column)

  protected def getStorageMatrix(): StrideMatrix2D = this

  def like(rows: Int, columns: Int): StrideMatrix2D = {
    new SparseCCMDoubleMatrix2D(rows, columns)
  }
}
