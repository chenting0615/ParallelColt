package cern.colt.matrix.tdouble.impl

import cern.colt.matrix.tdouble.StrideMatrix2D
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Sparse row-compressed-modified 2-d matrix holding <tt>double</tt> elements.
 * Each row is stored as SparseDoubleMatrix1D.
 *
 * @author Piotr Wendykier (piotr.wendykier@gmail.com)
 *
 */
@SerialVersionUID(1L)
class SparseRCMDoubleMatrix2D(rows: Int, columns: Int) extends WrapperMatrix2D(null) {

  var elements: Array[SparseDoubleMatrix1D] = new Array[SparseDoubleMatrix1D](rows)

  try {
    setUp(rows, columns)
  } catch {
    case exc: IllegalArgumentException => if ("matrix too large" != exc.getMessage) throw exc
  }

  for (i <- 0 until rows) elements(i) = new SparseDoubleMatrix1D(columns)

  def getQuick(row: Int, column: Int): Double = elements(row).getQuick(column)

  def setQuick(row: Int, column: Int, value: Double) {
    elements(row).setQuick(column, value)
  }

  def trimToSize() {
    for (r <- 0 until rows) {
      elements(r).trimToSize()
    }
  }

  def viewRow(row: Int): SparseDoubleMatrix1D = elements(row)

  protected def getStorageMatrix(): StrideMatrix2D = this

  def like(rows: Int, columns: Int): StrideMatrix2D = {
    new SparseRCMDoubleMatrix2D(rows, columns)
  }
}
