package cern.colt.matrix.tint.impl

import cern.colt.matrix.tint.IntMatrix2D
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Sparse row-compressed-modified 2-d matrix holding <tt>int</tt> elements. Each
 * row is stored as SparseIntMatrix1D.
 *
 * @author Piotr Wendykier (piotr.wendykier@gmail.com)
 *
 */
@SerialVersionUID(1L)
class SparseRCMIntMatrix2D(rows: Int, columns: Int) extends WrapperIntMatrix2D(null) {

  var elements: Array[SparseIntMatrix1D] = new Array[SparseIntMatrix1D](rows)

  try {
    setUp(rows, columns)
  } catch {
    case exc: IllegalArgumentException => if ("matrix too large" != exc.getMessage) throw exc
  }

  for (i <- 0 until rows) elements(i) = new SparseIntMatrix1D(columns)

  def getQuick(row: Int, column: Int): Int = elements(row).getQuick(column)

  def setQuick(row: Int, column: Int, value: Int) {
    elements(row).setQuick(column, value)
  }

  def trimToSize() {
    for (r <- 0 until rows) {
      elements(r).trimToSize()
    }
  }

  def viewRow(row: Int): SparseIntMatrix1D = elements(row)

  protected def getStorageMatrix(): IntMatrix2D = this

  def like(rows: Int, columns: Int): IntMatrix2D = new SparseRCMIntMatrix2D(rows, columns)
}
