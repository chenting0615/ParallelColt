package cern.colt.matrix.tint.impl

import cern.colt.matrix.tint.IntMatrix2D
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Sparse column-compressed-modified 2-d matrix holding <tt>int</tt> elements.
 * Each column is stored as SparseIntMatrix1D.
 *
 * @author Piotr Wendykier (piotr.wendykier@gmail.com)
 *
 */
@SerialVersionUID(1L)
class SparseCCMIntMatrix2D(rows: Int, columns: Int) extends WrapperIntMatrix2D(null) {

  var elements: Array[SparseIntMatrix1D] = new Array[SparseIntMatrix1D](columns)

  try {
    setUp(rows, columns)
  } catch {
    case exc: IllegalArgumentException => if ("matrix too large" != exc.getMessage) throw exc
  }

  for (i <- 0 until columns) elements(i) = new SparseIntMatrix1D(rows)

  def getQuick(row: Int, column: Int): Int = elements(column).getQuick(row)

  def setQuick(row: Int, column: Int, value: Int) {
    elements(column).setQuick(row, value)
  }

  def trimToSize() {
    for (c <- 0 until columns) {
      elements(c).trimToSize()
    }
  }

  def viewColumn(column: Int): SparseIntMatrix1D = elements(column)

  protected def getStorageMatrix(): IntMatrix2D = this

  def like(rows: Int, columns: Int): IntMatrix2D = new SparseCCMIntMatrix2D(rows, columns)
}
