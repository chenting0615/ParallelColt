package cern.colt.matrix.impl

/**
 * Sparse row-compressed-modified 2-d matrix holding typed elements.
 * Each row is stored as SparseDoubleMatrix1D.
 *
 * @author Piotr Wendykier (piotr.wendykier@gmail.com)
 *
 */
@SerialVersionUID(1L)
class SparseRCMMatrix2D[@specialized T: Manifest: Numeric](rows: Int, columns: Int) extends RemappedMatrix2D[T] {

  var elements = new Array[SparseHashMatrix1D[T]](rows)

  try {
    setUp(rows, columns)
  } catch {
    case exc: IllegalArgumentException => if ("matrix too large" != exc.getMessage) throw exc
  }

  override def getQuick(row: Int, column: Int): T = {
    if (elements(row) == null)
      zero
    else
      elements(row).getQuick(column)
  }

  override def setQuick(row: Int, column: Int, value: T) {
    if (elements(row) == null)
      elements(row) = new SparseHashMatrix1D[T](columnsVar)
    elements(row).setQuick(column, value)
  }

  override def viewRow(row: Int) = {
    checkRow(row)
    if (elements(row) == null)
      elements(row) = new SparseHashMatrix1D[T](columnsVar)
    elements(row)
  }

  override def trimToSize() {
    for (i <- 0 until rowsVar) {
      val mtrx = elements(i)
      if (mtrx != null) {
        if (mtrx.numNonZero == 0)
          elements(i) == null
        else
          mtrx.trimToSize()
      }
    }
  }

  def like2D(rows: Int, columns: Int) = new SparseRCMMatrix2D(rows, columns)

  def like1D(size: Int) = new SparseHashMatrix1D[T](size)
}
