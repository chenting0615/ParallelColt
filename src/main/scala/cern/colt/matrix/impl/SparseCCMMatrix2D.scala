package cern.colt.matrix.impl

/**
 * Sparse column-compressed-modified 2-d matrix holding typed elements.
 * Each column is stored as SparseDoubleMatrix1D.
 *
 * @author Piotr Wendykier (piotr.wendykier@gmail.com)
 *
 */
@SerialVersionUID(1L)
class SparseCCMMatrix2D[@specialized T: Manifest: Numeric](rows: Int, columns: Int) extends RemappedMatrix2D[T] {

  var elements = new Array[SparseHashMatrix1D[T]](columns)

  try {
    setUp(rows, columns)
  } catch {
    case exc: IllegalArgumentException => if ("matrix too large" != exc.getMessage) throw exc
  }

  override def getQuick(row: Int, column: Int): T = {
    if (elements(column) == null)
      zero
    else
      elements(column).getQuick(row)
  }

  override def setQuick(row: Int, column: Int, value: T) {
    if (elements(column) == null)
      elements(column) = new SparseHashMatrix1D[T](rowsVar)
    elements(column).setQuick(row, value)
  }

  override def viewColumn(column: Int) = {
    checkColumn(column)
    if (elements(column) == null)
      elements(column) = new SparseHashMatrix1D[T](rowsVar)
    elements(column)
  }

  override def trimToSize() {
    for (i <- 0 until columnsVar) {
      val mtrx = elements(i)
      if (mtrx != null) {
        if (mtrx.numNonZero == 0)
          elements(i) == null
        else
          mtrx.trimToSize()
      }
    }
  }

  def like2D(rows: Int, columns: Int) = new SparseCCMMatrix2D(rows, columns)

  def like1D(size: Int) = new SparseHashMatrix1D[T](size)
}
