package cern.colt.matrix.impl

import cern.colt.matrix.{Matrix2D, Matrix1D}

/**
 * 1-d matrix holding typed elements; a view wrapping the diagonal of another 2-d
 * matrix and therefore delegating calls to it.
 *
 * @author wolfgang.hoschek@cern.ch
 * @version 1.0, 09/24/99
 * @author Piotr Wendykier (piotr.wendykier@gmail.com)
 */
@specialized
@SerialVersionUID(1L)
class WrappedDiagonalMatrix2D[T: Manifest](content1D: Matrix1D[T]) extends RemappedMatrix2D[T] {

  this.isNoView = false
  setUp(content1D.size.toInt, content1D.size.toInt)

  def getQuick(row: Int, column: Int): T = {
    if (row != column)
      0.asInstanceOf[T]
    else
      content1D.getQuick(row)
  }

  def setQuick(row: Int, column: Int, value: T) {
    if (row != column)
      throw new IllegalArgumentException("Cannot set off-diagonal cells in diagonal matrix (row=" + row + ", col=" + column)
    content1D.setQuick(row, value)
  }

  /**
   * Sets all cells to the state specified by <tt>value</tt>.
   *
   * @param value
   * the value to be filled into the cells.
   * @return <tt>this</tt> (for convenience only).
   */
  override def assignConstant(value: T) = {
    content1D.assignConstant(value)
    this
  }

  override def forEachNonZero(f: Function3[Int, Int, T, T]) = {
    for(i <- 0 until content1D.size.toInt) {
      val oldValue: T = content1D.getQuick(i)
      if (oldValue != 0.0) {
        val newValue: T = f.apply(i, i, oldValue)
        if (newValue != oldValue) {
          content1D.setQuick(i, newValue)
        }
      }
    }
    this
  }

  /**
   * @return Returns true if this matrix uses a sparse representation for storing cell values
   */
  override def isSparse: Boolean = true

  override def like1D(size: Int) = content1D.like1D(size)

  override def like2D(rows: Int, columns: Int) = content1D.like2D(rows, columns)

  def reshape(rows: Int, columns: Int): Matrix2D[T] = {
    throw new IllegalArgumentException("This method is not supported.")
  }
}
