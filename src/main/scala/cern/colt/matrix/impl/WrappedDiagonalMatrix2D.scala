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
@SerialVersionUID(1L)
class WrappedDiagonalMatrix2D[@specialized T: Manifest: Numeric](content1D: Matrix1D[T]) extends AbstractMatrix2D[T] {

  isNoView = false
  setUp(content1D.size.toInt, content1D.size.toInt)

  def numeric = implicitly[Numeric[T]]

  /**
   * @return Returns true if this matrix uses a sparse representation for storing cell values
   */
  override def isSparse: Boolean = true

  override def allCellsAreSettable = false

  override def canSetCellAt(row: Int, column: Int): Boolean = row == column

  def getQuick(row: Int, column: Int): T = {
    if (row != column)
      numeric.zero
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

  override def forEachNonZeroRowMajor(f: Function3[Int, Int, T, T]) = {
    for(i <- 0 until content1D.size.toInt) {
      val oldValue = content1D.getQuick(i)
      if (oldValue != numeric.zero) {
        val newValue = f.apply(i, i, oldValue)
        if (newValue != oldValue) {
          content1D.setQuick(i, newValue)
        }
      }
    }
    this
  }

  override def forEachNonZeroColumnMajor(f: Function3[Int, Int, T, T]) = {
    forEachNonZeroRowMajor(f)
  }

  override def like1D(size: Int) = content1D.like1D(size)

  override def like2D(rows: Int, columns: Int) = content1D.like2D(rows, columns)

  def reshape(rows: Int, columns: Int): Matrix2D[T] = {
    throw new IllegalArgumentException("This method is not supported.")
  }

  def toArray: Array[Array[T]] = {
    val values = Array.ofDim[T](rows, columns)
    toArray(values)
  }

  def viewRow(row: Int): Matrix1D[T] = {
    checkRow(row)
    new WrappedRowMatrix1D[T](this, row)
  }

  def viewColumn(column: Int): Matrix1D[T] = {
    checkColumn(column)
    new WrappedColumnMatrix1D[T](this, column)
  }

  override def viewColumnFlip(): Matrix2D[T] = {
    if (columnsVar == 0) {
      this
    }
    else {
      val view = new WrapperMatrix2D[T](this) {
        override protected def remapIndexes(row: Int, column: Int): Tuple2[Int, Int] = (row, columns - 1 - column)
      }
      view
    }
  }

  override def viewTranspose(): Matrix2D[T] = {
    val view = new WrapperMatrix2D[T](this, columnsVar, rowsVar) {
      override protected def remapIndexes(row: Int, column: Int): Tuple2[Int, Int] = (column, row)
    }
    view
  }

  override def viewPart(boxRow: Int, boxColumn: Int, height: Int, width: Int): Matrix2D[T] = {
    checkBox(boxRow, boxColumn, height, width)
    val view = new WrapperMatrix2D[T](this, height, width) {
      override protected def remapIndexes(row: Int, column: Int): Tuple2[Int, Int] = (row+boxRow, column+boxColumn)
    }
    view
  }

  override def viewRowFlip(): Matrix2D[T] = {
    if (rowsVar == 0) {
      this
    }
    else {
      val view = new WrapperMatrix2D[T](this) {
        override protected def remapIndexes(row: Int, column: Int): Tuple2[Int, Int] = (rowsVar - 1 - row, column)
      }
      view
    }
  }

  override def viewSelection(rowIndexes: Array[Int], columnIndexes: Array[Int]): Matrix2D[T] = {
    checkRowIndexes(rowIndexes)
    checkColumnIndexes(columnIndexes)
    val viewRowsVar = if (rowIndexes == null) rowsVar else rowIndexes.length
    val viewColumnsVar = if (columnIndexes == null) columnsVar else columnIndexes.length
    val view = new WrapperMatrix2D(this, viewRowsVar, viewColumnsVar) {
      override protected def remapIndexes(row: Int, column: Int): Tuple2[Int, Int] = {
        (if (rowIndexes == null) row else rowIndexes(row), if (columnIndexes == null) column else columnIndexes(column))
      }
    }
    view
  }

  override def viewStrides(rowStride: Int, columnStride: Int): Matrix2D[T] = {
    if (rowStride <= 0 || columnStride <= 0) throw new IndexOutOfBoundsException("illegal stride")
    val viewRowsVar = if (rowsVar != 0) (rowsVar - 1) / rowStride + 1 else rowsVar
    val viewColumnsVar = if (columnsVar != 0) (columnsVar - 1) / columnStride + 1 else columnsVar
    val view = new WrapperMatrix2D(this, viewRowsVar, viewColumnsVar) {
      override protected def remapIndexes(row: Int, column: Int): Tuple2[Int, Int] = {
        (rowStride * row, columnStride * column)
      }
    }
    view
  }
}
