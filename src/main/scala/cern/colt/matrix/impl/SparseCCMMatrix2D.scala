package cern.colt.matrix.impl

import cern.colt.matrix.{Matrix2D, Matrix1D}

/**
 * Sparse column-compressed-modified 2-d matrix holding typed elements.
 * Each column is stored as SparseDoubleMatrix1D.
 *
 * @author Piotr Wendykier (piotr.wendykier@gmail.com)
 *
 */
@SerialVersionUID(1L)
class SparseCCMMatrix2D[@specialized T: Manifest: Numeric](rows: Int, columns: Int) extends AbstractMatrix2D[T] {

  var elements = new Array[SparseHashMatrix1D[T]](columns)

  try {
    setUp(rows, columns)
  } catch {
    case exc: IllegalArgumentException => if ("matrix too large" != exc.getMessage) throw exc
  }

  def numeric = implicitly[Numeric[T]]

  override def getQuick(row: Int, column: Int): T = {
    if (elements(column) == null)
      numeric.zero
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

  def toArray: Array[Array[T]] = {
    val values = Array.ofDim[T](rows, columns)
    toArray(values)
  }

  def viewRow(row: Int): Matrix1D[T] = {
    checkRow(row)
    new WrappedRowMatrix1D[T](this, row)
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
