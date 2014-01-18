package cern.colt.matrix.impl

import cern.colt.matrix._
import scala.Tuple2

/**
 * 2-d matrix holding elements of a certain type; either a view wrapping another
 * matrix or a matrix whose views are wrappers.
 *<p>
 * IMPORTANT:
 * If you derive from this matrix, you MUST override the following methods:
 * <pre>
 *    getQuick()
 *    setQuick()
 *    like1D()
 *    like2D()
 *    vectorize()
 * </pre>
 *
 * @author wolfgang.hoschek@cern.ch
 * @version 1.0, 04/14/2000
 *
 * @author Piotr Wendykier (piotr.wendykier@gmail.com)
 */
@SerialVersionUID(1L)
class WrapperMatrix2D[@specialized T: Manifest: Numeric](protected val content: Matrix2D[T], rows: Int, columns: Int) extends AbstractMatrix2D[T] {

  isNoView = false
  if (content == null)
    throw new NullPointerException("Wrapped content matrix is null")
  try {
    setUp(rows, columns)
  }
  catch {
    case exc: IllegalArgumentException => if ("matrix too large" != exc.getMessage) throw exc
  }

  def this(content: Matrix2D[T]) {
    this(content, content.rows, content.columns)
  }

  def numeric = implicitly[Numeric[T]]

  override def allCellsAreSettable = content.allCellsAreSettable

  override def canSetCellAt(row: Int, column: Int): Boolean = { val t = remapIndexes(row, column); content.canSetCellAt(t._1, t._2) }

  protected def remapIndexes(row: Int, column: Int): Tuple2[Int, Int] = (row, column)

  def getQuick(row: Int, column: Int): T = {
    val t = remapIndexes(row, column)
    content.getQuick(t._1, t._2)
  }

  def setQuick(row: Int, column: Int, value: T) {
    val t = remapIndexes(row, column)
    content.setQuick(t._1, t._2, value)
  }

  def like2D(rows: Int, columns: Int): Matrix2D[T] = content.like2D(rows, columns)

  def like1D(size: Int): Matrix1D[T] = content.like1D(size)

  override def vectorize() = {
    val v = content.like1D(size.toInt)
    var idx = 0
    for (c <- 0 until columnsVar; r <- 0 until rowsVar) {
      v.setQuick(idx, getQuick(r, c))
      idx += 1
    }
    v
  }

  override def getStorageMatrix = content.getStorageMatrix

  /**
   * @return Returns true if this matrix uses a sparse representation for storing cell values
   */
  override def isSparse: Boolean = content.isSparse

  override def isRowMajor: Boolean = content.isRowMajor

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
