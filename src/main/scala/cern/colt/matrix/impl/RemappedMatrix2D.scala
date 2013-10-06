package cern.colt.matrix.impl

import cern.colt.matrix._
import scala.Tuple2

/**
 * 2-d matrix holding elements of a certain type; either a view wrapping another
 * matrix or a matrix whose views are wrappers.
 *
 * @author wolfgang.hoschek@cern.ch
 * @version 1.0, 04/14/2000
 *
 * @author Piotr Wendykier (piotr.wendykier@gmail.com)
 */
@specialized
@SerialVersionUID(1L)
abstract class RemappedMatrix2D[T: Manifest: Numeric] extends AbstractMatrix2D[T] {

  isNoView = false

  def viewRow(row: Int): Matrix1D[T] = {
    checkRow(row)
    new WrappedRowMatrix1D[T](this, row)
  }

  def viewColumn(column: Int): Matrix1D[T] = {
    checkColumn(column)
    new WrappedColumnMatrix1D[T](this, column)
  }

  override def viewColumnFlip(): AbstractMatrix2D[T] = {
    if (columnsVar == 0) return this
    val view = new WrapperMatrix2D[T](this) {
      override protected def remapIndexes(row: Int, column: Int): Tuple2[Int, Int] = (row, columns - 1 - column)
    }
    view
  }

  override def viewTranspose(): WrapperMatrix2D[T] = {
    val view = new WrapperMatrix2D[T](this) {
      override protected def remapIndexes(row: Int, column: Int): Tuple2[Int, Int] = (column, row)
    }
    view.rowsVar = columnsVar
    view.columnsVar = rowsVar
    view
  }

  override def viewPart(boxRow: Int, boxColumn: Int, height: Int, width: Int): WrapperMatrix2D[T] = {
    checkBox(boxRow, boxColumn, height, width)
    val view = new WrapperMatrix2D[T](this) {
      override protected def remapIndexes(row: Int, column: Int): Tuple2[Int, Int] = (row+boxRow, column+boxColumn)
    }
    view.rowsVar = height
    view.columnsVar = width
    view
  }

  override def viewRowFlip(): AbstractMatrix2D[T] = {
    if (rowsVar == 0) return this
    val view = new WrapperMatrix2D[T](this) {
      override protected def remapIndexes(row: Int, column: Int): Tuple2[Int, Int] = (rowsVar - 1 - row, column)
    }
    view
  }

  override def viewSelection(rowIndexes: Array[Int], columnIndexes: Array[Int]) = {
    checkRowIndexes(rowIndexes)
    checkColumnIndexes(columnIndexes)
    val view = new WrapperMatrix2D(this) {
      override protected def remapIndexes(row: Int, column: Int): Tuple2[Int, Int] = {
        (if (rowIndexes == null) row else rowIndexes(row), if (columnIndexes == null) column else columnIndexes(column))
      }
    }
    view.rowsVar = if (rowIndexes == null) rowsVar else rowIndexes.length
    view.columnsVar = if (columnIndexes == null) columnsVar else columnIndexes.length
    view
  }

  override def viewStrides(rowStride: Int, columnStride: Int): WrapperMatrix2D[T] = {
    if (rowStride <= 0 || columnStride <= 0) throw new IndexOutOfBoundsException("illegal stride")
    val view = new WrapperMatrix2D(this) {
      override protected def remapIndexes(row: Int, column: Int): Tuple2[Int, Int] = {
        (rowStride * row, columnStride * column)
      }
    }
    if (rowsVar != 0) view.rowsVar = (rowsVar - 1) / rowStride + 1
    if (columnsVar != 0) view.columnsVar = (columnsVar - 1) / columnStride + 1
    view
  }
}
