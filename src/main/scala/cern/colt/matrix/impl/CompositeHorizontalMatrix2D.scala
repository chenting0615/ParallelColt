package cern.colt.matrix.impl

import java.util
import cern.colt.list.ArrayTypes.IntArrayList
import cern.colt.matrix.{Matrix1D, Matrix2D}

/**
 *
 */
@SerialVersionUID(1L)
class CompositeHorizontalMatrix2D[@specialized T: Manifest: Numeric](numberOfRows: Int) extends AbstractMatrix2D[T] {

  isNoView = false

  private val columnOffsets = new IntArrayList()

  private val matrices = new util.ArrayList[Matrix2D[T]]()

  setUp(numberOfRows, 0)

  def this() {
    this(0)
  }

  def numeric = implicitly[Numeric[T]]

  def getMatrixCount: Int = this.matrices.size

  def append(matrix: Matrix2D[T]) {
    var lastOffset = 0
    if (matrices.isEmpty) this.rowsVar = matrix.rows else lastOffset = columnOffsets.get(columnOffsets.size - 1)
    this.columnOffsets.add(matrix.columns + lastOffset)
    this.matrices.add(matrix)
    this.columnsVar += matrix.columns
  }

  override def columns: Int = {
    if (this.columnOffsets.size == 0) 0 else columnOffsets.get(columnOffsets.size - 1)
  }

  private def getMatrixIndex(column: Int): Array[Int] = {
    val matrixAndColumn = Array.ofDim[Int](2)
    for (i <- 0 until columnOffsets.size if column < columnOffsets.get(i)) {
      matrixAndColumn(0) = i
      matrixAndColumn(1) = if (i == 0) column else column - columnOffsets.get(i - 1)
      return matrixAndColumn
    }
    throw new IllegalStateException("Could not find column " + column + " in composite matrix, max columns: " + columns)
  }

  override def getQuick(row: Int, column: Int): T = {
    val matrixAndColumnIdx = getMatrixIndex(column)
    this.matrices.get(matrixAndColumnIdx(0)).get(row, matrixAndColumnIdx(1))
  }

  override def setQuick(row: Int, column: Int, value: T) {
    val matrixAndColumnIdx = getMatrixIndex(column)
    this.matrices.get(matrixAndColumnIdx(0)).set(row, matrixAndColumnIdx(1), value)
  }

  override def like2D(rows: Int, columns: Int) = {
    if (this.matrices.size == 0) new DenseMatrix2D[T](rows, columns) else this.matrices.get(0).like2D(rows, columns)
  }

  override def like1D(size: Int) = {
    if (this.matrices.size == 0) new DenseMatrix1D[T](size) else this.matrices.get(0).like1D(size)
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
