package cern.colt.matrix.impl

import java.util
import cern.colt.list.ArrayTypes.IntArrayList
import cern.colt.matrix.{Matrix1D, Matrix2D}

/**
 *
 */
@SerialVersionUID(1L)
class CompositeVerticalMatrix2D[@specialized T: Manifest: Numeric](numberOfColumns: Int) extends AbstractMatrix2D[T] {

  isNoView = false

  private val rowOffsets = new IntArrayList()

  private val matrices = new util.ArrayList[Matrix2D[T]]()

  setUp(0, numberOfColumns)

  def this() {
    this(0)
  }

  def this(numberOfRows: Int, numberOfColumns: Int, numberOfRowsPerBlock: Int) {
    this(numberOfColumns)
    while (rows < numberOfRows) {
      var blockRows = numberOfRows - rows
      if (blockRows > numberOfRowsPerBlock) blockRows = numberOfRowsPerBlock
      val m = new DenseMatrix2D[T](blockRows, numberOfColumns)
      append(m)
    }
  }

  def this(numberOfRows: Int, numberOfColumns: Int) {
    this(numberOfRows, numberOfColumns, 25000)
  }

  def numeric = implicitly[Numeric[T]]

  def getMatrixCount: Int = this.matrices.size

  def append(matrix: Matrix2D[T]) {
    var lastOffset = 0
    if (matrices.isEmpty) this.columnsVar = matrix.columns else lastOffset = rowOffsets.get(rowOffsets.size - 1)
    this.rowOffsets.add(matrix.rows + lastOffset)
    this.matrices.add(matrix)
    this.rowsVar += matrix.rows
  }

  override def rows: Int = {
    if (this.rowOffsets.size == 0) 0 else rowOffsets.get(rowOffsets.size - 1)
  }

  private def getMatrixIndex(row: Int): Array[Int] = {
    val matrixAndRow = Array.ofDim[Int](2)
    for (i <- 0 until rowOffsets.size if row < rowOffsets.get(i)) {
      matrixAndRow(0) = i
      matrixAndRow(1) = if (i == 0) row else row - rowOffsets.get(i - 1)
      return matrixAndRow
    }
    throw new IllegalStateException("Could not find row " + row + " in composite matrix, max rows: " + rows)
  }

  override def getQuick(row: Int, column: Int): T = {
    val matrixAndRowIdx = getMatrixIndex(row)
    this.matrices.get(matrixAndRowIdx(0)).get(matrixAndRowIdx(1), column)
  }

  override def setQuick(row: Int, column: Int, value: T) {
    val matrixAndRowIdx = getMatrixIndex(row)
    this.matrices.get(matrixAndRowIdx(0)).set(matrixAndRowIdx(1), column, value)
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
