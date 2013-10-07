package cern.colt.matrix.impl

import java.util
import cern.colt.list.ArrayTypes.IntArrayList
import cern.colt.matrix.Matrix2D

/**
 *
 */
@SerialVersionUID(1L)
class CompositeHorizontalMatrix2D[@specialized T: Manifest: Numeric](numberOfRows: Int) extends RemappedMatrix2D[T] {

  private val columnOffsets = new IntArrayList()

  private val matrices = new util.ArrayList[Matrix2D[T]]()

  setUp(numberOfRows, 0)

  def this() {
    this(0)
  }

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
}
