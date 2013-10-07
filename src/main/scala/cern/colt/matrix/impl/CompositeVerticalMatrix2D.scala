package cern.colt.matrix.impl

import java.util
import cern.colt.list.ArrayTypes.IntArrayList
import cern.colt.matrix.Matrix2D

/**
 *
 */
@SerialVersionUID(1L)
class CompositeVerticalMatrix2D[@specialized T: Manifest: Numeric](numberOfColumns: Int) extends RemappedMatrix2D[T] {

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
}
