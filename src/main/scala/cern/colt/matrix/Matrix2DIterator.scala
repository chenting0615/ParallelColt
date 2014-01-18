package cern.colt.matrix

class Matrix2DIterator[T](m: Matrix2D[T]) extends Iterator[Matrix1D[T]] {

  private var currentRow = -1

  def hasNext: Boolean = currentRow < m.rows-1

  def next(): Matrix1D[T] = {
    if (hasNext) {
      currentRow += 1
      m.viewRow(currentRow)
    }
    else {
      null
    }
  }
}
