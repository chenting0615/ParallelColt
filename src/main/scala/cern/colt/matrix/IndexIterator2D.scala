package cern.colt.matrix

/**
  */
@SerialVersionUID(1L)
trait IndexIterator2D[T] extends IndexIterator[T] {

  def row: Int

  def column: Int
}
