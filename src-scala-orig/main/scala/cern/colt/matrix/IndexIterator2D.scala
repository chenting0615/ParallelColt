package cern.colt.matrix

/**
  */
@specialized
@SerialVersionUID(1L)
trait IndexIterator2D[T] extends IndexIterator[T] {

  def row: Int

  def column: Int
}
