package cern.colt.matrix

/**
  */
@SerialVersionUID(1L)
trait IndexIterator2D[@specialized T] extends IndexIterator[T] {

  def row: Int

  def column: Int
}
