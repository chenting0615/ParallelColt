package cern.colt.matrix

/**
  */
@specialized
@SerialVersionUID(1L)
trait IndexIterator1D[T] extends IndexIterator[T] {

  def index: Int
}
