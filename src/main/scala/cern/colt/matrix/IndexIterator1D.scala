package cern.colt.matrix

/**
  */
@SerialVersionUID(1L)
trait IndexIterator1D[@specialized T] extends IndexIterator[T] {

  def index: Int
}
