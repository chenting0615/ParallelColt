package cern.colt.matrix

/**
  */
@SerialVersionUID(1L)
trait IndexIterator1D[T] extends IndexIterator[T] {

  def index: Int

  def setIndex(index: Int): Boolean
}
