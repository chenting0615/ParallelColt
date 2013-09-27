package cern.colt.matrix

/**
  */
@specialized
@SerialVersionUID(1L)
trait IndexIterator[T] {

  def hasNext: Boolean

  def next(): T
}
