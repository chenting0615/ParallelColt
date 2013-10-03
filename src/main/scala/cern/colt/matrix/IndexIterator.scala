package cern.colt.matrix

/**
  */
@specialized
@SerialVersionUID(1L)
trait IndexIterator[T] {

  def hasValue: Boolean

  def increment(): Boolean

  def value: T

}
