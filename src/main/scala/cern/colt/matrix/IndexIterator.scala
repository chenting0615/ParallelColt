package cern.colt.matrix

/**
  */
@SerialVersionUID(1L)
trait IndexIterator[T] {

  def hasValue: Boolean

  def increment(): Boolean

  def value: T

}
