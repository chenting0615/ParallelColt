package cern.colt.matrix

/**
  */
@SerialVersionUID(1L)
trait IndexIterator[@specialized T] {

  def hasValue: Boolean

  def increment(): Boolean

  def value: T

}
