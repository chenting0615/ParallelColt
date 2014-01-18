package cern.jet.math

/**
  */
object NumericFunctions {

  def plus[T](num: Numeric[T]) = new Function2[T, T, T]() {
    def apply(v1: T, v2: T) = num.plus(v1, v2)
  }

  def plus[T](num: Numeric[T], value: T) = new Function1[T, T]() {
    def apply(v1: T) = num.plus(v1, value)
  }

  def minus[T](num: Numeric[T]) = new Function2[T, T, T]() {
    def apply(v1: T, v2: T) = num.minus(v1, v2)
  }

  def minus[T](num: Numeric[T], value: T) = new Function1[T, T]() {
    def apply(v1: T) = num.minus(v1, value)
  }

  def mult[T](num: Numeric[T]) = new Function2[T, T, T]() {
    def apply(v1: T, v2: T) = num.times(v1, v2)
  }

  def mult[T](num: Numeric[T], value: T) = new Function1[T, T]() {
    def apply(v1: T) = num.times(v1, value)
  }

  def square[T](num: Numeric[T]) = new Function1[T, T]() {
    def apply(v1: T) = num.times(v1, v1)
  }

  def abs[T](num: Numeric[T]) = new Function1[T, T]() {
    def apply(v1: T) = num.abs(v1)
  }

  def identity[T] = new Function1[T, T]() {
    def apply(v1: T) = v1
  }

  def min[T](num: Numeric[T]) = new Function2[T, T, T]() {
    def apply(v1: T, v2: T) = if (num.lt(v1, v2)) v1 else v2
  }

  def max[T](num: Numeric[T]) = new Function2[T, T, T]() {
    def apply(v1: T, v2: T) = if (num.gt(v1, v2)) v1 else v2
  }

}
