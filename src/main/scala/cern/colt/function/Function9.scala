package cern.colt.function

/**
 * Interface that represents a function object: a function that takes 9
 * arguments and returns a single value.
 */
@specialized
trait Function9[T] {

  /**
   * Applies a function to nine arguments.
   *
   * @return the result of the function.
   */
  def apply(a00: T,
      a01: T,
      a02: T,
      a10: T,
      a11: T,
      a12: T,
      a20: T,
      a21: T,
      a22: T): T
}
