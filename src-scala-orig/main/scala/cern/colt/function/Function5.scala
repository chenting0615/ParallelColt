package cern.colt.function

/**
 * Interface that represents a function object: a function that takes 5
 * arguments and returns a single value.
 */
@specialized
trait Function5[T] {

  /**
   * Applies a function to two arguments.
   *
   * @param a
   *            the first argument passed to the function.
   * @param b
   *            the second argument passed to the function.
   * @param c
   *            the third argument passed to the function.
   * @param d
   *            the fourth argument passed to the function.
   * @param e
   *            the fifth argument passed to the function.
   * @return the result of the function.
   */
  def apply(a: T,  b: T, c: T, d: T, e: T): T
}
