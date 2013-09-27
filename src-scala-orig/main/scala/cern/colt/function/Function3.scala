package cern.colt.function

/**
 * Interface that represents a function object: a function that takes three
 * arguments.
 */
@specialized
trait Function3[T1, T2, T3, R] {

  /**
   * Applies a function to three arguments.
   *
   * @param first
   *            first argument passed to the function.
   * @param second
   *            second argument passed to the function.
   * @param third
   *            third argument passed to the function.
   * @return the result of the function.
   */
  def apply(first: T1, second: T2, third: T3): R
}
