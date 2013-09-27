package cern.colt.function

/**
 * Interface that represents a function object: a function that takes a single
 * argument and returns a single value.
 */
@specialized
trait Function1[T] {

  /**
   * Applies a function to an argument.
   *
   * @param argument
   *            argument passed to the function.
   * @return the result of the function.
   */
  def apply(argument: T): T
}
