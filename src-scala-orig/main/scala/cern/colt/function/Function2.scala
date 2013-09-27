package cern.colt.function

//remove if not needed
import scala.collection.JavaConversions._

/**
 * Interface that represents a function object: a function that takes two
 * arguments.
 */
@specialized
trait Function2[T1, T2, R] {

  /**
   * Applies a function to two arguments.
   *
   * @param first
   *            first argument passed to the function.
   * @param second
   *            second argument passed to the function.
   * @return the result of the function.
   */
  def apply(first: T1, second: T2): R
}
