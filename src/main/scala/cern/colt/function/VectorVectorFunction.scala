package cern.colt.function

import cern.colt.matrix.Matrix1D

/**
 * Interface that represents a function object: a function that takes two
 * argument vectors and returns a single value.
 */
trait VectorVectorFunction[T] {

  /**
   * Applies a function to two argument vectors.
   *
   * @param x1
   *            the argument vector passed to the function.
   * @param x2
   *            the argument vector passed to the function.
   * @return the value result of the function.
   */
  def apply(x1: Matrix1D[T], x2: Matrix1D[T]): T
}
