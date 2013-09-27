package cern.colt.matrix.tdouble.algo

import cern.colt.matrix.tdouble.StrideMatrix2D
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Interface that represents a function object: a function that takes two
 * arguments and returns a single value.
 */
trait DoubleMatrix2DMatrix2DFunction {

  /**
   * Applies a function to two arguments.
   *
   * @param x
   *            the first argument passed to the function.
   * @param y
   *            the second argument passed to the function.
   * @return the result of the function.
   */
  def apply(x: StrideMatrix2D, y: StrideMatrix2D): Double
}
