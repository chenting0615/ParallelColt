package cern.colt.function

import cern.colt.matrix.Matrix1D

/**
 * Interface that represents a function object: a function that takes two
 * argument vectors and returns a single value.
 */
trait VectorFunction[T] {

  /**
   * Applies a function to two argument vectors.
   *
   * @param rowOrColIdx  The row or column index for the given 1D matrix.
   *  Whether this is a row or column index depends on the method calling this function,
   *  depending on if it's processing rows or columns.
   *  See MatrixProcessor.processRows() or MatrixProcessor.processColumns().
   *
   * @param x
   *            the argument vector passed to the function.
   * @return the double value result of the function.
   */
  def apply(rowOrColIdx: Int, x: Matrix1D[T]): T
}
