package cern.colt.matrix

/**
 * Interface that represents a condition or procedure object: takes a single
 * argument and returns a boolean value.
 */
@specialized
trait Matrix1DProcedure[T] {

  /**
   * Applies a procedure to an argument. Optionally can return a boolean flag
   * to inform the object calling the procedure.
   *
   * <p>
   * Example: forEach() methods often use procedure objects. To signal to a
   * forEach() method whether iteration should continue normally or terminate
   * (because for example a matching element has been found), a procedure can
   * return <tt>false</tt> to indicate termination and <tt>true</tt> to
   * indicate continuation.
   *
   * @param element
   *            element passed to the procedure.
   * @return a flag to inform the object calling the procedure.
   */
  def apply(element: Matrix1D[T]): Boolean
}
