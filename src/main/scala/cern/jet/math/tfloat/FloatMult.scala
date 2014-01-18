package cern.jet.math.tfloat

import cern.colt.function.FunctionTypes.FloatFunction

object FloatMult {

  /**
   * <tt>a / constant</tt>.
   */
  def div(constant: Float): FloatMult = mult(1 / constant)

  /**
   * <tt>a * constant</tt>.
   */
  def mult(constant: Float): FloatMult = new FloatMult(constant)
}

/**
 * Only for performance tuning of compute intensive linear algebraic
 * computations. Constructs functions that return one of
 * <ul>
 * <li><tt>a * constant</tt>
 * <li><tt>a / constant</tt>
 * </ul>
 * <tt>a</tt> is variable, <tt>constant</tt> is fixed, but for performance
 * reasons publicly accessible. Intended to be passed to
 * <tt>matrix.assign(function)</tt> methods.
 */
class FloatMult(var multiplicator: Float) extends FloatFunction {

  /**
   * Returns the result of the function evaluation.
   */
  def apply(a: Float): Float = a * multiplicator
}
