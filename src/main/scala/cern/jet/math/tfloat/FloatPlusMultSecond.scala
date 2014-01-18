package cern.jet.math.tfloat

import cern.colt.function.FunctionTypes.FloatFloatFunction

object FloatPlusMultSecond {

  /**
   * <tt>a - b/constant</tt>.
   */
  def minusDiv(constant: Float): FloatPlusMultSecond = new FloatPlusMultSecond(-1 / constant)

  /**
   * <tt>a - b*constant</tt>.
   */
  def minusMult(constant: Float): FloatPlusMultSecond = new FloatPlusMultSecond(-constant)

  /**
   * <tt>a + b/constant</tt>.
   */
  def plusDiv(constant: Float): FloatPlusMultSecond = new FloatPlusMultSecond(1 / constant)

  /**
   * <tt>a + b*constant</tt>.
   */
  def plusMult(constant: Float): FloatPlusMultSecond = new FloatPlusMultSecond(constant)
}

/**
 * Only for performance tuning of compute intensive linear algebraic
 * computations. Constructs functions that return one of
 * <ul>
 * <li><tt>a + b*constant</tt>
 * <li><tt>a - b*constant</tt>
 * <li><tt>a + b/constant</tt>
 * <li><tt>a - b/constant</tt>
 * </ul>
 * <tt>a</tt> and <tt>b</tt> are variables, <tt>constant</tt> is fixed, but for
 * performance reasons publicly accessible. Intended to be passed to
 * <tt>matrix.assign(otherMatrix,function)</tt> methods.
 */
class FloatPlusMultSecond(var multiplicator: Float) extends FloatFloatFunction {

  /**
   * Returns the result of the function evaluation.
   */
  def apply(a: Float, b: Float): Float = a + b * multiplicator
}
