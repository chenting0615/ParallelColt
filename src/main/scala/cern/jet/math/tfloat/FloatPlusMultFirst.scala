package cern.jet.math.tfloat

import cern.colt.function.FunctionTypes.FloatFloatFunction

object FloatPlusMultFirst {

  /**
   * <tt>a/constant - b</tt>.
   */
  def minusDiv(constant: Float): FloatPlusMultFirst = new FloatPlusMultFirst(-1 / constant)

  /**
   * <tt>a*constant - b</tt>.
   */
  def minusMult(constant: Float): FloatPlusMultFirst = new FloatPlusMultFirst(-constant)

  /**
   * <tt>a/constant + b</tt>.
   */
  def plusDiv(constant: Float): FloatPlusMultFirst = new FloatPlusMultFirst(1 / constant)

  /**
   * <tt>a*constant + b</tt>.
   */
  def plusMult(constant: Float): FloatPlusMultFirst = new FloatPlusMultFirst(constant)
}

/**
 * Only for performance tuning of compute intensive linear algebraic
 * computations. Constructs functions that return one of
 * <ul>
 * <li><tt>a*constant + b</tt>
 * <li><tt>a*constant - b</tt>
 * <li><tt>a/constant + b</tt>
 * <li><tt>a/constant - b</tt>
 * </ul>
 * <tt>a</tt> and <tt>b</tt> are variables, <tt>constant</tt> is fixed, but for
 * performance reasons publicly accessible. Intended to be passed to
 * <tt>matrix.assign(otherMatrix,function)</tt> methods.
 */
class FloatPlusMultFirst(var multiplicator: Float) extends FloatFloatFunction {

  /**
   * Returns the result of the function evaluation.
   */
  def apply(a: Float, b: Float): Float = a * multiplicator + b
}
