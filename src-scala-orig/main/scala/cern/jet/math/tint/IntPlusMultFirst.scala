package cern.jet.math.tint

import cern.colt.function.tint.IntIntFunction


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
object IntPlusMultFirst {

  /**
   * <tt>a*constant - b</tt>.
   */
  def minusMult(constant: Int): IntIntFunction = new IntIntFunction {
    def apply(a: Int, b: Int): Int = a * constant - b
  }

  /**
   * <tt>a*constant + b</tt>.
   */
  def plusMult(constant: Int): IntIntFunction = new IntIntFunction {
      def apply(a: Int, b: Int): Int = a * constant + b
    }
 }
