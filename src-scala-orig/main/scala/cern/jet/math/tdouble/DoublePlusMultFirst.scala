package cern.jet.math.tdouble


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
object DoublePlusMultFirst {

  /**
   * <tt>a - b/constant</tt>.
   */
  def minusDiv(constant: Double): DoubleDoubleFunction = new DoubleDoubleFunction {
    def apply(a: Double, b: Double): Double = a / constant - b
  }

  /**
   * <tt>a - b*constant</tt>.
   */
  def minusMult(constant: Double): DoubleDoubleFunction = new DoubleDoubleFunction {
      def apply(a: Double, b: Double): Double = a * constant - b
    }

  /**
   * <tt>a + b/constant</tt>.
   */
  def plusDiv(constant: Double): DoubleDoubleFunction = new DoubleDoubleFunction {
      def apply(a: Double, b: Double): Double = a / constant + b
    }

  /**
   * <tt>a + b*constant</tt>.
   */
  def plusMult(constant: Double): DoubleDoubleFunction = new DoubleDoubleFunction {
      def apply(a: Double, b: Double): Double = a * constant + b
    }
}

