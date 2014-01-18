package cern.jet.math.tlong


object LongMult {

  /**
   * <tt>a / constant</tt>.
   */
  def div(constant: Long): LongMult = mult(1 / constant)

  /**
   * <tt>a * constant</tt>.
   */
  def mult(constant: Long): LongMult = new LongMult(constant)
}

/**
 * Only for performance tuning of compute longensive linear algebraic
 * computations. Constructs functions that return one of
 * <ul>
 * <li><tt>a * constant</tt>
 * <li><tt>a / constant</tt>
 * </ul>
 * <tt>a</tt> is variable, <tt>constant</tt> is fixed, but for performance
 * reasons publicly accessible. Longended to be passed to
 * <tt>matrix.assign(function)</tt> methods.
 */
class LongMult protected (var multiplicator: Long) extends Function1[Long, Long] {

  /**
   * Returns the result of the function evaluation.
   */
  def apply(a: Long): Long = a * multiplicator
}
