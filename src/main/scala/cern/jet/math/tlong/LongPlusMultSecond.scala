package cern.jet.math.tlong


object LongPlusMultSecond {

  /**
   * <tt>a - b*constant</tt>.
   */
  def minusMult(constant: Long): LongPlusMultSecond = new LongPlusMultSecond(-constant)

  /**
   * <tt>a + b*constant</tt>.
   */
  def plusMult(constant: Long): LongPlusMultSecond = new LongPlusMultSecond(constant)
}

/**
 * Only for performance tuning of compute longensive linear algebraic
 * computations. Constructs functions that return one of
 * <ul>
 * <li><tt>a + b*constant</tt>
 * <li><tt>a - b*constant</tt>
 * <li><tt>a + b/constant</tt>
 * <li><tt>a - b/constant</tt>
 * </ul>
 * <tt>a</tt> and <tt>b</tt> are variables, <tt>constant</tt> is fixed, but for
 * performance reasons publicly accessible. Longended to be passed to
 * <tt>matrix.assign(otherMatrix,function)</tt> methods.
 */
class LongPlusMultSecond(var multiplicator: Long) extends Function2[Long, Long, Long] {

  /**
   * Returns the result of the function evaluation.
   */
  def apply(a: Long, b: Long): Long = a + b * multiplicator
}
