package cern.jet.math.tdouble

/**
 * Polynomial functions.
 */
object Polynomial {

  /**
   * Evaluates the given polynomial of degree <tt>N</tt> at <tt>x</tt>,
   * assuming coefficient of N is 1.0. Otherwise same as <tt>polevl()</tt>.
   *
   * <pre>
   *                     2          N
   * y  =  C  + C x + C x  +...+ C x
   *        0    1     2          N
   *
   * where C  = 1 and hence is omitted from the array.
   *        N
   *
   * Coefficients are stored in reverse order:
   *
   * coef[0] = C  , ..., coef[N-1] = C  .
   *            N-1                   0
   *
   * Calling arguments are otherwise the same as polevl().
   * </pre>
   *
   * In the interest of speed, there are no checks for out of bounds
   * arithmetic.
   *
   * @param x
   *            argument to the polynomial.
   * @param coef
   *            the coefficients of the polynomial.
   * @param N
   *            the degree of the polynomial.
   */
  def p1evl(x: Double, coef: Array[Double], N: Int): Double = {
    var ans: Double = 0.0
    ans = x + coef(0)
    for (i <- 1 until N) {
      ans = ans * x + coef(i)
    }
    ans
  }

  /**
   * Evaluates the given polynomial of degree <tt>N</tt> at <tt>x</tt>.
   *
   * <pre>
   *                     2          N
   * y  =  C  + C x + C x  +...+ C x
   *        0    1     2          N
   *
   * Coefficients are stored in reverse order:
   *
   * coef[0] = C  , ..., coef[N] = C  .
   *            N                   0
   * </pre>
   *
   * In the interest of speed, there are no checks for out of bounds
   * arithmetic.
   *
   * @param x
   *            argument to the polynomial.
   * @param coef
   *            the coefficients of the polynomial.
   * @param N
   *            the degree of the polynomial.
   */
  def polevl(x: Double, coef: Array[Double], N: Int): Double = {
    var ans: Double = 0.0
    ans = coef(0)
    var i = 1
    while (i <= N) {ans = ans * x + coef(i); i += 1
    }
    ans
  }
}
