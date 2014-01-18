package cern.jet.math.tfloat

object FloatArithmetic {

  private val stirlingCorrectionData = Array(0.0f, 8.1061467e-02f, 4.1340696e-02f, 2.7677926e-02f, 2.0790672e-02f, 1.6644691e-02f, 1.3876129e-02f, 1.1896710e-02f, 1.0411265e-02f, 9.2554622e-03f, 8.3305634e-03f, 7.5736755e-03f, 6.9428401e-03f, 6.4089942e-03f, 5.9513701e-03f, 5.5547336e-03f, 5.2076559e-03f, 4.9013959e-03f, 4.6291537e-03f, 4.3855602e-03f, 4.1663197e-03f, 3.9679542e-03f, 3.7876181e-03f, 3.6229602e-03f, 3.4720214e-03f, 3.3331556e-03f, 3.2049702e-03f, 3.0862787e-03f, 2.9760640e-03f, 2.8734494e-03f, 2.7776749e-03f)

  protected val logFactorials = Array(0.0000000f, 0.0000000f, 0.69314718f, 1.7917595f, 3.1780538f, 4.7874917f, 6.5792512f, 8.5251613f, 10.6046029f, 12.8018275f, 15.1044126f, 17.5023078f, 19.9872145f, 22.5521639f, 25.1912212f, 27.8992714f, 30.6718601f, 33.5050735f, 36.3954452f, 39.3398842f, 42.3356165f, 45.3801389f, 48.4711814f, 51.6066756f, 54.7847294f, 58.0036052f, 61.2617018f, 64.5575386f, 67.8897431f, 71.2570390f)

  protected val longFactorials = Array(1L, 1L, 2L, 6L, 24L, 120L, 720L, 5040L, 40320L, 362880L, 3628800L, 39916800L, 479001600L, 6227020800L, 87178291200L, 1307674368000L, 20922789888000L, 355687428096000L, 6402373705728000L, 121645100408832000L, 2432902008176640000L)

  protected val floatFactorials = Array(5.1090942E19f, 1.1240007E21f, 2.5852017E22f, 6.2044840E23f, 1.5511210E25f, 4.0329146E26f, 1.0888869E28f, 3.0488834E29f, 8.8417620E30f, 2.6525286E32f, 8.2228387E33f, 2.6313084E35f, 8.6833176E36f, 2.9523280E38f)

  /**
   * Efficiently returns the binomial coefficient, often also referred to as
   * "n over k" or "n choose k". The binomial coefficient is defined as
   * <tt>(n * n-1 * ... * n-k+1 ) / ( 1 * 2 * ... * k )</tt>.
   * <ul>
   * <li>k<0<tt>: <tt>0</tt>.
   * <li>k==0<tt>: <tt>1</tt>.
   * <li>k==1<tt>: <tt>n</tt>.
   * <li>else: <tt>(n * n-1 * ... * n-k+1 ) / ( 1 * 2 * ... * k )</tt>.
   * </ul>
   *
   * @return the binomial coefficient.
   */
  def binomial(n: Float, k: Long): Float = {
    if (k < 0) return 0
    if (k == 0) return 1
    if (k == 1) return n
    var a = n - k + 1
    var b = 1
    var binomial = 1f
    var i = k
    while (i > 0) {
      binomial *= a / b
      a += 1
      b += 1
      i -= 1
    }
    binomial
  }

  /**
   * Efficiently returns the binomial coefficient, often also referred to as
   * "n over k" or "n choose k". The binomial coefficient is defined as
   * <ul>
   * <li>k<0<tt>: <tt>0</tt>.
   * <li>k==0 || k==n<tt>: <tt>1</tt>.
   * <li>k==1 || k==n-1<tt>: <tt>n</tt>.
   * <li>else: <tt>(n * n-1 * ... * n-k+1 ) / ( 1 * 2 * ... * k )</tt>.
   * </ul>
   *
   * @return the binomial coefficient.
   */
  def binomial(n: Long, k_p: Long): Float = {
    var k = k_p
    if (k < 0) return 0
    if (k == 0 || k == n) return 1
    if (k == 1 || k == n - 1) return n
    if (n > k) {
      val max = longFactorials.length + floatFactorials.length
      if (n < max) {
        val n_fac = factorial(n.toInt)
        val k_fac = factorial(k.toInt)
        val n_minus_k_fac = factorial((n - k).toInt)
        val nk = n_minus_k_fac * k_fac
        if (nk != Float.PositiveInfinity) {
          return n_fac / nk
        }
      }
      if (k > n / 2) k = n - k
    }
    var a = n - k + 1
    var b = 1
    var binomial = 1f
    var i = k
    while (i > 0) {
      binomial *= a.toFloat / b
      a += 1
      b += 1
      i -= 1
    }
    binomial
  }

  /**
   * Returns the smallest <code>long &gt;= value</code>. <dt>Examples:
   * <code>1.0 -> 1, 1.2 -> 2, 1.9 -> 2</code>. This method is safer than
   * using (long) Math.ceil(value), because of possible rounding error.
   */
  def ceil(value: Float): Long = Math.round(Math.ceil(value))

  /**
   * Evaluates the series of Chebyshev polynomials Ti at argument x/2. The
   * series is given by
   *
   * <pre>
   *        N-1
   *         - '
   *  y  =   &gt;   coef[i] T (x/2)
   *         -            i
   *        i=0
   * </pre>
   *
   * Coefficients are stored in reverse order, i.e. the zero order term is
   * last in the array. Note N is the number of coefficients, not the order.
   * <p>
   * If coefficients are for the interval a to b, x must have been transformed
   * to x -> 2(2x - b - a)/(b-a) before entering the routine. This maps x from
   * (a, b) to (-1, 1), over which the Chebyshev polynomials are defined.
   * <p>
   * If the coefficients are for the inverted interval, in which (a, b) is
   * mapped to (1/b, 1/a), the transformation required is x -> 2(2ab/x - b -
   * a)/(b-a). If b is infinity, this becomes x -> 4a/x - 1.
   * <p>
   * SPEED:
   * <p>
   * Taking advantage of the recurrence properties of the Chebyshev
   * polynomials, the routine requires one more addition per loop than
   * evaluating a nested polynomial of the same degree.
   *
   * @param x
   *            argument to the polynomial.
   * @param coef
   *            the coefficients of the polynomial.
   * @param N
   *            the number of coefficients.
   */
  def chbevl(x: Float, coef: Array[Float], N: Int): Float = {
    var b0: Float = 0.0f
    var b1: Float = 0.0f
    var b2: Float = 0.0f
    var p = 0
    var i: Int = 0
    b0 = coef(p)
    p += 1
    b1 = 0.0f
    i = N - 1
    do {
      b2 = b1
      b1 = b0
      b0 = x * b1 - b2 + coef(p)
      p += 1
    } while (i > 0)
    0.5f * (b0 - b2)
  }

  /**
   * Instantly returns the factorial <tt>k!</tt>.
   *
   * @param k
   *            must hold <tt>k &gt;= 0</tt>.
   */
  def factorial(k: Int): Float = {
    if (k < 0) throw new IllegalArgumentException()
    val length1 = longFactorials.length
    if (k < length1) return longFactorials(k)
    val length2 = floatFactorials.length
    if (k < length1 + length2) floatFactorials(k - length1) else Float.PositiveInfinity
  }

  /**
   * Returns the largest <code>long &lt;= value</code>. <dt>Examples: <code>
   * 1.0 -> 1, 1.2 -> 1, 1.9 -> 1 <dt>
   * 2.0 -> 2, 2.2 -> 2, 2.9 -> 2 </code> <dt>This method is safer than using
   * (long) Math.floor(value), because of possible rounding error.
   */
  def floor(value: Float): Long = Math.round(Math.floor(value))

  /**
   * Returns <tt>log<sub>base</sub>value</tt>.
   */
  def log(base: Float, value: Float): Float = {
    (Math.log(value) / Math.log(base)).toFloat
  }

  /**
   * Returns <tt>log<sub>10</sub>value</tt>.
   */
  def log10(value: Float): Float = {
    (Math.log(value) * 0.43429448190325176).toFloat
  }

  /**
   * Returns <tt>log<sub>2</sub>value</tt>.
   */
  def log2(value: Float): Float = {
    (Math.log(value) * 1.4426950408889634).toFloat
  }

  /**
   * Returns <tt>log(k!)</tt>. Tries to avoid overflows. For <tt>k<30</tt>
   * simply looks up a table in O(1). For <tt>k>=30</tt> uses stirlings
   * approximation.
   *
   * @param k
   *            must hold <tt>k &gt;= 0</tt>.
   */
  def logFactorial(k: Int): Float = {
    if (k >= 30) {
      var r: Float = 0.0f
      var rr: Float = 0.0f
      val C0 = 9.1893853e-01f
      val C1 = 8.3333333e-02f
      val C3 = -2.7777778e-03f
      val C5 = 7.9365079e-04f
      val C7 = -5.9523810e-04f
      r = 1.0f / k
      rr = r * r
      ((k + 0.5) * Math.log(k) - k + C0 + r * (C1 + rr * (C3 + rr * (C5 + rr * C7)))).toFloat
    } else logFactorials(k)
  }

  /**
   * Instantly returns the factorial <tt>k!</tt>.
   *
   * @param k
   *            must hold <tt>k &gt;= 0 && k &lt; 21</tt>.
   */
  def longFactorial(k: Int): Long = {
    if (k < 0) throw new IllegalArgumentException("Negative k")
    if (k < longFactorials.length) return longFactorials(k)
    throw new IllegalArgumentException("Overflow")
  }

  /**
   * Returns the StirlingCorrection.
   * <p>
   * Correction term of the Stirling approximation for <tt>log(k!)</tt>
   * (series in 1/k, or table values for small k) with int parameter k.
   * <p>
   * <tt>
   * log k! = (k + 1/2)log(k + 1) - (k + 1) + (1/2)log(2Pi) +
   *          stirlingCorrection(k + 1)
   * <p>
   * log k! = (k + 1/2)log(k)     -  k      + (1/2)log(2Pi) +
   *          stirlingCorrection(k)
   * </tt>
   */
  def stirlingCorrection(k: Int): Float = {
    val C1 = 8.3333333e-02f
    val C3 = -2.7777778e-03f
    val C5 = 7.9365079e-04f
    val C7 = -5.9523810e-04f
    var r: Float = 0.0f
    var rr: Float = 0.0f
    if (k > 30) {
      r = 1.0f / k
      rr = r * r
      r * (C1 + rr * (C3 + rr * (C5 + rr * C7)))
    }
    else
      stirlingCorrectionData(k)
  }
}
