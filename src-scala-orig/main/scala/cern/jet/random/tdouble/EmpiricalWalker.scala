package cern.jet.random.tdouble

import cern.jet.random.tdouble.engine.DoubleRandomEngine
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Discrete Empirical distribution (pdf's can be specified).
 * <p>
 * The probability distribution function (pdf) must be provided by the user as
 * an array of positive real numbers. The pdf does not need to be provided in
 * the form of relative probabilities, absolute probabilities are also accepted.
 * <p>
 * <p>
 * Instance methods operate on a user supplied uniform random number generator;
 * they are unsynchronized.
 * <dt>Static methods operate on a default uniform random number generator; they
 * are synchronized.
 * <p>
 * <b>Implementation:</b> Walker's algorithm. Generating a random number takes
 * <tt>O(1)</tt>, i.e. constant time, as opposed to commonly used algorithms
 * with logarithmic time complexity. Preprocessing time (on object construction)
 * is <tt>O(k)</tt> where <tt>k</tt> is the number of elements of the provided
 * empirical pdf. Space complexity is <tt>O(k)</tt>.
 * <p>
 * This is a port of <A HREF="http://sourceware.cygnus.com/cgi-bin/cvsweb.cgi/gsl/randist/discrete.c?cvsroot=gsl"
 * >discrete.c</A> which was written by James Theiler and is distributed with <A
 * HREF="http://sourceware.cygnus.com/gsl/">GSL 0.4.1</A>. Theiler's
 * implementation in turn is based upon
 * <p>
 * Alastair J. Walker, An efficient method for generating discrete random
 * variables with general distributions, ACM Trans Math Soft 3, 253-256 (1977).
 * <p>
 * See also: D. E. Knuth, The Art of Computer Programming, Volume 2
 * (Seminumerical algorithms), 3rd edition, Addison-Wesley (1997), p120.
 *
 * @author wolfgang.hoschek@cern.ch
 * @version 1.0, 09/24/99
 */
@SerialVersionUID(1L)
class EmpiricalWalker(pdf: Array[Double], interpolationType: Int, randomGenerator: DoubleRandomEngine)
    extends AbstractDiscreteDistribution {

  protected var K: Int = _

  protected var A: Array[Int] = _

  protected var F: Array[Double] = _

  protected var cdf: Array[Double] = _

  setRandomGenerator(randomGenerator)

  setState(pdf, interpolationType)

  setState2(pdf)

  /**
   * Returns the cumulative distribution function.
   */
  def cdf(k: Int): Double = {
    if (k < 0) return 0.0
    if (k >= cdf.length - 1) return 1.0
    cdf(k)
  }

  /**
   * Returns a deep copy of the receiver; the copy will produce identical
   * sequences. After this call has returned, the copy and the receiver have
   * equal but separate state.
   *
   * @return a copy of the receiver.
   */
  def clone(): AnyRef = {
    val copy = super.clone().asInstanceOf[EmpiricalWalker]
    if (this.cdf != null) copy.cdf = this.cdf.clone()
    if (this.A != null) copy.A = this.A.clone()
    if (this.F != null) copy.F = this.F.clone()
    copy
  }

  /**
   * Returns a random integer <tt>k</tt> with probability <tt>pdf(k)</tt>.
   */
  def nextInt(): Int = {
    var c = 0
    var u: Double = 0.0
    var f: Double = 0.0
    u = this.randomGenerator.raw()
    u *= this.K
    c = u.toInt
    u -= c
    f = this.F(c)
    if (f == 1.0) return c
    if (u < f) {
      c
    } else {
      this.A(c)
    }
  }

  /**
   * Returns the probability distribution function.
   */
  def pdf(k: Int): Double = {
    if (k < 0 || k >= cdf.length - 1) return 0.0
    cdf(k - 1) - cdf(k)
  }

  /**
   * Sets the distribution parameters. The <tt>pdf</tt> must satisfy all of
   * the following conditions
   * <ul>
   * <li><tt>pdf != null && pdf.length &gt; 0</tt>
   * <li><tt>0.0 &lt;= pdf[i] : 0 &lt; =i &lt;= pdf.length-1</tt>
   * <li><tt>0.0 &lt; Sum(pdf[i]) : 0 &lt;=i &lt;= pdf.length-1</tt>
   * </ul>
   *
   * @param pdf
   *            probability distribution function.
   * @throws IllegalArgumentException
   *             if at least one of the three conditions above is violated.
   */
  def setState(pdf: Array[Double], interpolationType: Int) {
    if (pdf == null || pdf.length == 0) {
      throw new IllegalArgumentException("Non-existing pdf")
    }
    val nBins = pdf.length
    this.cdf = Array.ofDim[Double](nBins + 1)
    cdf(0) = 0
    for (i <- 0 until nBins) {
      if (pdf(i) < 0.0) throw new IllegalArgumentException("Negative probability")
      cdf(i + 1) = cdf(i) + pdf(i)
    }
    if (cdf(nBins) <= 0.0) throw new IllegalArgumentException("At leat one probability must be > 0.0")
    for (i <- 0 until nBins + 1) {
      cdf(i) /= cdf(nBins)
    }
  }

  /**
   * Sets the distribution parameters. The <tt>pdf</tt> must satisfy both of
   * the following conditions
   * <ul>
   * <li><tt>0.0 &lt;= pdf[i] : 0 &lt; =i &lt;= pdf.length-1</tt>
   * <li><tt>0.0 &lt; Sum(pdf[i]) : 0 &lt;=i &lt;= pdf.length-1</tt>
   * </ul>
   *
   * @param pdf
   *            probability distribution function.
   * @throws IllegalArgumentException
   *             if at least one of the three conditions above is violated.
   */
  def setState2(pdf: Array[Double]) {
    val size = pdf.length
    var k: Int = 0
    var s: Int = 0
    var b: Int = 0
    var nBigs: Int = 0
    var nSmalls: Int = 0
    var Bigs: Stack = null
    var Smalls: Stack = null
    var E: Array[Double] = null
    var pTotal = 0
    var mean: Double = 0.0
    var d: Double = 0.0
    k = 0
    while (k < size) {
      pTotal += pdf(k)
      k
    }
    this.K = size
    this.F = Array.ofDim[Double](size)
    this.A = Array.ofDim[Int](size)
    E = Array.ofDim[Double](size)
    k = 0
    while (k < size) {
      E(k) = pdf(k) / pTotal
      k
    }
    mean = 1.0 / size
    nSmalls = 0
    nBigs = 0
    k = 0
    while (k < size) {
      if (E(k) < mean) nSmalls else nBigs
      k
    }
    Bigs = new Stack(nBigs)
    Smalls = new Stack(nSmalls)
    k = 0
    while (k < size) {
      if (E(k) < mean) {
        Smalls.push(k)
      } else {
        Bigs.push(k)
      }
      k
    }
    while (Smalls.size > 0) {
      s = Smalls.pop()
      if (Bigs.size == 0) {
        this.A(s) = s
        this.F(s) = 1.0
        //break
      }
      b = Bigs.pop()
      this.A(s) = b
      this.F(s) = size * E(s)
      d = mean - E(s)
      E(s) += d
      E(b) -= d
      if (E(b) < mean) {
        Smalls.push(b)
      } else if (E(b) > mean) {
        Bigs.push(b)
      } else {
        this.A(b) = b
        this.F(b) = 1.0
      }
    }
    while (Bigs.size > 0) {
      b = Bigs.pop()
      this.A(b) = b
      this.F(b) = 1.0
    }
  }

  /**
   * Returns a String representation of the receiver.
   */
  override def toString(): String = {
    val interpolation: String = null
    this.getClass.getName + "(" + (if ((cdf != null)) cdf.length else 0) + 
      ")"
  }
}
