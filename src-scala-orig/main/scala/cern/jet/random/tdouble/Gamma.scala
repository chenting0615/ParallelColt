package cern.jet.random.tdouble

import cern.jet.random.tdouble.engine.DoubleRandomEngine
import cern.jet.stat.tdouble.Probability
import Gamma._
//remove if not needed
import scala.collection.JavaConversions._

object Gamma {

  protected var shared: Gamma = new Gamma(1.0, 1.0, makeDefaultGenerator())

  /**
   * Returns a random number from the distribution.
   *
   * @throws IllegalArgumentException
   *             if <tt>alpha &lt;= 0.0 || lambda &lt;= 0.0</tt>.
   */
  def staticNextDouble(alpha: Double, lambda: Double): Double = {
    synchronized (shared) {
      shared.nextDouble(alpha, lambda)
    }
  }

  /**
   * Sets the uniform random number generated shared by all <b>static</b>
   * methods.
   *
   * @param randomGenerator
   *            the new uniform random number generator to be shared.
   */
  private def xstaticSetRandomGenerator(randomGenerator: DoubleRandomEngine) {
    synchronized (shared) {
      shared.setRandomGenerator(randomGenerator)
    }
  }
}

/**
 * Gamma distribution; <A
 * HREF="http://wwwinfo.cern.ch/asdoc/shortwrupsdir/g106/top.html"> math
 * definition</A>, <A HREF=
 * "http://www.cern.ch/RD11/rkb/AN16pp/node96.html#SECTION000960000000000000000"
 * > definition of gamma function</A> and <A
 * HREF="http://www.statsoft.com/textbook/glosf.html#Gamma Distribution">
 * animated definition</A>.
 * <p>
 * <tt>p(x) = k * x^(alpha-1) * e^(-x/beta)</tt> with
 * <tt>k = 1/(g(alpha) * b^a))</tt> and <tt>g(a)</tt> being the gamma function.
 * <p>
 * Valid parameter ranges: <tt>alpha &gt; 0</tt>.
 * <p>
 * Note: For a Gamma distribution to have the mean <tt>mean</tt> and variance
 * <tt>variance</tt>, set the parameters as follows:
 *
 * <pre>
 * alpha = mean * mean / variance;
 * lambda = 1 / (variance / mean);
 * </pre>
 *
 * <p>
 * Instance methods operate on a user supplied uniform random number generator;
 * they are unsynchronized.
 * <dt>Static methods operate on a default uniform random number generator; they
 * are synchronized.
 * <p>
 * <b>Implementation:</b>
 * <dt>Method: Acceptance Rejection combined with Acceptance Complement.
 * <dt>High performance implementation. This is a port of <A HREF=
 * "http://wwwinfo.cern.ch/asd/lhc++/clhep/manual/RefGuide/Random/RandGamma.html"
 * >RandGamma</A> used in <A HREF="http://wwwinfo.cern.ch/asd/lhc++/clhep">CLHEP
 * 1.4.0</A> (C++). CLHEP's implementation, in turn, is based on <tt>gds.c</tt>
 * from the <A HREF="http://www.cis.tu-graz.ac.at/stat/stadl/random.html">C-RAND
 * / WIN-RAND</A> library. C-RAND's implementation, in turn, is based upon
 * <p>
 * J.H. Ahrens, U. Dieter (1974): Computer methods for sampling from gamma,
 * beta, Poisson and binomial distributions, Computing 12, 223-246.
 * <p>
 * and
 * <p>
 * J.H. Ahrens, U. Dieter (1982): Generating gamma variates by a modified
 * rejection technique, Communications of the ACM 25, 47-54.
 *
 * @author wolfgang.hoschek@cern.ch
 * @version 1.0, 09/24/99
 */
@SerialVersionUID(1L)
class Gamma(alpha: Double, lambda: Double, randomGenerator: DoubleRandomEngine)
    extends AbstractContinousDoubleDistribution {

  protected var alpha: Double = _

  protected var lambda: Double = _

  setRandomGenerator(randomGenerator)

  setState(alpha, lambda)

  /**
   * Returns the cumulative distribution function.
   */
  def cdf(x: Double): Double = Probability.gamma(alpha, lambda, x)

  /**
   * Returns a random number from the distribution.
   */
  def nextDouble(): Double = nextDouble(alpha, lambda)

  /**
   * Returns a random number from the distribution; bypasses the internal
   * state.
   */
  def nextDouble(alpha: Double, lambda: Double): Double = {
    val a = alpha
    var aa = -1.0
    var aaa = -1.0
    var b = 0.0
    var c = 0.0
    var d = 0.0
    var e: Double = 0.0
    var r: Double = 0.0
    var s = 0.0
    var si = 0.0
    var ss = 0.0
    var q0 = 0.0
    var q1 = 0.0416666664
    var q2 = 0.0208333723
    var q3 = 0.0079849875
    var q4 = 0.0015746717
    var q5 = -0.0003349403
    var q6 = 0.0003340332
    var q7 = 0.0006053049
    var q8 = -0.0004701849
    var q9 = 0.0001710320
    var a1 = 0.333333333
    var a2 = -0.249999949
    var a3 = 0.199999867
    var a4 = -0.166677482
    var a5 = 0.142873973
    var a6 = -0.124385581
    var a7 = 0.110368310
    var a8 = -0.112750886
    var a9 = 0.104089866
    var e1 = 1.000000000
    var e2 = 0.499999994
    var e3 = 0.166666848
    var e4 = 0.041664508
    var e5 = 0.008345522
    var e6 = 0.001353826
    var e7 = 0.000247453
    var gds: Double = 0.0
    var p: Double = 0.0
    var q: Double = 0.0
    var t: Double = 0.0
    var sign_u: Double = 0.0
    var u: Double = 0.0
    var v: Double = 0.0
    var w: Double = 0.0
    var x: Double = 0.0
    var v1: Double = 0.0
    var v2: Double = 0.0
    var v12: Double = 0.0
    if (a <= 0.0) throw new IllegalArgumentException()
    if (lambda <= 0.0) new IllegalArgumentException()
    if (a < 1.0) {
      b = 1.0 + 0.36788794412 * a
      while (true) {
        p = b * randomGenerator.raw()
        if (p <= 1.0) {
          gds = Math.exp(Math.log(p) / a)
          if (Math.log(randomGenerator.raw()) <= -gds) (gds / lambda)
        } else {
          gds = -Math.log((b - p) / a)
          if (Math.log(randomGenerator.raw()) <= ((a - 1.0) * Math.log(gds))) (gds / lambda)
        }
      }
    } else {
      if (a != aa) {
        aa = a
        ss = a - 0.5
        s = Math.sqrt(ss)
        d = 5.656854249 - 12.0 * s
      }
      do {
        v1 = 2.0 * randomGenerator.raw() - 1.0
        v2 = 2.0 * randomGenerator.raw() - 1.0
        v12 = v1 * v1 + v2 * v2
      } while (v12 > 1.0);
      t = v1 * Math.sqrt(-2.0 * Math.log(v12) / v12)
      x = s + 0.5 * t
      gds = x * x
      if (t >= 0.0) return (gds / lambda)
      u = randomGenerator.raw()
      if (d * u <= t * t * t) return (gds / lambda)
      if (a != aaa) {
        aaa = a
        r = 1.0 / a
        q0 = ((((((((q9 * r + q8) * r + q7) * r + q6) * r + q5) * r + q4) * 
          r + 
          q3) * 
          r + 
          q2) * 
          r + 
          q1) * 
          r
        if (a > 3.686) {
          if (a > 13.022) {
            b = 1.77
            si = 0.75
            c = 0.1515 / s
          } else {
            b = 1.654 + 0.0076 * ss
            si = 1.68 / s + 0.275
            c = 0.062 / s + 0.024
          }
        } else {
          b = 0.463 + s - 0.178 * ss
          si = 1.235
          c = 0.195 / s - 0.079 + 0.016 * s
        }
      }
      if (x > 0.0) {
        v = t / (s + s)
        q = if (Math.abs(v) > 0.25) q0 - s * t + 0.25 * t * t + (ss + ss) * Math.log(1.0 + v) else q0 + 
          0.5 * t * t * 
          ((((((((a9 * v + a8) * v + a7) * v + a6) * v + a5) * v + a4) * 
          v + 
          a3) * 
          v + 
          a2) * 
          v + 
          a1) * 
          v
        if (Math.log(1.0 - u) <= q) return (gds / lambda)
      }
      while (true) {
        do {
          e = -Math.log(randomGenerator.raw())
          u = randomGenerator.raw()
          u = u + u - 1.0
          sign_u = if ((u > 0)) 1.0 else -1.0
          t = b + (e * si) * sign_u
        } while (t <= -0.71874483771719);
        v = t / (s + s)
        q = if (Math.abs(v) > 0.25) q0 - s * t + 0.25 * t * t + (ss + ss) * Math.log(1.0 + v) else q0 + 
          0.5 * t * t * 
          ((((((((a9 * v + a8) * v + a7) * v + a6) * v + a5) * v + a4) * 
          v + 
          a3) * 
          v + 
          a2) * 
          v + 
          a1) * 
          v
        if (q <= 0.0) //continue
        w = if (q > 0.5) Math.exp(q) - 1.0 else ((((((e7 * q + e6) * q + e5) * q + e4) * q + e3) * q + e2) * 
          q + 
          e1) * 
          q
        if (c * u * sign_u <= w * Math.exp(e - 0.5 * t * t)) {
          x = s + 0.5 * t
          (x * x / lambda)
        }
      }
    }
  }

  /**
   * Returns the probability distribution function.
   */
  def pdf(x: Double): Double = {
    if (x < 0) throw new IllegalArgumentException()
    if (x == 0) {
      if (alpha == 1.0) return 1.0 / lambda else return 0.0
    }
    if (alpha == 1.0) return Math.exp(-x / lambda) / lambda
    Math.exp((alpha - 1.0) * Math.log(x / lambda) - x / lambda - Fun.logGamma(alpha)) / 
      lambda
  }

  /**
   * Sets the mean and variance.
   *
   * @throws IllegalArgumentException
   *             if <tt>alpha &lt;= 0.0 || lambda &lt;= 0.0</tt>.
   */
  def setState(alpha: Double, lambda: Double) {
    if (alpha <= 0.0) throw new IllegalArgumentException()
    if (lambda <= 0.0) throw new IllegalArgumentException()
    this.alpha = alpha
    this.lambda = lambda
  }

  /**
   * Returns a String representation of the receiver.
   */
  override def toString(): String = {
    this.getClass.getName + "(" + alpha + "," + lambda + ")"
  }
}
