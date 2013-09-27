package cern.jet.random.tdouble

import cern.jet.math.tdouble.DoubleArithmetic
import cern.jet.random.tdouble.engine.DoubleRandomEngine
import cern.jet.stat.tdouble.Probability
import Binomial._
//remove if not needed
import scala.collection.JavaConversions._

object Binomial {

  protected var shared: Binomial = new Binomial(1, 0.5, makeDefaultGenerator())

  /**
   * Returns a random number from the distribution with the given parameters n
   * and p.
   *
   * @param n
   *            the number of trials
   * @param p
   *            the probability of success.
   * @throws IllegalArgumentException
   *             if <tt>n*Math.min(p,1-p) &lt;= 0.0</tt>
   */
  def staticNextInt(n: Int, p: Double): Int = {
    synchronized (shared) {
      shared.nextInt(n, p)
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
 * Binomial distribution; See the <A HREF=
 * "http://www.cern.ch/RD11/rkb/AN16pp/node19.html#SECTION000190000000000000000"
 * > math definition</A> and <A
 * HREF="http://www.statsoft.com/textbook/glosb.html#Binomial Distribution">
 * animated definition</A>.
 * <p>
 * <tt>p(x) = k * p^k * (1-p)^(n-k)</tt> with <tt>k = n! / (k! * (n-k)!)</tt>.
 * <p>
 * Instance methods operate on a user supplied uniform random number generator;
 * they are unsynchronized.
 * <dt>Static methods operate on a default uniform random number generator; they
 * are synchronized.
 * <p>
 * <b>Implementation:</b> High performance implementation. Acceptance
 * Rejection/Inversion method. This is a port of <A HREF="http://wwwinfo.cern.ch/asd/lhc++/clhep/manual/RefGuide/Random/RandBinomial.html"
 * >RandBinomial</A> used in <A
 * HREF="http://wwwinfo.cern.ch/asd/lhc++/clhep">CLHEP 1.4.0</A> (C++). CLHEP's
 * implementation is, in turn, based on
 * <p>
 * V. Kachitvichyanukul, B.W. Schmeiser (1988): Binomial random variate
 * generation, Communications of the ACM 31, 216-222.
 *
 * @author wolfgang.hoschek@cern.ch
 * @version 1.0, 09/24/99
 */
@SerialVersionUID(1L)
class Binomial(n: Int, p: Double, randomGenerator: DoubleRandomEngine) extends AbstractDiscreteDistribution {

  protected var n: Int = _

  protected var p: Double = _

  private var n_last: Int = -1

  private var n_prev: Int = -1

  private var par: Double = _

  private var np: Double = _

  private var p0: Double = _

  private var q: Double = _

  private var p_last: Double = -1.0

  private var p_prev: Double = -1.0

  private var b: Int = _

  private var m: Int = _

  private var nm: Int = _

  private var pq: Double = _

  private var rc: Double = _

  private var ss: Double = _

  private var xm: Double = _

  private var xl: Double = _

  private var xr: Double = _

  private var ll: Double = _

  private var lr: Double = _

  private var c: Double = _

  private var p1: Double = _

  private var p2: Double = _

  private var p3: Double = _

  private var p4: Double = _

  private var ch: Double = _

  private var log_p: Double = _

  private var log_q: Double = _

  private var log_n: Double = _

  setRandomGenerator(randomGenerator)

  setNandP(n, p)

  /**
   * Returns the cumulative distribution function.
   */
  def cdf(k: Int): Double = Probability.binomial(k, n, p)

  /**
   * Returns the cumulative distribution function.
   */
  private def cdfSlow(k: Int): Double = {
    if (k < 0) throw new IllegalArgumentException()
    var sum = 0.0
    var r = 0
    while (r <= k) {sum += pdf(r)r += 1
    }
    sum
  }

  /**
   *************************************************************************
   * * Binomial-Distribution - Acceptance Rejection/Inversion * *
   * ***************************************************************** *
   * Acceptance Rejection method combined with Inversion for * generating
   * Binomial random numbers with parameters * n (number of trials) and p
   * (probability of success). * For min(n*p,n*(1-p)) < 10 the Inversion
   * method is applied: * The random numbers are generated via sequential
   * search, * starting at the lowest index k=0. The cumulative probabilities
   * * are avoided by using the technique of chop-down. * For min(n*p,n*(1-p))
   * >= 10 Acceptance Rejection is used: * The algorithm is based on a
   * hat-function which is uniform in * the centre region and exponential in
   * the tails. * A triangular immediate acceptance region in the centre
   * speeds * up the generation of binomial variates. * If candidate k is near
   * the mode, f(k) is computed recursively * starting at the mode m. * The
   * acceptance test by Stirling's formula is modified * according to W.
   * Hoermann (1992): The generation of binomial * random variates, to appear
   * in J. Statist. Comput. Simul. * If p < .5 the algorithm is applied to
   * parameters n, p. * Otherwise p is replaced by 1-p, and k is replaced by n
   * - k. * *
   * ***************************************************************** *
   * FUNCTION: - samples a random number from the binomial * distribution with
   * parameters n and p and is * valid for n*min(p,1-p) > 0. * REFERENCE: - V.
   * Kachitvichyanukul, B.W. Schmeiser (1988): * Binomial random variate
   * generation, * Communications of the ACM 31, 216-222. * SUBPROGRAMS: -
   * StirlingCorrection() * ... Correction term of the Stirling *
   * approximation for log(k!) * (series in 1/k or table values * for small k)
   * with long int k * - randomGenerator ... (0,1)-Uniform engine * *
   *************************************************************************
   */
  protected def generateBinomial(n: Int, p: Double): Int = {
    val C1_3 = 0.33333333333333333
    val C5_8 = 0.62500000000000000
    val C1_6 = 0.16666666666666667
    val DMAX_KM = 20
    var bh: Int = 0
    var i: Int = 0
    var K: Int = 0
    var Km: Int = 0
    var nK: Int = 0
    var f: Double = 0.0
    var rm: Double = 0.0
    var U: Double = 0.0
    var V: Double = 0.0
    var X: Double = 0.0
    var T: Double = 0.0
    var E: Double = 0.0
    if (n != n_last || p != p_last) {
      n_last = n
      p_last = p
      par = Math.min(p, 1.0 - p)
      q = 1.0 - par
      np = n * par
      if (np <= 0.0) return -1
      rm = np + par
      m = rm.toInt
      if (np < 10) {
        p0 = Math.exp(n * Math.log(q))
        bh = (np + 10.0 * Math.sqrt(np * q)).toInt
        b = Math.min(n, bh)
      } else {
        rc = (n + 1.0) * (pq = par / q)
        ss = np * q
        i = (2.195 * Math.sqrt(ss) - 4.6 * q).toInt
        xm = m + 0.5
        xl = (m - i)
        xr = (m + i + 1L)
        f = (rm - xl) / (rm - xl * par)
        ll = f * (1.0 + 0.5 * f)
        f = (xr - rm) / (xr * q)
        lr = f * (1.0 + 0.5 * f)
        c = 0.134 + 20.5 / (15.3 + m)
        p1 = i + 0.5
        p2 = p1 * (1.0 + c + c)
        p3 = p2 + c / ll
        p4 = p3 + c / lr
      }
    }
    if (np < 10) {
      var pk: Double = 0.0
      K = 0
      pk = p0
      U = randomGenerator.raw()
      while (U > pk) {
        K
        if (K > b) {
          U = randomGenerator.raw()
          K = 0
          pk = p0
        } else {
          U -= pk
          pk = (((n - K + 1) * par * pk) / (K * q))
        }
      }
      return (if ((p > 0.5)) (n - K) else K)
    }
    while (true) {
      V = randomGenerator.raw()
      if ((U = randomGenerator.raw() * p4) <= p1) {
        K = (xm - U + p1 * V).toInt
        return if ((p > 0.5)) (n - K) else K
      }
      if (U <= p2) {
        X = xl + (U - p1) / c
        if ((V = V * c + 1.0 - Math.abs(xm - X) / p1) >= 1.0) //continue
        K = X.toInt
      } else if (U <= p3) {
        if ((X = xl + Math.log(V) / ll) < 0.0) //continue
        K = X.toInt
        V *= (U - p2) * ll
      } else {
        if ((K = (xr - Math.log(V) / lr).toInt) > n) //continue
        V *= (U - p3) * lr
      }
      if ((Km = Math.abs(K - m)) <= DMAX_KM || Km + Km + 2L >= ss) {
        f = 1.0
        if (m < K) {
          i = m
          while (i < K) {
            if ((f *= (rc / i - pq)) < V) //break
          }
        } else {
          i = K
          while (i < m) {
            if ((V *= (rc / i - pq)) > f) //break
          }
        }
        if (V <= f) //break
      } else {
        V = Math.log(V)
        T = -Km * Km / (ss + ss)
        E = (Km / ss) * ((Km * (Km * C1_3 + C5_8) + C1_6) / ss + 0.5)
        if (V <= T - E) //break
        if (V <= T + E) {
          if (n != n_prev || par != p_prev) {
            n_prev = n
            p_prev = par
            nm = n - m + 1
            ch = xm * Math.log((m + 1.0) / (pq * nm)) + DoubleArithmetic.stirlingCorrection(m + 1) + 
              DoubleArithmetic.stirlingCorrection(nm)
          }
          nK = n - K + 1
          if (V <= 
            ch + (n + 1.0) * Math.log(nm.toDouble / nK.toDouble) + 
            (K + 0.5) * Math.log(nK * pq / (K + 1.0)) - 
            DoubleArithmetic.stirlingCorrection(K + 1) - 
            DoubleArithmetic.stirlingCorrection(nK)) //break
        }
      }
    }
    if ((p > 0.5)) (n - K) else K
  }

  /**
   * Returns a random number from the distribution.
   */
  def nextInt(): Int = generateBinomial(n, p)

  /**
   * Returns a random number from the distribution with the given parameters n
   * and p; bypasses the internal state.
   *
   * @param n
   *            the number of trials
   * @param p
   *            the probability of success.
   * @throws IllegalArgumentException
   *             if <tt>n*Math.min(p,1-p) &lt;= 0.0</tt>
   */
  def nextInt(n: Int, p: Double): Int = {
    if (n * Math.min(p, 1 - p) <= 0.0) throw new IllegalArgumentException()
    generateBinomial(n, p)
  }

  /**
   * Returns the probability distribution function.
   */
  def pdf(k: Int): Double = {
    if (k < 0) throw new IllegalArgumentException()
    val r = this.n - k
    Math.exp(this.log_n - DoubleArithmetic.logFactorial(k) - DoubleArithmetic.logFactorial(r) + 
      this.log_p * k + 
      this.log_q * r)
  }

  /**
   * Sets the parameters number of trials and the probability of success.
   *
   * @param n
   *            the number of trials
   * @param p
   *            the probability of success.
   * @throws IllegalArgumentException
   *             if <tt>n*Math.min(p,1-p) &lt;= 0.0</tt>
   */
  def setNandP(n: Int, p: Double) {
    if (n * Math.min(p, 1 - p) <= 0.0) throw new IllegalArgumentException()
    this.n = n
    this.p = p
    this.log_p = Math.log(p)
    this.log_q = Math.log(1.0 - p)
    this.log_n = DoubleArithmetic.logFactorial(n)
  }

  /**
   * Returns a String representation of the receiver.
   */
  override def toString(): String = {
    this.getClass.getName + "(" + n + "," + p + ")"
  }
}
