package cern.jet.random.tdouble

import cern.jet.math.tdouble.DoubleArithmetic
import cern.jet.random.tdouble.engine.DoubleRandomEngine
import HyperGeometric._
//remove if not needed
import scala.collection.JavaConversions._

object HyperGeometric {

  protected var shared: HyperGeometric = new HyperGeometric(1, 1, 1, makeDefaultGenerator())

  private def fc_lnpk(k: Int, 
      N_Mn: Int, 
      M: Int, 
      n: Int): Double = {
    (DoubleArithmetic.logFactorial(k) + DoubleArithmetic.logFactorial(M - k) + 
      DoubleArithmetic.logFactorial(n - k) + 
      DoubleArithmetic.logFactorial(N_Mn + k))
  }

  /**
   * Returns a random number from the distribution.
   */
  def staticNextInt(N: Int, M: Int, n: Int): Double = {
    synchronized (shared) {
      shared.nextInt(N, M, n)
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
 * HyperGeometric distribution; See the <A
 * HREF="http://library.advanced.org/10030/6atpdvah.htm"> math definition</A>
 *
 * The hypergeometric distribution with parameters <tt>N</tt>, <tt>n</tt> and
 * <tt>s</tt> is the probability distribution of the random variable X, whose
 * value is the number of successes in a sample of <tt>n</tt> items from a
 * population of size <tt>N</tt> that has <tt>s</tt> 'success' items and
 * <tt>N - s</tt> 'failure' items.
 * <p>
 * <tt>p(k) = C(s,k) * C(N-s,n-k) / C(N,n)</tt> where
 * <tt>C(a,b) = a! / (b! * (a-b)!)</tt>.
 * <p>
 * valid for N >= 2, s,n <= N.
 * <p>
 * Instance methods operate on a user supplied uniform random number generator;
 * they are unsynchronized.
 * <dt>Static methods operate on a default uniform random number generator; they
 * are synchronized.
 * <p>
 * <b>Implementation:</b> High performance implementation. Patchwork
 * Rejection/Inversion method.
 * <dt>This is a port of <tt>hprsc.c</tt> from the <A
 * HREF="http://www.cis.tu-graz.ac.at/stat/stadl/random.html">C-RAND /
 * WIN-RAND</A> library. C-RAND's implementation, in turn, is based upon
 * <p>
 * H. Zechner (1994): Efficient sampling from continuous and discrete unimodal
 * distributions, Doctoral Dissertation, 156 pp., Technical University Graz,
 * Austria.
 *
 * @author wolfgang.hoschek@cern.ch
 * @version 1.0, 09/24/99
 */
@SerialVersionUID(1L)
class HyperGeometric(N: Int, 
    s: Int, 
    n: Int, 
    randomGenerator: DoubleRandomEngine) extends AbstractDiscreteDistribution {

  protected var my_N: Int = _

  protected var my_s: Int = _

  protected var my_n: Int = _

  private var N_last: Int = -1

  private var M_last: Int = -1

  private var n_last: Int = -1

  private var N_Mn: Int = _

  private var m: Int = _

  private var mp: Int = _

  private var b: Int = _

  private var Mp: Double = _

  private var np: Double = _

  private var fm: Double = _

  private var k2: Int = _

  private var k4: Int = _

  private var k1: Int = _

  private var k5: Int = _

  private var dl: Double = _

  private var dr: Double = _

  private var r1: Double = _

  private var r2: Double = _

  private var r4: Double = _

  private var r5: Double = _

  private var ll: Double = _

  private var lr: Double = _

  private var c_pm: Double = _

  private var f1: Double = _

  private var f2: Double = _

  private var f4: Double = _

  private var f5: Double = _

  private var p1: Double = _

  private var p2: Double = _

  private var p3: Double = _

  private var p4: Double = _

  private var p5: Double = _

  private var p6: Double = _

  setRandomGenerator(randomGenerator)

  setState(N, s, n)

  /**
   * Returns a random number from the distribution.
   */
  protected def hmdu(N: Int, 
      M: Int, 
      n: Int, 
      randomGenerator: DoubleRandomEngine): Int = {
    var I: Int = 0
    var K: Int = 0
    var p: Double = 0.0
    var nu: Double = 0.0
    var c: Double = 0.0
    var d: Double = 0.0
    var U: Double = 0.0
    if (N != N_last || M != M_last || n != n_last) {
      N_last = N
      M_last = M
      n_last = n
      Mp = (M + 1)
      np = (n + 1)
      N_Mn = N - M - n
      p = Mp / (N + 2.0)
      nu = np * p
      mp = if ((m = nu.toInt) == nu && p == 0.5) m -= 1 else m + 1
      fm = Math.exp(DoubleArithmetic.logFactorial(N - M) - DoubleArithmetic.logFactorial(N_Mn + m) - 
        DoubleArithmetic.logFactorial(n - m) + 
        DoubleArithmetic.logFactorial(M) - 
        DoubleArithmetic.logFactorial(M - m) - 
        DoubleArithmetic.logFactorial(m) - 
        DoubleArithmetic.logFactorial(N) + 
        DoubleArithmetic.logFactorial(N - n) + 
        DoubleArithmetic.logFactorial(n))
      b = (nu + 
        11.0 * 
        Math.sqrt(nu * (1.0 - p) * (1.0 - n / N.toDouble) + 1.0)).toInt
      if (b > n) b = n
    }
    while (true) {
      if ((U = randomGenerator.raw() - fm) <= 0.0) return (m)
      c = d = fm
      I = 1
      while (I <= m) {
        K = mp - I
        c *= K / (np - K) * ((N_Mn + K) / (Mp - K))
        if ((U -= c) <= 0.0) return (K - 1)
        K = m + I
        d *= (np - K) / K * ((Mp - K) / (N_Mn + K))
        if ((U -= d) <= 0.0) return (K)
        I += 1
      }
      K = mp + m
      while (K <= b) {
        d *= (np - K) / K * ((Mp - K) / (N_Mn + K))
        if ((U -= d) <= 0.0) return (K)
        K += 1
      }
    }
  }

  /**
   * Returns a random number from the distribution.
   */
  protected def hprs(N: Int, 
      M: Int, 
      n: Int, 
      randomGenerator: DoubleRandomEngine): Int = {
    var Dk: Int = 0
    var X: Int = 0
    var V: Int = 0
    var Mp: Double = 0.0
    var np: Double = 0.0
    var p: Double = 0.0
    var nu: Double = 0.0
    var U: Double = 0.0
    var Y: Double = 0.0
    var W: Double = 0.0
    if (N != N_last || M != M_last || n != n_last) {
      N_last = N
      M_last = M
      n_last = n
      Mp = (M + 1)
      np = (n + 1)
      N_Mn = N - M - n
      p = Mp / (N + 2.0)
      nu = np * p
      U = Math.sqrt(nu * (1.0 - p) * (1.0 - (n + 2.0) / (N + 3.0)) + 0.25)
      m = nu.toInt
      k2 = Math.ceil(nu - 0.5 - U).toInt
      if (k2 >= m) k2 = m - 1
      k4 = (nu - 0.5 + U).toInt
      k1 = k2 + k2 - m + 1
      k5 = k4 + k4 - m
      dl = (k2 - k1)
      dr = (k5 - k4)
      r1 = (np / k1 - 1.0) * (Mp - k1) / (N_Mn + k1)
      r2 = (np / k2 - 1.0) * (Mp - k2) / (N_Mn + k2)
      r4 = (np / (k4 + 1) - 1.0) * (M - k4) / (N_Mn + k4 + 1)
      r5 = (np / (k5 + 1) - 1.0) * (M - k5) / (N_Mn + k5 + 1)
      ll = Math.log(r1)
      lr = -Math.log(r5)
      c_pm = fc_lnpk(m, N_Mn, M, n)
      f2 = Math.exp(c_pm - fc_lnpk(k2, N_Mn, M, n))
      f4 = Math.exp(c_pm - fc_lnpk(k4, N_Mn, M, n))
      f1 = Math.exp(c_pm - fc_lnpk(k1, N_Mn, M, n))
      f5 = Math.exp(c_pm - fc_lnpk(k5, N_Mn, M, n))
      p1 = f2 * (dl + 1.0)
      p2 = f2 * dl + p1
      p3 = f4 * (dr + 1.0) + p2
      p4 = f4 * dr + p3
      p5 = f1 / ll + p4
      p6 = f5 / lr + p5
    }
    while (true) {
      if ((U = randomGenerator.raw() * p6) < p2) {
        if ((W = U - p1) < 0.0) return (k2 + (U / f2).toInt)
        if ((Y = W / dl) < f1) return (k1 + (W / f1).toInt)
        Dk = (dl * randomGenerator.raw()).toInt + 1
        if (Y <= f2 - Dk * (f2 - f2 / r2)) {
          return (k2 - Dk)
        }
        if ((W = f2 + f2 - Y) < 1.0) {
          V = k2 + Dk
          if (W <= f2 + Dk * (1.0 - f2) / (dl + 1.0)) {
            return (V)
          }
          if (Math.log(W) <= c_pm - fc_lnpk(V, N_Mn, M, n)) {
            return (V)
          }
        }
        X = k2 - Dk
      } else if (U < p4) {
        if ((W = U - p3) < 0.0) return (k4 - ((U - p2) / f4).toInt)
        if ((Y = W / dr) < f5) return (k5 - (W / f5).toInt)
        Dk = (dr * randomGenerator.raw()).toInt + 1
        if (Y <= f4 - Dk * (f4 - f4 * r4)) {
          return (k4 + Dk)
        }
        if ((W = f4 + f4 - Y) < 1.0) {
          V = k4 - Dk
          if (W <= f4 + Dk * (1.0 - f4) / dr) {
            return (V)
          }
          if (Math.log(W) <= c_pm - fc_lnpk(V, N_Mn, M, n)) {
            return (V)
          }
        }
        X = k4 + Dk
      } else {
        Y = randomGenerator.raw()
        if (U < p5) {
          Dk = (1.0 - Math.log(Y) / ll).toInt
          if ((X = k1 - Dk) < 0) //continue
          Y *= (U - p4) * ll
          if (Y <= f1 - Dk * (f1 - f1 / r1)) {
            return (X)
          }
        } else {
          Dk = (1.0 - Math.log(Y) / lr).toInt
          if ((X = k5 + Dk) > n) //continue
          Y *= (U - p5) * lr
          if (Y <= f5 - Dk * (f5 - f5 * r5)) {
            return (X)
          }
        }
      }
      if (Math.log(Y) <= c_pm - fc_lnpk(X, N_Mn, M, n)) (X)
    }
  }

  /**
   * Returns a random number from the distribution.
   */
  def nextInt(): Int = {
    nextInt(this.my_N, this.my_s, this.my_n, this.randomGenerator)
  }

  /**
   * Returns a random number from the distribution; bypasses the internal
   * state.
   */
  def nextInt(N: Int, s: Int, n: Int): Int = nextInt(N, s, n, this.randomGenerator)

  /**
   * Returns a random number from the distribution; bypasses the internal
   * state.
   */
  protected def nextInt(N: Int, 
      M: Int, 
      n: Int, 
      randomGenerator: DoubleRandomEngine): Int = {
    var Nhalf: Int = 0
    var n_le_Nhalf: Int = 0
    var M_le_Nhalf: Int = 0
    var K: Int = 0
    Nhalf = N / 2
    n_le_Nhalf = if ((n <= Nhalf)) n else N - n
    M_le_Nhalf = if ((M <= Nhalf)) M else N - M
    K = if ((n * M / N) < 10) if ((n_le_Nhalf <= M_le_Nhalf)) hmdu(N, M_le_Nhalf, n_le_Nhalf, randomGenerator) else hmdu(N, 
      n_le_Nhalf, M_le_Nhalf, randomGenerator) else if ((n_le_Nhalf <= M_le_Nhalf)) hprs(N, M_le_Nhalf, 
      n_le_Nhalf, randomGenerator) else hprs(N, n_le_Nhalf, M_le_Nhalf, randomGenerator)
    if (n <= Nhalf) {
      if ((M <= Nhalf)) K else n - K
    } else {
      if ((M <= Nhalf)) M - K else n - N + M + K
    }
  }

  /**
   * Returns the probability distribution function.
   */
  def pdf(k: Int): Double = {
    DoubleArithmetic.binomial(my_s, k) * DoubleArithmetic.binomial(my_N - my_s, my_n - k) / 
      DoubleArithmetic.binomial(my_N, my_n)
  }

  /**
   * Sets the parameters.
   */
  def setState(N: Int, s: Int, n: Int) {
    this.my_N = N
    this.my_s = s
    this.my_n = n
  }

  /**
   * Returns a String representation of the receiver.
   */
  override def toString(): String = {
    this.getClass.getName + "(" + my_N + "," + my_s + "," + 
      my_n + 
      ")"
  }
}
