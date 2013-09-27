package cern.jet.random.tdouble

import cern.jet.random.tdouble.engine.DoubleRandomEngine
import cern.jet.stat.tdouble.Probability
import Beta._

object Beta {

  protected var shared: Beta = new Beta(10.0, 10.0, makeDefaultGenerator())

  private def f(x: Double,
      a: Double,
      b: Double,
      m: Double): Double = {
    Math.exp(a * Math.log(x / m) + b * Math.log((1.0 - x) / (1.0 - m)))
  }

  /**
   * Returns a random number from the distribution.
   */
  def staticNextDouble(alpha: Double, beta: Double): Double = {
    synchronized (shared) {
      shared.nextDouble(alpha, beta)
    }
  }
}

/**
 * Beta distribution; <A HREF=
 * "http://www.cern.ch/RD11/rkb/AN16pp/node15.html#SECTION000150000000000000000"
 * > math definition</A> and <A
 * HREF="http://www.statsoft.com/textbook/glosb.html#Beta Distribution">
 * animated definition</A>.
 * <p>
 * <tt>p(x) = k * x^(alpha-1) * (1-x)^(beta-1)</tt> with
 * <tt>k = g(alpha+beta)/(g(alpha)*g(beta))</tt> and <tt>g(a)</tt> being the
 * gamma function.
 * <p>
 * Valid parameter ranges: <tt>alpha &gt; 0</tt> and <tt>beta &gt; 0</tt>.
 * <p>
 * Instance methods operate on a user supplied uniform random number generator;
 * they are unsynchronized.
 * <dt>Static methods operate on a default uniform random number generator; they
 * are synchronized.
 * <p>
 * <b>Implementation:</b>
 * <dt>Method: Stratified Rejection/Patchwork Rejection. High performance
 * implementation.
 * <dt>This is a port of <tt>bsprc.c</tt> from the <A
 * HREF="http://www.cis.tu-graz.ac.at/stat/stadl/random.html">C-RAND /
 * WIN-RAND</A> library. C-RAND's implementation, in turn, is based upon
 * <p>
 * H. Sakasegawa (1983): Stratified rejection and squeeze method for generating
 * beta random numbers, Ann. Inst. Statist. Math. 35 B, 291-302.
 * <p>
 * and
 * <p>
 * Stadlober E., H. Zechner (1993), <A
 * HREF="http://www.cis.tu-graz.ac.at/stat/stadl/random.html"> Generating beta
 * variates via patchwork rejection,</A>, Computing 50, 1-18.
 *
 * @author wolfgang.hoschek@cern.ch
 * @version 1.0, 09/24/99
 */
@SerialVersionUID(1L)
class Beta(protected var alpha: Double, protected var beta: Double, randomGenerator_p: DoubleRandomEngine) extends AbstractContinousDoubleDistribution {

  var PDF_CONST: Double = 0.0

  var a_last: Double = 0.0

  var b_last: Double = 0.0

  var a_a: Double = 0.0

  var b_b: Double = 0.0

  var t: Double = 0.0

  var fa: Double = 0.0

  var fb: Double = 0.0

  var p1: Double = 0.0

  var p2: Double = 0.0

  var c: Double = 0.0

  var ml: Double = 0.0

  var mu: Double = 0.0

  var p_last: Double = 0.0

  var q_last: Double = 0.0

  var a: Double = 0.0

  var b: Double = 0.0

  var s: Double = 0.0

  var m: Double = 0.0

  var D: Double = 0.0

  var Dl: Double = 0.0

  var x1: Double = 0.0

  var x2: Double = 0.0

  var x4: Double = 0.0

  var x5: Double = 0.0

  var f1: Double = 0.0

  var f2: Double = 0.0

  var f4: Double = 0.0

  var f5: Double = 0.0

  var ll: Double = 0.0

  var lr: Double = 0.0

  var z2: Double = 0.0

  var z4: Double = 0.0

  var p3: Double = 0.0

  var p4: Double = 0.0

  setRandomGenerator(randomGenerator_p)

  setState(alpha, beta)

  /**
   *
   */
  protected def b00(a: Double, b: Double, randomGenerator: DoubleRandomEngine): Double = {
    var U: Double = 0.0
    var V: Double = 0.0
    var X: Double = 0.0
    var Z: Double = 0.0
    if (a != a_last || b != b_last) {
      a_last = a
      b_last = b
      a_a = a - 1.0
      b_b = b - 1.0
      c = (b * b_b) / (a * a_a)
      t = if (c == 1.0) 0.5 else (1.0 - Math.sqrt(c)) / (1.0 - c)
      fa = Math.exp(a_a * Math.log(t))
      fb = Math.exp(b_b * Math.log(1.0 - t))
      p1 = t / a
      p2 = (1.0 - t) / b + p1
    }
    while (true) {
      U = randomGenerator.raw() * p2
      if (U <= p1) {
        Z = Math.exp(Math.log(U / p1) / a)
        X = t * Z
        V = randomGenerator.raw() * fb
        if (V <= 1.0 - b_b * X) //break
        if (V <= 1.0 + (fb - 1.0) * Z) {
          if (Math.log(V) <= b_b * Math.log(1.0 - X)) //break
        }
      } else {
        Z = Math.exp(Math.log((U - p1) / (p2 - p1)) / b)
        X = 1.0 - (1.0 - t) * Z
        V = randomGenerator.raw() * fa
        if (V <= 1.0 - a_a * (1.0 - X)) //break
        if (V <= 1.0 + (fa - 1.0) * Z) {
          if (Math.log(V) <= a_a * Math.log(X)) //break
        }
      }
    }
    X
  }

  /**
   *
   */
  protected def b01(a: Double, b: Double, randomGenerator: DoubleRandomEngine): Double = {
    var U: Double = 0.0
    var V: Double = 0.0
    var X: Double = 0.0
    var Z: Double = 0.0
    if (a != a_last || b != b_last) {
      a_last = a
      b_last = b
      a_a = a - 1.0
      b_b = b - 1.0
      t = a_a / (a - b)
      fb = Math.exp((b_b - 1.0) * Math.log(1.0 - t))
      fa = a - (a + b_b) * t
      t -= (t - (1.0 - fa) * (1.0 - t) * fb / b) / (1.0 - fa * fb)
      fa = Math.exp(a_a * Math.log(t))
      fb = Math.exp(b_b * Math.log(1.0 - t))
      if (b_b <= 1.0) {
        ml = (1.0 - fb) / t
        mu = b_b * t
      } else {
        ml = b_b
        mu = 1.0 - fb
      }
      p1 = t / a
      p2 = fb * (1.0 - t) / b + p1
    }
    while (true) {
      U = randomGenerator.raw() * p2
      if (U <= p1) {
        Z = Math.exp(Math.log(U / p1) / a)
        X = t * Z
        V = randomGenerator.raw()
        if (V <= 1.0 - ml * X) //break
        if (V <= 1.0 - mu * Z) {
          if (Math.log(V) <= b_b * Math.log(1.0 - X)) //break
        }
      } else {
        Z = Math.exp(Math.log((U - p1) / (p2 - p1)) / b)
        X = 1.0 - (1.0 - t) * Z
        V = randomGenerator.raw() * fa
        if (V <= 1.0 - a_a * (1.0 - X)) //break
        if (V <= 1.0 + (fa - 1.0) * Z) {
          if (Math.log(V) <= a_a * Math.log(X)) //break
        }
      }
    }
    X
  }

  /**
   *
   */
  protected def b1prs(p: Double, q: Double, randomGenerator: DoubleRandomEngine): Double = {
    var U: Double = 0.0
    var V: Double = 0.0
    var W: Double = 0.0
    var X: Double = 0.0
    var Y: Double = 0.0
    if (p != p_last || q != q_last) {
      p_last = p
      q_last = q
      a = p - 1.0
      b = q - 1.0
      s = a + b
      m = a / s
      if (a > 1.0 || b > 1.0) D = Math.sqrt(m * (1.0 - m) / (s - 1.0))
      if (a <= 1.0) {
        Dl = m * 0.5
        x2 = Dl
        z2 = 0.0
        x1 = z2
        ll = 0.0
        f1 = ll
      } else {
        x2 = m - D
        x1 = x2 - D
        z2 = x2 * (1.0 - (1.0 - x2) / (s * D))
        if (x1 <= 0.0 || (s - 6.0) * x2 - a + 3.0 > 0.0) {
          x1 = z2
          x2 = (x1 + m) * 0.5
          Dl = m - x2
        } else {
          Dl = D
        }
        f1 = f(x1, a, b, m)
        ll = x1 * (1.0 - x1) / (s * (m - x1))
      }
      f2 = f(x2, a, b, m)
      if (b <= 1.0) {
        D = (1.0 - m) * 0.5
        x4 = 1.0 - D
        z4 = 1.0
        x5 = z4
        lr = 0.0
        f5 = lr
      } else {
        x4 = m + D
        x5 = x4 + D
        z4 = x4 * (1.0 + (1.0 - x4) / (s * D))
        if (x5 >= 1.0 || (s - 6.0) * x4 - a + 3.0 < 0.0) {
          x5 = z4
          x4 = (m + x5) * 0.5
          D = x4 - m
        }
        f5 = f(x5, a, b, m)
        lr = x5 * (1.0 - x5) / (s * (x5 - m))
      }
      f4 = f(x4, a, b, m)
      p1 = f2 * (Dl + Dl)
      p2 = f4 * (D + D) + p1
      p3 = f1 * ll + p2
      p4 = f5 * lr + p3
    }
    while (true) {
      U = randomGenerator.raw() * p4
      if (U <= p1) {
        W = U / Dl - f2
        if (W <= 0.0) return m - U / f2
        if (W <= f1) return x2 - W / f1 * Dl
        U = randomGenerator.raw()
        V = Dl * U
        X = x2 - V
        Y = x2 + V
        if (W * (x2 - z2) <= f2 * (X - z2)) return X
        V = f2 + f2 - W
        if (V < 1.0) {
          if (V <= f2 + (1.0 - f2) * U) return Y
          if (V <= f(Y, a, b, m)) return Y
        }
      } else if (U <= p2) {
        U -= p1
        W = U / D - f4
        if (W  <= 0.0) return m + U / f4
        if (W <= f5) return x4 + W / f5 * D
        U = randomGenerator.raw()
        V = D * U
        X = x4 + V
        Y = x4 - V
        if (W * (z4 - x4) <= f4 * (z4 - X)) return X
        V = f4 + f4 - W
        if (V < 1.0) {
          if (V <= f4 + (1.0 - f4) * U) return Y
          if (V <= f(Y, a, b, m)) return Y
        }
      } else if (U <= p3) {
        Y = Math.log(U = (U - p2) / (p3 - p2))
        X = x1 + ll * Y
        if (X <= 0.0) //continue
        W = randomGenerator.raw() * U
        if (W <= 1.0 + Y) return X
        W *= f1
      } else {
        Y = Math.log(U = (U - p3) / (p4 - p3))
        X = x5 - lr * Y
        if (X >= 1.0) //continue
        W = randomGenerator.raw() * U
        if (W <= 1.0 + Y) return X
        W *= f5
      }
      if (Math.log(W) <=
        a * Math.log(X / m) + b * Math.log((1.0 - X) / (1.0 - m))) X
    }
    0.0 // Never happen
  }

  /**
   * Returns the cumulative distribution function.
   */
  def cdf(x: Double): Double = Probability.beta(alpha, beta, x)

  /**
   * Returns a random number from the distribution.
   */
  def nextDouble(): Double = nextDouble(alpha, beta)

  /**
   * Returns a beta distributed random number; bypasses the internal state.
   */
  def nextDouble(alpha: Double, beta: Double): Double = {
    val a = alpha
    val b = beta
    if (a > 1.0) {
      if (b > 1.0) return b1prs(a, b, randomGenerator)
      if (b < 1.0) return 1.0 - b01(b, a, randomGenerator)
      if (b == 1.0) {
        return Math.exp(Math.log(randomGenerator.raw()) / a)
      }
    }
    if (a < 1.0) {
      if (b > 1.0) return b01(a, b, randomGenerator)
      if (b < 1.0) return b00(a, b, randomGenerator)
      if (b == 1.0) {
        return Math.exp(Math.log(randomGenerator.raw()) / a)
      }
    }
    if (a == 1.0) {
      if (b != 1.0) return 1.0 - Math.exp(Math.log(randomGenerator.raw()) / b)
      if (b == 1.0) return randomGenerator.raw()
    }
    0.0
  }

  /**
   * Returns the cumulative distribution function.
   */
  def pdf(x: Double): Double = {
    if (x < 0 || x > 1) return 0.0
    Math.exp(PDF_CONST) * Math.pow(x, alpha - 1) * Math.pow(1 - x, beta - 1)
  }

  /**
   * Sets the parameters.
   */
  def setState(alpha: Double, beta: Double) {
    this.alpha = alpha
    this.beta = beta
    this.PDF_CONST = Fun.logGamma(alpha + beta) - Fun.logGamma(alpha) - Fun.logGamma(beta)
  }

  /**
   * Returns a String representation of the receiver.
   */
  override def toString: String = {
    this.getClass.getName + "(" + alpha + "," + beta + ")"
  }
}
