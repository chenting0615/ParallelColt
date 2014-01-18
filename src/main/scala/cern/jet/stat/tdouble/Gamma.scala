package cern.jet.stat.tdouble

import cern.jet.math.tdouble.Polynomial
import cern.jet.math.tdouble.DoubleConstants._

/**
 * Gamma and Beta functions.
 * <p>
 * <b>Implementation:</b>
 * <dt>Some code taken and adapted from the <A
 * HREF="http://www.sci.usq.edu.au/staff/leighb/graph/Top.html">Java 2D Graph
 * Package 2.4</A>, which in turn is a port from the <A
 * HREF="http://people.ne.mediaone.net/moshier/index.html#Cephes">Cephes 2.2</A>
 * math Library (C). Most Cephes code (missing from the 2D Graph Package)
 * directly ported.
 *
 * @author wolfgang.hoschek@cern.ch
 * @version 0.9, 22-Jun-99
 */
object Gamma {

  /**
   * Returns the beta function of the arguments.
   *
   * <pre>
   *                   -     -
   *                  | (a) | (b)
   * beta( a, b )  =  -----------.
   *                     -
   *                    | (a+b)
   * </pre>
   */
  def beta(a: Double, b: Double): Double = {
    var y: Double = 0.0
    y = a + b
    y = gamma(y)
    if (y == 0.0) return 1.0
    if (a > b) {
      y = gamma(a) / y
      y *= gamma(b)
    } else {
      y = gamma(b) / y
      y *= gamma(a)
    }
    y
  }

  /**
   * Returns the Gamma function of the argument.
   */
  def gamma(x_p: Double): Double = {
    var x = x_p
    val P = Array(1.60119522476751861407E-4, 1.19135147006586384913E-3, 1.04213797561761569935E-2, 4.76367800457137231464E-2, 2.07448227648435975150E-1, 4.94214826801497100753E-1, 9.99999999999999996796E-1)
    val Q = Array(-2.31581873324120129819E-5, 5.39605580493303397842E-4, -4.45641913851797240494E-3, 1.18139785222060435552E-2, 3.58236398605498653373E-2, -2.34591795718243348568E-1, 7.14304917030273074085E-2, 1.00000000000000000320E0)
    var p: Double = 0.0
    var z: Double = 0.0
    var i: Int = 0
    var q = math.abs(x)
    if (q > 33.0) {
      if (x < 0.0) {
        p = math.floor(q)
        if (p == q) throw new ArithmeticException("gamma: overflow")
        i = p.toInt
        z = q - p
        if (z > 0.5) {
          p += 1.0
          z = q - p
        }
        z = q * math.sin(math.Pi * z)
        if (z == 0.0) throw new ArithmeticException("gamma: overflow")
        z = math.abs(z)
        z = math.Pi / (z * stirlingFormula(q))
        return -z
      } else {
        return stirlingFormula(x)
      }
    }
    z = 1.0
    while (x >= 3.0) {
      x -= 1.0
      z *= x
    }
    while (x < 0.0) {
      if (x == 0.0) {
        throw new ArithmeticException("gamma: singular")
      } else if (x > -1.E-9) {
        return z / ((1.0 + 0.5772156649015329 * x) * x)
      }
      z /= x
      x += 1.0
    }
    while (x < 2.0) {
      if (x == 0.0) {
        throw new ArithmeticException("gamma: singular")
      } else if (x < 1.e-9) {
        return z / ((1.0 + 0.5772156649015329 * x) * x)
      }
      z /= x
      x += 1.0
    }
    if ((x == 2.0) || (x == 3.0)) return z
    x -= 2.0
    p = Polynomial.polevl(x, P, 6)
    q = Polynomial.polevl(x, Q, 7)
    z * p / q
  }

  /**
   * Returns the Incomplete Beta Function evaluated from zero to <tt>xx</tt>;
   * formerly named <tt>ibeta</tt>.
   *
   * @param aa
   *            the alpha parameter of the beta distribution.
   * @param bb
   *            the beta parameter of the beta distribution.
   * @param xx
   *            the integration end point.
   */
  def incompleteBeta(aa: Double, bb: Double, xx: Double): Double = {
    var a: Double = 0.0
    var b: Double = 0.0
    var t: Double = 0.0
    var x: Double = 0.0
    var xc: Double = 0.0
    var w: Double = 0.0
    var y: Double = 0.0
    var flag: Boolean = false
    if (aa <= 0.0 || bb <= 0.0) throw new ArithmeticException("ibeta: Domain error!")
    if ((xx <= 0.0) || (xx >= 1.0)) {
      if (xx == 0.0) return 0.0
      if (xx == 1.0) return 1.0
      throw new ArithmeticException("ibeta: Domain error!")
    }
    flag = false
    if ((bb * xx) <= 1.0 && xx <= 0.95) {
      t = powerSeries(aa, bb, xx)
      return t
    }
    w = 1.0 - xx
    if (xx > (aa / (aa + bb))) {
      flag = true
      a = bb
      b = aa
      xc = xx
      x = w
    } else {
      a = aa
      b = bb
      xc = w
      x = xx
    }
    if (flag && (b * x) <= 1.0 && x <= 0.95) {
      t = powerSeries(a, b, x)
      t = if (t <= MACHEP) 1.0 - MACHEP else 1.0 - t
      return t
    }
    y = x * (a + b - 2.0) - (a - 1.0)
    w = if (y < 0.0) incompleteBetaFraction1(a, b, x) else incompleteBetaFraction2(a, b, x) / xc
    y = a * math.log(x)
    t = b * math.log(xc)
    if ((a + b) < MAXGAM && math.abs(y) < MAXLOG && math.abs(t) < MAXLOG) {
      t = math.pow(xc, b)
      t *= math.pow(x, a)
      t /= a
      t *= w
      t *= gamma(a + b) / (gamma(a) * gamma(b))
      if (flag) {
        t = if (t <= MACHEP) 1.0 - MACHEP else 1.0 - t
      }
      return t
    }
    y += t + logGamma(a + b) - logGamma(a) - logGamma(b)
    y += math.log(w / a)
    t = if (y < MINLOG) 0.0 else math.exp(y)
    if (flag) {
      t = if (t <= MACHEP) 1.0 - MACHEP else 1.0 - t
    }
    t
  }

  /**
   * Continued fraction expansion #1 for incomplete beta integral; formerly
   * named <tt>incbcf</tt>.
   */
  def incompleteBetaFraction1(a: Double, b: Double, x: Double): Double = {
    var xk: Double = 0.0
    var pk: Double = 0.0
    var pkm1: Double = 0.0
    var pkm2: Double = 0.0
    var qk: Double = 0.0
    var qkm1: Double = 0.0
    var qkm2: Double = 0.0
    var k1: Double = 0.0
    var k2: Double = 0.0
    var k3: Double = 0.0
    var k4: Double = 0.0
    var k5: Double = 0.0
    var k6: Double = 0.0
    var k7: Double = 0.0
    var k8: Double = 0.0
    var r: Double = 0.0
    var t: Double = 0.0
    var ans: Double = 0.0
    var thresh: Double = 0.0
    var n: Int = 0
    k1 = a
    k2 = a + b
    k3 = a
    k4 = a + 1.0
    k5 = 1.0
    k6 = b - 1.0
    k7 = k4
    k8 = a + 2.0
    pkm2 = 0.0
    qkm2 = 1.0
    pkm1 = 1.0
    qkm1 = 1.0
    ans = 1.0
    r = 1.0
    n = 0
    thresh = 3.0 * MACHEP
    do {
      xk = -(x * k1 * k2) / (k3 * k4)
      pk = pkm1 + pkm2 * xk
      qk = qkm1 + qkm2 * xk
      pkm2 = pkm1
      pkm1 = pk
      qkm2 = qkm1
      qkm1 = qk
      xk = (x * k5 * k6) / (k7 * k8)
      pk = pkm1 + pkm2 * xk
      qk = qkm1 + qkm2 * xk
      pkm2 = pkm1
      pkm1 = pk
      qkm2 = qkm1
      qkm1 = qk
      if (qk != 0) r = pk / qk
      if (r != 0) {
        t = math.abs((ans - r) / r)
        ans = r
      } else t = 1.0
      if (t < thresh) return ans
      k1 += 1.0
      k2 += 1.0
      k3 += 2.0
      k4 += 2.0
      k5 += 1.0
      k6 -= 1.0
      k7 += 2.0
      k8 += 2.0
      if ((math.abs(qk) + math.abs(pk)) > big) {
        pkm2 *= biginv
        pkm1 *= biginv
        qkm2 *= biginv
        qkm1 *= biginv
      }
      if ((math.abs(qk) < biginv) || (math.abs(pk) < biginv)) {
        pkm2 *= big
        pkm1 *= big
        qkm2 *= big
        qkm1 *= big
      }
    } while (n < 300)
    ans
  }

  /**
   * Continued fraction expansion #2 for incomplete beta integral; formerly
   * named <tt>incbd</tt>.
   */
  def incompleteBetaFraction2(a: Double, b: Double, x: Double): Double = {
    var xk: Double = 0.0
    var pk: Double = 0.0
    var pkm1: Double = 0.0
    var pkm2: Double = 0.0
    var qk: Double = 0.0
    var qkm1: Double = 0.0
    var qkm2: Double = 0.0
    var k1: Double = 0.0
    var k2: Double = 0.0
    var k3: Double = 0.0
    var k4: Double = 0.0
    var k5: Double = 0.0
    var k6: Double = 0.0
    var k7: Double = 0.0
    var k8: Double = 0.0
    var r: Double = 0.0
    var t: Double = 0.0
    var ans: Double = 0.0
    var z: Double = 0.0
    var thresh: Double = 0.0
    var n: Int = 0
    k1 = a
    k2 = b - 1.0
    k3 = a
    k4 = a + 1.0
    k5 = 1.0
    k6 = a + b
    k7 = a + 1.0

    k8 = a + 2.0
    pkm2 = 0.0
    qkm2 = 1.0
    pkm1 = 1.0
    qkm1 = 1.0
    z = x / (1.0 - x)
    ans = 1.0
    r = 1.0
    n = 0
    thresh = 3.0 * MACHEP
    do {
      xk = -(z * k1 * k2) / (k3 * k4)
      pk = pkm1 + pkm2 * xk
      qk = qkm1 + qkm2 * xk
      pkm2 = pkm1
      pkm1 = pk
      qkm2 = qkm1
      qkm1 = qk
      xk = (z * k5 * k6) / (k7 * k8)
      pk = pkm1 + pkm2 * xk
      qk = qkm1 + qkm2 * xk
      pkm2 = pkm1
      pkm1 = pk
      qkm2 = qkm1
      qkm1 = qk
      if (qk != 0) r = pk / qk
      if (r != 0) {
        t = math.abs((ans - r) / r)
        ans = r
      } else t = 1.0
      if (t < thresh) return ans
      k1 += 1.0
      k2 -= 1.0
      k3 += 2.0
      k4 += 2.0
      k5 += 1.0
      k6 += 1.0
      k7 += 2.0
      k8 += 2.0
      if ((math.abs(qk) + math.abs(pk)) > big) {
        pkm2 *= biginv
        pkm1 *= biginv
        qkm2 *= biginv
        qkm1 *= biginv
      }
      if ((math.abs(qk) < biginv) || (math.abs(pk) < biginv)) {
        pkm2 *= big
        pkm1 *= big
        qkm2 *= big
        qkm1 *= big
      }
    } while (n < 300)
    ans
  }

  /**
   * Returns the Incomplete Gamma function; formerly named <tt>igamma</tt>.
   *
   * @param a
   *            the parameter of the gamma distribution.
   * @param x
   *            the integration end point.
   */
  def incompleteGamma(a: Double, x: Double): Double = {
    var ans: Double = 0.0
    var ax: Double = 0.0
    var c: Double = 0.0
    var r: Double = 0.0
    if (x <= 0 || a <= 0) return 0.0
    if (x > 1.0 && x > a) return 1.0 - incompleteGammaComplement(a, x)
    ax = a * math.log(x) - x - logGamma(a)
    if (ax < -MAXLOG) return 0.0
    ax = math.exp(ax)
    r = a
    c = 1.0
    ans = 1.0
    do {
      r += 1.0
      c *= x / r
      ans += c
    } while (c / ans > MACHEP)
    ans * ax / a
  }

  /**
   * Returns the Complemented Incomplete Gamma function; formerly named
   * <tt>igamc</tt>.
   *
   * @param a
   *            the parameter of the gamma distribution.
   * @param x
   *            the integration start point.
   */
  def incompleteGammaComplement(a: Double, x: Double): Double = {
    var ans: Double = 0.0
    var ax: Double = 0.0
    var c: Double = 0.0
    var yc: Double = 0.0
    var r: Double = 0.0
    var t: Double = 0.0
    var y: Double = 0.0
    var z: Double = 0.0
    var pk: Double = 0.0
    var pkm1: Double = 0.0
    var pkm2: Double = 0.0
    var qk: Double = 0.0
    var qkm1: Double = 0.0
    var qkm2: Double = 0.0
    if (x <= 0 || a <= 0) return 1.0
    if (x < 1.0 || x < a) return 1.0 - incompleteGamma(a, x)
    ax = a * math.log(x) - x - logGamma(a)
    if (ax < -MAXLOG) return 0.0
    ax = math.exp(ax)
    y = 1.0 - a
    z = x + y + 1.0
    c = 0.0
    pkm2 = 1.0
    qkm2 = x
    pkm1 = x + 1.0
    qkm1 = z * x
    ans = pkm1 / qkm1
    do {
      c += 1.0
      y += 1.0
      z += 2.0
      yc = y * c
      pk = pkm1 * z - pkm2 * yc
      qk = qkm1 * z - qkm2 * yc
      if (qk != 0) {
        r = pk / qk
        t = math.abs((ans - r) / r)
        ans = r
      } else t = 1.0
      pkm2 = pkm1
      pkm1 = pk
      qkm2 = qkm1
      qkm1 = qk
      if (math.abs(pk) > big) {
        pkm2 *= biginv
        pkm1 *= biginv
        qkm2 *= biginv
        qkm1 *= biginv
      }
    } while (t > MACHEP)
    ans * ax
  }

  /**
   * Returns the natural logarithm of the gamma function; formerly named
   * <tt>lgamma</tt>.
   */
  def logGamma(x_p: Double): Double = {
    var x = x_p
    var p: Double = 0.0
    var q: Double = 0.0
    var w: Double = 0.0
    var z: Double = 0.0
    val A = Array(8.11614167470508450300E-4, -5.95061904284301438324E-4, 7.93650340457716943945E-4, -2.77777777730099687205E-3, 8.33333333333331927722E-2)
    val B = Array(-1.37825152569120859100E3, -3.88016315134637840924E4, -3.31612992738871184744E5, -1.16237097492762307383E6, -1.72173700820839662146E6, -8.53555664245765465627E5)
    val C = Array(-3.51815701436523470549E2, -1.70642106651881159223E4, -2.20528590553854454839E5, -1.13933444367982507207E6, -2.53252307177582951285E6, -2.01889141433532773231E6)
    if (x < -34.0) {
      q = -x
      w = logGamma(q)
      p = math.floor(q)
      if (p == q) throw new ArithmeticException("lgam: Overflow")
      z = q - p
      if (z > 0.5) {
        p += 1.0
        z = p - q
      }
      z = q * math.sin(math.Pi * z)
      if (z == 0.0) throw new ArithmeticException("lgamma: Overflow")
      z = LOGPI - math.log(z) - w
      return z
    }
    if (x < 13.0) {
      z = 1.0
      while (x >= 3.0) {
        x -= 1.0
        z *= x
      }
      while (x < 2.0) {
        if (x == 0.0) throw new ArithmeticException("lgamma: Overflow")
        z /= x
        x += 1.0
      }
      if (z < 0.0) z = -z
      if (x == 2.0) return math.log(z)
      x -= 2.0
      p = x * Polynomial.polevl(x, B, 5) / Polynomial.p1evl(x, C, 6)
      return math.log(z) + p
    }
    if (x > 2.556348e305) throw new ArithmeticException("lgamma: Overflow")
    q = (x - 0.5) * math.log(x) - x + 0.91893853320467274178
    if (x > 1.0e8) return q
    p = 1.0 / (x * x)
    if (x >= 1000.0) q += ((7.9365079365079365079365e-4 * p - 2.7777777777777777777778e-3) *
      p +
      0.0833333333333333333333) /
      x else q += Polynomial.polevl(p, A, 4) / x
    q
  }

  /**
   * Power series for incomplete beta integral; formerly named
   * <tt>pseries</tt>. Use when b*x is small and x not too close to 1.
   */
  def powerSeries(a: Double, b: Double, x: Double): Double = {
    var s: Double = 0.0
    var t: Double = 0.0
    var u: Double = 0.0
    var v: Double = 0.0
    var n: Double = 0.0
    var t1: Double = 0.0
    var z: Double = 0.0
    var ai: Double = 0.0
    ai = 1.0 / a
    u = (1.0 - b) * x
    v = u / (a + 1.0)
    t1 = v
    t = u
    n = 2.0
    s = 0.0
    z = MACHEP * ai
    while (math.abs(v) > z) {
      u = (n - b) * x / n
      t *= u
      v = t / (a + n)
      s += v
      n += 1.0
    }
    s += t1
    s += ai
    u = a * math.log(x)
    if ((a + b) < MAXGAM && math.abs(u) < MAXLOG) {
      t = Gamma.gamma(a + b) / (Gamma.gamma(a) * Gamma.gamma(b))
      s = s * t * math.pow(x, a)
    } else {
      t = Gamma.logGamma(a + b) - Gamma.logGamma(a) - Gamma.logGamma(b) +
        u +
        math.log(s)
      s = if (t < MINLOG) 0.0 else math.exp(t)
    }
    s
  }

  /**
   * Returns the Gamma function computed by Stirling's formula; formerly named
   * <tt>stirf</tt>. The polynomial STIR is valid for 33 <= x <= 172.
   */
  def stirlingFormula(x: Double): Double = {
    val STIR = Array(7.87311395793093628397E-4, -2.29549961613378126380E-4, -2.68132617805781232825E-3, 3.47222221605458667310E-3, 8.33333333333482257126E-2)
    val MAXSTIR = 143.01608
    var w = 1.0 / x
    var y = math.exp(x)
    w = 1.0 + w * Polynomial.polevl(w, STIR, 4)
    if (x > MAXSTIR) {
      val v = math.pow(x, 0.5 * x - 0.25)
      y = v * (v / y)
    } else {
      y = math.pow(x, x - 0.5) / y
    }
    y = SQTPI * y * w
    y
  }
}
