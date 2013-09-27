package cern.jet.random.tdouble

import cern.jet.math.tdouble.DoubleArithmetic
import cern.jet.random.tdouble.engine.DoubleRandomEngine
import cern.jet.stat.tdouble.Probability
import Poisson._
//remove if not needed
import scala.collection.JavaConversions._

object Poisson {

  protected val MEAN_MAX = Integer.MAX_VALUE

  protected val SWITCH_MEAN = 10.0

  protected var shared: Poisson = new Poisson(0.0, makeDefaultGenerator())

  private def f(k: Int, l_nu: Double, c_pm: Double): Double = {
    Math.exp(k * l_nu - DoubleArithmetic.logFactorial(k) - c_pm)
  }

  /**
   * Returns a random number from the distribution with the given mean.
   */
  def staticNextInt(mean: Double): Int = {
    synchronized (shared) {
      shared.setMean(mean)
      shared.nextInt()
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
 * Poisson distribution (quick); See the <A HREF=
 * "http://www.cern.ch/RD11/rkb/AN16pp/node208.html#SECTION0002080000000000000000"
 * > math definition</A> and <A
 * HREF="http://www.statsoft.com/textbook/glosp.html#Poisson Distribution">
 * animated definition</A>.
 * <p>
 * <tt>p(k) = (mean^k / k!) * exp(-mean)</tt> for <tt>k &gt;= 0</tt>.
 * <p>
 * Valid parameter ranges: <tt>mean &gt; 0</tt>. Note: if
 * <tt>mean &lt;= 0.0</tt> then always returns zero.
 * <p>
 * Instance methods operate on a user supplied uniform random number generator;
 * they are unsynchronized.
 * <dt>Static methods operate on a default uniform random number generator; they
 * are synchronized.
 * <p>
 * <b>Implementation:</b> High performance implementation. Patchwork
 * Rejection/Inversion method.
 * <dt>This is a port of <tt>pprsc.c</tt> from the <A
 * HREF="http://www.cis.tu-graz.ac.at/stat/stadl/random.html">C-RAND /
 * WIN-RAND</A> library. C-RAND's implementation, in turn, is based upon
 * <p>
 * H. Zechner (1994): Efficient sampling from continuous and discrete unimodal
 * distributions, Doctoral Dissertation, 156 pp., Technical University Graz,
 * Austria.
 * <p>
 * Also see
 * <p>
 * Stadlober E., H. Zechner (1999), <A
 * HREF="http://www.cis.tu-graz.ac.at/stat/stadl/random.html">The patchwork
 * rejection method for sampling from unimodal distributions</A>, to appear in
 * ACM Transactions on Modelling and Simulation.
 *
 * @author wolfgang.hoschek@cern.ch
 * @version 1.0, 09/24/99
 */
@SerialVersionUID(1L)
class Poisson(mean: Double, randomGenerator: DoubleRandomEngine) extends AbstractDiscreteDistribution {

  protected var mean: Double = _

  protected var my_old: Double = -1.0

  protected var p: Double = _

  protected var q: Double = _

  protected var p0: Double = _

  protected var pp: Array[Double] = new Array[Double](36)

  protected var llll: Int = _

  protected var my_last: Double = -1.0

  protected var ll: Double = _

  protected var k2: Int = _

  protected var k4: Int = _

  protected var k1: Int = _

  protected var k5: Int = _

  protected var dl: Double = _

  protected var dr: Double = _

  protected var r1: Double = _

  protected var r2: Double = _

  protected var r4: Double = _

  protected var r5: Double = _

  protected var lr: Double = _

  protected var l_my: Double = _

  protected var c_pm: Double = _

  protected var f1: Double = _

  protected var f2: Double = _

  protected var f4: Double = _

  protected var f5: Double = _

  protected var p1: Double = _

  protected var p2: Double = _

  protected var p3: Double = _

  protected var p4: Double = _

  protected var p5: Double = _

  protected var p6: Double = _

  protected var m: Int = _

  setRandomGenerator(randomGenerator)

  setMean(mean)

  /**
   * Returns the cumulative distribution function.
   */
  def cdf(k: Int): Double = Probability.poisson(k, this.mean)

  /**
   * Returns a deep copy of the receiver; the copy will produce identical
   * sequences. After this call has returned, the copy and the receiver have
   * equal but separate state.
   *
   * @return a copy of the receiver.
   */
  def clone(): AnyRef = {
    val copy = super.clone().asInstanceOf[Poisson]
    if (this.pp != null) copy.pp = this.pp.clone()
    copy
  }

  /**
   * Returns a random number from the distribution.
   */
  def nextInt(): Int = nextInt(this.mean)

  /**
   * Returns a random number from the distribution; bypasses the internal
   * state.
   */
  def nextInt(theMean: Double): Int = {
    val gen = this.randomGenerator
    val my = theMean
    val t: Double = 0.0
    val g: Double = 0.0
    val my_k: Double = 0.0
    val gx: Double = 0.0
    val gy: Double = 0.0
    val px: Double = 0.0
    val py: Double = 0.0
    val e: Double = 0.0
    val x: Double = 0.0
    val xx: Double = 0.0
    val delta: Double = 0.0
    val v: Double = 0.0
    val sign: Int = 0
    var u: Double = 0.0
    var k: Int = 0
    var i: Int = 0
    if (my < SWITCH_MEAN) {
      if (my != my_old) {
        my_old = my
        llll = 0
        p = Math.exp(-my)
        q = p
        p0 = p
      }
      m = if ((my > 1.0)) my.toInt else 1
      while (true) {
        u = gen.raw()
        k = 0
        if (u <= p0) return (k)
        if (llll != 0) {
          i = if ((u > 0.458)) Math.min(llll, m) else 1
          k = i
          while (k <= llll) {if (u <= pp(k)) return (k)k += 1
          }
          if (llll == 35) //continue
        }
        k = llll + 1
        while (k <= 35) {
          p *= my / k
          q += p
          pp(k) = q
          if (u <= q) {
            llll = k
            return (k)
          }
          k += 1
        }
        llll = 35
      }
    } else if (my < MEAN_MAX) {
      var Dk: Int = 0
      var X: Int = 0
      var Y: Int = 0
      var Ds: Double = 0.0
      var U: Double = 0.0
      var V: Double = 0.0
      var W: Double = 0.0
      m = my.toInt
      if (my != my_last) {
        my_last = my
        Ds = Math.sqrt(my + 0.25)
        k2 = Math.ceil(my - 0.5 - Ds).toInt
        k4 = (my - 0.5 + Ds).toInt
        k1 = k2 + k2 - m + 1
        k5 = k4 + k4 - m
        dl = (k2 - k1)
        dr = (k5 - k4)
        r1 = my / k1
        r2 = my / k2
        r4 = my / (k4 + 1)
        r5 = my / (k5 + 1)
        ll = Math.log(r1)
        lr = -Math.log(r5)
        l_my = Math.log(my)
        c_pm = m * l_my - DoubleArithmetic.logFactorial(m)
        f2 = f(k2, l_my, c_pm)
        f4 = f(k4, l_my, c_pm)
        f1 = f(k1, l_my, c_pm)
        f5 = f(k5, l_my, c_pm)
        p1 = f2 * (dl + 1.0)
        p2 = f2 * dl + p1
        p3 = f4 * (dr + 1.0) + p2
        p4 = f4 * dr + p3
        p5 = f1 / ll + p4
        p6 = f5 / lr + p5
      }
      while (true) {
        if ((U = gen.raw() * p6) < p2) {
          if ((V = U - p1) < 0.0) return (k2 + (U / f2).toInt)
          if ((W = V / dl) < f1) return (k1 + (V / f1).toInt)
          Dk = (dl * gen.raw()).toInt + 1
          if (W <= f2 - Dk * (f2 - f2 / r2)) {
            return (k2 - Dk)
          }
          if ((V = f2 + f2 - W) < 1.0) {
            Y = k2 + Dk
            if (V <= f2 + Dk * (1.0 - f2) / (dl + 1.0)) {
              return (Y)
            }
            if (V <= f(Y, l_my, c_pm)) return (Y)
          }
          X = k2 - Dk
        } else if (U < p4) {
          if ((V = U - p3) < 0.0) return (k4 - ((U - p2) / f4).toInt)
          if ((W = V / dr) < f5) return (k5 - (V / f5).toInt)
          Dk = (dr * gen.raw()).toInt + 1
          if (W <= f4 - Dk * (f4 - f4 * r4)) {
            return (k4 + Dk)
          }
          if ((V = f4 + f4 - W) < 1.0) {
            Y = k4 - Dk
            if (V <= f4 + Dk * (1.0 - f4) / dr) {
              return (Y)
            }
            if (V <= f(Y, l_my, c_pm)) return (Y)
          }
          X = k4 + Dk
        } else {
          W = gen.raw()
          if (U < p5) {
            Dk = (1.0 - Math.log(W) / ll).toInt
            if ((X = k1 - Dk) < 0) //continue
            W *= (U - p4) * ll
            if (W <= f1 - Dk * (f1 - f1 / r1)) return (X)
          } else {
            Dk = (1.0 - Math.log(W) / lr).toInt
            X = k5 + Dk
            W *= (U - p5) * lr
            if (W <= f5 - Dk * (f5 - f5 * r5)) return (X)
          }
        }
        if (Math.log(W) <= X * l_my - DoubleArithmetic.logFactorial(X) - c_pm) (X)
      }
    } else {
      my.toInt
    }
  }

  /**
   * Returns the probability distribution function.
   */
  def pdf(k: Int): Double = {
    Math.exp(k * Math.log(this.mean) - DoubleArithmetic.logFactorial(k) - 
      this.mean)
  }

  /**
   * Sets the mean.
   */
  def setMean(mean: Double) {
    this.mean = mean
  }

  /**
   * Returns a String representation of the receiver.
   */
  override def toString(): String = {
    this.getClass.getName + "(" + mean + ")"
  }
}
