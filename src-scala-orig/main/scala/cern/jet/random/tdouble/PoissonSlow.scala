package cern.jet.random.tdouble

import cern.jet.random.tdouble.engine.DoubleRandomEngine
import PoissonSlow._
//remove if not needed
import scala.collection.JavaConversions._

object PoissonSlow {

  protected val MEAN_MAX = Integer.MAX_VALUE

  protected val SWITCH_MEAN = 12.0

  protected val cof = Array(76.18009172947146, -86.50532032941677, 24.01409824083091, -1.231739572450155, 0.1208650973866179e-2, -0.5395239384953e-5)

  protected var shared: PoissonSlow = new PoissonSlow(0.0, makeDefaultGenerator())

  /**
   * Returns the value ln(Gamma(xx) for xx > 0. Full accuracy is obtained for
   * xx > 1. For 0 < xx < 1. the reflection formula (6.1.4) can be used first.
   * (Adapted from Numerical Recipes in C)
   */
  def logGamma(xx: Double): Double = {
    var x = xx - 1.0
    var tmp = x + 5.5
    tmp -= (x + 0.5) * Math.log(tmp)
    var ser = 1.000000000190015
    val coeff = cof
    var j = 0
    while (j <= 5) {
      x += 1
      ser += coeff(j) / x
      j += 1
    }
    -tmp + Math.log(2.5066282746310005 * ser)
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
 * Poisson distribution; See the <A HREF=
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
 * <b>Implementation:</b> This is a port of <A HREF=
 * "http://wwwinfo.cern.ch/asd/lhc++/clhep/manual/RefGuide/Random/RandPoisson.html"
 * >RandPoisson</A> used in <A
 * HREF="http://wwwinfo.cern.ch/asd/lhc++/clhep">CLHEP 1.4.0</A> (C++). CLHEP's
 * implementation, in turn, is based upon "W.H.Press et al., Numerical Recipes
 * in C, Second Edition".
 *
 * @author wolfgang.hoschek@cern.ch
 * @version 1.0, 09/24/99
 */
@SerialVersionUID(1L)
class PoissonSlow(mean: Double, randomGenerator: DoubleRandomEngine) extends AbstractDiscreteDistribution {

  protected var mean: Double = _

  protected var cached_sq: Double = _

  protected var cached_alxm: Double = _

  protected var cached_g: Double = _

  setRandomGenerator(randomGenerator)

  setMean(mean)

  /**
   * Returns a random number from the distribution.
   */
  def nextInt(): Int = nextInt(this.mean)

  /**
   * Returns a random number from the distribution; bypasses the internal
   * state.
   */
  private def nextInt(theMean: Double): Int = {
    val xm = theMean
    val g = this.cached_g
    if (xm == -1.0) return 0
    if (xm < SWITCH_MEAN) {
      var poisson = -1
      var product = 1
      do {
        poisson += 1
        product *= randomGenerator.raw()
      } while (product >= g);
      poisson
    } else if (xm < MEAN_MAX) {
      var t: Double = 0.0
      var em: Double = 0.0
      val sq = this.cached_sq
      val alxm = this.cached_alxm
      val rand = this.randomGenerator
      do {
        var y: Double = 0.0
        do {
          y = Math.tan(Math.PI * rand.raw())
          em = sq * y + xm
        } while (em < 0.0);
        em = (em).toInt
        t = 0.9 * (1.0 + y * y) * Math.exp(em * alxm - logGamma(em + 1.0) - g)
      } while (rand.raw() > t);
      em.toInt
    } else {
      xm.toInt
    }
  }

  /**
   * Returns a random number from the distribution.
   */
  protected def nextIntSlow(): Int = {
    val bound = Math.exp(-mean)
    var count = 0
    var product: Double = 0.0
    product = 1.0
    while (product >= bound && product > 0.0) {
      product *= randomGenerator.raw()
      count += 1
    }
    if (product <= 0.0 && bound > 0.0) return Math.round(mean).toInt
    count - 1
  }

  /**
   * Sets the mean.
   */
  def setMean(mean: Double) {
    if (mean != this.mean) {
      this.mean = mean
      if (mean == -1.0) return
      if (mean < SWITCH_MEAN) {
        this.cached_g = Math.exp(-mean)
      } else {
        this.cached_sq = Math.sqrt(2.0 * mean)
        this.cached_alxm = Math.log(mean)
        this.cached_g = mean * cached_alxm - logGamma(mean + 1.0)
      }
    }
  }

  /**
   * Returns a String representation of the receiver.
   */
  override def toString(): String = {
    this.getClass.getName + "(" + mean + ")"
  }
}
