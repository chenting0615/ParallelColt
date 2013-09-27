package cern.jet.random.tdouble

import cern.jet.random.tdouble.engine.DoubleRandomEngine
import cern.jet.stat.tdouble.Probability
import ChiSquare._
//remove if not needed
import scala.collection.JavaConversions._

object ChiSquare {

  protected var shared: ChiSquare = new ChiSquare(1.0, makeDefaultGenerator())

  /**
   * Returns a random number from the distribution.
   *
   * @param freedom
   *            degrees of freedom.
   * @throws IllegalArgumentException
   *             if <tt>freedom &lt; 1.0</tt>.
   */
  def staticNextDouble(freedom: Double): Double = {
    synchronized (shared) {
      shared.nextDouble(freedom)
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
 * ChiSquare distribution; See the <A HREF=
 * "http://www.cern.ch/RD11/rkb/AN16pp/node31.html#SECTION000310000000000000000"
 * > math definition</A> and <A
 * HREF="http://www.statsoft.com/textbook/glosc.html#Chi-square Distribution">
 * animated definition</A>. <dt>A special case of the Gamma distribution.
 * <p>
 * <tt>p(x) = (1/g(f/2)) * (x/2)^(f/2-1) * exp(-x/2)</tt> with <tt>g(a)</tt>
 * being the gamma function and <tt>f</tt> being the degrees of freedom.
 * <p>
 * Valid parameter ranges: <tt>freedom &gt; 0</tt>.
 * <p>
 * Instance methods operate on a user supplied uniform random number generator;
 * they are unsynchronized.
 * <dt>Static methods operate on a default uniform random number generator; they
 * are synchronized.
 * <p>
 * <b>Implementation:</b>
 * <dt>Method: Ratio of Uniforms with shift.
 * <dt>High performance implementation. This is a port of <A HREF="http://wwwinfo.cern.ch/asd/lhc++/clhep/manual/RefGuide/Random/RandChiSquare.html"
 * >RandChiSquare</A> used in <A
 * HREF="http://wwwinfo.cern.ch/asd/lhc++/clhep">CLHEP 1.4.0</A> (C++). CLHEP's
 * implementation, in turn, is based on <tt>chru.c</tt> from the <A
 * HREF="http://www.cis.tu-graz.ac.at/stat/stadl/random.html">C-RAND /
 * WIN-RAND</A> library. C-RAND's implementation, in turn, is based upon
 * <p>
 * J.F. Monahan (1987): An algorithm for generating chi random variables, ACM
 * Trans. Math. Software 13, 168-172.
 *
 * @author wolfgang.hoschek@cern.ch
 * @version 1.0, 09/24/99
 */
@SerialVersionUID(1L)
class ChiSquare(freedom: Double, randomGenerator: DoubleRandomEngine) extends AbstractContinousDoubleDistribution {

  protected var freedom: Double = _

  private var freedom_in: Double = -1.0

  private var b: Double = _

  private var vm: Double = _

  private var vp: Double = _

  private var vd: Double = _

  setRandomGenerator(randomGenerator)

  setState(freedom)

  /**
   * Returns the cumulative distribution function.
   */
  def cdf(x: Double): Double = Probability.chiSquare(freedom, x)

  /**
   * Returns a random number from the distribution.
   */
  def nextDouble(): Double = nextDouble(this.freedom)

  /**
   * Returns a random number from the distribution; bypasses the internal
   * state.
   *
   * @param freedom
   *            degrees of freedom. It should hold <tt>freedom &lt; 1.0</tt>.
   */
  def nextDouble(freedom: Double): Double = {
    var u: Double = 0.0
    var v: Double = 0.0
    var z: Double = 0.0
    var zz: Double = 0.0
    var r: Double = 0.0
    if (freedom == 1.0) {
      while (true) {
        u = randomGenerator.raw()
        v = randomGenerator.raw() * 0.857763884960707
        z = v / u
        if (z < 0) //continue
        zz = z * z
        r = 2.5 - zz
        if (z < 0.0) r = r + zz * z / (3.0 * z)
        if (u < r * 0.3894003915) return (z * z)
        if (zz > (1.036961043 / u + 1.4)) //continue
        if (2.0 * Math.log(u) < (-zz * 0.5)) (z * z)
      }
    } else {
      if (freedom != freedom_in) {
        b = Math.sqrt(freedom - 1.0)
        vm = -0.6065306597 * (1.0 - 0.25 / (b * b + 1.0))
        vm = if ((-b > vm)) -b else vm
        vp = 0.6065306597 * (0.7071067812 + b) / (0.5 + b)
        vd = vp - vm
        freedom_in = freedom
      }
      while (true) {
        u = randomGenerator.raw()
        v = randomGenerator.raw() * vd + vm
        z = v / u
        if (z < -b) //continue
        zz = z * z
        r = 2.5 - zz
        if (z < 0.0) r = r + zz * z / (3.0 * (z + b))
        if (u < r * 0.3894003915) return ((z + b) * (z + b))
        if (zz > (1.036961043 / u + 1.4)) //continue
        if (2.0 * Math.log(u) < (Math.log(1.0 + z / b) * b * b - zz * 0.5 - z * b)) ((z + b) * (z + b))
      }
    }
  }

  /**
   * Returns the probability distribution function.
   */
  def pdf(x: Double): Double = {
    if (x <= 0.0) throw new IllegalArgumentException()
    val logGamma = Fun.logGamma(freedom / 2.0)
    Math.exp((freedom / 2.0 - 1.0) * Math.log(x / 2.0) - x / 2.0 - 
      logGamma) / 
      2.0
  }

  /**
   * Sets the distribution parameter.
   *
   * @param freedom
   *            degrees of freedom.
   * @throws IllegalArgumentException
   *             if <tt>freedom &lt; 1.0</tt>.
   */
  def setState(freedom: Double) {
    if (freedom < 1.0) throw new IllegalArgumentException()
    this.freedom = freedom
  }

  /**
   * Returns a String representation of the receiver.
   */
  override def toString(): String = {
    this.getClass.getName + "(" + freedom + ")"
  }
}
