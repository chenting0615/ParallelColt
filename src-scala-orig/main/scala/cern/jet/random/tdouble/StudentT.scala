package cern.jet.random.tdouble

import cern.jet.random.tdouble.engine.DoubleRandomEngine
import cern.jet.stat.tdouble.Probability
import StudentT._
//remove if not needed
import scala.collection.JavaConversions._

object StudentT {

  protected var shared: StudentT = new StudentT(1.0, makeDefaultGenerator())

  /**
   * Returns a random number from the distribution.
   *
   * @param freedom
   *            degrees of freedom.
   * @throws IllegalArgumentException
   *             if <tt>freedom &lt;= 0.0</tt>.
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
 * StudentT distribution (aka T-distribution); See the <A HREF=
 * "http://www.cern.ch/RD11/rkb/AN16pp/node279.html#SECTION0002790000000000000000"
 * > math definition</A> and <A
 * HREF="http://www.statsoft.com/textbook/gloss.html#Student's t Distribution">
 * animated definition</A>.
 * <p>
 * <tt>p(x) = k  *  (1+x^2/f) ^ -(f+1)/2</tt> where
 * <tt>k = g((f+1)/2) / (sqrt(pi*f) * g(f/2))</tt> and <tt>g(a)</tt> being the
 * gamma function and <tt>f</tt> being the degrees of freedom.
 * <p>
 * Valid parameter ranges: <tt>freedom &gt; 0</tt>.
 * <p>
 * Instance methods operate on a user supplied uniform random number generator;
 * they are unsynchronized.
 * <dt>Static methods operate on a default uniform random number generator; they
 * are synchronized.
 * <p>
 * <b>Implementation:</b>
 * <dt>Method: Adapted Polar Box-Muller transformation.
 * <dt>This is a port of <A HREF="http://wwwinfo.cern.ch/asd/lhc++/clhep/manual/RefGuide/Random/RandStudentT.html"
 * >RandStudentT</A> used in <A
 * HREF="http://wwwinfo.cern.ch/asd/lhc++/clhep">CLHEP 1.4.0</A> (C++). CLHEP's
 * implementation, in turn, is based on <tt>tpol.c</tt> from the <A
 * HREF="http://www.cis.tu-graz.ac.at/stat/stadl/random.html">C-RAND /
 * WIN-RAND</A> library. C-RAND's implementation, in turn, is based upon
 * <p>
 * R.W. Bailey (1994): Polar generation of random variates with the
 * t-distribution, Mathematics of Computation 62, 779-781.
 *
 * @author wolfgang.hoschek@cern.ch
 * @version 1.0, 09/24/99
 */
@SerialVersionUID(1L)
class StudentT(freedom: Double, randomGenerator: DoubleRandomEngine) extends AbstractContinousDoubleDistribution {

  protected var freedom: Double = _

  protected var TERM: Double = _

  setRandomGenerator(randomGenerator)

  setState(freedom)

  /**
   * Returns the cumulative distribution function.
   */
  def cdf(x: Double): Double = Probability.studentT(freedom, x)

  /**
   * Returns a random number from the distribution.
   */
  def nextDouble(): Double = nextDouble(this.freedom)

  /**
   * Returns a random number from the distribution; bypasses the internal
   * state.
   *
   * @param degreesOfFreedom
   *            degrees of freedom.
   * @throws IllegalArgumentException
   *             if <tt>a &lt;= 0.0</tt>.
   */
  def nextDouble(degreesOfFreedom: Double): Double = {
    if (degreesOfFreedom <= 0.0) throw new IllegalArgumentException()
    var u: Double = 0.0
    var v: Double = 0.0
    var w: Double = 0.0
    do {
      u = 2.0 * randomGenerator.raw() - 1.0
      v = 2.0 * randomGenerator.raw() - 1.0
    } while ((w = u * u + v * v) > 1.0);
    (u * 
      Math.sqrt(degreesOfFreedom * 
      (Math.exp(-2.0 / degreesOfFreedom * Math.log(w)) - 1.0) / 
      w))
  }

  /**
   * Returns the probability distribution function.
   */
  def pdf(x: Double): Double = {
    this.TERM * 
      Math.pow((1 + x * x / freedom), -(freedom + 1) * 0.5)
  }

  /**
   * Sets the distribution parameter.
   *
   * @param freedom
   *            degrees of freedom.
   * @throws IllegalArgumentException
   *             if <tt>freedom &lt;= 0.0</tt>.
   */
  def setState(freedom: Double) {
    if (freedom <= 0.0) throw new IllegalArgumentException()
    this.freedom = freedom
    val `val` = Fun.logGamma((freedom + 1) / 2) - Fun.logGamma(freedom / 2)
    this.TERM = Math.exp(`val`) / Math.sqrt(Math.PI * freedom)
  }

  /**
   * Returns a String representation of the receiver.
   */
  override def toString(): String = {
    this.getClass.getName + "(" + freedom + ")"
  }
}
