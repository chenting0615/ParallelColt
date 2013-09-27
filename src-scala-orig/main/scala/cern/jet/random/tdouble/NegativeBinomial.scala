package cern.jet.random.tdouble

import cern.jet.random.tdouble.engine.DoubleRandomEngine
import cern.jet.stat.tdouble.Probability
import NegativeBinomial._
//remove if not needed
import scala.collection.JavaConversions._

object NegativeBinomial {

  protected var shared: NegativeBinomial = new NegativeBinomial(1, 0.5, makeDefaultGenerator())

  /**
   * Returns a random number from the distribution with the given parameters n
   * and p.
   *
   * @param n
   *            the number of trials
   * @param p
   *            the probability of success.
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
 * Negative Binomial distribution; See the <A
 * HREF="http://www.statlets.com/usermanual/glossary2.htm"> math definition</A>.
 * <p>
 * Instance methods operate on a user supplied uniform random number generator;
 * they are unsynchronized.
 * <dt>Static methods operate on a default uniform random number generator; they
 * are synchronized.
 * <p>
 * <b>Implementation:</b> High performance implementation. Compound method.
 * <dt>This is a port of <tt>nbp.c</tt> from the <A
 * HREF="http://www.cis.tu-graz.ac.at/stat/stadl/random.html">C-RAND /
 * WIN-RAND</A> library. C-RAND's implementation, in turn, is based upon
 * <p>
 * J.H. Ahrens, U. Dieter (1974): Computer methods for sampling from gamma,
 * beta, Poisson and binomial distributions, Computing 12, 223--246.
 *
 * @author wolfgang.hoschek@cern.ch
 * @version 1.0, 09/24/99
 */
@SerialVersionUID(1L)
class NegativeBinomial(n: Int, p: Double, randomGenerator: DoubleRandomEngine)
    extends AbstractDiscreteDistribution {

  protected var n: Int = _

  protected var p: Double = _

  protected var gamma: Gamma = new Gamma(n, 1.0, randomGenerator)

  protected var poisson: Poisson = new Poisson(0.0, randomGenerator)

  setRandomGenerator(randomGenerator)

  setNandP(n, p)

  /**
   * Returns the cumulative distribution function.
   */
  def cdf(k: Int): Double = Probability.negativeBinomial(k, n, p)

  /**
   * Returns a deep copy of the receiver; the copy will produce identical
   * sequences. After this call has returned, the copy and the receiver have
   * equal but separate state.
   *
   * @return a copy of the receiver.
   */
  def clone(): AnyRef = {
    val copy = super.clone().asInstanceOf[NegativeBinomial]
    if (this.poisson != null) copy.poisson = this.poisson.clone().asInstanceOf[Poisson]
    copy.poisson.setRandomGenerator(copy.getRandomGenerator)
    if (this.gamma != null) copy.gamma = this.gamma.clone().asInstanceOf[Gamma]
    copy.gamma.setRandomGenerator(copy.getRandomGenerator)
    copy
  }

  /**
   * Returns a random number from the distribution.
   */
  def nextInt(): Int = nextInt(n, p)

  /**
   * Returns a random number from the distribution; bypasses the internal
   * state.
   */
  def nextInt(n: Int, p: Double): Int = {
    val x = p / (1.0 - p)
    val p1 = p
    val y = x * this.gamma.nextDouble(n, 1.0)
    this.poisson.nextInt(y)
  }

  /**
   * Returns the probability distribution function.
   */
  def pdf(k: Int): Double = {
    if (k > n) throw new IllegalArgumentException()
    cern.jet.math.tdouble.DoubleArithmetic.binomial(n, k) * 
      Math.pow(p, k) * 
      Math.pow(1.0 - p, n - k)
  }

  /**
   * Sets the parameters number of trials and the probability of success.
   *
   * @param n
   *            the number of trials
   * @param p
   *            the probability of success.
   */
  def setNandP(n: Int, p: Double) {
    this.n = n
    this.p = p
  }

  /**
   * Returns a String representation of the receiver.
   */
  override def toString(): String = {
    this.getClass.getName + "(" + n + "," + p + ")"
  }
}
