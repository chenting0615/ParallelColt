package cern.jet.random.tdouble

import cern.jet.random.tdouble.engine.DoubleRandomEngine
import ExponentialPower._
//remove if not needed
import scala.collection.JavaConversions._

object ExponentialPower {

  protected var shared: ExponentialPower = new ExponentialPower(1.0, makeDefaultGenerator())

  /**
   * Returns a random number from the distribution.
   *
   * @throws IllegalArgumentException
   *             if <tt>tau &lt; 1.0</tt>.
   */
  def staticNextDouble(tau: Double): Double = {
    synchronized (shared) {
      shared.nextDouble(tau)
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
 * Exponential Power distribution.
 * <p>
 * Valid parameter ranges: <tt>tau &gt;= 1</tt>.
 * <p>
 * Instance methods operate on a user supplied uniform random number generator;
 * they are unsynchronized.
 * <dt>Static methods operate on a default uniform random number generator; they
 * are synchronized.
 * <p>
 * <b>Implementation:</b>
 * <dt>Method: Non-universal rejection method for logconcave densities.
 * <dt>This is a port of <tt>epd.c</tt> from the <A
 * HREF="http://www.cis.tu-graz.ac.at/stat/stadl/random.html">C-RAND /
 * WIN-RAND</A> library. C-RAND's implementation, in turn, is based upon
 * <p>
 * L. Devroye (1986): Non-Uniform Random Variate Generation , Springer Verlag,
 * New York.
 * <p>
 *
 * @author wolfgang.hoschek@cern.ch
 * @version 1.0, 09/24/99
 */
@SerialVersionUID(1L)
class ExponentialPower(tau: Double, randomGenerator: DoubleRandomEngine) extends AbstractContinousDoubleDistribution {

  protected var tau: Double = _

  private var s: Double = _

  private var sm1: Double = _

  private var tau_set: Double = -1.0

  setRandomGenerator(randomGenerator)

  setState(tau)

  /**
   * Returns a random number from the distribution.
   */
  def nextDouble(): Double = nextDouble(this.tau)

  /**
   * Returns a random number from the distribution; bypasses the internal
   * state.
   *
   * @throws IllegalArgumentException
   *             if <tt>tau &lt; 1.0</tt>.
   */
  def nextDouble(tau: Double): Double = {
    var u: Double = 0.0
    var u1: Double = 0.0
    var v: Double = 0.0
    var x: Double = 0.0
    var y: Double = 0.0
    if (tau != tau_set) {
      s = 1.0 / tau
      sm1 = 1.0 - s
      tau_set = tau
    }
    do {
      u = randomGenerator.raw()
      u = (2.0 * u) - 1.0
      u1 = Math.abs(u)
      v = randomGenerator.raw()
      if (u1 <= sm1) {
        x = u1
      } else {
        y = tau * (1.0 - u1)
        x = sm1 - s * Math.log(y)
        v = v * y
      }
    } while (Math.log(v) > -Math.exp(Math.log(x) * tau));
    if (u < 0.0) x else -x
  }

  /**
   * Sets the distribution parameter.
   *
   * @throws IllegalArgumentException
   *             if <tt>tau &lt; 1.0</tt>.
   */
  def setState(tau: Double) {
    if (tau < 1.0) throw new IllegalArgumentException()
    this.tau = tau
  }

  /**
   * Returns a String representation of the receiver.
   */
  override def toString(): String = this.getClass.getName + "(" + tau + ")"
}
