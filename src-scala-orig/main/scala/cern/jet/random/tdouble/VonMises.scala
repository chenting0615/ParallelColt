package cern.jet.random.tdouble

import cern.jet.random.tdouble.engine.DoubleRandomEngine
import VonMises._
//remove if not needed
import scala.collection.JavaConversions._

object VonMises {

  protected var shared: VonMises = new VonMises(1.0, makeDefaultGenerator())

  /**
   * Returns a random number from the distribution.
   *
   * @throws IllegalArgumentException
   *             if <tt>k &lt;= 0.0</tt>.
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
 * Von Mises distribution.
 * <p>
 * Valid parameter ranges: <tt>k &gt; 0</tt>.
 * <p>
 * Instance methods operate on a user supplied uniform random number generator;
 * they are unsynchronized.
 * <dt>Static methods operate on a default uniform random number generator; they
 * are synchronized.
 * <p>
 * <b>Implementation:</b>
 * <dt>Method: Acceptance Rejection.
 * <dt>This is a port of <tt>mwc.c</tt> from the <A
 * HREF="http://www.cis.tu-graz.ac.at/stat/stadl/random.html">C-RAND /
 * WIN-RAND</A> library. C-RAND's implementation, in turn, is based upon
 * <p>
 * D.J. Best, N.I. Fisher (1979): Efficient simulation of the von Mises
 * distribution, Appl. Statist. 28, 152-157.
 *
 * @author wolfgang.hoschek@cern.ch
 * @version 1.0, 09/24/99
 */
@SerialVersionUID(1L)
class VonMises(freedom: Double, randomGenerator: DoubleRandomEngine) extends AbstractContinousDoubleDistribution {

  protected var my_k: Double = _

  private var k_set: Double = -1.0

  private var tau: Double = _

  private var rho: Double = _

  private var r: Double = _

  setRandomGenerator(randomGenerator)

  setState(freedom)

  /**
   * Returns a random number from the distribution.
   */
  def nextDouble(): Double = nextDouble(this.my_k)

  /**
   * Returns a random number from the distribution; bypasses the internal
   * state.
   *
   * @throws IllegalArgumentException
   *             if <tt>k &lt;= 0.0</tt>.
   */
  def nextDouble(k: Double): Double = {
    var u: Double = 0.0
    var v: Double = 0.0
    var w: Double = 0.0
    var c: Double = 0.0
    var z: Double = 0.0
    if (k <= 0.0) throw new IllegalArgumentException()
    if (k_set != k) {
      tau = 1.0 + Math.sqrt(1.0 + 4.0 * k * k)
      rho = (tau - Math.sqrt(2.0 * tau)) / (2.0 * k)
      r = (1.0 + rho * rho) / (2.0 * rho)
      k_set = k
    }
    do {
      u = randomGenerator.raw()
      v = randomGenerator.raw()
      z = Math.cos(Math.PI * u)
      w = (1.0 + r * z) / (r + z)
      c = k * (r - w)
    } while ((c * (2.0 - c) < v) && (Math.log(c / v) + 1.0 < c));
    if ((randomGenerator.raw() > 0.5)) Math.acos(w) else -Math.acos(w)
  }

  /**
   * Sets the distribution parameter.
   *
   * @throws IllegalArgumentException
   *             if <tt>k &lt;= 0.0</tt>.
   */
  def setState(k: Double) {
    if (k <= 0.0) throw new IllegalArgumentException()
    this.my_k = k
  }

  /**
   * Returns a String representation of the receiver.
   */
  override def toString(): String = {
    this.getClass.getName + "(" + my_k + ")"
  }
}
