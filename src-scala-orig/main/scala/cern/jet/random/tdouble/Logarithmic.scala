package cern.jet.random.tdouble

import cern.jet.random.tdouble.engine.DoubleRandomEngine
import Logarithmic._
//remove if not needed
import scala.collection.JavaConversions._

object Logarithmic {

  protected var shared: Logarithmic = new Logarithmic(0.5, makeDefaultGenerator())

  /**
   * Returns a random number from the distribution.
   */
  def staticNextDouble(p: Double): Double = {
    synchronized (shared) {
      shared.nextDouble(p)
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
 * Logarithmic distribution.
 * <p>
 * Valid parameter ranges: <tt>0 &lt; p &lt; 1</tt>.
 * <p>
 * Instance methods operate on a user supplied uniform random number generator;
 * they are unsynchronized.
 * <dt>Static methods operate on a default uniform random number generator; they
 * are synchronized.
 * <p>
 * <b>Implementation:</b>
 * <dt>Method: Inversion/Transformation.
 * <dt>This is a port of <tt>lsk.c</tt> from the <A
 * HREF="http://www.cis.tu-graz.ac.at/stat/stadl/random.html">C-RAND /
 * WIN-RAND</A> library. C-RAND's implementation, in turn, is based upon
 * <p>
 * A.W. Kemp (1981): Efficient generation of logarithmically distributed
 * pseudo-random variables, Appl. Statist. 30, 249-253.
 *
 * @author wolfgang.hoschek@cern.ch
 * @version 1.0, 09/24/99
 */
@SerialVersionUID(1L)
class Logarithmic(p: Double, randomGenerator: DoubleRandomEngine) extends AbstractContinousDoubleDistribution {

  protected var my_p: Double = _

  private var t: Double = _

  private var h: Double = _

  private var a_prev: Double = -1.0

  setRandomGenerator(randomGenerator)

  setState(p)

  /**
   * Returns a random number from the distribution.
   */
  def nextDouble(): Double = nextDouble(this.my_p)

  /**
   * Returns a random number from the distribution; bypasses the internal
   * state.
   */
  def nextDouble(a: Double): Double = {
    var u: Double = 0.0
    var v: Double = 0.0
    var p: Double = 0.0
    var q: Double = 0.0
    var k: Int = 0
    if (a != a_prev) {
      a_prev = a
      if (a < 0.97) t = -a / Math.log(1.0 - a) else h = Math.log(1.0 - a)
    }
    u = randomGenerator.raw()
    if (a < 0.97) {
      k = 1
      p = t
      while (u > p) {
        u -= p
        k += 1
        p *= a * (k - 1.0) / k
      }
      return k
    }
    if (u > a) return 1
    u = randomGenerator.raw()
    v = u
    q = 1.0 - Math.exp(v * h)
    if (u <= q * q) {
      k = (1 + Math.log(u) / Math.log(q)).toInt
      return k
    }
    if (u > q) return 1
    2
  }

  /**
   * Sets the distribution parameter.
   */
  def setState(p: Double) {
    this.my_p = p
  }

  /**
   * Returns a String representation of the receiver.
   */
  override def toString(): String = {
    this.getClass.getName + "(" + my_p + ")"
  }
}
