package cern.jet.random.tdouble

import cern.jet.random.tdouble.engine.DoubleRandomEngine
import Hyperbolic._
//remove if not needed
import scala.collection.JavaConversions._

object Hyperbolic {

  protected var shared: Hyperbolic = new Hyperbolic(10.0, 10.0, makeDefaultGenerator())

  /**
   * Returns a random number from the distribution.
   */
  def staticNextDouble(alpha: Double, beta: Double): Double = {
    synchronized (shared) {
      shared.nextDouble(alpha, beta)
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
 * Hyperbolic distribution.
 * <p>
 * Valid parameter ranges: <tt>alpha &gt; 0</tt> and <tt>beta &gt; 0</tt>.
 * <p>
 * Instance methods operate on a user supplied uniform random number generator;
 * they are unsynchronized.
 * <dt>Static methods operate on a default uniform random number generator; they
 * are synchronized.
 * <p>
 * <b>Implementation:</b>
 * <dt>Method: Non-Universal Rejection. High performance implementation.
 * <dt>This is a port of <tt>hyplc.c</tt> from the <A
 * HREF="http://www.cis.tu-graz.ac.at/stat/stadl/random.html">C-RAND /
 * WIN-RAND</A> library. C-RAND's implementation, in turn, is based upon
 * <p>
 * L. Devroye (1986): Non-Uniform Random Variate Generation, Springer Verlag,
 * New York.
 *
 * @author wolfgang.hoschek@cern.ch
 * @version 1.0, 09/24/99
 */
@SerialVersionUID(1L)
class Hyperbolic(alpha: Double, beta: Double, randomGenerator: DoubleRandomEngine)
    extends AbstractContinousDoubleDistribution {

  protected var alpha: Double = _

  protected var beta: Double = _

  protected var a_setup: Double = 0.0

  protected var b_setup: Double = -1.0

  protected var x: Double = _

  protected var u: Double = _

  protected var v: Double = _

  protected var e: Double = _

  protected var hr: Double = _

  protected var hl: Double = _

  protected var s: Double = _

  protected var pm: Double = _

  protected var pr: Double = _

  protected var samb: Double = _

  protected var pmr: Double = _

  protected var mpa_1: Double = _

  protected var mmb_1: Double = _

  setRandomGenerator(randomGenerator)

  setState(alpha, beta)

  /**
   * Returns a random number from the distribution.
   */
  def nextDouble(): Double = nextDouble(alpha, beta)

  /**
   * Returns a hyperbolic distributed random number; bypasses the internal
   * state.
   */
  def nextDouble(alpha: Double, beta: Double): Double = {
    val a = alpha
    val b = beta
    if ((a_setup != a) || (b_setup != b)) {
      var mpa: Double = 0.0
      var mmb: Double = 0.0
      var mode: Double = 0.0
      var amb: Double = 0.0
      var a_: Double = 0.0
      var b_: Double = 0.0
      var a_1: Double = 0.0
      var b_1: Double = 0.0
      var pl: Double = 0.0
      var help_1: Double = 0.0
      var help_2: Double = 0.0
      amb = a * a - b * b
      samb = Math.sqrt(amb)
      mode = b / samb
      help_1 = a * Math.sqrt(2.0 * samb + 1.0)
      help_2 = b * (samb + 1.0)
      mpa = (help_2 + help_1) / amb
      mmb = (help_2 - help_1) / amb
      a_ = mpa - mode
      b_ = -mmb + mode
      hr = -1.0 / (-a * mpa / Math.sqrt(1.0 + mpa * mpa) + b)
      hl = 1.0 / (-a * mmb / Math.sqrt(1.0 + mmb * mmb) + b)
      a_1 = a_ - hr
      b_1 = b_ - hl
      mmb_1 = mode - b_1
      mpa_1 = mode + a_1
      s = (a_ + b_)
      pm = (a_1 + b_1) / s
      pr = hr / s
      pmr = pm + pr
      a_setup = a
      b_setup = b
    }
    while (true) {
      u = randomGenerator.raw()
      v = randomGenerator.raw()
      if (u <= pm) {
        x = mmb_1 + u * s
        if (Math.log(v) <= (-a * Math.sqrt(1.0 + x * x) + b * x + samb)) //break
      } else {
        if (u <= pmr) {
          e = -Math.log((u - pm) / pr)
          x = mpa_1 + hr * e
          if ((Math.log(v) - e) <= (-a * Math.sqrt(1.0 + x * x) + b * x + samb)) //break
        } else {
          e = Math.log((u - pmr) / (1.0 - pmr))
          x = mmb_1 + hl * e
          if ((Math.log(v) + e) <= (-a * Math.sqrt(1.0 + x * x) + b * x + samb)) //break
        }
      }
    }
    (x)
  }

  /**
   * Sets the parameters.
   */
  def setState(alpha: Double, beta: Double) {
    this.alpha = alpha
    this.beta = beta
  }

  /**
   * Returns a String representation of the receiver.
   */
  override def toString(): String = {
    this.getClass.getName + "(" + alpha + "," + beta + ")"
  }
}
