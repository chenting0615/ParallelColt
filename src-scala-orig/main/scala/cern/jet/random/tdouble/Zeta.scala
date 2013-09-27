package cern.jet.random.tdouble

import cern.jet.random.tdouble.engine.DoubleRandomEngine
import Zeta._
//remove if not needed
import scala.collection.JavaConversions._

object Zeta {

  protected var shared: Zeta = new Zeta(1.0, 1.0, makeDefaultGenerator())

  /**
   * Returns a random number from the distribution.
   */
  def staticNextInt(ro: Double, pk: Double): Int = {
    synchronized (shared) {
      shared.setState(ro, pk)
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
 * Zeta distribution.
 * <p>
 * Valid parameter ranges: <tt>ro &gt; 0</tt> and <tt>pk &gt;= 0</tt>.
 * <dt>If either <tt>ro &gt; 100</tt> or <tt>k &gt; 10000</tt> numerical
 * problems in computing the theoretical moments arise, therefore
 * <tt>ro &lt;= 100</tt> and <tt>k &lt;= 10000</tt> are recommended.
 * <p>
 * Instance methods operate on a user supplied uniform random number generator;
 * they are unsynchronized.
 * <dt>Static methods operate on a default uniform random number generator; they
 * are synchronized.
 * <p>
 * <b>Implementation:</b>
 * <dt>Method: Acceptance/Rejection. High performance implementation.
 * <dt>This is a port and adaption of <tt>Zeta.c</tt> from the <A
 * HREF="http://www.cis.tu-graz.ac.at/stat/stadl/random.html">C-RAND /
 * WIN-RAND</A> library. C-RAND's implementation, in turn, is based upon
 * <p>
 * J. Dagpunar (1988): Principles of Random Variate Generation, Clarendon Press,
 * Oxford.
 *
 * @author wolfgang.hoschek@cern.ch
 * @version 1.0, 09/24/99
 */
@SerialVersionUID(1L)
class Zeta(ro: Double, pk: Double, randomGenerator: DoubleRandomEngine) extends AbstractDiscreteDistribution {

  protected var ro: Double = _

  protected var pk: Double = _

  protected var c: Double = _

  protected var d: Double = _

  protected var ro_prev: Double = -1.0

  protected var pk_prev: Double = -1.0

  protected var maxlongint: Double = Long.MAX_VALUE - 1.5

  setRandomGenerator(randomGenerator)

  setState(ro, pk)

  /**
   * Returns a zeta distributed random number.
   */
  protected def generateZeta(ro: Double, pk: Double, randomGenerator: DoubleRandomEngine): Long = {
    var u: Double = 0.0
    var v: Double = 0.0
    var e: Double = 0.0
    var x: Double = 0.0
    var k: Long = 0l
    if (ro != ro_prev || pk != pk_prev) {
      ro_prev = ro
      pk_prev = pk
      if (ro < pk) {
        c = pk - 0.5
        d = 0
      } else {
        c = ro - 0.5
        d = (1.0 + ro) * Math.log((1.0 + pk) / (1.0 + ro))
      }
    }
    do {
      do {
        u = randomGenerator.raw()
        v = randomGenerator.raw()
        x = (c + 0.5) * Math.exp(-Math.log(u) / ro) - c
      } while (x <= 0.5 || x >= maxlongint);
      k = (x + 0.5).toInt
      e = -Math.log(v)
    } while (e < (1.0 + ro) * Math.log((k + pk) / (x + c)) - d);
    k
  }

  /**
   * Returns a random number from the distribution.
   */
  def nextInt(): Int = {
    generateZeta(ro, pk, randomGenerator).toInt
  }

  /**
   * Sets the parameters.
   */
  def setState(ro: Double, pk: Double) {
    this.ro = ro
    this.pk = pk
  }

  /**
   * Returns a String representation of the receiver.
   */
  override def toString(): String = {
    this.getClass.getName + "(" + ro + "," + pk + ")"
  }
}
