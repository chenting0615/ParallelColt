package cern.jet.random.tdouble

import cern.jet.random.tdouble.engine.DoubleRandomEngine
import Exponential._
//remove if not needed
import scala.collection.JavaConversions._

object Exponential {

  protected var shared: Exponential = new Exponential(1.0, makeDefaultGenerator())

  /**
   * Returns a random number from the distribution with the given lambda.
   */
  def staticNextDouble(lambda: Double): Double = {
    synchronized (shared) {
      shared.nextDouble(lambda)
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
 * Exponential Distribution (aka Negative Exponential Distribution); See the <A
 * HREF=
 * "http://www.cern.ch/RD11/rkb/AN16pp/node78.html#SECTION000780000000000000000"
 * > math definition</A> <A
 * HREF="http://www.statsoft.com/textbook/glose.html#Exponential Distribution">
 * animated definition</A>.
 * <p>
 * <tt>p(x) = lambda*exp(-x*lambda)</tt> for <tt>x &gt;= 0</tt>,
 * <tt>lambda &gt; 0</tt>.
 * <p>
 * Instance methods operate on a user supplied uniform random number generator;
 * they are unsynchronized.
 * <dt>Static methods operate on a default uniform random number generator; they
 * are synchronized.
 * <p>
 *
 * @author wolfgang.hoschek@cern.ch
 * @version 1.0, 09/24/99
 */
@SerialVersionUID(1L)
class Exponential(lambda: Double, randomGenerator: DoubleRandomEngine) extends AbstractContinousDoubleDistribution {

  protected var lambda: Double = _

  setRandomGenerator(randomGenerator)

  setState(lambda)

  /**
   * Returns the cumulative distribution function.
   */
  def cdf(x: Double): Double = {
    if (x <= 0.0) return 0.0
    1.0 - Math.exp(-x * lambda)
  }

  /**
   * Returns a random number from the distribution.
   */
  def nextDouble(): Double = nextDouble(lambda)

  /**
   * Returns a random number from the distribution; bypasses the internal
   * state.
   */
  def nextDouble(lambda: Double): Double = {
    -Math.log(randomGenerator.raw()) / lambda
  }

  /**
   * Returns the probability distribution function.
   */
  def pdf(x: Double): Double = {
    if (x < 0.0) return 0.0
    lambda * Math.exp(-x * lambda)
  }

  /**
   * Sets the mean.
   */
  def setState(lambda: Double) {
    this.lambda = lambda
  }

  /**
   * Returns a String representation of the receiver.
   */
  override def toString(): String = {
    this.getClass.getName + "(" + lambda + ")"
  }
}
