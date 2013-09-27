package cern.jet.random.tdouble

import cern.jet.random.tdouble.engine.DoubleRandomEngine
import BreitWigner._
//remove if not needed
import scala.collection.JavaConversions._

object BreitWigner {

  protected var shared: BreitWigner = new BreitWigner(1.0, 0.2, 1.0, makeDefaultGenerator())

  /**
   * Returns a random number from the distribution.
   *
   * @param cut
   *            </tt>cut==Double.NEGATIVE_INFINITY</tt> indicates "don't cut".
   */
  def staticNextDouble(mean: Double, gamma: Double, cut: Double): Double = {
    synchronized (shared) {
      shared.nextDouble(mean, gamma, cut)
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
 * BreitWigner (aka Lorentz) distribution; See the <A HREF=
 * "http://www.cern.ch/RD11/rkb/AN16pp/node23.html#SECTION000230000000000000000"
 * > math definition</A>. A more general form of the Cauchy distribution.
 * <p>
 * Instance methods operate on a user supplied uniform random number generator;
 * they are unsynchronized.
 * <dt>Static methods operate on a default uniform random number generator; they
 * are synchronized.
 * <p>
 * <b>Implementation:</b> This is a port of <A HREF="http://wwwinfo.cern.ch/asd/lhc++/clhep/manual/RefGuide/Random/RandBreitWigner.html"
 * >RandBreitWigner</A> used in <A
 * HREF="http://wwwinfo.cern.ch/asd/lhc++/clhep">CLHEP 1.4.0</A> (C++).
 *
 * @author wolfgang.hoschek@cern.ch
 * @version 1.0, 09/24/99
 */
@SerialVersionUID(1L)
class BreitWigner(mean: Double, 
    gamma: Double, 
    cut: Double, 
    randomGenerator: DoubleRandomEngine) extends AbstractContinousDoubleDistribution {

  protected var mean: Double = _

  protected var gamma: Double = _

  protected var cut: Double = _

  setRandomGenerator(randomGenerator)

  setState(mean, gamma, cut)

  /**
   * Returns a random number from the distribution.
   */
  def nextDouble(): Double = nextDouble(mean, gamma, cut)

  /**
   * Returns a random number from the distribution; bypasses the internal
   * state.
   *
   * @param cut
   *            </tt>cut==Double.NEGATIVE_INFINITY</tt> indicates "don't cut".
   */
  def nextDouble(mean: Double, gamma: Double, cut: Double): Double = {
    var `val`: Double = 0.0
    var rval: Double = 0.0
    var displ: Double = 0.0
    if (gamma == 0.0) return mean
    if (cut == Double.NEGATIVE_INFINITY) {
      rval = 2.0 * randomGenerator.raw() - 1.0
      displ = 0.5 * gamma * Math.tan(rval * (Math.PI / 2.0))
      mean + displ
    } else {
      `val` = Math.atan(2.0 * cut / gamma)
      rval = 2.0 * randomGenerator.raw() - 1.0
      displ = 0.5 * gamma * Math.tan(rval * `val`)
      mean + displ
    }
  }

  /**
   * Sets the mean, gamma and cut parameters.
   *
   * @param cut
   *            </tt>cut==Double.NEGATIVE_INFINITY</tt> indicates "don't cut".
   */
  def setState(mean: Double, gamma: Double, cut: Double) {
    this.mean = mean
    this.gamma = gamma
    this.cut = cut
  }

  /**
   * Returns a String representation of the receiver.
   */
  override def toString(): String = {
    this.getClass.getName + "(" + mean + "," + gamma + "," + 
      cut + 
      ")"
  }
}
