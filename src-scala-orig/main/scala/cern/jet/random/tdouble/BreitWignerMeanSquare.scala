package cern.jet.random.tdouble

import cern.jet.random.tdouble.engine.DoubleRandomEngine
import BreitWignerMeanSquare._
//remove if not needed
import scala.collection.JavaConversions._

object BreitWignerMeanSquare {

  protected var shared: BreitWigner = new BreitWignerMeanSquare(1.0, 0.2, 1.0, makeDefaultGenerator())

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
 * Mean-square BreitWigner distribution; See the <A HREF=
 * "http://www.cern.ch/RD11/rkb/AN16pp/node23.html#SECTION000230000000000000000"
 * > math definition</A>.
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
class BreitWignerMeanSquare(mean: Double, 
    gamma: Double, 
    cut: Double, 
    randomGenerator: DoubleRandomEngine) extends BreitWigner(mean, gamma, cut, randomGenerator) {

  protected var uniform: DoubleUniform = new DoubleUniform(randomGenerator)

  /**
   * Returns a deep copy of the receiver; the copy will produce identical
   * sequences. After this call has returned, the copy and the receiver have
   * equal but separate state.
   *
   * @return a copy of the receiver.
   */
  def clone(): AnyRef = {
    val copy = super.clone().asInstanceOf[BreitWignerMeanSquare]
    if (this.uniform != null) copy.uniform = new DoubleUniform(copy.randomGenerator)
    copy
  }

  /**
   * Returns a mean-squared random number from the distribution; bypasses the
   * internal state.
   *
   * @param cut
   *            </tt>cut==Double.NEGATIVE_INFINITY</tt> indicates "don't cut".
   */
  def nextDouble(mean: Double, gamma: Double, cut: Double): Double = {
    if (gamma == 0.0) return mean
    if (cut == Double.NEGATIVE_INFINITY) {
      val `val` = Math.atan(-mean / gamma)
      val rval = this.uniform.nextDoubleFromTo(`val`, Math.PI / 2.0)
      val displ = gamma * Math.tan(rval)
      Math.sqrt(mean * mean + mean * displ)
    } else {
      val tmp = Math.max(0.0, mean - cut)
      val lower = Math.atan((tmp * tmp - mean * mean) / (mean * gamma))
      val upper = Math.atan(((mean + cut) * (mean + cut) - mean * mean) / (mean * gamma))
      val rval = this.uniform.nextDoubleFromTo(lower, upper)
      val displ = gamma * Math.tan(rval)
      Math.sqrt(Math.max(0.0, mean * mean + mean * displ))
    }
  }
}
