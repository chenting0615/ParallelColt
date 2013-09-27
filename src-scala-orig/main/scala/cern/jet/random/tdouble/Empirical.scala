package cern.jet.random.tdouble

import cern.jet.random.tdouble.engine.DoubleRandomEngine
import Empirical._
//remove if not needed
import scala.collection.JavaConversions._

object Empirical {

  val LINEAR_INTERPOLATION = 0

  val NO_INTERPOLATION = 1
}

/**
 * Empirical distribution.
 * <p>
 * The probability distribution function (pdf) must be provided by the user as
 * an array of positive real numbers. The pdf does not need to be provided in
 * the form of relative probabilities, absolute probabilities are also accepted.
 * <p>
 * If <tt>interpolationType == LINEAR_INTERPOLATION</tt> a linear interpolation
 * within the bin is computed, resulting in a constant density within each bin.
 * <dt>If <tt>interpolationType == NO_INTERPOLATION</tt> no interpolation is
 * performed and the result is a discrete distribution.
 * <p>
 * Instance methods operate on a user supplied uniform random number generator;
 * they are unsynchronized.
 * <dt>Static methods operate on a default uniform random number generator; they
 * are synchronized.
 * <p>
 * <b>Implementation:</b> A uniform random number is generated using a user
 * supplied generator. The uniform number is then transformed to the user's
 * distribution using the cumulative probability distribution constructed from
 * the pdf. The cumulative distribution is inverted using a binary search for
 * the nearest bin boundary.
 * <p>
 * This is a port of <A HREF=
 * "http://wwwinfo.cern.ch/asd/lhc++/clhep/manual/RefGuide/Random/RandGeneral.html"
 * >RandGeneral</A> used in <A
 * HREF="http://wwwinfo.cern.ch/asd/lhc++/clhep">CLHEP 1.4.0</A> (C++).
 *
 * @author wolfgang.hoschek@cern.ch
 * @version 1.0, 09/24/99
 */
@SerialVersionUID(1L)
class Empirical(pdf: Array[Double], interpolationType: Int, randomGenerator: DoubleRandomEngine)
    extends AbstractContinousDoubleDistribution {

  protected var cdf: Array[Double] = _

  protected var interpolationType: Int = _

  setRandomGenerator(randomGenerator)

  setState(pdf, interpolationType)

  /**
   * Returns the cumulative distribution function.
   */
  def cdf(k: Int): Double = {
    if (k < 0) return 0.0
    if (k >= cdf.length - 1) return 1.0
    cdf(k)
  }

  /**
   * Returns a deep copy of the receiver; the copy will produce identical
   * sequences. After this call has returned, the copy and the receiver have
   * equal but separate state.
   *
   * @return a copy of the receiver.
   */
  def clone(): AnyRef = {
    val copy = super.clone().asInstanceOf[Empirical]
    if (this.cdf != null) copy.cdf = this.cdf.clone()
    copy
  }

  /**
   * Returns a random number from the distribution.
   */
  def nextDouble(): Double = {
    val rand = randomGenerator.raw()
    if (this.cdf == null) return rand
    val nBins = cdf.length - 1
    var nbelow = 0
    var nabove = nBins
    while (nabove > nbelow + 1) {
      val middle = (nabove + nbelow + 1) >> 1
      if (rand >= cdf(middle)) nbelow = middle else nabove = middle
    }
    if (this.interpolationType == NO_INTERPOLATION) {
      nbelow.toDouble / nBins
    } else if (this.interpolationType == LINEAR_INTERPOLATION) {
      val binMeasure = cdf(nabove) - cdf(nbelow)
      if (binMeasure == 0.0) {
        return (nbelow + 0.5) / nBins
      }
      val binFraction = (rand - cdf(nbelow)) / binMeasure
      (nbelow + binFraction) / nBins
    } else throw new InternalError()
  }

  /**
   * Returns the probability distribution function.
   */
  def pdf(x: Double): Double = {
    throw new RuntimeException("not implemented")
  }

  /**
   * Returns the probability distribution function.
   */
  def pdf(k: Int): Double = {
    if (k < 0 || k >= cdf.length - 1) return 0.0
    cdf(k - 1) - cdf(k)
  }

  /**
   * Sets the distribution parameters. The <tt>pdf</tt> must satisfy both of
   * the following conditions
   * <ul>
   * <li><tt>0.0 &lt;= pdf[i] : 0 &lt; =i &lt;= pdf.length-1</tt>
   * <li><tt>0.0 &lt; Sum(pdf[i]) : 0 &lt;=i &lt;= pdf.length-1</tt>
   * </ul>
   *
   * @param pdf
   *            probability distribution function.
   * @param interpolationType
   *            can be either <tt>Empirical.NO_INTERPOLATION</tt> or
   *            <tt>Empirical.LINEAR_INTERPOLATION</tt>.
   * @throws IllegalArgumentException
   *             if at least one of the three conditions above is violated.
   */
  def setState(pdf: Array[Double], interpolationType: Int) {
    if (interpolationType != LINEAR_INTERPOLATION && interpolationType != NO_INTERPOLATION) {
      throw new IllegalArgumentException("Illegal Interpolation Type")
    }
    this.interpolationType = interpolationType
    if (pdf == null || pdf.length == 0) {
      this.cdf = null
      return
    }
    val nBins = pdf.length
    this.cdf = Array.ofDim[Double](nBins + 1)
    cdf(0) = 0
    for (ptn <- 0 until nBins) {
      val prob = pdf(ptn)
      if (prob < 0.0) throw new IllegalArgumentException("Negative probability")
      cdf(ptn + 1) = cdf(ptn) + prob
    }
    if (cdf(nBins) <= 0.0) throw new IllegalArgumentException("At leat one probability must be > 0.0")
    for (ptn <- 0 until nBins + 1) {
      cdf(ptn) /= cdf(nBins)
    }
  }

  /**
   * Returns a String representation of the receiver.
   */
  override def toString(): String = {
    var interpolation: String = null
    if (interpolationType == NO_INTERPOLATION) interpolation = "NO_INTERPOLATION"
    if (interpolationType == LINEAR_INTERPOLATION) interpolation = "LINEAR_INTERPOLATION"
    this.getClass.getName + "(" + (if ((cdf != null)) cdf.length else 0) + 
      "," + 
      interpolation + 
      ")"
  }

  /**
   * Not yet commented.
   *
   * @return int
   */
  private def xnBins(): Int = cdf.length - 1
}
