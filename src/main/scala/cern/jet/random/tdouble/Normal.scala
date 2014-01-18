package cern.jet.random.tdouble

import cern.jet.random.tdouble.engine.DoubleRandomEngine
import cern.jet.stat.tdouble.Probability

object Normal {

  def makeDefaultGenerator() = AbstractDoubleDistribution.makeDefaultGenerator()

  protected var shared: Normal = new Normal(0.0, 1.0, makeDefaultGenerator())

  /**
   * Returns a random number from the distribution with the given mean and
   * standard deviation.
   */
  def staticNextDouble(mean: Double, standardDeviation: Double): Double = {
    shared.synchronized {
      shared.nextDouble(mean, standardDeviation)
    }
  }
}

/**
 * Normal (aka Gaussian) distribution; See the <A HREF=
 * "http://www.cern.ch/RD11/rkb/AN16pp/node188.html#SECTION0001880000000000000000"
 * > math definition</A> and <A
 * HREF="http://www.statsoft.com/textbook/glosn.html#Normal Distribution">
 * animated definition</A>.
 *
 * <pre>
 *
 *  1                       2
 *  pdf(x) = ---------    exp( - (x-mean) / 2v )
 *  sqrt(2pi*v)
 *
 *  x
 *  -
 *  1        | |                 2
 *  cdf(x) = ---------    |    exp( - (t-mean) / 2v ) dt
 *  sqrt(2pi*v)| |
 *  -
 *  -inf.
 * </pre>
 *
 * where <tt>v = variance = standardDeviation**2</tt>.
 * <p>
 * Instance methods operate on a user supplied uniform random number generator;
 * they are unsynchronized.
 * <dt>Static methods operate on a default uniform random number generator; they
 * are synchronized.
 * <p>
 * <b>Implementation:</b> Polar Box-Muller transformation. See G.E.P. Box, M.E.
 * Muller (1958): A note on the generation of random normal deviates, Annals
 * Math. Statist. 29, 610-611.
 * <p>
 *
 * @author wolfgang.hoschek@cern.ch
 * @version 1.0, 09/24/99
 */
@SerialVersionUID(1L)
class Normal(mean: Double, standardDeviation: Double, randomGenerator: DoubleRandomEngine) extends AbstractContinousDoubleDistribution {

  protected var meanVar: Double = _

  protected var variance: Double = _

  protected var standardDeviationVar: Double = _

  protected var cache: Double = _

  protected var cacheFilled: Boolean = _

  protected var SQRT_INV: Double = _

  setRandomGenerator(randomGenerator)

  setState(mean, standardDeviation)

  /**
   * Returns the cumulative distribution function.
   */
  def cdf(x: Double): Double = Probability.normal(mean, variance, x)

  /**
   * Returns a random number from the distribution.
   */
  def nextDouble(): Double = {
    nextDouble(this.mean, this.standardDeviation)
  }

  /**
   * Returns a random number from the distribution; bypasses the internal
   * state.
   */
  def nextDouble(mean: Double, standardDeviation: Double): Double = {
    if (cacheFilled && this.mean == mean && this.standardDeviation == standardDeviation) {
      cacheFilled = false
      return cache
    }

    var x: Double = 0.0
    var y: Double = 0.0
    var r: Double = 0.0
    var z: Double = 0.0
    do {
      x = 2.0 * randomGenerator.raw() - 1.0
      y = 2.0 * randomGenerator.raw() - 1.0
      r = x * x + y * y
    } while (r >= 1.0)
    z = math.sqrt(-2.0 * math.log(r) / r)
    if (this.mean == mean && this.standardDeviation == standardDeviation) {
      cache = mean + standardDeviation * x * z
      cacheFilled = true
    }
    mean + standardDeviation * y * z
  }

  /**
   * Returns the probability distribution function.
   */
  def pdf(x: Double): Double = {
    val diff = x - mean
    SQRT_INV * math.exp(-(diff * diff) / (2.0 * variance))
  }

  /**
   * Sets the uniform random generator internally used.
   */
  override protected def setRandomGenerator(randomGenerator: DoubleRandomEngine) {
    super.setRandomGenerator(randomGenerator)
    this.cacheFilled = false
  }

  /**
   * Sets the mean and variance.
   */
  def setState(mean: Double, standardDeviation: Double) {
    if (mean != this.mean || standardDeviation != this.standardDeviation) {
      this.meanVar = mean
      this.standardDeviationVar = standardDeviation
      this.variance = standardDeviation * standardDeviation
      this.cacheFilled = false
      this.SQRT_INV = 1.0 / math.sqrt(2.0 * math.Pi * variance)
    }
  }

  /**
   * Returns a String representation of the receiver.
   */
  override def toString: String = {
    this.getClass.getName + "(" + mean + "," + standardDeviation + ")"
  }
}
