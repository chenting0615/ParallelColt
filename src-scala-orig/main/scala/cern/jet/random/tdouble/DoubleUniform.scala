package cern.jet.random.tdouble

import cern.jet.random.tdouble.engine.DoubleRandomEngine
import DoubleUniform._
//remove if not needed
import scala.collection.JavaConversions._

object DoubleUniform {

  protected var shared: DoubleUniform = new DoubleUniform(makeDefaultGenerator())

  /**
   * Returns a uniformly distributed random <tt>boolean</tt>.
   */
  def staticNextBoolean(): Boolean = {
    synchronized (shared) {
      shared.nextBoolean()
    }
  }

  /**
   * Returns a uniformly distributed random number in the open interval
   * <tt>(0,1)</tt> (excluding <tt>0</tt> and <tt>1</tt>).
   */
  def staticNextDouble(): Double = {
    synchronized (shared) {
      shared.nextDouble()
    }
  }

  /**
   * Returns a uniformly distributed random number in the open interval
   * <tt>(from,to)</tt> (excluding <tt>from</tt> and <tt>to</tt>). Pre
   * conditions: <tt>from &lt;= to</tt>.
   */
  def staticNextDoubleFromTo(from: Double, to: Double): Double = {
    synchronized (shared) {
      shared.nextDoubleFromTo(from, to)
    }
  }

  /**
   * Returns a uniformly distributed random number in the open interval
   * <tt>(from,to)</tt> (excluding <tt>from</tt> and <tt>to</tt>). Pre
   * conditions: <tt>from &lt;= to</tt>.
   */
  def staticNextFloatFromTo(from: Float, to: Float): Float = {
    synchronized (shared) {
      shared.nextFloatFromTo(from, to)
    }
  }

  /**
   * Returns a uniformly distributed random number in the closed interval
   * <tt>[from,to]</tt> (including <tt>from</tt> and <tt>to</tt>). Pre
   * conditions: <tt>from &lt;= to</tt>.
   */
  def staticNextIntFromTo(from: Int, to: Int): Int = {
    synchronized (shared) {
      shared.nextIntFromTo(from, to)
    }
  }

  /**
   * Returns a uniformly distributed random number in the closed interval
   * <tt>[from,to]</tt> (including <tt>from</tt> and <tt>to</tt>). Pre
   * conditions: <tt>from &lt;= to</tt>.
   */
  def staticNextLongFromTo(from: Long, to: Long): Long = {
    synchronized (shared) {
      shared.nextLongFromTo(from, to)
    }
  }

  /**
   * Sets the uniform random number generation engine shared by all
   * <b>static</b> methods.
   *
   * @param randomGenerator
   *            the new uniform random number generation engine to be shared.
   */
  def staticSetRandomEngine(randomGenerator: DoubleRandomEngine) {
    synchronized (shared) {
      shared.setRandomGenerator(randomGenerator)
    }
  }
}

/**
 * Uniform distribution; <A HREF=
 * "http://www.cern.ch/RD11/rkb/AN16pp/node292.html#SECTION0002920000000000000000"
 * > Math definition</A> and <A
 * HREF="http://www.statsoft.com/textbook/glosu.html#Uniform Distribution">
 * animated definition</A>.
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
class DoubleUniform(min: Double, max: Double, randomGenerator: DoubleRandomEngine)
    extends AbstractContinousDoubleDistribution {

  protected var min: Double = _

  protected var max: Double = _

  setRandomGenerator(randomGenerator)

  setState(min, max)

  /**
   * Constructs a uniform distribution with the given minimum and maximum,
   * using a {@link cern.jet.random.tdouble.engine.DoubleMersenneTwister}
   * seeded with the given seed.
   */
  def this(min: Double, max: Double, seed: Int) {
    this(min, max, new cern.jet.random.tdouble.engine.DoubleMersenneTwister(seed))
  }

  /**
   * Constructs a uniform distribution with <tt>min=0.0</tt> and
   * <tt>max=1.0</tt>.
   */
  def this(randomGenerator: DoubleRandomEngine) {
    this(0, 1, randomGenerator)
  }

  /**
   * Returns the cumulative distribution function (assuming a continous
   * uniform distribution).
   */
  def cdf(x: Double): Double = {
    if (x <= min) return 0.0
    if (x >= max) return 1.0
    (x - min) / (max - min)
  }

  /**
   * Returns a uniformly distributed random <tt>boolean</tt>.
   */
  def nextBoolean(): Boolean = randomGenerator.raw() > 0.5

  /**
   * Returns a uniformly distributed random number in the open interval
   * <tt>(min,max)</tt> (excluding <tt>min</tt> and <tt>max</tt>).
   */
  def nextDouble(): Double = {
    min + (max - min) * randomGenerator.raw()
  }

  /**
   * Returns a uniformly distributed random number in the open interval
   * <tt>(from,to)</tt> (excluding <tt>from</tt> and <tt>to</tt>). Pre
   * conditions: <tt>from &lt;= to</tt>.
   */
  def nextDoubleFromTo(from: Double, to: Double): Double = {
    from + (to - from) * randomGenerator.raw()
  }

  /**
   * Returns a uniformly distributed random number in the open interval
   * <tt>(from,to)</tt> (excluding <tt>from</tt> and <tt>to</tt>). Pre
   * conditions: <tt>from &lt;= to</tt>.
   */
  def nextFloatFromTo(from: Float, to: Float): Float = nextDoubleFromTo(from, to).toFloat

  /**
   * Returns a uniformly distributed random number in the closed interval
   * <tt>[min,max]</tt> (including <tt>min</tt> and <tt>max</tt>).
   */
  def nextInt(): Int = {
    nextIntFromTo(Math.round(min).toInt, Math.round(max).toInt)
  }

  /**
   * Returns a uniformly distributed random number in the closed interval
   * <tt>[from,to]</tt> (including <tt>from</tt> and <tt>to</tt>). Pre
   * conditions: <tt>from &lt;= to</tt>.
   */
  def nextIntFromTo(from: Int, to: Int): Int = {
    (from + ((1L + to - from) * randomGenerator.raw()).toLong).toInt
  }

  /**
   * Returns a uniformly distributed random number in the closed interval
   * <tt>[from,to]</tt> (including <tt>from</tt> and <tt>to</tt>). Pre
   * conditions: <tt>from &lt;= to</tt>.
   */
  def nextLongFromTo(from: Long, to: Long): Long = {
    if (from >= 0 && to < Long.MAX_VALUE) {
      return from + (nextDoubleFromTo(0.0, to - from + 1)).toLong
    }
    val diff = to.toDouble - from.toDouble + 1.0
    if (diff <= Long.MAX_VALUE) {
      return from + (nextDoubleFromTo(0.0, diff)).toLong
    }
    var random: Long = 0l
    if (from == Long.MIN_VALUE) {
      if (to == Long.MAX_VALUE) {
        val i1 = nextIntFromTo(Integer.MIN_VALUE, Integer.MAX_VALUE)
        val i2 = nextIntFromTo(Integer.MIN_VALUE, Integer.MAX_VALUE)
        return ((i1 & 0xFFFFFFFFL) << 32) | (i2 & 0xFFFFFFFFL)
      }
      random = Math.round(nextDoubleFromTo(from, to + 1))
      if (random > to) random = from
    } else {
      random = Math.round(nextDoubleFromTo(from - 1, to))
      if (random < from) random = to
    }
    random
  }

  /**
   * Returns the probability distribution function (assuming a continous
   * uniform distribution).
   */
  def pdf(x: Double): Double = {
    if (x <= min || x >= max) return 0.0
    1.0 / (max - min)
  }

  /**
   * Sets the internal state.
   */
  def setState(min: Double, max: Double) {
    if (max < min) {
      setState(max, min)
      return
    }
    this.min = min
    this.max = max
  }

  /**
   * Returns a String representation of the receiver.
   */
  override def toString(): String = {
    this.getClass.getName + "(" + min + "," + max + ")"
  }
}
