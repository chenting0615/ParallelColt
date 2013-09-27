package cern.jet.random.tdouble.engine

import java.util.Date

/**
 * Same as <tt>MersenneTwister</tt> except that method <tt>raw()</tt> returns 64
 * bit random numbers instead of 32 bit random numbers.
 *
 * @author wolfgang.hoschek@cern.ch
 * @version 1.0, 09/24/99
 * @see DoubleMersenneTwister
 * Constructs and returns a random number generator with the given seed.
 *
 * @param seed
 *            should not be 0, in such a case
 *            <tt>MersenneTwister64.DEFAULT_SEED</tt> is silently
 *            substituted.
 */
@SerialVersionUID(1L)
class MersenneTwister64(seed: Int) extends DoubleMersenneTwister(seed) {

  def this() {
    this(seed)
  }

  /**
   * Constructs and returns a random number generator seeded with the given
   * date.
   *
   * @param d
   *            typically <tt>new java.util.Date()</tt>
   */
  def this(d: Date) {
    this(d.getTime.toInt)
  }

  /**
   * Returns a 64 bit uniformly distributed random number in the open unit
   * interval <code>(0.0,1.0)</code> (excluding 0.0 and 1.0).
   */
  override def raw(): Double = nextDouble()
}
