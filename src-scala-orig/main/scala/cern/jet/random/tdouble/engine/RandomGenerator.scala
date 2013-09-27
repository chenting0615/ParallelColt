package cern.jet.random.tdouble.engine

/**
 * Interface for uniform pseudo-random number generators.
 */
trait RandomGenerator {

  /**
   * Returns a 32 bit uniformly distributed random number in the open unit
   * interval <code>(0.0,1.0)</code> (excluding 0.0 and 1.0).
   */
  def raw(): Double

  /**
   * Returns a 64 bit uniformly distributed random number in the open unit
   * interval <code>(0.0,1.0)</code> (excluding 0.0 and 1.0).
   */
  def nextDouble(): Double

  /**
   * Returns a 32 bit uniformly distributed random number in the closed
   * interval <tt>[Integer.MIN_VALUE,Integer.MAX_VALUE]</tt> (including
   * <tt>Integer.MIN_VALUE</tt> and <tt>Integer.MAX_VALUE</tt>);
   */
  def nextInt(): Int

  /**
   * Returns a 64 bit uniformly distributed random number in the closed
   * interval <tt>[Long.MIN_VALUE,Long.MAX_VALUE]</tt> (including
   * <tt>Long.MIN_VALUE</tt> and <tt>Long.MAX_VALUE</tt>).
   */
  def nextLong(): Long

  /**
   * Returns a 32 bit uniformly distributed random number in the open unit
   * interval <code>(0.0f,1.0f)</code> (excluding 0.0f and 1.0f).
   */
  def nextFloat(): Float
}
