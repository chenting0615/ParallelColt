package cern.jet.random.tdouble.engine

object DoubleRandomEngine {

  /**
   * Constructs and returns a new uniform random number engine seeded with the
   * current time. Currently this is
   * cern.jet.random.tdouble.engine.DoubleMersenneTwister.
   */
  def makeDefault(): DoubleRandomEngine = {
    new cern.jet.random.tdouble.engine.DoubleMersenneTwister(System.currentTimeMillis().toInt)
  }
}

@SerialVersionUID(1L)
abstract class DoubleRandomEngine protected () extends cern.colt.PersistentObject {

  /**
   * Returns a 64 bit uniformly distributed random number in the open unit
   * interval <code>(0.0,1.0)</code> (excluding 0.0 and 1.0).
   */
  def nextDouble(): Double = {
    var nextDouble: Double = 0.0
    do {
      nextDouble = (nextLong() - -9.223372036854776E18) * 5.421010862427522E-20
    } while (!(nextDouble > 0.0 && nextDouble < 1.0))
    nextDouble
  }

  /**
   * Returns a 32 bit uniformly distributed random number in the open unit
   * interval <code>(0.0f,1.0f)</code> (excluding 0.0f and 1.0f).
   */
  def nextFloat(): Float = {
    var nextFloat: Float = 0.0f
    do {
      nextFloat = raw().toFloat
    } while (nextFloat >= 1.0f)
    nextFloat
  }

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
  def nextLong(): Long = {
    ((nextInt() & 0xFFFFFFFFL) << 32) | (nextInt() & 0xFFFFFFFFL)
  }

  /**
   * Returns a 32 bit uniformly distributed random number in the open unit
   * interval <code>(0.0,1.0)</code> (excluding 0.0 and 1.0).
   */
  def raw(): Double = {
    var next: Int = 0
    do {
      next = nextInt()
    } while (next == 0)
    (next & 0xFFFFFFFFL) * 2.3283064365386963E-10
  }
}
