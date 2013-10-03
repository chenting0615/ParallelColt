package cern.colt.map

/**
 * Provides various hash functions.
 *
 * @author wolfgang.hoschek@cern.ch
 * @version 1.0, 09/24/99
 */
object HashFunctions {

  /**
   * Returns a hashcode for the specified value.
   *
   * @return a hash code value for the specified value.
   */
  def hash(value: Char): Int = value

  /**
   * Returns a hashcode for the specified value.
   *
   * @return a hash code value for the specified value.
   */
  def hash(value: Double): Int = {
    val bits = java.lang.Double.doubleToLongBits(value)
    (bits ^ (bits >>> 32)).toInt
  }

  /**
   * Returns a hashcode for the specified value.
   *
   * @return a hash code value for the specified value.
   */
  def hash(value: Float): Int = {
    java.lang.Float.floatToIntBits(value * 663608941.737f)
  }

  /**
   * Returns a hashcode for the specified value.
   *
   * @return a hash code value for the specified value.
   */
  def hash(value: Int): Int = value

  /**
   * Returns a hashcode for the specified value.
   *
   * @return a hash code value for the specified value.
   */
  def hash(value: Long): Int = (value ^ (value >> 32)).toInt

  /**
   * Returns a hashcode for the specified object.
   *
   * @return a hash code value for the specified object.
   */
  def hash(`object`: AnyRef): Int = {
    if (`object` == null) 0 else `object`.hashCode
  }

  /**
   * Returns a hashcode for the specified value.
   *
   * @return a hash code value for the specified value.
   */
  def hash(value: Short): Int = value

  /**
   * Returns a hashcode for the specified value.
   *
   * @return a hash code value for the specified value.
   */
  def hash(value: Boolean): Int = if (value) 1231 else 1237

  /**
   * Returns a hashcode for the specified value.
   *
   * @return a hash code value for the specified value.
   */
  def hash(value: Byte): Int = value
}
