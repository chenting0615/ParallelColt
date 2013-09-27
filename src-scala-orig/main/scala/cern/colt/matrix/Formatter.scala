package cern.colt.matrix

/**
 * Formats a double or complex (double[]) into a string (like sprintf in C).
 *
 * @author wolfgang.hoschek@cern.ch
 * @author Piotr Wendykier (piotr.wendykier@gmail.com)
 * @version 1.0, 21/07/00
 * @see java.util.Comparator
 * @see cern.colt
 * @see cern.colt.Sorting
 */
trait Formatter {

  /**
   * Formats a double into a string (like sprintf in C).
   *
   * @param value
   *            the number to format
   * @return the formatted string
   * @throws IllegalArgumentException
   *                if bad argument
   */
  def format(value: Double): String

  /**
   * Formats a float into a string (like sprintf in C).
   *
   * @param value
   *            the number to format
   * @return the formatted string
   * @throws IllegalArgumentException
   *                if bad argument
   */
  def format(value: Float): String

  /**
   * Formats an int into a string (like sprintf in C).
   *
   * @param value
   *            the number to format
   * @return the formatted string
   * @throws IllegalArgumentException
   *                if bad argument
   */
  def format(value: Int): String

  /**
   * Formats an long into a string (like sprintf in C).
   *
   * @param value
   *            the number to format
   * @return the formatted string
   * @throws IllegalArgumentException
   *                if bad argument
   */
  def format(value: Long): String

  /**
   * Formats a complex (double[]) into a string (like sprintf in C).
   *
   * @param value
   *            the number to format
   * @return the formatted string
   * @throws IllegalArgumentException
   *                if bad argument
   */
  def format(value: Array[Double]): String

  /**
   * Formats a complex (float[]) into a string (like sprintf in C).
   *
   * @param value
   *            the number to format
   * @return the formatted string
   * @throws IllegalArgumentException
   *                if bad argument
   */
  def format(value: Array[Float]): String
}
