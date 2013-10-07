package cern.colt

/**
 * Array manipulations; complements <tt>java.util.Arrays</tt>.
 *
 * @see java.util.Arrays
 * @see cern.colt.Sorting
 *
 * @author wolfgang.hoschek@cern.ch
 * @version 1.0, 03-Jul-99
 */
object Arrays {

  /**
   * Ensures that a given array can hold up to <tt>minCapacity</tt> elements.
   *
   * Returns the identical array if it can hold at least the number of
   * elements specified. Otherwise, returns a new array with increased
   * capacity containing the same elements, ensuring that it can hold at least
   * the number of elements specified by the minimum capacity argument.
   *
   * @param minCapacity
   *            the desired minimum capacity.
   */
  def ensureCapacity[@specialized T: Manifest](array: Array[T], minCapacity: Int): Array[T] = {
    val oldCapacity = array.length
    var newArray: Array[T] = null
    if (minCapacity > oldCapacity) {
      var newCapacity = (oldCapacity * 3) / 2 + 1
      if (newCapacity < minCapacity) {
        newCapacity = minCapacity
      }
      newArray = Array.ofDim[T](newCapacity)
      System.arraycopy(array, 0, newArray, 0, oldCapacity)
    } else {
      newArray = array
    }
    newArray
  }

  /**
   * Returns a string representation of the specified array. The string
   * representation consists of a list of the arrays's elements, enclosed in
   * square brackets (<tt>"[]"</tt>). Adjacent elements are separated by the
   * characters <tt>", "</tt> (comma and space).
   *
   * @return a string representation of the specified array.
   */
  def toString[T](array: Array[T], maxIndex: Int): String = {
    val buf = new StringBuffer()
    buf.append("[")
    var i = 0
    while (i < maxIndex) {
      buf.append(array(i))
      if (i < maxIndex) buf.append(", ")
      i += 1
    }
    buf.append("]")
    buf.toString
  }

  /**
   * Ensures that the specified array cannot hold more than
   * <tt>maxCapacity</tt> elements. An application can use this operation to
   * minimize array storage.
   * <p>
   * Returns the identical array if <tt>array.length &lt;= maxCapacity</tt>.
   * Otherwise, returns a new array with a length of <tt>maxCapacity</tt>
   * containing the first <tt>maxCapacity</tt> elements of <tt>array</tt>.
   *
   * @param maxCapacity
   *            the desired maximum capacity.
   */
  def trimToCapacity[@specialized T: Manifest](arrayP: Array[T], maxCapacity: Int): Array[T] = {
    var array = arrayP
    if (array.length > maxCapacity) {
      val oldArray = array
      array = Array.ofDim[T](maxCapacity)
      System.arraycopy(oldArray, 0, array, 0, maxCapacity)
    }
    array
  }
}
