package cern.colt.list

@SerialVersionUID(1L)
trait AbstractCollection[@specialized T] extends cern.colt.PersistentObject {

  /**
   * Removes all elements from the receiver. The receiver will be empty after
   * this call returns.
   */
  def clear(): Unit

  /**
   * Tests if the receiver has no elements.
   *
   * @return <code>true</code> if the receiver has no elements;
   *         <code>false</code> otherwise.
   */
  def isEmpty: Boolean = size == 0

  /**
   * Returns the number of elements contained in the receiver.
   *
   * @return the number of elements contained in the receiver.
   */
  def size: Int

  /**
   * Returns a <code>java.util.ArrayList</code> containing all the elements in
   * the receiver.
   */
  def toList: java.util.ArrayList[T]

  /**
   * Returns a string representation of the receiver, containing the String
   * representation of each element.
   */
  override def toString: String = toList.toString
}
