package cern.jet.stat.tfloat.quantile

import cern.colt.list.tfloat.FloatArrayList
import cern.jet.stat.Buffer
//remove if not needed
import scala.collection.JavaConversions._

/**
 * A buffer holding <tt>float</tt> elements; internally used for computing
 * approximate quantiles.
 */
@SerialVersionUID(1L)
class FloatBuffer(k: Int) extends Buffer(k) {

  protected var values: FloatArrayList = new FloatArrayList(0)

  protected var isSorted: Boolean = false

  /**
   * Adds a value to the receiver.
   */
  def add(value: Float) {
    if (!isAllocated) allocate()
    values.add(value)
    this.isSorted = false
  }

  /**
   * Adds a value to the receiver.
   */
  def addAllOfFromTo(elements: FloatArrayList, from: Int, to: Int) {
    if (!isAllocated) allocate()
    values.addAllOfFromTo(elements, from, to)
    this.isSorted = false
  }

  /**
   * Allocates the receiver.
   */
  protected def allocate() {
    isAllocated = true
    values.ensureCapacity(k)
  }

  /**
   * Clears the receiver.
   */
  def clear() {
    values.clear()
  }

  /**
   * Returns a deep copy of the receiver.
   *
   * @return a deep copy of the receiver.
   */
  def clone(): AnyRef = {
    val copy = super.clone().asInstanceOf[FloatBuffer]
    if (this.values != null) copy.values = copy.values.copy()
    copy
  }

  /**
   * Returns whether the specified element is contained in the receiver.
   */
  def contains(element: Float): Boolean = {
    this.sort()
    values.contains(element)
  }

  /**
   * Returns whether the receiver is empty.
   */
  def isEmpty(): Boolean = values.size == 0

  /**
   * Returns whether the receiver is empty.
   */
  def isFull(): Boolean = values.size == k

  /**
   * Returns the number of elements currently needed to store all contained
   * elements. This number usually differs from the results of method
   * <tt>size()</tt>, according to the underlying algorithm.
   */
  def memory(): Int = values.elements().length

  /**
   * Returns the rank of a given element within the sorted sequence of the
   * receiver. A rank is the number of elements <= element. Ranks are of the
   * form {1,2,...size()}. If no element is <= element, then the rank is zero.
   * If the element lies in between two contained elements, then uses linear
   * interpolation.
   *
   * @return the rank of the element.
   * @param list
   *            cern.colt.list.FloatArrayList
   * @param element
   *            the element to search for
   */
  def rank(element: Float): Float = {
    this.sort()
    cern.jet.stat.tfloat.FloatDescriptive.rankInterpolated(this.values, element)
  }

  /**
   * Returns the number of elements contained in the receiver.
   */
  def size(): Int = values.size

  /**
   * Sorts the receiver.
   */
  def sort() {
    if (!this.isSorted) {
      values.sort()
      this.isSorted = true
    }
  }

  /**
   * Returns a String representation of the receiver.
   */
  override def toString(): String = {
    "k=" + this.k + ", w=" + Long toString weight() + ", l=" + 
      Integer toString level() + 
      ", size=" + 
      values.size
  }
}
