package cern.jet.stat.tdouble.quantile

import cern.colt.list.tdouble.DoubleArrayList
import cern.jet.stat.Buffer
//remove if not needed
import scala.collection.JavaConversions._

/**
 * A buffer holding <tt>double</tt> elements; internally used for computing
 * approximate quantiles.
 */
@SerialVersionUID(1L)
class DoubleBuffer(k: Int) extends Buffer(k) {

  protected var values: DoubleArrayList = new DoubleArrayList(0)

  protected var isSorted: Boolean = false

  /**
   * Adds a value to the receiver.
   */
  def add(value: Double) {
    if (!isAllocated) allocate()
    values.add(value)
    this.isSorted = false
  }

  /**
   * Adds a value to the receiver.
   */
  def addAllOfFromTo(elements: DoubleArrayList, from: Int, to: Int) {
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
    val copy = super.clone().asInstanceOf[DoubleBuffer]
    if (this.values != null) copy.values = copy.values.copy()
    copy
  }

  /**
   * Returns whether the specified element is contained in the receiver.
   */
  def contains(element: Double): Boolean = {
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
   * @param element
   *            the element to search for
   */
  def rank(element: Double): Double = {
    this.sort()
    cern.jet.stat.tdouble.DoubleDescriptive.rankInterpolated(this.values, element)
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
