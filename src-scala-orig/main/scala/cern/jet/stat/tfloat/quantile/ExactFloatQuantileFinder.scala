package cern.jet.stat.tfloat.quantile

import cern.colt.list.tfloat.FloatArrayList
//remove if not needed
import scala.collection.JavaConversions._

@SerialVersionUID(1L)
class ExactFloatQuantileFinder extends cern.colt.PersistentObject with FloatQuantileFinder {

  protected var buffer: FloatArrayList = new FloatArrayList(0)

  protected var isSorted: Boolean = _

  this.clear()

  /**
   * Adds a value to the receiver.
   *
   * @param value
   *            the value to add.
   */
  def add(value: Float) {
    this.buffer.add(value)
    this.isSorted = false
  }

  /**
   * Adds all values of the specified list to the receiver.
   *
   * @param values
   *            the list of which all values shall be added.
   */
  def addAllOf(values: FloatArrayList) {
    addAllOfFromTo(values, 0, values.size - 1)
  }

  /**
   * Adds the part of the specified list between indexes <tt>from</tt>
   * (inclusive) and <tt>to</tt> (inclusive) to the receiver.
   *
   * @param values
   *            the list of which elements shall be added.
   * @param from
   *            the index of the first element to be added (inclusive).
   * @param to
   *            the index of the last element to be added (inclusive).
   */
  def addAllOfFromTo(values: FloatArrayList, from: Int, to: Int) {
    buffer.addAllOfFromTo(values, from, to)
    this.isSorted = false
  }

  /**
   * Removes all elements from the receiver. The receiver will be empty after
   * this call returns, and its memory requirements will be close to zero.
   */
  def clear() {
    this.buffer.clear()
    this.buffer.trimToSize()
    this.isSorted = false
  }

  /**
   * Returns a deep copy of the receiver.
   *
   * @return a deep copy of the receiver.
   */
  def clone(): AnyRef = {
    val copy = super.clone().asInstanceOf[ExactFloatQuantileFinder]
    if (this.buffer != null) copy.buffer = copy.buffer.copy()
    copy
  }

  /**
   * Returns whether the specified element is contained in the receiver.
   */
  def contains(element: Float): Boolean = {
    this.sort()
    buffer.binarySearch(element) >= 0
  }

  /**
   * Applies a procedure to each element of the receiver, if any. Iterates
   * over the receiver in no particular order.
   *
   * @param procedure
   *            the procedure to be applied. Stops iteration if the procedure
   *            returns <tt>false</tt>, otherwise continues.
   * @return <tt>false</tt> if the procedure stopped before all elements where
   *         iterated over, <tt>true</tt> otherwise.
   */
  def forEach(procedure: cern.colt.function.tfloat.FloatProcedure): Boolean = {
    val theElements = buffer.elements()
    val theSize = size.toInt
    var i = 0
    while (i < theSize) if (!procedure.apply(theElements(i += 1))) return false
    true
  }

  /**
   * Returns the number of elements currently needed to store all contained
   * elements. This number usually differs from the results of method
   * <tt>size()</tt>, according to the underlying datastructure.
   */
  def memory(): Long = buffer.elements().length

  /**
   * Returns how many percent of the elements contained in the receiver are
   * <tt>&lt;= element</tt>. Does linear interpolation if the element is not
   * contained but lies in between two contained elements.
   *
   * @param the
   *            element to search for.
   * @return the percentage <tt>p</tt> of elements <tt>&lt;= element</tt> (
   *         <tt>0.0 &lt;= p &lt;=1.0)</tt>.
   */
  def phi(element: Float): Float = {
    this.sort()
    cern.jet.stat.tfloat.FloatDescriptive.rankInterpolated(buffer, element) / 
      this.size
  }

  /**
   * Computes the specified quantile elements over the values previously
   * added.
   *
   * @param phis
   *            the quantiles for which elements are to be computed. Each phi
   *            must be in the interval [0.0,1.0]. <tt>phis</tt> must be
   *            sorted ascending.
   * @return the exact quantile elements.
   */
  def quantileElements(phis: FloatArrayList): FloatArrayList = {
    this.sort()
    cern.jet.stat.tfloat.FloatDescriptive.quantiles(this.buffer, phis)
  }

  /**
   * Returns the number of elements currently contained in the receiver
   * (identical to the number of values added so far).
   */
  def size(): Long = buffer.size

  /**
   * Sorts the receiver.
   */
  protected def sort() {
    if (!isSorted) {
      buffer.sort()
      this.isSorted = true
    }
  }

  /**
   * Returns a String representation of the receiver.
   */
  override def toString(): String = {
    var s = this.getClass.getName
    s = s.substring(s.lastIndexOf('.') + 1)
    s + "(mem=" + memory() + ", size=" + size + ")"
  }

  /**
   * Returns the number of elements currently needed to store all contained
   * elements. This number usually differs from the results of method
   * <tt>size()</tt>, according to the underlying datastructure.
   */
  def totalMemory(): Long = memory()
}
