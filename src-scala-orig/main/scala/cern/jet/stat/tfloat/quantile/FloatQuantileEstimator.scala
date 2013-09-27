package cern.jet.stat.tfloat.quantile

import cern.colt.list.tfloat.FloatArrayList
import cern.colt.list.tobject.ObjectArrayList
import cern.jet.stat.Utils
//remove if not needed
import scala.collection.JavaConversions._

@SerialVersionUID(1L)
abstract class FloatQuantileEstimator protected () extends cern.colt.PersistentObject with FloatQuantileFinder {

  protected var bufferSet: FloatBufferSet = _

  protected var currentBufferToFill: FloatBuffer = _

  protected var totalElementsFilled: Int = _

  /**
   * Adds a value to the receiver.
   *
   * @param value
   *            the value to add.
   */
  def add(value: Float) {
    totalElementsFilled += 1
    if (!sampleNextElement()) return
    if (currentBufferToFill == null) {
      if (bufferSet._getFirstEmptyBuffer() == null) collapse()
      newBuffer()
    }
    currentBufferToFill.add(value)
    if (currentBufferToFill.isFull) currentBufferToFill = null
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
    val valuesToAdd = values.elements()
    val k = this.bufferSet.k()
    var bufferSize = k
    var bufferValues: Array[Float] = null
    if (currentBufferToFill != null) {
      bufferValues = currentBufferToFill.values.elements()
      bufferSize = currentBufferToFill.size
    }
    var i = from - 1
    while (i <= to) {
      if (sampleNextElement()) {
        if (bufferSize == k) {
          if (bufferSet._getFirstEmptyBuffer() == null) collapse()
          newBuffer()
          if (!currentBufferToFill.isAllocated) currentBufferToFill.allocate()
          currentBufferToFill.isSorted = false
          bufferValues = currentBufferToFill.values.elements()
          bufferSize = 0
        }
        bufferValues(bufferSize += 1) = valuesToAdd(i)
        if (bufferSize == k) {
          currentBufferToFill.values.setSize(bufferSize)
          currentBufferToFill = null
        }
      }
    }
    if (this.currentBufferToFill != null) {
      this.currentBufferToFill.values.setSize(bufferSize)
    }
    this.totalElementsFilled += to - from + 1
  }

  /**
   * Not yet commented.
   */
  protected def buffersToCollapse(): Array[FloatBuffer] = {
    val minLevel = bufferSet._getMinLevelOfFullOrPartialBuffers()
    bufferSet._getFullOrPartialBuffersWithLevel(minLevel)
  }

  /**
   * Removes all elements from the receiver. The receiver will be empty after
   * this call returns, and its memory requirements will be close to zero.
   */
  def clear() {
    this.totalElementsFilled = 0
    this.currentBufferToFill = null
    this.bufferSet.clear()
  }

  /**
   * Returns a deep copy of the receiver.
   *
   * @return a deep copy of the receiver.
   */
  def clone(): AnyRef = {
    val copy = super.clone().asInstanceOf[FloatQuantileEstimator]
    if (this.bufferSet != null) {
      copy.bufferSet = copy.bufferSet.clone().asInstanceOf[FloatBufferSet]
      if (this.currentBufferToFill != null) {
        val index = new ObjectArrayList(this.bufferSet.buffers).indexOf(this.currentBufferToFill, true)
        copy.currentBufferToFill = copy.bufferSet.buffers(index)
      }
    }
    copy
  }

  /**
   * Not yet commented.
   */
  protected def collapse() {
    val toCollapse = buffersToCollapse()
    val outputBuffer = bufferSet.collapse(toCollapse)
    val minLevel = toCollapse(0).level()
    outputBuffer.level(minLevel + 1)
    postCollapse(toCollapse)
  }

  /**
   * Returns whether the specified element is contained in the receiver.
   */
  def contains(element: Float): Boolean = bufferSet.contains(element)

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
  def forEach(procedure: cern.colt.function.tfloat.FloatProcedure): Boolean = this.bufferSet.forEach(procedure)

  /**
   * Returns the number of elements currently needed to store all contained
   * elements. This number usually differs from the results of method
   * <tt>size()</tt>, according to the underlying datastructure.
   */
  def memory(): Long = bufferSet.memory()

  /**
   * Not yet commented.
   */
  protected def newBuffer(): Unit

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
  def phi(element: Float): Float = bufferSet.phi(element)

  /**
   * Not yet commented.
   */
  protected def postCollapse(toCollapse: Array[FloatBuffer]): Unit

  /**
   * Default implementation does nothing.
   */
  protected def preProcessPhis(phis: FloatArrayList): FloatArrayList = phis

  /**
   * Computes the specified quantile elements over the values previously
   * added.
   *
   * @param phis
   *            the quantiles for which elements are to be computed. Each phi
   *            must be in the interval [0.0,1.0]. <tt>phis</tt> must be
   *            sorted ascending.
   * @return the approximate quantile elements.
   */
  def quantileElements(phis: FloatArrayList): FloatArrayList = {
    phis = preProcessPhis(phis)
    val triggerPositions = Array.ofDim[Long](phis.size)
    val totalSize = this.bufferSet.totalSize()
    var i = phis.size
    while (i >= 0) {
      triggerPositions(i) = Utils.epsilonCeiling(phis.get(i) * totalSize) - 1
    }
    val fullBuffers = bufferSet._getFullOrPartialBuffers()
    val quantileElements = Array.ofDim[Float](phis.size)
    new FloatArrayList(bufferSet.getValuesAtPositions(fullBuffers, triggerPositions))
  }

  /**
   * Not yet commented.
   */
  protected def sampleNextElement(): Boolean

  /**
   * Initializes the receiver
   */
  protected def setUp(b: Int, k: Int) {
    if (!(b >= 2 && k >= 1)) {
      throw new IllegalArgumentException("Assertion: b>=2 && k>=1")
    }
    this.bufferSet = new FloatBufferSet(b, k)
    this.clear()
  }

  /**
   * Returns the number of elements currently contained in the receiver
   * (identical to the number of values added so far).
   */
  def size(): Long = totalElementsFilled

  /**
   * Returns a String representation of the receiver.
   */
  override def toString(): String = {
    var s = this.getClass.getName
    s = s.substring(s.lastIndexOf('.') + 1)
    val b = bufferSet.b()
    val k = bufferSet.k()
    s + "(mem=" + memory() + ", b=" + b + ", k=" + k + ", size=" + 
      size + 
      ", totalSize=" + 
      this.bufferSet.totalSize() + 
      ")"
  }

  /**
   * Returns the number of elements currently needed to store all contained
   * elements. This number usually differs from the results of method
   * <tt>size()</tt>, according to the underlying datastructure.
   */
  def totalMemory(): Long = bufferSet.b() * bufferSet.k()
}
