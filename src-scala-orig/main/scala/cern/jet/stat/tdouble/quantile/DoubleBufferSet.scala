package cern.jet.stat.tdouble.quantile

import cern.jet.stat.BufferSet
//remove if not needed
import scala.collection.JavaConversions._

/**
 * A set of buffers holding <tt>double</tt> elements; internally used for
 * computing approximate quantiles.
 */
@SerialVersionUID(1L)
class DoubleBufferSet(b: Int, k: Int) extends BufferSet {

  protected var buffers: Array[DoubleBuffer] = new Array[DoubleBuffer](b)

  private var nextTriggerCalculationState: Boolean = _

  this.clear(k)

  /**
   * Returns an empty buffer if at least one exists. Preferably returns a
   * buffer which has already been used, i.e. a buffer which has already been
   * allocated.
   */
  def _getFirstEmptyBuffer(): DoubleBuffer = {
    var emptyBuffer: DoubleBuffer = null
    var i = buffers.length
    while (i >= 0) {
      if (buffers(i).isEmpty) {
        if (buffers(i).isAllocated) return buffers(i)
        emptyBuffer = buffers(i)
      }
    }
    emptyBuffer
  }

  /**
   * Returns all full or partial buffers.
   */
  def _getFullOrPartialBuffers(): Array[DoubleBuffer] = {
    var count = 0
    var i = buffers.length
    while (i >= 0) {
      if (!buffers(i).isEmpty) count += 1
    }
    val collectedBuffers = Array.ofDim[DoubleBuffer](count)
    val j = 0
    var i = buffers.length
    while (i >= 0) {
      if (!buffers(i).isEmpty) {
        collectedBuffers(j += 1) = buffers(i)
      }
    }
    collectedBuffers
  }

  /**
   * Determines all full buffers having the specified level.
   *
   * @return all full buffers having the specified level
   */
  def _getFullOrPartialBuffersWithLevel(level: Int): Array[DoubleBuffer] = {
    var count = 0
    var i = buffers.length
    while (i >= 0) {
      if ((!buffers(i).isEmpty) && buffers(i).level() == level) count += 1
    }
    val collectedBuffers = Array.ofDim[DoubleBuffer](count)
    val j = 0
    var i = buffers.length
    while (i >= 0) {
      if ((!buffers(i).isEmpty) && buffers(i).level() == level) {
        collectedBuffers(j += 1) = buffers(i)
      }
    }
    collectedBuffers
  }

  /**
   * @return The minimum level of all buffers which are full.
   */
  def _getMinLevelOfFullOrPartialBuffers(): Int = {
    val b = this.b()
    var minLevel = Integer.MAX_VALUE
    var buffer: DoubleBuffer = null
    for (i <- 0 until b) {
      buffer = this.buffers(i)
      if ((!buffer.isEmpty) && (buffer.level() < minLevel)) {
        minLevel = buffer.level()
      }
    }
    minLevel
  }

  /**
   * Returns the number of empty buffers.
   */
  def _getNumberOfEmptyBuffers(): Int = {
    var count = 0
    var i = buffers.length
    while (i >= 0) {
      if (buffers(i).isEmpty) count += 1
    }
    count
  }

  /**
   * Returns all empty buffers.
   */
  def _getPartialBuffer(): DoubleBuffer = {
    var i = buffers.length
    while (i >= 0) {
      if (buffers(i).isPartial) return buffers(i)
    }
    null
  }

  /**
   * @return the number of buffers
   */
  def b(): Int = buffers.length

  /**
   * Removes all elements from the receiver. The receiver will be empty after
   * this call returns, and its memory requirements will be close to zero.
   */
  def clear() {
    clear(k())
  }

  /**
   * Removes all elements from the receiver. The receiver will be empty after
   * this call returns, and its memory requirements will be close to zero.
   */
  protected def clear(k: Int) {
    var i = b()
    while (i >= 0) this.buffers(i) = new DoubleBuffer(k)
    this.nextTriggerCalculationState = true
  }

  /**
   * Returns a deep copy of the receiver.
   *
   * @return a deep copy of the receiver.
   */
  def clone(): AnyRef = {
    val copy = super.clone().asInstanceOf[DoubleBufferSet]
    copy.buffers = copy.buffers.clone()
    var i = buffers.length
    while (i >= 0) {
      copy.buffers(i) = copy.buffers(i).clone().asInstanceOf[DoubleBuffer]
    }
    copy
  }

  /**
   * Collapses the specified full buffers (must not include partial buffer).
   *
   * @return a full buffer containing the collapsed values. The buffer has
   *         accumulated weight.
   * @param buffers
   *            the buffers to be collapsed (all of them must be full or
   *            partially full)
   */
  def collapse(buffers: Array[DoubleBuffer]): DoubleBuffer = {
    var W = 0
    for (i <- 0 until buffers.length) {
      W += buffers(i).weight()
    }
    val k = this.k()
    val triggerPositions = Array.ofDim[Long](k)
    for (j <- 0 until k) {
      triggerPositions(j) = this.nextTriggerPosition(j, W)
    }
    val outputValues = this.getValuesAtPositions(buffers, triggerPositions)
    for (b <- 1 until buffers.length) buffers(b).clear()
    val outputBuffer = buffers(0)
    outputBuffer.values.elements(outputValues)
    outputBuffer.weight(W)
    outputBuffer
  }

  /**
   * Returns whether the specified element is contained in the receiver.
   */
  def contains(element: Double): Boolean = {
    var i = buffers.length
    while (i >= 0) {
      if ((!buffers(i).isEmpty) && buffers(i).contains(element)) {
        return true
      }
    }
    false
  }

  /**
   * Applies a procedure to each element of the receiver, if any. Iterates
   * over the receiver in no particular order.
   *
   * @param procedure
   *            the procedure to be applied. Stops iteration if the procedure
   *            returns <tt>false</tt>, otherwise continues.
   */
  def forEach(procedure: cern.colt.function.tdouble.Procedure1): Boolean = {
    var i = buffers.length
    while (i >= 0) {
      var w = buffers(i).weight()
      while (w >= 0) {
        if (!(buffers(i).values.forEach(procedure))) return false
      }
    }
    true
  }

  /**
   * Determines all values of the specified buffers positioned at the
   * specified triggerPositions within the sorted sequence and fills them into
   * outputValues.
   *
   * @param buffers
   *            the buffers to be searched (all must be full or partial)
   * @param triggerPositions
   *            the positions of elements within the sorted sequence to be
   *            retrieved
   * @return outputValues a list filled with the values at triggerPositions
   */
  protected def getValuesAtPositions(buffers: Array[DoubleBuffer], triggerPositions: Array[Long]): Array[Double] = {
    var i = buffers.length
    while (i >= 0) {
      buffers(i).sort()
    }
    val bufferSizes = Array.ofDim[Int](buffers.length)
    val bufferValues = Array.ofDim[DoubleDouble,](buffers.length)
    var totalBuffersSize = 0
    var i = buffers.length
    while (i >= 0) {
      bufferSizes(i) = buffers(i).size
      bufferValues(i) = buffers(i).values.elements()
      totalBuffersSize += bufferSizes(i)
    }
    val buffersSize = buffers.length
    val triggerPositionsLength = triggerPositions.length
    val j = 0
    val cursors = Array.ofDim[Int](buffers.length)
    var counter = 0
    var nextHit = triggerPositions(j)
    val outputValues = Array.ofDim[Double](triggerPositionsLength)
    if (totalBuffersSize == 0) {
      for (i <- 0 until triggerPositions.length) {
        outputValues(i) = Double.NaN
      }
      return outputValues
    }
    while (j < triggerPositionsLength) {
      var minValue = Double.POSITIVE_INFINITY
      var minBufferIndex = -1
      var b = buffersSize
      while (b >= 0) {
        if (cursors(b) < bufferSizes(b)) {
          val value = bufferValues(b)(cursors(b))
          if (value <= minValue) {
            minValue = value
            minBufferIndex = b
          }
        }
      }
      val minBuffer = buffers(minBufferIndex)
      counter += minBuffer.weight()
      while (counter > nextHit && j < triggerPositionsLength) {
        outputValues(j += 1) = minValue
        if (j < triggerPositionsLength) nextHit = triggerPositions(j)
      }
      cursors(minBufferIndex) += 1
    }
    outputValues
  }

  /**
   * @return the number of elements within a buffer.
   */
  def k(): Int = buffers(0).k

  /**
   * Returns the number of elements currently needed to store all contained
   * elements.
   */
  def memory(): Long = {
    var memory = 0
    var i = buffers.length
    while (i >= 0) {
      memory = memory + buffers(i).memory()
    }
    memory
  }

  /**
   * Computes the next triggerPosition for collapse
   *
   * @return the next triggerPosition for collapse
   * @param j
   *            specifies that the j-th trigger position is to be computed
   * @param W
   *            the accumulated weights
   */
  protected def nextTriggerPosition(j: Int, W: Long): Long = {
    var nextTriggerPosition: Long = 0l
    nextTriggerPosition = if (W % 2L != 0) j * W + (W + 1) / 2 else if (nextTriggerCalculationState) j * W + W / 2 else j * W + (W + 2) / 2
    nextTriggerPosition
  }

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
  def phi(element: Double): Double = {
    var elementsLessThanOrEqualToElement = 0.0
    var i = buffers.length
    while (i >= 0) {
      if (!buffers(i).isEmpty) {
        elementsLessThanOrEqualToElement += buffers(i).weight * buffers(i).rank(element)
      }
    }
    elementsLessThanOrEqualToElement / totalSize()
  }

  /**
   * @return a String representation of the receiver
   */
  override def toString(): String = {
    val buf = new StringBuffer()
    for (b <- 0 until this.b() if !buffers(b).isEmpty) {
      buf.append("buffer#" + b + " = ")
      buf.append(buffers(b).toString + "\n")
    }
    buf.toString
  }

  /**
   * Returns the number of elements in all buffers.
   */
  def totalSize(): Long = {
    val fullBuffers = _getFullOrPartialBuffers()
    var totalSize = 0
    var i = fullBuffers.length
    while (i >= 0) {
      totalSize += fullBuffers(i).size * fullBuffers(i).weight()
    }
    totalSize
  }
}
