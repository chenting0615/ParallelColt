package hep.aida.bin

import cern.jet.stat.tdouble.DoubleDescriptive
import cern.colt.list.ArrayTypes.DoubleArrayList

/**
 * 1-dimensional non-rebinnable bin consuming <tt>double</tt> elements;
 * Efficiently computes basic statistics of data sequences. First see the <a
 * href="package-summary.html">package summary</a> and javadoc <a
 * href="package-tree.html">tree view</a> to get the broad picture.
 * <p>
 * The data streamed into a <tt>SimpleBin1D</tt> is not preserved! As a
 * consequence infinitely many elements can be added to this bin. As a further
 * consequence this bin cannot compute more than basic statistics. It is also
 * not rebinnable. If these drawbacks matter, consider to use a
 * DynamicDoubleBin1D, which overcomes them at the expense of increased
 * memory requirements.
 * <p>
 * This class is fully thread safe (all public methods are synchronized). Thus,
 * you can have one or more threads adding to the bin as well as one or more
 * threads reading and viewing the statistics of the bin <i>while it is
 * filled</i>. For high performance, add data in large chunks (buffers) via
 * method <tt>addAllOf</tt> rather than piecewise via method <tt>add</tt>.
 * <p>
 * <b>Implementation</b>: Incremental maintainance. Performance linear in the
 * number of elements added.
 *
 * @author wolfgang.hoschek@cern.ch
 * @version 0.9, 03-Jul-99
 */
@specialized
@SerialVersionUID(1L)
class StaticBin1D[T: Numeric] extends AbstractBin1D[T] {

  val num = implicitly[Numeric[T]]

  /**
   * The number of elements consumed by incremental parameter maintainance.
   */
  protected var sizeVar: Int = 0

  protected var minVar: Double = Double.NegativeInfinity

  protected var maxVar: Double = Double.PositiveInfinity

  protected var sumVar: Double = 0.0

  protected var sum_xxVar: Double = 0.0

  /**
   * Function arguments used by method addAllOf(...) For memory tuning only.
   * Avoids allocating a new array of arguments each time addAllOf(...) is
   * called.
   *
   * Each bin does not need its own set of argument vars since they are
   * declared as "static". addAllOf(...) of this class uses only 4 entries.
   * Subclasses computing additional incremental statistics may need more
   * arguments. So, to be on the safe side we allocate space for 20 args. Be
   * sure you access this arguments only in synchronized blocks like
   * synchronized (arguments) { do it }
   *
   * By the way, the whole fuss would be unnecessary if Java would know INOUT
   * parameters (call by reference).
   */
  @transient protected var arguments: Array[Double] = new Array[Double](20)

  clear()

  /**
   * Adds the specified element to the receiver.
   *
   * @param element_p
   *            element to be appended.
   */
  def add(element_p: T) {
    val element = num.toDouble(element_p)
    if (element < minVar)
      minVar = element
    if (element > maxVar)
      maxVar = element
    sizeVar += 1
    sumVar += element
    sum_xxVar += element * element
  }

  /**
   * Adds the part of the specified list between indexes <tt>from</tt>
   * (inclusive) and <tt>to</tt> (inclusive) to the receiver.
   *
   * @param list
   *            the list of which elements shall be added.
   * @param from
   *            the index of the first element to be added (inclusive).
   * @param to
   *            the index of the last element to be added (inclusive).
   * @throws IndexOutOfBoundsException
   *             if
   *             <tt>list.size()&gt;0 && (from&lt;0 || from&gt;to || to&gt;=list.size())</tt>
   *             .
   */
  def addAllOfFromTo(list: DoubleArrayList, from: Int, to: Int) {
    arguments(0) = this.minVar
    arguments(1) = this.maxVar
    arguments(2) = this.sumVar
    arguments(3) = this.sum_xxVar
    DoubleDescriptive.incrementalUpdate(list, from, to, arguments)
    this.minVar = arguments(0)
    this.maxVar = arguments(1)
    this.sumVar = arguments(2)
    this.sum_xxVar = arguments(3)
    this.sizeVar += to - from + 1
  }

  /**
   * Removes all elements from the receiver. The receiver will be empty after
   * this call returns.
   */
  def clear() {
    clearAllMeasures()
    this.sizeVar = 0
  }

  /**
   * Resets the values of all measures.
   */
  protected def clearAllMeasures() {
    this.minVar = Double.PositiveInfinity
    this.maxVar = Double.NegativeInfinity
    this.sumVar = 0.0
    this.sum_xxVar = 0.0
  }

  /**
   * Returns <tt>false</tt>. Returns whether a client can obtain all elements
   * added to the receiver. In other words, tells whether the receiver
   * internally preserves all added elements. If the receiver is rebinnable,
   * the elements can be obtained via <tt>elements()</tt> methods.
   *
   */
  def isRebinnable: Boolean = false

  /**
   * Returns the maximum.
   */
  def max: Double = maxVar

  /**
   * Returns the minimum.
   */
  def min: Double = minVar

  /**
   * Returns the number of elements contained in the receiver.
   *
   * @return the number of elements contained in the receiver.
   */
  def size = sizeVar

  /**
   * Returns the sum of all elements, which is <tt>Sum( x[i] )</tt>.
   */
  def sum: Double = sumVar

  /**
   * Returns the sum of squares, which is <tt>Sum( x[i] * x[i] )</tt>.
   */
  def sumOfSquares: Double = sum_xxVar
}
