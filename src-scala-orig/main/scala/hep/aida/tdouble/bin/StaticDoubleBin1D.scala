package hep.aida.tdouble.bin

import cern.colt.list.tdouble.DoubleArrayList
import cern.jet.stat.tdouble.DoubleDescriptive
import StaticDoubleBin1D._
//remove if not needed
import scala.collection.JavaConversions._

object StaticDoubleBin1D {

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
}

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
 * {@link DynamicDoubleBin1D}, which overcomes them at the expense of increased
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
@SerialVersionUID(1L)
class StaticDoubleBin1D extends AbstractDoubleBin1D {

  /**
   * The number of elements consumed by incremental parameter maintainance.
   */
  protected var size: Int = 0

  protected var min: Double = 0.0

  protected var max: Double = 0.0

  protected var sum: Double = 0.0

  protected var sum_xx: Double = 0.0

  clear()

  /**
   * Adds the specified element to the receiver.
   *
   * @param element
   *            element to be appended.
   */
  def add(element: Double) {
    synchronized {
      this.addAllOf(new DoubleArrayList(Array(element)))
    }
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
    synchronized {
      synchronized (arguments) {
        arguments(0) = this.min
        arguments(1) = this.max
        arguments(2) = this.sum
        arguments(3) = this.sum_xx
        DoubleDescriptive.incrementalUpdate(list, from, to, arguments)
        this.min = arguments(0)
        this.max = arguments(1)
        this.sum = arguments(2)
        this.sum_xx = arguments(3)
        this.size += to - from + 1
      }
    }
  }

  /**
   * Removes all elements from the receiver. The receiver will be empty after
   * this call returns.
   */
  def clear() {
    synchronized {
      clearAllMeasures()
      this.size = 0
    }
  }

  /**
   * Resets the values of all measures.
   */
  protected def clearAllMeasures() {
    this.min = Double.POSITIVE_INFINITY
    this.max = Double.NEGATIVE_INFINITY
    this.sum = 0.0
    this.sum_xx = 0.0
  }

  /**
   * Returns <tt>false</tt>. Returns whether a client can obtain all elements
   * added to the receiver. In other words, tells whether the receiver
   * internally preserves all added elements. If the receiver is rebinnable,
   * the elements can be obtained via <tt>elements()</tt> methods.
   *
   */
  def isRebinnable(): Boolean = synchronized {
  false
}

  /**
   * Returns the maximum.
   */
  def max(): Double = synchronized {
  this.max
}

  /**
   * Returns the minimum.
   */
  def min(): Double = synchronized {
  this.min
}

  /**
   * Returns the number of elements contained in the receiver.
   *
   * @return the number of elements contained in the receiver.
   */
  def size(): Int = synchronized {
  this.size
}

  /**
   * Returns the sum of all elements, which is <tt>Sum( x[i] )</tt>.
   */
  def sum(): Double = synchronized {
  this.sum
}

  /**
   * Returns the sum of squares, which is <tt>Sum( x[i] * x[i] )</tt>.
   */
  def sumOfSquares(): Double = synchronized {
  this.sum_xx
}
}
