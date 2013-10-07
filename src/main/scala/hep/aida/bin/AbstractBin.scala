package hep.aida.bin

/**
 * Abstract base class for all arbitrary-dimensional bins consumes
 * <tt>double</tt> elements. First see the <a
 * href="package-summary.html">package summary</a> and javadoc <a
 * href="package-tree.html">tree view</a> to get the broad picture.
 * <p>
 * This class is fully thread safe (all public methods are synchronized). Thus,
 * you can have one or more threads adding to the bin as well as one or more
 * threads reading and viewing the statistics of the bin <i>while it is
 * filled</i>. For high performance, add data in large chunks (buffers) via
 * method <tt>addAllOf</tt> rather than piecewise via method <tt>add</tt>.
 *
 * @author wolfgang.hoschek@cern.ch
 * @version 0.9, 03-Jul-99
 */
@SerialVersionUID(1L)
abstract class AbstractBin[@specialized T: Numeric] extends cern.colt.PersistentObject {

  /**
   * Returns <tt>center(0)</tt>.
   */
  def center(): T = center(0)

  /**
   * Returns a custom definable "center" measure; override this method if
   * necessary. Returns the absolute or relative center of this bin. For
   * example, the center of gravity.
   *
   * The <i>real</i> absolute center can be obtained as follow:
   * <tt>partition(i).min(j) * bin(j).offset() + bin(j).center(i)</tt>, where
   * <tt>i</tt> is the dimension. and <tt>j</tt> is the index of this bin.
   *
   * <p>
   * This default implementation always returns 0.5.
   *
   * @param dimension
   *            the dimension to be considered (zero based).
   */
  def center(dimension: Int): T = implicitly[Numeric[T]].one

  /**
   * Removes all elements from the receiver. The receiver will be empty after
   * this call returns.
   */
  def clear(): Unit

  /**
   * Returns whether two objects are equal; This default implementation
   * returns true if the other object is a bin and has the same size, value,
   * error and center.
   */
  override def equals(otherObj: Any): Boolean = {
    if (otherObj == null) return false
    otherObj match {
      case other: AbstractBin[T] => {
        if (other eq this) return true
        size == other.size && value() == other.value() && error() == other.error() && center() == other.center()
      }
      case _ => false
    }
  }

  /**
   * Returns <tt>error(0)</tt>.
   */
  def error(): T = error(0)

  /**
   * Returns a custom definable error measure; override this method if
   * necessary. This default implementation always returns <tt>0</tt>.
   *
   * @param dimension
   *            the dimension to be considered.
   */
  def error(dimension: Int): T = implicitly[Numeric[T]].zero

  /**
   * Returns whether a client can obtain all elements added to the receiver.
   * In other words, tells whether the receiver internally preserves all added
   * elements. If the receiver is rebinnable, the elements can be obtained via
   * <tt>elements()</tt> methods.
   */
  def isRebinnable: Boolean

  /**
   * Returns <tt>offset(0)</tt>.
   */
  def offset(): T = offset(0)

  /**
   * Returns the relative or absolute position for the center of the bin;
   * override this method if necessary. Returns 1.0 if a relative center is
   * stored in the bin. Returns 0.0 if an absolute center is stored in the
   * bin.
   *
   * <p>
   * This default implementation always returns 1.0 (relative).
   *
   * @param dimension
   *            the index of the considered dimension (zero based);
   */
  def offset(dimension: Int): T = implicitly[Numeric[T]].one

  /**
   * Returns the number of elements contained.
   *
   * @return the number of elements contained.
   */
  def size: Int

  /**
   * Returns a String representation of the receiver.
   */
  override def toString: String = {
    val buf = new StringBuffer()
    buf.append(getClass.getName)
    buf.append("\n-------------")
    buf.append("\n")
    buf.toString
  }

  /**
   * Trims the capacity of the receiver to be the receiver's current size.
   * Releases any superfluos internal memory. An application can use this
   * operation to minimize the storage of the receiver.
   *
   * This default implementation does nothing.
   */
  def trimToSize() {
  }

  /**
   * Returns <tt>value(0)</tt>.
   */
  def value(): T = value(0)

  /**
   * Returns a custom definable "value" measure; override this method if
   * necessary.
   * <p>
   * This default implementation always returns 0.0.
   *
   * @param dimension
   *            the dimension to be considered.
   */
  def value(dimension: Int): T = implicitly[Numeric[T]].zero
}
