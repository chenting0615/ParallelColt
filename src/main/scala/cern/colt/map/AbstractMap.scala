package cern.colt.map

/**
 * Abstract base class for hash maps holding objects or primitive data types
 * such as <code>int</code>, <code>float</code>, etc. as keys and/or values.
 * First see the <a href="package-summary.html">package summary</a> and javadoc
 * <a href="package-tree.html">tree view</a> to get the broad picture.
 * <p>
 * Note that implementations are not synchronized.
 *
 * @author wolfgang.hoschek@cern.ch
 * @version 1.0, 09/24/99
 * @see java.util.HashMap
 */
@SerialVersionUID(1L)
abstract class AbstractMap extends cern.colt.PersistentObject {

  /**
   * The number of distinct associations in the map; its "size()".
   */
  protected var distinct: Int = _

  /**
   * The table capacity c=table.length always satisfies the invariant
   * <tt>c * minLoadFactor <= s <= c * maxLoadFactor</tt>, where s=size() is
   * the number of associations currently contained. The term "c *
   * minLoadFactor" is called the "lowWaterMark", "c * maxLoadFactor" is
   * called the "highWaterMark". In other words, the table capacity (and
   * proportionally the memory used by this class) oscillates within these
   * constraints. The terms are precomputed and cached to avoid recalculating
   * them each time put(..) or removeKey(...) is called.
   */
  protected var lowWaterMark: Int = _

  protected var highWaterMark: Int = _

  /**
   * Chooses a new prime table capacity optimized for growing that
   * (approximately) satisfies the invariant
   * <tt>c * minLoadFactor <= size <= c * maxLoadFactor</tt> and has at least
   * one FREE slot for the given size.
   */
  protected def chooseGrowCapacity(size: Int, minLoad: Double, maxLoad: Double): Int = {
    nextPrime(Math.max(size + 1, (4 * size / (3 * minLoad + maxLoad)).toInt))
  }

  /**
   * Returns new high water mark threshold based on current capacity and
   * maxLoadFactor.
   *
   * @return int the new threshold.
   */
  protected def chooseHighWaterMark(capacity: Int, maxLoad: Double): Int = {
    Math.min(capacity - 2, (capacity * maxLoad).toInt)
  }

  /**
   * Returns new low water mark threshold based on current capacity and
   * minLoadFactor.
   *
   * @return int the new threshold.
   */
  protected def chooseLowWaterMark(capacity: Int, minLoad: Double): Int = (capacity * minLoad).toInt

  /**
   * Chooses a new prime table capacity neither favoring shrinking nor
   * growing, that (approximately) satisfies the invariant
   * <tt>c * minLoadFactor <= size <= c * maxLoadFactor</tt> and has at least
   * one FREE slot for the given size.
   */
  protected def chooseMeanCapacity(size: Int, minLoad: Double, maxLoad: Double): Int = {
    nextPrime(Math.max(size + 1, (2 * size / (minLoad + maxLoad)).toInt))
  }

  /**
   * Chooses a new prime table capacity optimized for shrinking that
   * (approximately) satisfies the invariant
   * <tt>c * minLoadFactor <= size <= c * maxLoadFactor</tt> and has at least
   * one FREE slot for the given size.
   */
  protected def chooseShrinkCapacity(size: Int, minLoad: Double, maxLoad: Double): Int = {
    nextPrime(Math.max(size + 1, (4 * size / (minLoad + 3 * maxLoad)).toInt))
  }

  /**
   * Removes all (key,value) associations from the receiver.
   */
  def clear(): Unit

  /**
   * Ensures that the receiver can hold at least the specified number of
   * elements without needing to allocate new internal memory. If necessary,
   * allocates new internal memory and increases the capacity of the receiver.
   * <p>
   * This method never need be called; it is for performance tuning only.
   * Calling this method before <tt>put()</tt>ing a large number of
   * associations boosts performance, because the receiver will grow only once
   * instead of potentially many times.
   * <p>
   * <b>This default implementation does nothing.</b> Override this method if
   * necessary.
   *
   * @param minCapacity
   *            the desired minimum capacity.
   */
  def ensureCapacity(minCapacity: Int) {
  }

  /**
   * Returns <tt>true</tt> if the receiver contains no (key,value)
   * associations.
   *
   * @return <tt>true</tt> if the receiver contains no (key,value)
   *         associations.
   */
  def isEmpty: Boolean = distinct == 0

  /**
   * Returns a prime number which is <code>&gt;= desiredCapacity</code> and
   * very close to <code>desiredCapacity</code> (within 11% if
   * <code>desiredCapacity &gt;= 1000</code>).
   *
   * @param desiredCapacity
   *            the capacity desired by the user.
   * @return the capacity which should be used for a hashtable.
   */
  protected def nextPrime(desiredCapacity: Int): Int = PrimeFinder.nextPrime(desiredCapacity)

  /**
   * Initializes the receiver. You will almost certainly need to override this
   * method in subclasses to initialize the hash table.
   *
   * @param initialCapacity
   *            the initial capacity of the receiver.
   * @param minLoadFactor
   *            the minLoadFactor of the receiver.
   * @param maxLoadFactor
   *            the maxLoadFactor of the receiver.
   * @throws IllegalArgumentException
   *             if
   *
   *             <tt>initialCapacity < 0 || (minLoadFactor < 0.0 || minLoadFactor >= 1.0) || (maxLoadFactor <= 0.0 || maxLoadFactor >= 1.0) || (minLoadFactor >= maxLoadFactor)</tt>
   *             .
   */
  protected def setUp(initialCapacity: Int, minLoadFactor: Double, maxLoadFactor: Double) {
    if (initialCapacity < 0) throw new IllegalArgumentException("Initial Capacity must not be less than zero: " + initialCapacity)
    if (minLoadFactor < 0.0 || minLoadFactor >= 1.0) throw new IllegalArgumentException("Illegal minLoadFactor: " + minLoadFactor)
    if (maxLoadFactor <= 0.0 || maxLoadFactor >= 1.0) throw new IllegalArgumentException("Illegal maxLoadFactor: " + maxLoadFactor)
    if (minLoadFactor >= maxLoadFactor) throw new IllegalArgumentException("Illegal minLoadFactor: " + minLoadFactor + " and maxLoadFactor: " +
      maxLoadFactor)
  }

  /**
   * Returns the number of (key,value) associations currently contained.
   *
   * @return the number of (key,value) associations currently contained.
   */
  def size: Int = distinct

  /**
   * Trims the capacity of the receiver to be the receiver's current size.
   * Releases any superfluous internal memory. An application can use this
   * operation to minimize the storage of the receiver.
   * <p>
   * This default implementation does nothing. Override this method if
   * necessary.
   */
  def trimToSize() {
  }
}
