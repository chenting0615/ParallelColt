package cern.colt.list

/**
 * Abstract base class for resizable lists holding objects or primitive data
 * types such as <code>int</code>, <code>float</code>, etc. First see the <a
 * href="package-summary.html">package summary</a> and javadoc <a
 * href="package-tree.html">tree view</a> to get the broad picture.
 * <p>
 * <b>Note that this implementation is not synchronized.</b>
 *
 * @author wolfgang.hoschek@cern.ch
 * @version 1.0, 09/24/99
 * @see java.util.ArrayList
 * @see java.util.Vector
 * @see java.util.Arrays
 */
@specialized
@SerialVersionUID(1L)
abstract class AbstractList[@specialized T: Manifest] extends AbstractCollection[T] {

  /**
   * Checks if the given index is in range.
   */
  def checkRange(index: Int) {
    if (index >= size || index < 0) throw new IndexOutOfBoundsException("Index: " + index + ", Size: " + size)
  }

  /**
   * Checks if the given range is within the contained array's bounds.
   *
   * @throws IndexOutOfBoundsException
   *             if
   *             <tt>to!=from-1 || from&lt;0 || from&gt;to || to&gt;=size()</tt>
   *             .
   */
  def checkRangeFromTo(from: Int, to: Int) {
    if (to == from - 1) return
    if (from < 0 || from > to || to >= size) throw new IndexOutOfBoundsException("from: " + from + ", to: " + to + ", size=" + size)
  }

  /**
   * Appends the specified element to the end of this list.
   *
   * @param element
   *            element to be appended to this list.
   */
  def add(element: T)

  /**
   * Appends all of the elements of the specified Collection to the receiver.
   *
   * @throws ClassCastException
   *                if an element in the collection is not of the same
   *                parameter type of the receiver.
   */
  def addAllOf(collection: java.util.Collection[T]) {
    this.beforeInsertAllOf(size, collection)
  }

  /**
   * Inserts all elements of the specified collection before the specified
   * position into the receiver. Shifts the element currently at that position
   * (if any) and any subsequent elements to the right (increases their
   * indexes).
   *
   * @param index
   *            index before which to insert first element from the specified
   *            collection.
   * @param collection
   *            the collection to be inserted
   * @throws ClassCastException
   *                if an element in the collection is not of the same
   *                parameter type of the receiver.
   * @throws IndexOutOfBoundsException
   *             if <tt>index &lt; 0 || index &gt; size()</tt>.
   */
  def beforeInsertAllOf(index: Int, collection: java.util.Collection[T]) {
    this.beforeInsertDummies(index, collection.size)
    this.replaceFromWith(index, collection)
  }

  /**
   * Inserts <tt>length</tt> dummy elements before the specified position into
   * the receiver. Shifts the element currently at that position (if any) and
   * any subsequent elements to the right. <b>This method must set the new
   * size to be <tt>size()+length</tt>.
   *
   * @param index
   *            index before which to insert dummy elements (must be in
   *            [0,size])..
   * @param length
   *            number of dummy elements to be inserted.
   * @throws IndexOutOfBoundsException
   *             if <tt>index &lt; 0 || index &gt; size()</tt>.
   */
  protected def beforeInsertDummies(index: Int, length: Int): Unit

  /**
   * Removes all elements from the receiver. The receiver will be empty after
   * this call returns, but keep its current capacity.
   */
  def clear() {
    removeFromTo(0, size)
  }

  /**
   * Removes the element at the specified position from the receiver. Shifts
   * any subsequent elements to the left.
   *
   * @param index
   *            the index of the element to removed.
   * @throws IndexOutOfBoundsException
   *             if <tt>index &lt; 0 || index &gt;= size()</tt>.
   */
  def remove(index: Int) {
    removeFromTo(index, index)
  }

  /**
   * Removes from the receiver all elements whose index is between
   * <code>from</code>, inclusive and <code>to</code>, inclusive. Shifts any
   * succeeding elements to the left (reduces their index). This call shortens
   * the list by <tt>(to - from + 1)</tt> elements.
   *
   * @param fromIndex
   *            index of first element to be removed.
   * @param toIndex
   *            index of last element to be removed.
   * @throws IndexOutOfBoundsException
   *             if
   *             <tt>(from&lt;0 || from&gt;to || to&gt;=size()) && to!=from-1</tt>
   *             .
   */
  def removeFromTo(fromIndex: Int, toIndex: Int): Unit

  /**
   * Replaces the part of the receiver starting at <code>from</code>
   * (inclusive) with all the elements of the specified collection. Does not
   * alter the size of the receiver. Replaces exactly
   * <tt>Math.max(0,Math.min(size()-from, other.size()))</tt> elements.
   *
   * @param from
   *            the index at which to copy the first element from the
   *            specified collection.
   * @param other
   *            Collection to replace part of the receiver
   * @throws IndexOutOfBoundsException
   *             if <tt>index &lt; 0 || index &gt;= size()</tt>.
   */
  def replaceFromWith(from: Int, other: java.util.Collection[T]): Unit

  /**
   * Reverses the elements of the receiver. Last becomes first, second last
   * becomes second first, and so on.
   */
  def reverse(): Unit

  /**
   * Sets the size of the receiver. If the new size is greater than the
   * current size, new null or zero items are added to the end of the
   * receiver. If the new size is less than the current size, all components
   * at index newSize and greater are discarded. This method does not release
   * any superfluos internal memory. Use method <tt>trimToSize</tt> to release
   * superfluos internal memory.
   *
   * @param newSize
   *            the new size of the receiver.
   * @throws IndexOutOfBoundsException
   *             if <tt>newSize &lt; 0</tt>.
   */
  def setSize(newSize: Int) {
    if (newSize < 0) throw new IndexOutOfBoundsException("newSize:" + newSize)
    val currentSize = size
    if (newSize != currentSize) {
      if (newSize > currentSize)
        beforeInsertDummies(currentSize, newSize - currentSize)
      else
        removeFromTo(newSize, currentSize)
    }
  }

  /**
   * Trims the capacity of the receiver to be the receiver's current size.
   * Releases any superfluos internal memory. An application can use this
   * operation to minimize the storage of the receiver.
   * <p>
   * This default implementation does nothing. Override this method in space
   * efficient implementations.
   */
  def trimToSize() {
  }

  def indexOf(elem: T): Int

  /**
   * Returns the element at the specified position in the receiver.
   *
   * @param index
   *            index of element to return.
   * @throws IndexOutOfBoundsException
   *                index is out of range (index &lt; 0 || index &gt;=
   *                size()).
   */
  def get(index: Int): T = {
    if (index >= size || index < 0) throw new IndexOutOfBoundsException("Index: " + index + ", Size: " + size)
    getQuick(index)
  }

  /**
   * Returns the element at the specified position in the receiver;
   * <b>WARNING:</b> Does not check preconditions. Provided with invalid
   * parameters this method may return invalid elements without throwing any
   * exception! <b>You should only use this method when you are absolutely
   * sure that the index is within bounds.</b> Precondition (unchecked):
   * <tt>index &gt;= 0 && index &lt; size()</tt>.
   *
   * @param index
   *            index of element to return.
   */
  def getQuick(index: Int): T

  /**
   * Replaces the element at the specified position in the receiver with the
   * specified element.
   *
   * @param index
   *            index of element to replace.
   * @param element
   *            element to be stored at the specified position.
   * @throws IndexOutOfBoundsException
   *             if <tt>index &lt; 0 || index &gt;= size()</tt>.
   */
  def set(index: Int, element: T) {
    if (index >= size || index < 0) throw new IndexOutOfBoundsException("Index: " + index + ", Size: " + size)
    setQuick(index, element)
  }

  /**
   * Replaces the element at the specified position in the receiver with the
   * specified element; <b>WARNING:</b> Does not check preconditions. Provided
   * with invalid parameters this method may access invalid indexes without
   * throwing any exception! <b>You should only use this method when you are
   * absolutely sure that the index is within bounds.</b> Precondition
   * (unchecked): <tt>index &gt;= 0 && index &lt; size()</tt>.
   *
   * This method is normally only used internally in large loops where bounds
   * are explicitly checked before the loop and need no be rechecked within
   * the loop. However, when desperately, you can give this method
   * <tt>public</tt> visibility in subclasses.
   *
   * @param index
   *            index of element to replace.
   * @param element
   *            element to be stored at the specified position.
   */
  def setQuick(index: Int, element: T): Unit

  /**
   * Retains (keeps) only the elements in the receiver that are contained in
   * the specified other list. In other words, removes from the receiver all
   * of its elements that are not contained in the specified other list.
   *
   * @param other
   *            the other list to test against.
   * @return <code>true</code> if the receiver changed as a result of the
   *         call.
   */
  def retainAll(other: AbstractList[T]): Boolean

  /**
   * Inserts the specified element before the specified position into the
   * receiver. Shifts the element currently at that position (if any) and any
   * subsequent elements to the right.
   *
   * @param index
   *            index before which the specified element is to be inserted
   *            (must be in [0,size]).
   * @param element
   *            element to be inserted.
   * @throws IndexOutOfBoundsException
   *                index is out of range (
   *                <tt>index &lt; 0 || index &gt; size</tt>).
   */
  def beforeInsert(index: Int, element: T)
  /**
   * Ensures that the receiver can hold at least the specified number of
   * elements without needing to allocate new internal memory. If necessary,
   * allocates new internal memory and increases the capacity of the receiver.
   *
   * @param minCapacity
   *            the desired minimum capacity.
   */
  def ensureCapacity(minCapacity: Int)

}
