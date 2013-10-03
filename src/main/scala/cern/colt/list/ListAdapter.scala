package cern.colt.list

/**
 * Adapter that permits an cern.colt.list.tdouble.AbstractList to
 * be viewed and treated as a JDK 1.2 java.util.AbstractList. Makes the
 * contained list compatible with the JDK 1.2 Collections Framework.
 * <p>
 * Any attempt to pass elements other than <tt>java.lang.Number</tt> to setter
 * methods will throw a <tt>java.lang.ClassCastException</tt>.
 * <tt>java.lang.Number.doubleValue()</tt> is used to convert objects into
 * primitive values which are then stored in the backing templated list. Getter
 * methods return <tt>java.lang.Double</tt> objects.
 */
class ListAdapter[T](private val content: cern.colt.list.AbstractList[T]) extends java.util.AbstractList[T] with java.util.List[T] {

  /**
   * Inserts the specified element at the specified position in this list
   * (optional operation). Shifts the element currently at that position (if
   * any) and any subsequent elements to the right (adds one to their
   * indexes).
   * <p>
   *
   * @param index
   *            index at which the specified element is to be inserted.
   * @param element
   *            element to be inserted.
   *
   * @throws ClassCastException
   *             if the class of the specified element prevents it from being
   *             added to this list.
   * @throws IllegalArgumentException
   *             if some aspect of the specified element prevents it from
   *             being added to this list.
   * @throws IndexOutOfBoundsException
   *             index is out of range (<tt>index &lt;
   * 		  0 || index &gt; size()</tt>).
   */
  override def add(index: Int, element: T) {
    content.beforeInsert(index, element)
    modCount += 1
  }

  /**
   * Returns the element at the specified position in this list.
   *
   * @param index
   *            index of element to return.
   *
   * @return the element at the specified position in this list.
   * @throws IndexOutOfBoundsException
   *             if the given index is out of range (
   *             <tt>index &lt; 0 || index &gt;= size()</tt>).
   */
  def get(index: Int): T = content.get(index)

  /**
   * Removes the element at the specified position in this list (optional
   * operation). Shifts any subsequent elements to the left (subtracts one
   * from their indexes). Returns the element that was removed from the list.
   * <p>
   *
   * @param index
   *            the index of the element to remove.
   * @return the element previously at the specified position.
   *
   * @throws IndexOutOfBoundsException
   *             if the specified index is out of range (
   *             <tt>index &lt; 0 || index &gt;= size()</tt>).
   */
  override def remove(index: Int): T = {
    val old = get(index)
    content.remove(index)
    modCount += 1
    old
  }

  /**
   * Replaces the element at the specified position in this list with the
   * specified element (optional operation).
   * <p>
   *
   * @param index
   *            index of element to replace.
   * @param element
   *            element to be stored at the specified position.
   * @return the element previously at the specified position.
   *
   * @throws ClassCastException
   *             if the class of the specified element prevents it from being
   *             added to this list.
   * @throws IllegalArgumentException
   *             if some aspect of the specified element prevents it from
   *             being added to this list.
   *
   * @throws IndexOutOfBoundsException
   *             if the specified index is out of range (
   *             <tt>index &lt; 0 || index &gt;= size()</tt>).
   */
  override def set(index: Int, element: T): T = {
    val old = get(index)
    content.set(index, element)
    old
  }

  /**
   * Returns the number of elements in this list.
   *
   * @return the number of elements in this list.
   */
  def size(): Int = content.size
}
