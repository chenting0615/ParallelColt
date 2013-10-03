package cern.colt.list.impl

import cern.colt.function.Procedure1
import cern.colt.list.AbstractList

/**
 * Resizable list holding <code>double</code> elements; implemented with arrays.
 * First see the <a href="package-summary.html">package summary</a> and javadoc
 * <a href="package-tree.html">tree view</a> to get the broad picture.
 */
@SerialVersionUID(1L)
class ArrayList[T: Manifest](elements_p: Array[T]) extends AbstractList[T] {

  /**
   * The array buffer into which the elements of the list are stored. The
   * capacity of the list is the length of this array buffer.
   */
  protected var elementsVar: Array[T] = elements_p
  /**
   * The size of the list. This is a READ_ONLY variable for all methods but
   * setSizeRaw(int newSize) !!! If you violate this principle in subclasses,
   * you should exactly know what you are doing.
   */
  protected var sizeVar: Int = elements_p.length

  /**
   * Constructs an empty list with the specified initial capacity.
   *
   * @param initialCapacity
   *            the number of elements the receiver can hold without
   *            auto-expanding itself by allocating new internal memory.
   */
  def this(initialCapacity: Int) {
    this(Array.ofDim[T](initialCapacity))
    sizeVar = 0
  }

  /**
   * Constructs an empty list.
   */
  def this() {
    this(10)
  }

  /**
   * Appends the specified element to the end of this list.
   *
   * @param element
   *            element to be appended to this list.
   */
  def add(element: T) {
    if (sizeVar == elementsVar.length) ensureCapacity(sizeVar + 1)
    elementsVar(size) = element
    sizeVar += 1
  }

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
  def beforeInsert(index: Int, element: T) {
    if (sizeVar == index) {
      add(element)
      return
    }
    if (index > sizeVar || index < 0) throw new IndexOutOfBoundsException("Index: " + index + ", Size: " + sizeVar)
    ensureCapacity(sizeVar + 1)
    System.arraycopy(elementsVar, index, elementsVar, index + 1, size - index)
    elementsVar(index) = element
    sizeVar += 1
  }

  /**
   * Returns a deep copy of the receiver.
   *
   * @return a deep copy of the receiver.
   */
  override def clone(): AnyRef = {
    val clone = new ArrayList(elementsVar.clone())
    clone.sizeVar = sizeVar
    clone
  }

  /**
   * Returns a deep copy of the receiver; uses <code>clone()</code> and casts
   * the result.
   *
   * @return a deep copy of the receiver.
   */
  def copy(): ArrayList[T] = clone().asInstanceOf[ArrayList[T]]

  /**
   * Returns the elements currently stored, including invalid elements between
   * size and capacity, if any.
   *
   * <b>WARNING:</b> For efficiency reasons and to keep memory usage low,
   * <b>the array is not copied</b>. So if subsequently you modify the
   * returned array directly via the [] operator, be sure you know what you're
   * doing.
   *
   * @return the elements currently stored.
   */
  def elements(): Array[T] = elementsVar

  /**
   * Sets the receiver's elements to be the specified array (not a copy of
   * it).
   *
   * The size and capacity of the list is the length of the array.
   * <b>WARNING:</b> For efficiency reasons and to keep memory usage low,
   * <b>the array is not copied</b>. So if subsequently you modify the
   * specified array directly via the [] operator, be sure you know what
   * you're doing.
   *
   * @param elements
   *            the new elements to be stored.
   * @return the receiver itself.
   */
  protected def setElements(elements: Array[T]) {
    this.elementsVar = elements
    this.sizeVar = elements.length
  }

  /**
   * Ensures that the receiver can hold at least the specified number of
   * elements without needing to allocate new internal memory. If necessary,
   * allocates new internal memory and increases the capacity of the receiver.
   *
   * @param minCapacity
   *            the desired minimum capacity.
   */
  def ensureCapacity(minCapacity: Int) {
    elementsVar = cern.colt.Arrays.ensureCapacity(elementsVar, minCapacity)
  }

  /**
   * Compares the specified Object with the receiver. Returns true if and only
   * if the specified Object is also an ArrayList of the same type, both Lists
   * have the same size, and all corresponding pairs of elements in the two
   * Lists are identical. In other words, two Lists are defined to be equal if
   * they contain the same elements in the same order.
   *
   * @param otherObj
   *            the Object to be compared for equality with the receiver.
   * @return true if the specified Object is equal to the receiver.
   */
  override def equals(otherObj: Any): Boolean = {
    if (otherObj == null) return false
    if (this == otherObj) return true
    if (!otherObj.isInstanceOf[ArrayList[T]]) return super.equals(otherObj)
    val other = otherObj.asInstanceOf[ArrayList[T]]
    if (sizeVar != other.sizeVar) return false
    for(i <- 0 until sizeVar) {
      if (elementsVar(i) != other.elementsVar(i)) return false
    }
    true
  }

  /**
   * Applies a procedure to each element of the receiver, if any. Starts at
   * index 0, moving rightwards.
   *
   * @param procedure
   *            the procedure to be applied. Stops iteration if the procedure
   *            returns <tt>false</tt>, otherwise continues.
   * @return <tt>false</tt> if the procedure stopped before all elements where
   *         iterated over, <tt>true</tt> otherwise.
   */
  def forEach(procedure: Procedure1[T]): Boolean = {
    for(i <- 0 until sizeVar) if (!procedure.apply(elementsVar(i))) return false
    true
  }

  /**
   * Returns the element at the specified position in the receiver.
   *
   * @param index
   *            index of element to return.
   * @throws IndexOutOfBoundsException
   *                index is out of range (index &lt; 0 || index &gt;=
   *                size()).
   */
  override def get(index: Int): T = {
    if (index >= sizeVar || index < 0) throw new IndexOutOfBoundsException("Index: " + index + ", Size: " + sizeVar)
    elementsVar(index)
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
  def getQuick(index: Int): T = elementsVar(index)

  /**
   * Removes from the receiver all elements that are contained in the
   * specified list. Tests for identity.
   *
   * @param other
   *            the other list.
   * @return <code>true</code> if the receiver changed as a result of the
   *         call.
   */
  def removeAll(other: AbstractList[T]): Boolean = {
    if (other.size == 0) return false

    var j = 0
    other match {
      case dbl: ArrayList[T] => {
        for (i <- 0 until sizeVar) {
          if (other.indexOf(elementsVar(i)) < 0) {
            elementsVar(j) = elementsVar(i)
            j += 1
          }
        }
      }
      case _ => {
        for (i <- 0 until sizeVar) {
          val value = getQuick(i)
          if (other.indexOf(value) < 0) {
            elementsVar(j) = value
            j += 1
          }
        }
      }
    }
    val modified = j != sizeVar
    setSize(j)
    modified
  }

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
  def retainAll(other: AbstractList[T]): Boolean = {
    if (other.size == 0) return false

    var j = 0
    other match {
      case dbl: ArrayList[T] => {
        for (i <- 0 until sizeVar) {
          if (other.indexOf(elementsVar(i)) >= 0) {
            elementsVar(j) = elementsVar(i)
            j += 1
          }
        }
      }
      case _ => {
        for (i <- 0 until sizeVar) {
          val value = getQuick(i)
          if (other.indexOf(value) >= 0) {
            elementsVar(j) = value
            j += 1
          }
        }
      }
    }
    val modified = j != sizeVar
    setSize(j)
    modified
  }

  /**
   * Reverses the elements of the receiver. Last becomes first, second last
   * becomes second first, and so on.
   */
  def reverse() {
    val limit = size / 2
    var j = size - 1
    var i = 0
    while (i < limit) {
      val tmp = elementsVar(i)
      elementsVar(i) = elementsVar(j)
      elementsVar(j) = tmp
      i += 1
      j -= 1
    }
  }

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
  override def set(index: Int, element: T) {
    if (index >= sizeVar || index < 0) throw new IndexOutOfBoundsException("Index: " + index + ", Size: " + sizeVar)
    elementsVar(index) = element
  }

  /**
   * Replaces the element at the specified position in the receiver with the
   * specified element; <b>WARNING:</b> Does not check preconditions. Provided
   * with invalid parameters this method may access invalid indexes without
   * throwing any exception! <b>You should only use this method when you are
   * absolutely sure that the index is within bounds.</b> Precondition
   * (unchecked): <tt>index &gt;= 0 && index &lt; size()</tt>.
   *
   * @param index
   *            index of element to replace.
   * @param element
   *            element to be stored at the specified position.
   */
  def setQuick(index: Int, element: T) {
    elementsVar(index) = element
  }

  /**
   * Trims the capacity of the receiver to be the receiver's current size.
   * Releases any superfluos internal memory. An application can use this
   * operation to minimize the storage of the receiver.
   */
  override def trimToSize() {
    elementsVar = cern.colt.Arrays.trimToCapacity(elementsVar, sizeVar)
  }

  /**
   * Appends all elements of the specified list to the receiver.
   *
   * @param other
   *            the list of which all elements shall be appended.
   */
  def addAllOf(other: AbstractList[T]) {
    beforeInsertAllOf(sizeVar, other)
  }

  /**
   * Inserts the part of the specified list between <code>otherFrom</code>
   * (inclusive) and <code>otherTo</code> (inclusive) before the specified
   * position into the receiver. Shifts the element currently at that position
   * (if any) and any subsequent elements to the right.
   *
   * @param index
   *            index before which to insert first element from the specified
   *            list (must be in [0,size])..
   * @param other
   *            list of which a part is to be inserted into the receiver.
   * @throws IndexOutOfBoundsException
   *                index is out of range (
   *                <tt>other.size()&gt;0 && (from&lt;0 || from&gt;to || to&gt;=other.size())</tt>
   *                ).
   * @throws IndexOutOfBoundsException
   *                index is out of range (
   *                <tt>index &lt; 0 || index &gt; size()</tt>).
   */
  def beforeInsertAllOf(index: Int, other: AbstractList[T]) {
    val length = other.size
    this.beforeInsertDummies(index, length)
    this.replaceFromToWithFrom(index, index+length, other, 0)
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
  protected def beforeInsertDummies(index: Int, length: Int) {
    if (index > sizeVar || index < 0) throw new IndexOutOfBoundsException("Index: " + index + ", Size: " + sizeVar)
    if (length > 0) {
      ensureCapacity(sizeVar + length)
      sizeVar += length
      replaceFromToWithFrom(index + length, sizeVar, this, index)
    }
  }

  /**
   * Returns true if the receiver contains the specified element.
   *
   * @param elem
   *            element whose presence in the receiver is to be tested.
   */
  def contains(elem: T): Boolean = indexOf(elem) >= 0

  /**
   * Deletes the first element from the receiver that is identical to the
   * specified element. Does nothing, if no such matching element is
   * contained.
   *
   * @param element
   *            the element to be deleted.
   */
  def delete(element: T) {
    val index = indexOf(element)
    if (index >= 0) remove(index)
  }

  /**
   * Sets the specified range of elements in the specified array to the
   * specified value.
   *
   * @param from
   *            the index of the first element (inclusive) to be filled with
   *            the specified value.
   * @param to
   *            the index of the last element (inclusive) to be filled with
   *            the specified value.
   * @param value
   *            the value to be stored in the specified elements of the
   *            receiver.
   */
  def fillFromToWith(from: Int, to: Int, value: T) {
    checkRangeFromTo(from, to)
    for(i <- from until to+1) setQuick(i, value)
  }

  /**
   * Returns the index of the first occurrence of the specified element.
   * Returns <code>-1</code> if the receiver does not contain this element.
   *
   * @param element
   *            the element to be searched for.
   * @return the index of the first occurrence of the element in the receiver;
   *         returns <code>-1</code> if the element is not found.
   */
  def indexOf(element: T): Int = {
    for(i <- 0 until sizeVar) {
      if (element == elementsVar(i)) return i
    }
    -1
  }

  /**
   * Returns the index of the last occurrence of the specified element.
   * Returns <code>-1</code> if the receiver does not contain this element.
   *
   * @param element
   *            the element to be searched for.
   * @return the index of the last occurrence of the element in the receiver;
   *         returns <code>-1</code> if the element is not found.
   */
  def lastIndexOf(element: T): Int = {
    for(i <- sizeVar-1 to 0 by -1) {
      if (element == elementsVar(i)) return i
    }
    -1
  }

  /**
   * Removes from the receiver all elements whose index is between
   * <code>from</code>, inclusive and <code>to</code>, inclusive. Shifts any
   * succeeding elements to the left (reduces their index). This call shortens
   * the list by <tt>(to - from + 1)</tt> elements.
   *
   * @param from
   *            index of first element to be removed.
   * @param to
   *            index after last element to be removed (exclusive).
   * @throws IndexOutOfBoundsException
   *                index is out of range (
   *                <tt>size()&gt;0 && (from&lt;0 || from&gt;to || to&gt;size)</tt>
   *                ).
   */
  def removeFromTo(from: Int, to: Int) {
    checkRangeFromTo(from, to-1)
    val numMoved = sizeVar - to
    if (numMoved > 0) {
      replaceFromToWithFrom(from, from + numMoved, this, to)
      sizeVar -= numMoved
    }
  }

  /**
   * Replaces a number of elements in the receiver with the same number of
   * elements of another list. Replaces elements in the receiver, between
   * <code>from</code> (inclusive) and <code>to</code> (inclusive), with
   * elements of <code>other</code>, starting from <code>otherFrom</code>
   * (inclusive).
   *
   * @param from_p
   *            the position of the first element to be replaced in the
   *            receiver
   * @param to_p
   *            the position of the last element to be replaced in the
   *            receiver
   * @param other
   *            list holding elements to be copied into the receiver.
   * @param otherFrom_p
   *            position of first element within other list to be copied.
   */
  def replaceFromToWithFrom(from_p: Int, to_p: Int, other: AbstractList[T], otherFrom_p: Int) {
    checkRangeFromTo(from_p, to_p)

    var from = from_p
    var otherFrom = otherFrom_p
    var to = to_p

    val length = to - from
    other.checkRangeFromTo(otherFrom, otherFrom+length-1)

    if (length > 0) {
      if (from <= otherFrom) {
        while (length >= 0) {setQuick(from, other.getQuick(otherFrom)); from += 1; otherFrom += 1}
      }
      else {
        var otherTo = otherFrom + length - 1
        while (length >= 0) {setQuick(to, other.getQuick(otherTo)); to -= 1; otherTo -= 1}
      }
    }
  }

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
   *                index is out of range (index &lt; 0 || index &gt;=
   *                size()).
   */
  def replaceFromWith(from: Int, other: java.util.Collection[T]) {
    checkRange(from)
    val e = other.iterator()
    val limit = Math.min(sizeVar - from, other.size)
    for (i <- from until limit) setQuick(i, e.next())
  }

  /**
   * Returns the number of elements contained in the receiver.
   *
   * @return the number of elements contained in the receiver.
   */
  def size: Int = sizeVar

  /**
   * Returns a <code>java.util.ArrayList</code> containing all the elements in
   * the receiver.
   */
  def toList: java.util.ArrayList[T] = {
    val list = new java.util.ArrayList[T](sizeVar)
    for (i <- 0 until sizeVar) list.add(elementsVar(i))
    list
  }

  /**
   * Returns a string representation of the receiver, containing the String
   * representation of each element.
   */
  override def toString: String = {
    cern.colt.Arrays.toString(elementsVar, sizeVar)
  }
}
