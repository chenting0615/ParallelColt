package cern.colt.map.impl

import cern.colt.list.impl.ArrayList
import cern.colt.map.HashFunctions
import cern.colt.map.PrimeFinder
import cern.colt.function.{Procedure2, Procedure1}
import cern.colt.list.AbstractList

object OpenHashMap {

  protected val defaultCapacity = 277

  protected val defaultMinLoadFactor = 0.2

  protected val defaultMaxLoadFactor = 0.5
}

/**
 * Hash map holding (key,value) associations of type <tt>(int-->double)</tt>;
 * Automatically grows and shrinks as needed; Implemented using open addressing
 * with double hashing. First see the <a href="package-summary.html">package
 * summary</a> and javadoc <a href="package-tree.html">tree view</a> to get the
 * broad picture.
 *
 * Overrides many methods for performance reasons only.
 *
 * @author wolfgang.hoschek@cern.ch
 * @version 1.0, 09/24/99
 * @see java.util.HashMap
 */
@specialized
@SerialVersionUID(1L)
class OpenHashMap[K: Manifest, V: Manifest](initialCapacity: Int, protected var minLoadFactor: Double, protected var maxLoadFactor: Double) extends AbstractTypedMap[K, V] {

  protected val FREE: Byte = 0
  protected val FULL: Byte = 1
  protected val REMOVED: Byte = 2

  /**
   * The hash table keys.
   */
  protected var table: Array[K] = _

  /**
   * The hash table values.
   */
  protected var valuesVar: Array[V] = _

  /**
   * The state of each hash table entry (FREE, FULL, REMOVED).
   */
  protected var state: Array[Byte] = _

  /**
   * The number of table entries in state==FREE.
   */
  protected var freeEntries: Int = _

  setUp()

  /**
   * Constructs an empty map with the specified initial capacity and default
   * load factors.
   *
   * @param initialCapacity
   *            the initial capacity of the map.
   * @throws IllegalArgumentException
   *             if the initial capacity is less than zero.
   */
  def this(initialCapacity: Int) {
    this(initialCapacity, OpenHashMap.defaultMinLoadFactor, OpenHashMap.defaultMaxLoadFactor)
  }

  /**
   * Constructs an empty map with default capacity and default load factors.
   */
  def this() {
    this(OpenHashMap.defaultCapacity)
  }

  /**
   * Clears the receiver, then adds all (key,value) pairs of <tt>other</tt>
   * valuesVar to it.
   *
   * @param other
   *            the other map to be copied into the receiver.
   */
  override def assign(other: AbstractTypedMap[K, V]) {
    if (!other.isInstanceOf[OpenHashMap[K, V]]) {
      super.assign(other)
      return
    }
    val source = other.asInstanceOf[OpenHashMap[K, V]]
    val copy = source.copy().asInstanceOf[OpenHashMap[K, V]]
    this.valuesVar = copy.valuesVar
    this.table = copy.table
    this.state = copy.state
    this.freeEntries = copy.freeEntries
    this.distinct = copy.distinct
    this.lowWaterMark = copy.lowWaterMark
    this.highWaterMark = copy.highWaterMark
    this.minLoadFactor = copy.minLoadFactor
    this.maxLoadFactor = copy.maxLoadFactor
  }

  /**
   * Removes all (key,value) associations from the receiver. Implicitly calls
   * <tt>trimToSize()</tt>.
   */
  def clear() {
    new ArrayList[Byte](this.state).fillFromToWith(0, this.state.length - 1, FREE)
    this.distinct = 0
    this.freeEntries = table.length
    trimToSize()
  }

  /**
   * Returns a deep copy of the receiver.
   *
   * @return a deep copy of the receiver.
   */
  override def clone(): AnyRef = {
    val copy = super.clone().asInstanceOf[OpenHashMap[K, V]]
    copy.table = copy.table.clone()
    copy.valuesVar = copy.valuesVar.clone()
    copy.state = copy.state.clone()
    copy
  }

  /**
   * Returns <tt>true</tt> if the receiver contains the specified key.
   *
   * @return <tt>true</tt> if the receiver contains the specified key.
   */
  override def containsKey(key: K): Boolean = indexOfKey(key) >= 0

  /**
   * Returns <tt>true</tt> if the receiver contains the specified value.
   *
   * @return <tt>true</tt> if the receiver contains the specified value.
   */
  override def containsValue(value: V): Boolean = indexOfValue(value) >= 0

  /**
   * Ensures that the receiver can hold at least the specified number of
   * associations without needing to allocate new internal memory. If
   * necessary, allocates new internal memory and increases the capacity of
   * the receiver.
   * <p>
   * This method never need be called; it is for performance tuning only.
   * Calling this method before <tt>put()</tt>ing a large number of
   * associations boosts performance, because the receiver will grow only once
   * instead of potentially many times and hash collisions get less probable.
   *
   * @param minCapacity
   *            the desired minimum capacity.
   */
  override def ensureCapacity(minCapacity: Int) {
    if (table.length < minCapacity) {
      val newCapacity = nextPrime(minCapacity)
      rehash(newCapacity)
    }
  }

  /**
   * Applies a procedure to each key of the receiver, if any. Note: Iterates
   * over the keys in no particular order. Subclasses can define a particular
   * order, for example, "sorted by key". All methods which <i>can</i> be
   * expressed in terms of this method (most methods can) <i>must
   * guarantee</i> to use the <i>same</i> order defined by this method, even
   * if it is no particular order. This is necessary so that, for example,
   * methods <tt>keys</tt> and <tt>values</tt> will yield association pairs,
   * not two uncorrelated lists.
   *
   * @param procedure
   *            the procedure to be applied. Stops iteration if the procedure
   *            returns <tt>false</tt>, otherwise continues.
   * @return <tt>false</tt> if the procedure stopped before all keys where
   *         iterated over, <tt>true</tt> otherwise.
   */
  def forEachKey(procedure: Procedure1[K]): Boolean = {
    for(i <- 0 until table.length) {
      if (state(i) == FULL) if (!procedure.apply(table(i))) return false
    }
    true
  }

  /**
   * Applies a procedure to each (key,value) pair of the receiver, if any.
   * Iteration order is guaranteed to be <i>identical</i> to the order used by
   * method forEachKey(LongProcedure).
   *
   * @param procedure
   *            the procedure to be applied. Stops iteration if the procedure
   *            returns <tt>false</tt>, otherwise continues.
   * @return <tt>false</tt> if the procedure stopped before all keys where
   *         iterated over, <tt>true</tt> otherwise.
   */
  override def forEachPair(procedure: Procedure2[K, V]): Boolean = {
    for(i <- 0 until table.length) {
      if (state(i) == FULL) if (!procedure.apply(table(i), valuesVar(i))) return false
    }
    true
  }

  /**
   * Returns the value associated with the specified key. It is often a good
   * idea to first check with containsKey(long) whether the given key
   * has a value associated or not, i.e. whether there exists an association
   * for the given key or not.
   *
   * @param key
   *            the key to be searched for.
   * @return the value associated with the specified key; <tt>0</tt> if no
   *         such key is present.
   */
  def get(key: K): V = {
    val i = indexOfKey(key)
    if (i < 0) return 0.asInstanceOf[V]
    valuesVar(i)
  }

  /**
   * @param key
   *            the key to be added to the receiver.
   * @return the index where the key would need to be inserted, if it is not
   *         already contained. Returns -index-1 if the key is already
   *         contained at slot index. Therefore, if the returned index < 0,
   *         then it is already contained at slot -index-1. If the returned
   *         index >= 0, then it is NOT already contained and should be
   *         inserted at slot index.
   */
  protected def indexOfInsertion(key: K): Int = {
    val tab = table
    val stat = state
    val length = tab.length
    // TODO: Fix this
    val hash = HashFunctions.hash(key.asInstanceOf[Long]) & 0x7FFFFFFF
    var i = hash % length
    var decrement = hash % (length - 2)
    if (decrement == 0) decrement = 1
    while (stat(i) == FULL && tab(i) != key) {
      i -= decrement
      if (i < 0) i += length
    }
    if (stat(i) == REMOVED) {
      val j = i
      while (stat(i) != FREE && (stat(i) == REMOVED || tab(i) != key)) {
        i -= decrement
        if (i < 0) i += length
      }
      if (stat(i) == FREE) i = j
    }
    if (stat(i) == FULL) {
      return -i - 1
    }
    i
  }

  /**
   * @param key
   *            the key to be searched in the receiver.
   * @return the index where the key is contained in the receiver, else
   *         returns -1.
   */
  protected def indexOfKey(key: K): Int = {
    val tab = table
    val stat = state
    val length = tab.length
    // TODO: Fix this
    val hash = HashFunctions.hash(key.asInstanceOf[Long]) & 0x7FFFFFFF
    var i = hash % length
    var decrement = hash % (length - 2)
    if (decrement == 0) decrement = 1
    while (stat(i) != FREE && (stat(i) == REMOVED || tab(i) != key)) {
      i -= decrement
      if (i < 0) i += length
    }
    if (stat(i) == FREE) return -1
    i
  }

  /**
   * @param value
   *            the value to be searched in the receiver.
   * @return the index where the value is contained in the receiver, returns
   *         -1 if the value was not found.
   */
  protected def indexOfValue(value: V): Int = {
    for(i <- 0 until state.length) {
      if (state(i) == FULL && valuesVar(i) == value) return i
    }
    -1
  }

  /**
   * Returns the first key the given value is associated with. It is often a
   * good idea to first check with containsValue(V) whether
   * there exists an association from a key to this value. Search order is
   * guaranteed to be <i>identical</i> to the order used by method
   * forEachKey().
   *
   * @param value
   *            the value to search for.
   * @return the first key for which holds <tt>get(key) == value</tt>; returns
   *         <tt>Long.MIN_VALUE</tt> if no such key exists.
   */
  override def keyOf(value: V): K = {
    val i = indexOfValue(value)
    if (i < 0) return (-1).asInstanceOf[K]
    table(i)
  }

  /**
   * Fills all keys contained in the receiver into the specified list. Fills
   * the list, starting at index 0. After this call returns the specified list
   * has a new size that equals <tt>this.size()</tt>. Iteration order is
   * guaranteed to be <i>identical</i> to the order used by method
   * forEachKey().
   * <p>
   * This method can be used to iterate over the keys of the receiver.
   *
   * @param list
   *            the list to be filled, can have any size.
   */
  override def keys(list: AbstractList[K]) {
    list.clear()
    list.ensureCapacity(distinct)
    for(i <- 0 until table.length) {
      if (state(i) == FULL) list.add(table(i))
    }
  }

  /**
   * Fills all pairs satisfying a given condition into the specified lists.
   * Fills into the lists, starting at index 0. After this call returns the
   * specified lists both have a new size, the number of pairs satisfying the
   * condition. Iteration order is guaranteed to be <i>identical</i> to the
   * order used by method forEachKey().
   * <p>
   * <b>Example:</b> <br>
   *
   * <pre>
   *   LongDoubleProcedure condition = new LongDoubleProcedure() { // match even keys only
   *   public boolean apply(long key, double value) { return key%2==0; }
   *   }
   *   keys = (8,7,6), values = (1,2,2) --&gt; keyList = (6,8), valueList = (2,1)
   * &lt;/tt&gt;
   * </pre>
   *
   * @param condition
   *            the condition to be matched. Takes the current key as first
   *            and the current value as second argument.
   * @param keyList
   *            the list to be filled with keys, can have any size.
   * @param valueList
   *            the list to be filled with values, can have any size.
   */
  override def pairsMatching(condition: Procedure2[K, V], keyList: AbstractList[K], valueList: AbstractList[V]) {
    keyList.clear()
    valueList.clear()
    for(i <- 0 until table.length) {
      if (state(i) == FULL && condition.apply(table(i), valuesVar(i))) {
        keyList.add(table(i))
        valueList.add(valuesVar(i))
      }
    }
  }

  /**
   * Associates the given key with the given value. Replaces any old
   * <tt>(key,someOtherValue)</tt> association, if existing.
   *
   * @param key
   *            the key the value shall be associated with.
   * @param value
   *            the value to be associated.
   * @return <tt>true</tt> if the receiver did not already contain such a key;
   *         <tt>false</tt> if the receiver did already contain such a key -
   *         the new value has now replaced the formerly associated value.
   */
  def put(key: K, value: V): Boolean = {
    var i = indexOfInsertion(key)
    if (i < 0) {
      i = -i - 1
      this.valuesVar(i) = value
      return false
    }
    if (this.distinct > this.highWaterMark) {
      val newCapacity = chooseGrowCapacity(this.distinct + 1, this.minLoadFactor, this.maxLoadFactor)
      rehash(newCapacity)
      return put(key, value)
    }
    this.table(i) = key
    this.valuesVar(i) = value
    if (this.state(i) == FREE) this.freeEntries -= 1
    this.state(i) = FULL
    this.distinct += 1
    if (this.freeEntries < 1) {
      val newCapacity = chooseGrowCapacity(this.distinct + 1, this.minLoadFactor, this.maxLoadFactor)
      rehash(newCapacity)
    }
    true
  }

  /**
   * Rehashes the contents of the receiver into a new table with a smaller or
   * larger capacity. This method is called automatically when the number of
   * keys in the receiver exceeds the high water mark or falls below the low
   * water mark.
   */
  protected def rehash(newCapacity: Int) {
    val oldCapacity = table.length
    if (newCapacity <= this.distinct) throw new InternalError()
    val oldTable = table
    val oldValues = valuesVar
    val oldState = state
    val newTable = Array.ofDim[K](newCapacity)
    val newValues = Array.ofDim[V](newCapacity)
    val newState = Array.ofDim[Byte](newCapacity)
    this.lowWaterMark = chooseLowWaterMark(newCapacity, this.minLoadFactor)
    this.highWaterMark = chooseHighWaterMark(newCapacity, this.maxLoadFactor)
    this.table = newTable
    this.valuesVar = newValues
    this.state = newState
    this.freeEntries = newCapacity - this.distinct
    var i = oldCapacity
    while (i > 0) {
      i -= 1
      if (oldState(i) == FULL) {
        val element = oldTable(i)
        val index = indexOfInsertion(element)
        newTable(index) = element
        newValues(index) = oldValues(i)
        newState(index) = FULL
      }
    }
  }

  /**
   * Removes the given key with its associated element from the receiver, if
   * present.
   *
   * @param key
   *            the key to be removed from the receiver.
   * @return <tt>true</tt> if the receiver contained the specified key,
   *         <tt>false</tt> otherwise.
   */
  def remove(key: K): Boolean = {
    val i = indexOfKey(key)
    if (i < 0) return false
    this.state(i) = REMOVED
    this.distinct -= 1
    if (this.distinct < this.lowWaterMark) {
      val newCapacity = chooseShrinkCapacity(this.distinct, this.minLoadFactor, this.maxLoadFactor)
      rehash(newCapacity)
    }
    true
  }

  /**
   * Initializes the receiver.
   * @throws IllegalArgumentException
   *             if
   *
   *             <tt>initialCapacity < 0 || (minLoadFactor < 0.0 || minLoadFactor >= 1.0) || (maxLoadFactor <= 0.0 || maxLoadFactor >= 1.0) || (minLoadFactor >= maxLoadFactor)</tt>
   *             .
   */
  protected def setUp() {
    var capacity = this.initialCapacity
    super.setUp(capacity, minLoadFactor, maxLoadFactor)
    capacity = nextPrime(capacity)
    if (capacity == 0) capacity = 1
    this.table = Array.ofDim[K](capacity)
    this.valuesVar = Array.ofDim[V](capacity)
    this.state = Array.ofDim[Byte](capacity)
    if (capacity == PrimeFinder.largestPrime)
      this.maxLoadFactor = 1.0
    this.distinct = 0
    this.freeEntries = capacity
    this.lowWaterMark = 0
    this.highWaterMark = chooseHighWaterMark(capacity, this.maxLoadFactor)
  }

  /**
   * Trims the capacity of the receiver to be the receiver's current size.
   * Releases any superfluous internal memory. An application can use this
   * operation to minimize the storage of the receiver.
   */
  override def trimToSize() {
    val newCapacity = nextPrime((1 + 1.2 * size).toInt)
    if (table.length > newCapacity) {
      rehash(newCapacity)
    }
  }

  /**
   * Fills all values contained in the receiver into the specified list. Fills
   * the list, starting at index 0. After this call returns the specified list
   * has a new size that equals <tt>this.size()</tt>. Iteration order is
   * guaranteed to be <i>identical</i> to the order used by method
   * forEachKey(LongProcedure).
   * <p>
   * This method can be used to iterate over the values of the receiver.
   *
   * @param list
   *            the list to be filled, can have any size.
   */
  override def values(list: AbstractList[V]) {
    list.clear()
    list.ensureCapacity(distinct)
    for(i <- 0 until table.length) {
      if (state(i) == FULL) list.add(valuesVar(i))
    }
  }
}
