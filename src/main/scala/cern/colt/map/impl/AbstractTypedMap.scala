package cern.colt.map.impl

import cern.colt.list.impl.ArrayList
import cern.colt.map.AbstractMap
import cern.colt.function.{Procedure1, Procedure2}
import cern.colt.list.AbstractList

/**
 * Abstract base class for hash maps holding (key,value) associations of type
 * <tt>(int-->double)</tt>. First see the <a href="package-summary.html">package
 * summary</a> and javadoc <a href="package-tree.html">tree view</a> to get the
 * broad picture.
 * <p>
 * <b>Implementation</b>:
 * <p>
 * Almost all methods are expressed in terms of
 * forEachKey(Procedure1[T]). As such they are fully functional, but
 * inefficient. Override them in subclasses if necessary.
 *
 * @author wolfgang.hoschek@cern.ch
 * @version 1.0, 09/24/99
 * @see java.util.HashMap
 */
@SerialVersionUID(1L)
abstract class AbstractTypedMap[K: Manifest, V: Manifest] extends AbstractMap {

  /**
   * Assigns the result of a function to each value;
   * <tt>v[i] = function(v[i])</tt>.
   *
   * @param function
   *            a function object taking as argument the current association's
   *            value.
   */
  def assign(function: Function1[V, V]) {
    forEachPair(new Procedure2[K, V]() {
      def apply(key: K, value: V): Boolean = {
        put(key, function.apply(value))
        true
      }
    })
  }

  /**
   * Clears the receiver, then adds all (key,value) pairs of <tt>other</tt>
   * values to it.
   *
   * @param other
   *            the other map to be copied into the receiver.
   */
  def assign(other: AbstractTypedMap[K, V]) {
    clear()
    other.forEachPair(new Procedure2[K, V]() {

      def apply(key: K, value: V): Boolean = {
        put(key, value)
        true
      }
    })
  }

  /**
   * Returns <tt>true</tt> if the receiver contains the specified key.
   *
   * @return <tt>true</tt> if the receiver contains the specified key.
   */
  def containsKey(key: K): Boolean = {
    !forEachKey(new Procedure1[K]() {
      def apply(iterKey: K): Boolean = key != iterKey
    })
  }

  /**
   * Returns <tt>true</tt> if the receiver contains the specified value.
   *
   * @return <tt>true</tt> if the receiver contains the specified value.
   */
  def containsValue(value: V): Boolean = {
    !forEachPair(new Procedure2[K, V]() {

      def apply(iterKey: K, iterValue: V): Boolean = value != iterValue
    })
  }

  /**
   * Returns a deep copy of the receiver; uses <code>clone()</code> and casts
   * the result.
   *
   * @return a deep copy of the receiver.
   */
  def copy(): AbstractTypedMap[K, V] = {
    clone().asInstanceOf[AbstractTypedMap[K, V]]
  }

  /**
   * Compares the specified object with this map for equality. Returns
   * <tt>true</tt> if the given object is also a map and the two maps
   * represent the same mappings. More formally, two maps <tt>m1</tt> and
   * <tt>m2</tt> represent the same mappings iff
   *
   * <pre>
   * m1.forEachPair(
   *  new IntDoubleProcedure() {
   *      public boolean apply(int key, double value) {
   *          return m2.containsKey(key) &amp;&amp; m2.get(key) == value;
   *      }
   *  }
   * )
   * &amp;&amp;
   * m2.forEachPair(
   *  new IntDoubleProcedure() {
   *      public boolean apply(int key, double value) {
   *          return m1.containsKey(key) &amp;&amp; m1.get(key) == value;
   *      }
   *  }
   * );
   * </pre>
   *
   * This implementation first checks if the specified object is this map; if
   * so it returns <tt>true</tt>. Then, it checks if the specified object is a
   * map whose size is identical to the size of this set; if not, it it
   * returns <tt>false</tt>. If so, it applies the iteration as described
   * above.
   *
   * @param obj
   *            object to be compared for equality with this map.
   * @return <tt>true</tt> if the specified object is equal to this map.
   */
  override def equals(obj: Any): Boolean = {
    if (obj == this) return true
    if (! obj.isInstanceOf[AbstractTypedMap[K, V]]) return false
    val other = obj.asInstanceOf[AbstractTypedMap[K, V]]
    if (other.size != size) return false
    forEachPair(new Procedure2[K, V]() {

      def apply(key: K, value: V): Boolean = {
        other.containsKey(key) && other.get(key) == value
      }
    }) &&
      other.forEachPair(new Procedure2[K, V]() {

      def apply(key: K, value: V): Boolean = containsKey(key) && get(key) == value
    })
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
  def forEachKey(procedure: Procedure1[K]): Boolean

  /**
   * Applies a procedure to each (key,value) pair of the receiver, if any.
   * Iteration order is guaranteed to be <i>identical</i> to the order used by
   * methodforEachKey(Procedure1[K]).
   *
   * @param procedure
   *            the procedure to be applied. Stops iteration if the procedure
   *            returns <tt>false</tt>, otherwise continues.
   * @return <tt>false</tt> if the procedure stopped before all keys where
   *         iterated over, <tt>true</tt> otherwise.
   */
  def forEachPair(procedure: Procedure2[K, V]): Boolean = {
    forEachKey(new Procedure1[K]() {

      def apply(key: K): Boolean = procedure.apply(key, get(key))
    })
  }

  def putAll(other: AbstractTypedMap[K, V]) {
    other.forEachPair(new Procedure2[K, V]() {
      def apply(k: K, v: V) = {
        put(k, v)
        true
      }
    })
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
  def get(key: K): V

  /**
   * Returns the first key the given value is associated with. It is often a
   * good idea to first check with containsValue(double) whether
   * there exists an association from a key to this value. Search order is
   * guaranteed to be <i>identical</i> to the order used by method
   * orEachKey(Procedure1[K]).
   *
   * @param value
   *            the value to search for.
   * @return the first key for which holds <tt>get(key) == value</tt>; returns
   *         <tt>Integer.MIN_VALUE</tt> if no such key exists.
   */
  def keyOf(value: V): K = {
    val foundKey = Array.ofDim[K](1)
    val notFound = forEachPair(new Procedure2[K, V]() {
      def apply(iterKey: K, iterValue: V): Boolean = {
        var found = value == iterValue
        if (found) foundKey(0) = iterKey
        !found
      }
    })
    if (notFound) return Integer.MIN_VALUE.asInstanceOf[K]
    foundKey(0)
  }

  /**
   * Returns a list filled with all keys contained in the receiver. The
   * returned list has a size that equals <tt>this.size()</tt>. Iteration
   * order is guaranteed to be <i>identical</i> to the order used by method
   * forEachKey(Procedure1[K]).
   * <p>
   * This method can be used to iterate over the keys of the receiver.
   *
   * @return the keys.
   */
  def keys(): ArrayList[K] = {
    val list = new ArrayList[K](size)
    keys(list)
    list
  }

  /**
   * Fills all keys contained in the receiver into the specified list. Fills
   * the list, starting at index 0. After this call returns the specified list
   * has a new size that equals <tt>this.size()</tt>. Iteration order is
   * guaranteed to be <i>identical</i> to the order used by method
   * forEachKey(Procedure1[K]).
   * <p>
   * This method can be used to iterate over the keys of the receiver.
   *
   * @param list
   *            the list to be filled, can have any size.
   */
  def keys(list: AbstractList[K]) {
    list.clear()
    forEachKey(new Procedure1[K]() {
      def apply(key: K): Boolean = {
        list.add(key)
        true
      }
    })
  }

  /**
   * Fills all pairs satisfying a given condition into the specified lists.
   * Fills into the lists, starting at index 0. After this call returns the
   * specified lists both have a new size, the number of pairs satisfying the
   * condition. Iteration order is guaranteed to be <i>identical</i> to the
   * order used by method forEachKey(Procedure1[K]).
   * <p>
   * <b>Example:</b> <br>
   *
   * <pre>
   *   Procedure2[K, V] condition = new Procedure2[K, V]() { // match even keys only
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
  def pairsMatching(condition: Procedure2[K, V], keyList: AbstractList[K], valueList: AbstractList[V]) {
    keyList.clear()
    valueList.clear()
    forEachPair(new Procedure2[K, V]() {

      def apply(key: K, value: V): Boolean = {
        if (condition.apply(key, value)) {
          keyList.add(key)
          valueList.add(value)
        }
        true
      }
    })
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
  def put(key: K, value: V): Boolean

  /**
   * Removes the given key with its associated element from the receiver, if
   * present.
   *
   * @param key
   *            the key to be removed from the receiver.
   * @return <tt>true</tt> if the receiver contained the specified key,
   *         <tt>false</tt> otherwise.
   */
  def remove(key: K): Boolean

  /**
   * Returns a string representation of the receiver, containing the String
   * representation of each key-value pair, sorted ascending by key.
   */
  override def toString: String = {
    val theKeys = keys()
    val buf = new StringBuffer(theKeys.toString + "\n")
    buf.append("[")
    val maxIndex = theKeys.size - 1
    var i = 0
    while (i <= maxIndex) {
      val key = theKeys.get(i)
      buf.append(String.valueOf(key))
      buf.append("->")
      buf.append(String.valueOf(get(key)))
      if (i < maxIndex) buf.append(", ")
      i += 1
    }
    buf.append("]")
    buf.toString
  }

  /**
   * Returns a list filled with all values contained in the receiver. The
   * returned list has a size that equals <tt>this.size()</tt>. Iteration
   * order is guaranteed to be <i>identical</i> to the order used by method
   * forEachKey(Procedure1[K]).
   * <p>
   * This method can be used to iterate over the values of the receiver.
   *
   * @return the values.
   */
  def values: AbstractList[V] = {
    val list = new ArrayList[V](size)
    values(list)
    list
  }

  /**
   * Fills all values contained in the receiver into the specified list. Fills
   * the list, starting at index 0. After this call returns the specified list
   * has a new size that equals <tt>this.size()</tt>. Iteration order is
   * guaranteed to be <i>identical</i> to the order used by method
   * forEachKey(Procedure1[K]).
   * <p>
   * This method can be used to iterate over the values of the receiver.
   *
   * @param list
   *            the list to be filled, can have any size.
   */
  def values(list: AbstractList[V]) {
    list.clear()
    forEachKey(new Procedure1[K]() {
      def apply(key: K): Boolean = {
        list.add(get(key))
        true
      }
    })
  }
}
