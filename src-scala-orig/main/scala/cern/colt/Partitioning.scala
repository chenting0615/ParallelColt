package cern.colt

import cern.colt.function.tint.IntComparator
import cern.colt.list.tdouble.DoubleArrayList
import cern.colt.list.tint.IntArrayList
import Partitioning._
//remove if not needed
import scala.collection.JavaConversions._

object Partitioning {

  private val SMALL = 7

  private val MEDIUM = 40

  protected var steps: Int = 0

  var swappedElements: Int = 0

  /**
   * Finds the given key "a" within some generic data using the binary search
   * algorithm.
   *
   * @param a
   *            the index of the key to search for.
   * @param from
   *            the leftmost search position, inclusive.
   * @param to
   *            the rightmost search position, inclusive.
   * @param comp
   *            the comparator determining the order of the generic data.
   *            Takes as first argument the index <tt>a</tt> within the
   *            generic splitters <tt>s</tt>. Takes as second argument the
   *            index <tt>b</tt> within the generic data <tt>g</tt>.
   * @return index of the search key, if it is contained in the list;
   *         otherwise, <tt>(-(<i>insertion point</i>) - 1)</tt>. The
   *         <i>insertion point</i> is defined as the the point at which the
   *         value would be inserted into the list: the index of the first
   *         element greater than the key, or <tt>list.length</tt>, if all
   *         elements in the list are less than the specified key. Note that
   *         this guarantees that the return value will be &gt;= 0 if and only
   *         if the key is found.
   */
  private def binarySearchFromTo(a: Int, 
      from: Int, 
      to: Int, 
      comp: IntComparator): Int = {
    while (from <= to) {
      val mid = (from + to) / 2
      val comparison = comp.compare(mid, a)
      if (comparison < 0) from = mid + 1 else if (comparison > 0) to = mid - 1 else return mid
    }
    -(from + 1)
  }

  /**
   * Same as {@link #dualPartition(int[],int[],int,int,int[],int,int,int[])}
   * except that it <i>synchronously</i> partitions <tt>double[]</tt> rather
   * than <tt>int[]</tt> arrays.
   */
  def dualPartition(list: Array[Double], 
      secondary: Array[Double], 
      from: Int, 
      to: Int, 
      splitters: Array[Double], 
      splitFrom: Int, 
      splitTo: Int, 
      splitIndexes: Array[Int]) {
    var splitter: Double = 0.0
    if (splitFrom > splitTo) return
    if (from > to) {
      from -= 1
      var i = splitFrom
      while (i <= splitTo) splitIndexes(i += 1) = from
      return
    }
    var medianIndex: Int = 0
    if (splitFrom == splitTo) {
      medianIndex = splitFrom
    } else {
      var m = (from + to) / 2
      val len = to - from + 1
      if (len > SMALL) {
        var l = from
        var n = to
        if (len > MEDIUM) {
          val s = len / 8
          l = med3(list, l, l + s, l + 2 * s)
          m = med3(list, m - s, m, m + s)
          n = med3(list, n - 2 * s, n - s, n)
        }
        m = med3(list, l, m, n)
      }
      medianIndex = Sorting.binarySearchFromTo(splitters, list(m), splitFrom, splitTo)
      if (medianIndex < 0) medianIndex = -medianIndex - 1
      if (medianIndex > splitTo) medianIndex = splitTo
    }
    splitter = splitters(medianIndex)
    val splitIndex = dualPartition(list, secondary, from, to, splitter)
    splitIndexes(medianIndex) = splitIndex
    if (splitIndex < from) {
      val i = medianIndex - 1
      while (i >= splitFrom && (!(splitter < splitters(i)))) splitIndexes(i -= 1) = splitIndex
      splitFrom = medianIndex + 1
    } else if (splitIndex >= to) {
      val i = medianIndex + 1
      while (i <= splitTo && (!(splitter > splitters(i)))) splitIndexes(i += 1) = splitIndex
      splitTo = medianIndex - 1
    }
    if (splitFrom <= medianIndex - 1) {
      dualPartition(list, secondary, from, splitIndex, splitters, splitFrom, medianIndex - 1, splitIndexes)
    }
    if (medianIndex + 1 <= splitTo) {
      dualPartition(list, secondary, splitIndex + 1, to, splitters, medianIndex + 1, splitTo, splitIndexes)
    }
  }

  /**
   * Same as {@link #dualPartition(int[],int[],int,int,int)} except that it
   * <i>synchronously</i> partitions <tt>double[]</tt> rather than
   * <tt>int[]</tt> arrays.
   */
  def dualPartition(list: Array[Double], 
      secondary: Array[Double], 
      from: Int, 
      to: Int, 
      splitter: Double): Int = {
    var element: Double = 0.0
    var i = from - 1
    while (i <= to) {
      element = list(i)
      if (element < splitter) {
        list(i) = list(from)
        list(from) = element
        element = secondary(i)
        secondary(i) = secondary(from)
        secondary(from += 1) = element
      }
    }
    from - 1
  }

  /**
   * Same as {@link #partition(int[],int,int,int[],int,int,int[])} except that
   * this method <i>synchronously</i> partitions two arrays at the same time;
   * both arrays are partially sorted according to the elements of the primary
   * array. In other words, each time an element in the primary array is moved
   * from index A to B, the correspoding element within the secondary array is
   * also moved from index A to B.
   * <p>
   * <b>Use cases:</b>
   * <p>
   * Image having a large list of 2-dimensional points. If memory consumption
   * and performance matter, it is a good idea to physically lay them out as
   * two 1-dimensional arrays (using something like <tt>Point2D</tt> objects
   * would be prohibitively expensive, both in terms of time and space). Now
   * imagine wanting to histogram the points. We may want to partially sort
   * the points by x-coordinate into intervals. This method efficiently does
   * the job.
   * <p>
   * <b>Performance:</b>
   * <p>
   * Same as for single-partition methods.
   */
  def dualPartition(list: Array[Int], 
      secondary: Array[Int], 
      from: Int, 
      to: Int, 
      splitters: Array[Int], 
      splitFrom: Int, 
      splitTo: Int, 
      splitIndexes: Array[Int]) {
    var splitter: Int = 0
    if (splitFrom > splitTo) return
    if (from > to) {
      from -= 1
      var i = splitFrom
      while (i <= splitTo) splitIndexes(i += 1) = from
      return
    }
    var medianIndex: Int = 0
    if (splitFrom == splitTo) {
      medianIndex = splitFrom
    } else {
      var m = (from + to) / 2
      val len = to - from + 1
      if (len > SMALL) {
        var l = from
        var n = to
        if (len > MEDIUM) {
          val s = len / 8
          l = med3(list, l, l + s, l + 2 * s)
          m = med3(list, m - s, m, m + s)
          n = med3(list, n - 2 * s, n - s, n)
        }
        m = med3(list, l, m, n)
      }
      medianIndex = Sorting.binarySearchFromTo(splitters, list(m), splitFrom, splitTo)
      if (medianIndex < 0) medianIndex = -medianIndex - 1
      if (medianIndex > splitTo) medianIndex = splitTo
    }
    splitter = splitters(medianIndex)
    val splitIndex = dualPartition(list, secondary, from, to, splitter)
    splitIndexes(medianIndex) = splitIndex
    if (splitIndex < from) {
      val i = medianIndex - 1
      while (i >= splitFrom && (!(splitter < splitters(i)))) splitIndexes(i -= 1) = splitIndex
      splitFrom = medianIndex + 1
    } else if (splitIndex >= to) {
      val i = medianIndex + 1
      while (i <= splitTo && (!(splitter > splitters(i)))) splitIndexes(i += 1) = splitIndex
      splitTo = medianIndex - 1
    }
    if (splitFrom <= medianIndex - 1) {
      dualPartition(list, secondary, from, splitIndex, splitters, splitFrom, medianIndex - 1, splitIndexes)
    }
    if (medianIndex + 1 <= splitTo) {
      dualPartition(list, secondary, splitIndex + 1, to, splitters, medianIndex + 1, splitTo, splitIndexes)
    }
  }

  /**
   * Same as {@link #partition(int[],int,int,int)} except that this method
   * <i>synchronously</i> partitions two arrays at the same time; both arrays
   * are partially sorted according to the elements of the primary array. In
   * other words, each time an element in the primary array is moved from
   * index A to B, the correspoding element within the secondary array is also
   * moved from index A to B.
   * <p>
   * <b>Performance:</b>
   * <p>
   * Same as for single-partition methods.
   */
  def dualPartition(list: Array[Int], 
      secondary: Array[Int], 
      from: Int, 
      to: Int, 
      splitter: Int): Int = {
    var element: Int = 0
    var i = from - 1
    while (i <= to) {
      element = list(i)
      if (element < splitter) {
        list(i) = list(from)
        list(from) = element
        element = secondary(i)
        secondary(i) = secondary(from)
        secondary(from += 1) = element
      }
    }
    from - 1
  }

  /**
   * Same as {@link #partition(int[],int,int,int[],int,int,int[])} except that
   * it <i>generically</i> partitions arbitrary shaped data (for example
   * matrices or multiple arrays) rather than <tt>int[]</tt> arrays.
   * <p>
   * This method operates on arbitrary shaped data and arbitrary shaped
   * splitters. In fact, it has no idea what kind of data by what kind of
   * splitters it is partitioning. Comparisons and swapping are delegated to
   * user provided objects which know their data and can do the job.
   * <p>
   * Lets call the generic data <tt>g</tt> (it may be a matrix, one array,
   * three linked lists or whatever). Lets call the generic splitters
   * <tt>s</tt>. This class takes a user comparison function operating on two
   * indexes <tt>(a,b)</tt>, namely an {@link IntComparator}. The comparison
   * function determines whether <tt>s[a]</tt> is equal, less or greater than
   * <tt>g[b]</tt>. This method can then decide to swap the data <tt>g[b]</tt>
   * with the data <tt>g[c]</tt> (yes, <tt>c</tt>, not <tt>a</tt>). It calls a
   * user provided {@link cern.colt.Swapper} object that knows how to swap the
   * data of these two indexes.
   * <p>
   * Again, note the details: Comparisons compare <tt>s[a]</tt> with
   * <tt>g[b]</tt>. Swaps swap <tt>g[b]</tt> with <tt>g[c]</tt>. Prior to
   * calling this method, the generic splitters <tt>s</tt> must be sorted
   * ascending and must not contain multiple equal values. These preconditions
   * are not checked; be sure that they are met.
   *
   * @param from
   *            the index of the first element within <tt>g</tt> to be
   *            considered.
   * @param to
   *            the index of the last element within <tt>g</tt> to be
   *            considered. The method considers the elements
   *            <tt>g[from] .. g[to]</tt>.
   *
   *
   * @param splitFrom
   *            the index of the first splitter element to be considered.
   * @param splitTo
   *            the index of the last splitter element to be considered. The
   *            method considers the splitter elements
   *            <tt>s[splitFrom] .. s[splitTo]</tt>.
   *
   * @param splitIndexes
   *            a list into which this method fills the indexes of elements
   *            delimiting intervals. Upon return
   *            <tt>splitIndexes[splitFrom..splitTo]</tt> will be set
   *            accordingly. Therefore, must satisfy
   *            <tt>splitIndexes.length > splitTo</tt>.
   *
   * @param comp
   *            the comparator comparing a splitter with an element of the
   *            generic data. Takes as first argument the index <tt>a</tt>
   *            within the generic splitters <tt>s</tt>. Takes as second
   *            argument the index <tt>b</tt> within the generic data
   *            <tt>g</tt>.
   * @param comp2
   *            the comparator to determine the order of the generic data.
   *            Takes as first argument the index <tt>a</tt> within the
   *            generic data <tt>g</tt>. Takes as second argument the index
   *            <tt>b</tt> within the generic data <tt>g</tt>.
   * @param comp3
   *            the comparator comparing a splitter with another splitter.
   *            Takes as first argument the index <tt>a</tt> within the
   *            generic splitters <tt>s</tt>. Takes as second argument the
   *            index <tt>b</tt> within the generic splitters <tt>g</tt>.
   * @param swapper
   *            an object that knows how to swap the elements at any two
   *            indexes (a,b). Takes as first argument the index <tt>b</tt>
   *            within the generic data <tt>g</tt>. Takes as second argument
   *            the index <tt>c</tt> within the generic data <tt>g</tt>.
   *
   *            <p>
   *            Tip: Normally you will have
   *            <tt>splitIndexes.length == s.length</tt> as well as
   *            <tt>from==0, to==g.length-1</tt> and
   *            <tt>splitFrom==0, splitTo==s.length-1</tt>.
   *
   * @see Sorting#binarySearchFromTo(int,int,IntComparator)
   */
  def genericPartition(from: Int, 
      to: Int, 
      splitFrom: Int, 
      splitTo: Int, 
      splitIndexes: Array[Int], 
      comp: IntComparator, 
      comp2: IntComparator, 
      comp3: IntComparator, 
      swapper: Swapper) {
    var splitter: Int = 0
    if (splitFrom > splitTo) return
    if (from > to) {
      from -= 1
      var i = splitFrom
      while (i <= splitTo) splitIndexes(i += 1) = from
      return
    }
    var medianIndex: Int = 0
    if (splitFrom == splitTo) {
      medianIndex = splitFrom
    } else {
      var m = (from + to) / 2
      val len = to - from + 1
      if (len > SMALL) {
        var l = from
        var n = to
        if (len > MEDIUM) {
          val s = len / 8
          l = med3(l, l + s, l + 2 * s, comp2)
          m = med3(m - s, m, m + s, comp2)
          n = med3(n - 2 * s, n - s, n, comp2)
        }
        m = med3(l, m, n, comp2)
      }
      medianIndex = binarySearchFromTo(m, splitFrom, splitTo, comp)
      if (medianIndex < 0) medianIndex = -medianIndex - 1
      if (medianIndex > splitTo) medianIndex = splitTo
    }
    splitter = medianIndex
    val splitIndex = genericPartition(from, to, splitter, comp, swapper)
    splitIndexes(medianIndex) = splitIndex
    if (splitIndex < from) {
      val i = medianIndex - 1
      while (i >= splitFrom && (!(comp3.compare(splitter, i) < 0))) splitIndexes(i -= 1) = splitIndex
      splitFrom = medianIndex + 1
    } else if (splitIndex >= to) {
      val i = medianIndex + 1
      while (i <= splitTo && (!(comp3.compare(splitter, i) > 0))) splitIndexes(i += 1) = splitIndex
      splitTo = medianIndex - 1
    }
    if (splitFrom <= medianIndex - 1) {
      genericPartition(from, splitIndex, splitFrom, medianIndex - 1, splitIndexes, comp, comp2, comp3, 
        swapper)
    }
    if (medianIndex + 1 <= splitTo) {
      genericPartition(splitIndex + 1, to, medianIndex + 1, splitTo, splitIndexes, comp, comp2, comp3, 
        swapper)
    }
  }

  /**
   * Same as {@link #partition(int[],int,int,int)} except that it
   * <i>generically</i> partitions arbitrary shaped data (for example matrices
   * or multiple arrays) rather than <tt>int[]</tt> arrays.
   */
  private def genericPartition(from: Int, 
      to: Int, 
      splitter: Int, 
      comp: IntComparator, 
      swapper: Swapper): Int = {
    var i = from - 1
    while (i <= to) {
      if (comp.compare(splitter, i) > 0) {
        swapper.swap(i, from)
        from += 1
      }
    }
    from - 1
  }

  /**
   * Returns the index of the median of the three indexed elements.
   */
  private def med3(x: Array[Double], 
      a: Int, 
      b: Int, 
      c: Int): Int = {
    (if (x(a) < x(b)) (if (x(b) < x(c)) b else if (x(a) < x(c)) c else a) else (if (x(b) > x(c)) b else if (x(a) > x(c)) c else a))
  }

  /**
   * Returns the index of the median of the three indexed elements.
   */
  private def med3(x: Array[Int], 
      a: Int, 
      b: Int, 
      c: Int): Int = {
    (if (x(a) < x(b)) (if (x(b) < x(c)) b else if (x(a) < x(c)) c else a) else (if (x(b) > x(c)) b else if (x(a) > x(c)) c else a))
  }

  /**
   * Returns the index of the median of the three indexed chars.
   */
  private def med3(x: Array[AnyRef], 
      a: Int, 
      b: Int, 
      c: Int, 
      comp: java.util.Comparator): Int = {
    val ab = comp.compare(x(a), x(b))
    val ac = comp.compare(x(a), x(c))
    val bc = comp.compare(x(b), x(c))
    (if (ab < 0) (if (bc < 0) b else if (ac < 0) c else a) else (if (bc > 0) b else if (ac > 0) c else a))
  }

  /**
   * Returns the index of the median of the three indexed chars.
   */
  private def med3(a: Int, 
      b: Int, 
      c: Int, 
      comp: IntComparator): Int = {
    val ab = comp.compare(a, b)
    val ac = comp.compare(a, c)
    val bc = comp.compare(b, c)
    (if (ab < 0) (if (bc < 0) b else if (ac < 0) c else a) else (if (bc > 0) b else if (ac > 0) c else a))
  }

  /**
   * Same as {@link #partition(int[],int,int,int[],int,int,int[])} except that
   * it partitions <tt>double[]</tt> rather than <tt>int[]</tt> arrays.
   */
  def partition(list: Array[Double], 
      from: Int, 
      to: Int, 
      splitters: Array[Double], 
      splitFrom: Int, 
      splitTo: Int, 
      splitIndexes: Array[Int]) {
    var splitter: Double = 0.0
    if (splitFrom > splitTo) return
    if (from > to) {
      from -= 1
      var i = splitFrom
      while (i <= splitTo) splitIndexes(i += 1) = from
      return
    }
    var medianIndex: Int = 0
    if (splitFrom == splitTo) {
      medianIndex = splitFrom
    } else {
      var m = (from + to) / 2
      val len = to - from + 1
      if (len > SMALL) {
        var l = from
        var n = to
        if (len > MEDIUM) {
          val s = len / 8
          l = med3(list, l, l + s, l + 2 * s)
          m = med3(list, m - s, m, m + s)
          n = med3(list, n - 2 * s, n - s, n)
        }
        m = med3(list, l, m, n)
      }
      medianIndex = Sorting.binarySearchFromTo(splitters, list(m), splitFrom, splitTo)
      if (medianIndex < 0) medianIndex = -medianIndex - 1
      if (medianIndex > splitTo) medianIndex = splitTo
    }
    splitter = splitters(medianIndex)
    val splitIndex = partition(list, from, to, splitter)
    splitIndexes(medianIndex) = splitIndex
    if (splitIndex < from) {
      val i = medianIndex - 1
      while (i >= splitFrom && (!(splitter < splitters(i)))) splitIndexes(i -= 1) = splitIndex
      splitFrom = medianIndex + 1
    } else if (splitIndex >= to) {
      val i = medianIndex + 1
      while (i <= splitTo && (!(splitter > splitters(i)))) splitIndexes(i += 1) = splitIndex
      splitTo = medianIndex - 1
    }
    if (splitFrom <= medianIndex - 1) {
      partition(list, from, splitIndex, splitters, splitFrom, medianIndex - 1, splitIndexes)
    }
    if (medianIndex + 1 <= splitTo) {
      partition(list, splitIndex + 1, to, splitters, medianIndex + 1, splitTo, splitIndexes)
    }
  }

  /**
   * Same as {@link #partition(int[],int,int,int)} except that it partitions
   * <tt>double[]</tt> rather than <tt>int[]</tt> arrays.
   */
  def partition(list: Array[Double], 
      from: Int, 
      to: Int, 
      splitter: Double): Int = {
    var element: Double = 0.0
    var i = from - 1
    while (i <= to) {
      element = list(i)
      if (element < splitter) {
        list(i) = list(from)
        list(from += 1) = element
      }
    }
    from - 1
  }

  /**
   * Partitions (partially sorts) the given list such that all elements
   * falling into some intervals are placed next to each other. Returns the
   * indexes of elements delimiting intervals.
   * <p>
   * <b>Example:</b>
   * <p>
   * <tt>list = (7, 4, 5, 50, 6, 4, 3, 6), splitters = (5, 10, 30)</tt>
   * defines the three intervals <tt>[-infinity,5), [5,10), [10,30)</tt>. Lets
   * define to sort the entire list (<tt>from=0, to=7</tt>) using all
   * splitters (<tt>splitFrom==0, splitTo=2</tt>).
   * <p>
   * The method modifies the list to be
   * <tt>list = (4, 4, 3, 6, 7, 5, 6, 50)</tt> and returns the
   * <tt>splitIndexes = (2, 6, 6)</tt>. In other words,
   * <ul>
   * <li>All values <tt>list[0..2]</tt> fall into <tt>[-infinity,5)</tt>.
   * <li>All values <tt>list[3..6]</tt> fall into <tt>[5,10)</tt>.
   * <li>All values <tt>list[7..6]</tt> fall into <tt>[10,30)</tt>, i.e. no
   * elements, since <tt>7>6</tt>.
   * <li>All values <tt>list[7 .. 7=list.length-1]</tt> fall into
   * <tt>[30,infinity]</tt>.
   * <li>In general, all values
   * <tt>list[splitIndexes[j-1]+1 .. splitIndexes[j]]</tt> fall into interval
   * <tt>j</tt>.
   * </ul>
   * As can be seen, the list is partially sorted such that values falling
   * into a certain interval are placed next to each other. Note that
   * <i>within</i> an interval, elements are entirelly unsorted. They are only
   * sorted across interval boundaries. In particular, this partitioning
   * algorithm is not <i>stable</i>: the relative order of elements is not
   * preserved (Producing a stable algorithm would require no more than minor
   * modifications to method partition(int[],int,int,int)).
   * <p>
   * More formally, this method guarantees that upon return
   * <tt>for all j = splitFrom .. splitTo</tt> there holds: <br>
   * <tt>for all i = splitIndexes[j-1]+1 .. splitIndexes[j]: splitters[j-1] <= list[i] < splitters[j]</tt>.
   * <p>
   * <b>Performance:</b>
   * <p>
   * Let <tt>N=to-from+1</tt> be the number of elements to be partitioned. Let
   * <tt>k=splitTo-splitFrom+1</tt> be the number of splitter elements. Then
   * we have the following time complexities
   * <ul>
   * <li>Worst case: <tt>O( N * log(k) )</tt>.
   * <li>Average case: <tt>O( N * log(k) )</tt>.
   * <li>Best case: <tt>O( N )</tt>. In general, the more uniform (skewed) the
   * data is spread across intervals, the more performance approaches the
   * worst (best) case. If no elements fall into the given intervals, running
   * time is linear.
   * </ul>
   * No temporary memory is allocated; the sort is in-place.
   * <p>
   * <b>Implementation:</b>
   * <p>
   * The algorithm can be seen as a Bentley/McIlroy quicksort where swapping
   * and insertion sort are omitted. It is designed to detect and take
   * advantage of skew while maintaining good performance in the uniform case.
   *
   * @param list
   *            the list to be partially sorted.
   *
   * @param from
   *            the index of the first element within <tt>list</tt> to be
   *            considered.
   * @param to
   *            the index of the last element within <tt>list</tt> to be
   *            considered. The method considers the elements
   *            <tt>list[from] .. list[to]</tt>.
   *
   * @param splitters
   *            the values at which the list shall be split into intervals.
   *            Must be sorted ascending and must not contain multiple
   *            identical values. These preconditions are not checked; be sure
   *            that they are met.
   *
   * @param splitFrom
   *            the index of the first splitter element to be considered.
   * @param splitTo
   *            the index of the last splitter element to be considered. The
   *            method considers the splitter elements
   *            <tt>splitters[splitFrom] .. splitters[splitTo]</tt>.
   *
   * @param splitIndexes
   *            a list into which this method fills the indexes of elements
   *            delimiting intervals. Upon return
   *            <tt>splitIndexes[splitFrom..splitTo]</tt> will be set
   *            accordingly. Therefore, must satisfy
   *            <tt>splitIndexes.length > splitTo</tt>.
   *            <p>
   *            Tip: Normally you will have
   *            <tt>splitIndexes.length == splitters.length</tt> as well as
   *            <tt>from==0, to==list.length-1</tt> and
   *            <tt>splitFrom==0, splitTo==splitters.length-1</tt>.
   *
   * @see cern.colt.Arrays
   * @see cern.colt.GenericSorting
   * @see java.util.Arrays
   */
  def partition(list: Array[Int], 
      from: Int, 
      to: Int, 
      splitters: Array[Int], 
      splitFrom: Int, 
      splitTo: Int, 
      splitIndexes: Array[Int]) {
    var element: Int = 0
    var splitter: Int = 0
    if (splitFrom > splitTo) return
    if (from > to) {
      from -= 1
      var i = splitFrom
      while (i <= splitTo) splitIndexes(i += 1) = from
      return
    }
    var medianIndex: Int = 0
    if (splitFrom == splitTo) {
      medianIndex = splitFrom
    } else {
      var m = (from + to) / 2
      val len = to - from + 1
      if (len > SMALL) {
        var l = from
        var n = to
        if (len > MEDIUM) {
          val s = len / 8
          l = med3(list, l, l + s, l + 2 * s)
          m = med3(list, m - s, m, m + s)
          n = med3(list, n - 2 * s, n - s, n)
        }
        m = med3(list, l, m, n)
      }
      medianIndex = Sorting.binarySearchFromTo(splitters, list(m), splitFrom, splitTo)
      if (medianIndex < 0) medianIndex = -medianIndex - 1
      if (medianIndex > splitTo) medianIndex = splitTo
    }
    splitter = splitters(medianIndex)
    val splitIndex = partition(list, from, to, splitter)
    splitIndexes(medianIndex) = splitIndex
    if (splitIndex < from) {
      val i = medianIndex - 1
      while (i >= splitFrom && (!(splitter < splitters(i)))) splitIndexes(i -= 1) = splitIndex
      splitFrom = medianIndex + 1
    } else if (splitIndex >= to) {
      val i = medianIndex + 1
      while (i <= splitTo && (!(splitter > splitters(i)))) splitIndexes(i += 1) = splitIndex
      splitTo = medianIndex - 1
    }
    if (splitFrom <= medianIndex - 1) {
      partition(list, from, splitIndex, splitters, splitFrom, medianIndex - 1, splitIndexes)
    }
    if (medianIndex + 1 <= splitTo) {
      partition(list, splitIndex + 1, to, splitters, medianIndex + 1, splitTo, splitIndexes)
    }
  }

  /**
   * Partitions (partially sorts) the given list such that all elements
   * falling into the given interval are placed next to each other. Returns
   * the index of the element delimiting the interval.
   * <p>
   * <b>Example:</b>
   * <p>
   * <tt>list = (7, 4, 5, 50, 6, 4, 3, 6), splitter = 5</tt> defines the two
   * intervals <tt>[-infinity,5), [5,+infinity]</tt>.
   * <p>
   * The method modifies the list to be
   * <tt>list = (4, 4, 3, 50, 6, 7, 5, 6)</tt> and returns the split index
   * <tt>2</tt>. In other words,
   * <ul>
   * <li>All values <tt>list[0..2]</tt> fall into <tt>[-infinity,5)</tt>.
   * <li>All values <tt>list[3=2+1 .. 7=list.length-1]</tt> fall into
   * <tt>[5,+infinity]</tt>.
   * </ul>
   * As can be seen, the list is partially sorted such that values falling
   * into a certain interval are placed next to each other. Note that
   * <i>within</i> an interval, elements are entirelly unsorted. They are only
   * sorted across interval boundaries. In particular, this partitioning
   * algorithm is not <i>stable</i>.
   * <p>
   * More formally, this method guarantees that upon return there holds:
   * <ul>
   * <li>for all <tt>i = from .. returnValue: list[i] < splitter</tt> and
   * <li>for all
   * <tt>i = returnValue+1 .. list.length-1: !(list[i] < splitter)</tt>.
   * </ul>
   * <p>
   * <b>Performance:</b>
   * <p>
   * Let <tt>N=to-from+1</tt> be the number of elements to be partially
   * sorted. Then the time complexity is <tt>O( N )</tt>. No temporary memory
   * is allocated; the sort is in-place.
   *
   * <p>
   *
   * @param list
   *            the list to be partially sorted.
   *
   * @param from
   *            the index of the first element within <tt>list</tt> to be
   *            considered.
   * @param to
   *            the index of the last element within <tt>list</tt> to be
   *            considered. The method considers the elements
   *            <tt>list[from] .. list[to]</tt>.
   *
   * @param splitter
   *            the value at which the list shall be split.
   *
   * @return the index of the largest element falling into the interval
   *         <tt>[-infinity,splitter)</tt>, as seen after partitioning.
   */
  def partition(list: Array[Int], 
      from: Int, 
      to: Int, 
      splitter: Int): Int = {
    steps += to - from + 1
    var element: Int = 0
    var i = from - 1
    while (i <= to) {
      element = list(i)
      if (element < splitter) {
        list(i) = list(from)
        list(from += 1) = element
      }
    }
    from - 1
  }

  /**
   * Same as {@link #partition(int[],int,int,int[],int,int,int[])} except that
   * it partitions <tt>Object[]</tt> rather than <tt>int[]</tt> arrays.
   */
  def partition(list: Array[Any], 
      from: Int, 
      to: Int, 
      splitters: Array[Any], 
      splitFrom: Int, 
      splitTo: Int, 
      splitIndexes: Array[Int], 
      comp: java.util.Comparator) {
    var splitter: AnyRef = null
    if (splitFrom > splitTo) return
    if (from > to) {
      from -= 1
      var i = splitFrom
      while (i <= splitTo) splitIndexes(i += 1) = from
      return
    }
    var medianIndex: Int = 0
    if (splitFrom == splitTo) {
      medianIndex = splitFrom
    } else {
      var m = (from + to) / 2
      val len = to - from + 1
      if (len > SMALL) {
        var l = from
        var n = to
        if (len > MEDIUM) {
          val s = len / 8
          l = med3(list, l, l + s, l + 2 * s, comp)
          m = med3(list, m - s, m, m + s, comp)
          n = med3(list, n - 2 * s, n - s, n, comp)
        }
        m = med3(list, l, m, n, comp)
      }
      medianIndex = Sorting.binarySearchFromTo(splitters, list(m), splitFrom, splitTo, comp)
      if (medianIndex < 0) medianIndex = -medianIndex - 1
      if (medianIndex > splitTo) medianIndex = splitTo
    }
    splitter = splitters(medianIndex)
    val splitIndex = partition(list, from, to, splitter, comp)
    splitIndexes(medianIndex) = splitIndex
    if (splitIndex < from) {
      val i = medianIndex - 1
      while (i >= splitFrom && (!(comp.compare(splitter, splitters(i)) < 0))) splitIndexes(i -= 1) = splitIndex
      splitFrom = medianIndex + 1
    } else if (splitIndex >= to) {
      val i = medianIndex + 1
      while (i <= splitTo && (!(comp.compare(splitter, splitters(i)) > 0))) splitIndexes(i += 1) = splitIndex
      splitTo = medianIndex - 1
    }
    if (splitFrom <= medianIndex - 1) {
      partition(list, from, splitIndex, splitters, splitFrom, medianIndex - 1, splitIndexes, comp)
    }
    if (medianIndex + 1 <= splitTo) {
      partition(list, splitIndex + 1, to, splitters, medianIndex + 1, splitTo, splitIndexes, comp)
    }
  }

  /**
   * Same as {@link #partition(int[],int,int,int)} except that it
   * <i>synchronously</i> partitions the objects of the given list by the
   * order of the given comparator.
   */
  def partition(list: Array[Any], 
      from: Int, 
      to: Int, 
      splitter: AnyRef, 
      comp: java.util.Comparator): Int = {
    var element: AnyRef = null
    var i = from - 1
    while (i <= to) {
      element = list(i)
      if (comp.compare(element, splitter) < 0) {
        list(i) = list(from)
        list(from) = element
        from += 1
      }
    }
    from - 1
  }

  /**
   * Equivalent to
   * <tt>partition(list.elements(), from, to, splitters.elements(), 0, splitters.size()-1, splitIndexes.elements())</tt>
   * .
   */
  def partition(list: DoubleArrayList, 
      from: Int, 
      to: Int, 
      splitters: DoubleArrayList, 
      splitIndexes: IntArrayList) {
    partition(list.elements(), from, to, splitters.elements(), 0, splitters.size - 1, splitIndexes.elements())
  }

  /**
   * Equivalent to
   * <tt>partition(list.elements(), from, to, splitters.elements(), 0, splitters.size()-1, splitIndexes.elements())</tt>
   * .
   */
  def partition(list: IntArrayList, 
      from: Int, 
      to: Int, 
      splitters: IntArrayList, 
      splitIndexes: IntArrayList) {
    partition(list.elements(), from, to, splitters.elements(), 0, splitters.size - 1, splitIndexes.elements())
  }

  /**
   * Same as
   * {@link #triplePartition(int[],int[],int[],int,int,int[],int,int,int[])}
   * except that it <i>synchronously</i> partitions <tt>double[]</tt> rather
   * than <tt>int[]</tt> arrays.
   */
  def triplePartition(list: Array[Double], 
      secondary: Array[Double], 
      tertiary: Array[Double], 
      from: Int, 
      to: Int, 
      splitters: Array[Double], 
      splitFrom: Int, 
      splitTo: Int, 
      splitIndexes: Array[Int]) {
    var splitter: Double = 0.0
    if (splitFrom > splitTo) return
    if (from > to) {
      from -= 1
      var i = splitFrom
      while (i <= splitTo) splitIndexes(i += 1) = from
      return
    }
    var medianIndex: Int = 0
    if (splitFrom == splitTo) {
      medianIndex = splitFrom
    } else {
      var m = (from + to) / 2
      val len = to - from + 1
      if (len > SMALL) {
        var l = from
        var n = to
        if (len > MEDIUM) {
          val s = len / 8
          l = med3(list, l, l + s, l + 2 * s)
          m = med3(list, m - s, m, m + s)
          n = med3(list, n - 2 * s, n - s, n)
        }
        m = med3(list, l, m, n)
      }
      medianIndex = Sorting.binarySearchFromTo(splitters, list(m), splitFrom, splitTo)
      if (medianIndex < 0) medianIndex = -medianIndex - 1
      if (medianIndex > splitTo) medianIndex = splitTo
    }
    splitter = splitters(medianIndex)
    val splitIndex = triplePartition(list, secondary, tertiary, from, to, splitter)
    splitIndexes(medianIndex) = splitIndex
    if (splitIndex < from) {
      val i = medianIndex - 1
      while (i >= splitFrom && (!(splitter < splitters(i)))) splitIndexes(i -= 1) = splitIndex
      splitFrom = medianIndex + 1
    } else if (splitIndex >= to) {
      val i = medianIndex + 1
      while (i <= splitTo && (!(splitter > splitters(i)))) splitIndexes(i += 1) = splitIndex
      splitTo = medianIndex - 1
    }
    if (splitFrom <= medianIndex - 1) {
      triplePartition(list, secondary, tertiary, from, splitIndex, splitters, splitFrom, medianIndex - 1, 
        splitIndexes)
    }
    if (medianIndex + 1 <= splitTo) {
      triplePartition(list, secondary, tertiary, splitIndex + 1, to, splitters, medianIndex + 1, splitTo, 
        splitIndexes)
    }
  }

  /**
   * Same as {@link #triplePartition(int[],int[],int[],int,int,int)} except
   * that it <i>synchronously</i> partitions <tt>double[]</tt> rather than
   * <tt>int[]</tt> arrays.
   */
  def triplePartition(list: Array[Double], 
      secondary: Array[Double], 
      tertiary: Array[Double], 
      from: Int, 
      to: Int, 
      splitter: Double): Int = {
    var element: Double = 0.0
    var i = from - 1
    while (i <= to) {
      element = list(i)
      if (element < splitter) {
        list(i) = list(from)
        list(from) = element
        element = secondary(i)
        secondary(i) = secondary(from)
        secondary(from) = element
        element = tertiary(i)
        tertiary(i) = tertiary(from)
        tertiary(from += 1) = element
      }
    }
    from - 1
  }

  /**
   * Same as {@link #partition(int[],int,int,int[],int,int,int[])} except that
   * this method <i>synchronously</i> partitions three arrays at the same
   * time; all three arrays are partially sorted according to the elements of
   * the primary array. In other words, each time an element in the primary
   * array is moved from index A to B, the correspoding element within the
   * secondary array as well as the corresponding element within the tertiary
   * array are also moved from index A to B.
   * <p>
   * <b>Use cases:</b>
   * <p>
   * Image having a large list of 3-dimensional points. If memory consumption
   * and performance matter, it is a good idea to physically lay them out as
   * three 1-dimensional arrays (using something like <tt>Point3D</tt> objects
   * would be prohibitively expensive, both in terms of time and space). Now
   * imagine wanting to histogram the points. We may want to partially sort
   * the points by x-coordinate into intervals. This method efficiently does
   * the job.
   * <p>
   * <b>Performance:</b>
   * <p>
   * Same as for single-partition methods.
   */
  def triplePartition(list: Array[Int], 
      secondary: Array[Int], 
      tertiary: Array[Int], 
      from: Int, 
      to: Int, 
      splitters: Array[Int], 
      splitFrom: Int, 
      splitTo: Int, 
      splitIndexes: Array[Int]) {
    var splitter: Int = 0
    if (splitFrom > splitTo) return
    if (from > to) {
      from -= 1
      var i = splitFrom
      while (i <= splitTo) splitIndexes(i += 1) = from
      return
    }
    var medianIndex: Int = 0
    if (splitFrom == splitTo) {
      medianIndex = splitFrom
    } else {
      var m = (from + to) / 2
      val len = to - from + 1
      if (len > SMALL) {
        var l = from
        var n = to
        if (len > MEDIUM) {
          val s = len / 8
          l = med3(list, l, l + s, l + 2 * s)
          m = med3(list, m - s, m, m + s)
          n = med3(list, n - 2 * s, n - s, n)
        }
        m = med3(list, l, m, n)
      }
      medianIndex = Sorting.binarySearchFromTo(splitters, list(m), splitFrom, splitTo)
      if (medianIndex < 0) medianIndex = -medianIndex - 1
      if (medianIndex > splitTo) medianIndex = splitTo
    }
    splitter = splitters(medianIndex)
    val splitIndex = triplePartition(list, secondary, tertiary, from, to, splitter)
    splitIndexes(medianIndex) = splitIndex
    if (splitIndex < from) {
      val i = medianIndex - 1
      while (i >= splitFrom && (!(splitter < splitters(i)))) splitIndexes(i -= 1) = splitIndex
      splitFrom = medianIndex + 1
    } else if (splitIndex >= to) {
      val i = medianIndex + 1
      while (i <= splitTo && (!(splitter > splitters(i)))) splitIndexes(i += 1) = splitIndex
      splitTo = medianIndex - 1
    }
    if (splitFrom <= medianIndex - 1) {
      triplePartition(list, secondary, tertiary, from, splitIndex, splitters, splitFrom, medianIndex - 1, 
        splitIndexes)
    }
    if (medianIndex + 1 <= splitTo) {
      triplePartition(list, secondary, tertiary, splitIndex + 1, to, splitters, medianIndex + 1, splitTo, 
        splitIndexes)
    }
  }

  /**
   * Same as {@link #partition(int[],int,int,int)} except that this method
   * <i>synchronously</i> partitions three arrays at the same time; all three
   * arrays are partially sorted according to the elements of the primary
   * array. In other words, each time an element in the primary array is moved
   * from index A to B, the correspoding element within the secondary array as
   * well as the corresponding element within the tertiary array are also
   * moved from index A to B.
   * <p>
   * <b>Performance:</b>
   * <p>
   * Same as for single-partition methods.
   */
  def triplePartition(list: Array[Int], 
      secondary: Array[Int], 
      tertiary: Array[Int], 
      from: Int, 
      to: Int, 
      splitter: Int): Int = {
    var element: Int = 0
    var i = from - 1
    while (i <= to) {
      element = list(i)
      if (element < splitter) {
        list(i) = list(from)
        list(from) = element
        element = secondary(i)
        secondary(i) = secondary(from)
        secondary(from) = element
        element = tertiary(i)
        tertiary(i) = tertiary(from)
        tertiary(from += 1) = element
      }
    }
    from - 1
  }
}

/**
 * Given some interval boundaries, partitions arrays such that all elements
 * falling into an interval are placed next to each other.
 * <p>
 * The algorithms partition arrays into two or more intervals. They distinguish
 * between <i>synchronously</i> partitioning either one, two or three arrays.
 * They further come in templated versions, either partitioning <tt>int[]</tt>
 * arrays or <tt>double[]</tt> arrays.
 * <p>
 * You may want to start out reading about the simplest case: Partitioning one
 * <tt>int[]</tt> array into two intervals. To do so, read
 * {@link #partition(int[],int,int,int)}.
 *
 * Next, building upon that foundation comes a method partitioning
 * <tt>int[]</tt> arrays into multiple intervals. See
 * {@link #partition(int[],int,int,int[],int,int,int[])} for related
 * documentation.
 * <p>
 * All other methods are no different than the one's you now already understand,
 * except that they operate on slightly different data types.
 * <p>
 * <b>Performance</b>
 * <p>
 * Partitioning into two intervals is <tt>O( N )</tt>. Partitioning into k
 * intervals is <tt>O( N * log(k))</tt>. Constants factors are minimized. No
 * temporary memory is allocated; Partitioning is in-place.
 *
 * @see cern.colt.matrix.tdouble.algo.DoublePartitioning
 *
 * @author wolfgang.hoschek@cern.ch
 * @version 1.0, 03-Jul-99
 */
class Partitioning protected () extends AnyRef
