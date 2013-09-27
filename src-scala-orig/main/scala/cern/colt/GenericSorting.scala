package cern.colt

import cern.colt.function.tint.IntComparator
import GenericSorting._
//remove if not needed
import scala.collection.JavaConversions._

object GenericSorting {

  private val SMALL = 7

  private val MEDIUM = 40

  /**
   * Transforms two consecutive sorted ranges into a single sorted range. The
   * initial ranges are <code>[first, middle)</code> and
   * <code>[middle, last)</code>, and the resulting range is
   * <code>[first, last)</code>. Elements in the first input range will
   * precede equal elements in the second.
   */
  private def inplace_merge(first: Int,
      middle: Int,
      last: Int,
      comp: IntComparator,
      swapper: Swapper) {
    if (first >= middle || middle >= last) return
    if (last - first == 2) {
      if (comp.compare(middle, first) < 0) {
        swapper.swap(first, middle)
      }
      return
    }
    var firstCut: Int = 0
    var secondCut: Int = 0
    if (middle - first > last - middle) {
      firstCut = first + (middle - first) / 2
      secondCut = lower_bound(middle, last, firstCut, comp)
    } else {
      secondCut = middle + (last - middle) / 2
      firstCut = upper_bound(first, middle, secondCut, comp)
    }
    val first2 = firstCut
    val middle2 = middle
    val last2 = secondCut
    if (middle2 != first2 && middle2 != last2) {
      var first1 = first2
      var last1 = middle2
      while (first1 < last1) {swapper.swap(first1, last1); first1 += 1}
      first1 = middle2
      last1 = last2
      while (first1 < last1) {swapper.swap(first1, last1); first1 += 1}
      first1 = first2
      last1 = last2
      while (first1 < last1) {swapper.swap(first1, last1); first1 += 1}
    }
    val newMiddle = firstCut + (secondCut - middle)
    inplace_merge(first, firstCut, newMiddle, comp, swapper)
    inplace_merge(newMiddle, secondCut, last, comp, swapper)
  }

  /**
   * Performs a binary search on an already-sorted range: finds the first
   * position where an element can be inserted without violating the ordering.
   * Sorting is by a user-supplied comparison function.
   *
   * @param firstP
   *            Beginning of the range.
   * @param lastP
   *            One past the end of the range.
   * @param x
   *            Element to be searched for.
   * @param comp
   *            Comparison function.
   * @return The largest index i such that, for every j in the range
   *         <code>[first, i)</code>, <code>comp.apply(array[j], x)</code> is
   *         <code>true</code>.
   * @see Sorting#upper_bound
   * @see Sorting#equal_range
   * @see Sorting#binary_search
   */
  private def lower_bound(firstP: Int,
      lastP: Int,
      x: Int,
      comp: IntComparator): Int = {
    var first = firstP
    var last = lastP
    var len = last - first
    while (len > 0) {
      val half = len / 2
      val middle = first + half
      if (comp.compare(middle, x) < 0) {
        first = middle + 1
        len -= half + 1
      } else {
        len = half
      }
    }
    first
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
   * Sorts the specified range of elements according to the order induced by
   * the specified comparator. All elements in the range must be <i>mutually
   * comparable</i> by the specified comparator (that is,
   * <tt>c.compare(a, b)</tt> must not throw an exception for any indexes
   * <tt>a</tt> and <tt>b</tt> in the range).
   * <p>
   *
   * This sort is guaranteed to be <i>stable</i>: equal elements will not be
   * reordered as a result of the sort.
   * <p>
   *
   * The sorting algorithm is a modified mergesort (in which the merge is
   * omitted if the highest element in the low sublist is less than the lowest
   * element in the high sublist). This algorithm offers guaranteed n*log(n)
   * performance, and can approach linear performance on nearly sorted lists.
   *
   * @param fromIndex
   *            the index of the first element (inclusive) to be sorted.
   * @param toIndex
   *            the index of the last element (exclusive) to be sorted.
   * @param c
   *            the comparator to determine the order of the generic data.
   * @param swapper
   *            an object that knows how to swap the elements at any two
   *            indexes (a,b).
   *
   * @see IntComparator
   * @see Swapper
   */
  def mergeSort(fromIndex: Int,
      toIndex: Int,
      c: IntComparator,
      swapper: Swapper) {
    val length = toIndex - fromIndex
    if (length < SMALL) {
      for (i <- fromIndex until toIndex) {
        var j = i
        while (j > fromIndex && (c.compare(j - 1, j) > 0)) {
          swapper.swap(j, j - 1)
          j -= 1
        }
      }
      return
    }
    val mid = (fromIndex + toIndex) / 2
    mergeSort(fromIndex, mid, c, swapper)
    mergeSort(mid, toIndex, c, swapper)
    if (c.compare(mid - 1, mid) <= 0) return
    inplace_merge(fromIndex, mid, toIndex, c, swapper)
  }

  /**
   * Sorts the specified range of elements according to the order induced by
   * the specified comparator. All elements in the range must be <i>mutually
   * comparable</i> by the specified comparator (that is,
   * <tt>c.compare(a, b)</tt> must not throw an exception for any indexes
   * <tt>a</tt> and <tt>b</tt> in the range).
   * <p>
   *
   * The sorting algorithm is a tuned quicksort, adapted from Jon L. Bentley
   * and M. Douglas McIlroy's "Engineering a Sort Function", Software-Practice
   * and Experience, Vol. 23(11) P. 1249-1265 (November 1993). This algorithm
   * offers n*log(n) performance on many data sets that cause other quicksorts
   * to degrade to quadratic performance.
   *
   * @param fromIndex
   *            the index of the first element (inclusive) to be sorted.
   * @param toIndex
   *            the index of the last element (exclusive) to be sorted.
   * @param c
   *            the comparator to determine the order of the generic data.
   * @param swapper
   *            an object that knows how to swap the elements at any two
   *            indexes (a,b).
   *
   * @see IntComparator
   * @see Swapper
   */
  def quickSort(fromIndex: Int,
      toIndex: Int,
      c: IntComparator,
      swapper: Swapper) {
    quickSort1(fromIndex, toIndex - fromIndex, c, swapper)
  }

  /**
   * Sorts the specified sub-array into ascending order.
   */
  private def quickSort1(off: Int,
      len: Int,
      comp: IntComparator,
      swapper: Swapper) {
    if (len < SMALL) {
      for (i <- off until len + off) {
        var j = i
        while (j > off && (comp.compare(j - 1, j) > 0)) {
          swapper.swap(j, j - 1)
          j -= 1
        }
      }
      return
    }
    var m = off + len / 2
    if (len > SMALL) {
      var l = off
      var n = off + len - 1
      if (len > MEDIUM) {
        val s = len / 8
        l = med3(l, l + s, l + 2 * s, comp)
        m = med3(m - s, m, m + s, comp)
        n = med3(n - 2 * s, n - s, n, comp)
      }
      m = med3(l, m, n, comp)
    }
    var a = off
    var b = a
    var c = off + len - 1
    var d = c
    while (true) {
      var comparison: Int = 0
      comparison = comp.compare(b, m)
      while (b <= c && comparison <= 0) {
        if (comparison == 0) {
          if (a == m) m = b else if (b == m) m = a
          swapper.swap(a, b)
          a += 1
        }
        b += 1
        comparison = comp.compare(b, m)
      }
      comparison = comp.compare(c, m)
      while (c >= b && comparison >= 0) {
        if (comparison == 0) {
          if (c == m) m = d else if (d == m) m = c
          swapper.swap(c, d)
          d -= 1
        }
        c -= 1
        comparison = comp.compare(c, m)
      }
      if (b > c) //break
      if (b == m) m = d else if (c == m) m = c
      swapper.swap(b, c)
      b += 1
      c -= 1
    }
    var s: Int = 0
    var n = off + len
    s = Math.min(a - off, b - a)
    vecswap(swapper, off, b - s, s)
    s = Math.min(d - c, n - d - 1)
    vecswap(swapper, b, n - s, s)
    if (b - a > 1) { s = b-a; quickSort1(off, s, comp, swapper)}
    if (d - c > 1) { s = d-c; quickSort1(n - s, s, comp, swapper)}
  }

  /**
   * Reverses a sequence of elements.
   *
   * @param firstP
   *            Beginning of the range
   * @param last
   *            One past the end of the range
   * @throws ArrayIndexOutOfBoundsException
   *                If the range is invalid.
   */
  private def reverse(firstP: Int, last: Int, swapper: Swapper) {
    var first = firstP
    while (first < last) {
      swapper.swap(first, last)
      first += 1
    }
  }

  /**
   * Rotate a range in place: <code>array[middle]</code> is put in
   * <code>array[first]</code>, <code>array[middle+1]</code> is put in
   * <code>array[first+1]</code>, etc. Generally, the element in position
   * <code>i</code> is put into position
   * <code>(i + (last-middle)) % (last-first)</code>.
   *
   * @param first
   *            Beginning of the range
   * @param middle
   *            Index of the element that will be put in
   *            <code>array[first]</code>
   * @param last
   *            One past the end of the range
   */
  private def rotate(first: Int,
      middle: Int,
      last: Int,
      swapper: Swapper) {
    if (middle != first && middle != last) {
      reverse(first, middle, swapper)
      reverse(middle, last, swapper)
      reverse(first, last, swapper)
    }
  }

  /**
   * Performs a binary search on an already-sorted range: finds the last
   * position where an element can be inserted without violating the ordering.
   * Sorting is by a user-supplied comparison function.
   *
   * @param firstP
   *            Beginning of the range.
   * @param last
   *            One past the end of the range.
   * @param x
   *            Element to be searched for.
   * @param comp
   *            Comparison function.
   * @return The largest index i such that, for every j in the range
   *         <code>[first, i)</code>, <code>comp.apply(x, array[j])</code> is
   *         <code>false</code>.
   * @see Sorting#lower_bound
   * @see Sorting#equal_range
   * @see Sorting#binary_search
   */
  private def upper_bound(firstP: Int,
      last: Int,
      x: Int,
      comp: IntComparator): Int = {
    var first = firstP
    var len = last - first
    while (len > 0) {
      val half = len / 2
      val middle = first + half
      if (comp.compare(x, middle) < 0) {
        len = half
      } else {
        first = middle + 1
        len -= half + 1
      }
    }
    first
  }

  /**
   * Swaps x[a .. (a+n-1)] with x[b .. (b+n-1)].
   */
  private def vecswap(swapper: Swapper,
      aP: Int,
      bP: Int,
      n: Int) {
    var i = 0
    var a = aP
    var b = bP
    while (i < n) {
      swapper.swap(a, b)
      i += 1
      a += 1
      b += 1
    }
  }
}

/**
 * Generically sorts arbitrary shaped data (for example multiple arrays, 1,2 or
 * 3-d matrices, and so on) using a quicksort or mergesort. This class addresses
 * two problems, namely
 * <ul>
 * <li><i>Sorting multiple arrays in sync</i>
 * <li><i>Sorting by multiple sorting criteria</i> (primary, secondary,
 * tertiary, ...)
 * </ul>
 * <h4>Sorting multiple arrays in sync</h4>
 * <p>
 * Assume we have three arrays X, Y and Z. We want to sort all three arrays by X
 * (or some arbitrary comparison function). For example, we have<br>
 * <tt>X=[3, 2, 1], Y=[3.0, 2.0, 1.0], Z=[6.0, 7.0, 8.0]</tt>. The output should
 * be <tt><br>
 X=[1, 2, 3], Y=[1.0, 2.0, 3.0], Z=[8.0, 7.0, 6.0]</tt>.
 * </p>
 * <p>
 * How can we achive this? Here are several alternatives. We could ...
 * </p>
 * <ol>
 * <li>make a list of Point3D objects, sort the list as desired using a
 * comparison function, then copy the results back into X, Y and Z. The classic
 * object-oriented way.</li>
 * <li>make an index list [0,1,2,...,N-1], sort the index list using a
 * comparison function, then reorder the elements of X,Y,Z as defined by the
 * index list. Reordering cannot be done in-place, so we need to copy X to some
 * temporary array, then copy in the right order back from the temporary into X.
 * Same for Y and Z.</li>
 * <li>use a generic quicksort or mergesort which, whenever two elements in X
 * are swapped, also swaps the corresponding elements in Y and Z.</li>
 * </ol>
 * Alternatives 1 and 2 involve quite a lot of copying and allocate significant
 * amounts of temporary memory. Alternative 3 involves more swapping, more
 * polymorphic message dispatches, no copying and does not need any temporary
 * memory.
 * <p>
 * This class implements alternative 3. It operates on arbitrary shaped data. In
 * fact, it has no idea what kind of data it is sorting. Comparisons and
 * swapping are delegated to user provided objects which know their data and can
 * do the job.
 * <p>
 * Lets call the generic data <tt>g</tt> (it may be one array, three linked
 * lists or whatever). This class takes a user comparison function operating on
 * two indexes <tt>(a,b)</tt>, namely an {@link IntComparator}. The comparison
 * function determines whether <tt>g[a]</tt> is equal, less or greater than
 * <tt>g[b]</tt>. The sort, depending on its implementation, can decide to swap
 * the data at index <tt>a</tt> with the data at index <tt>b</tt>. It calls a
 * user provided {@link cern.colt.Swapper} object that knows how to swap the
 * data of these indexes.
 * <p>
 * The following snippet shows how to solve the problem.
 * <table>
 * <td class="PRE">
 *
 * <pre>
 * final int[] x;
 * final double[] y;
 * final double[] z;
 *
 * x = new int[] { 3, 2, 1 };
 * y = new double[] { 3.0, 2.0, 1.0 };
 * z = new double[] { 6.0, 7.0, 8.0 };
 *
 * // this one knows how to swap two indexes (a,b)
 * Swapper swapper = new Swapper() {
 *     public void swap(int a, int b) {
 *         int t1;
 *         double t2, t3;
 *         t1 = x[a];
 *         x[a] = x[b];
 *         x[b] = t1;
 *         t2 = y[a];
 *         y[a] = y[b];
 *         y[b] = t2;
 *         t3 = z[a];
 *         z[a] = z[b];
 *         z[b] = t3;
 *     }
 * };
 * // simple comparison: compare by X and ignore Y,Z
 * &lt;br&gt;
 * IntComparator comp = new IntComparator() {
 *     public int compare(int a, int b) {
 *         return x[a] == x[b] ? 0 : (x[a] &lt; x[b] ? -1 : 1);
 *     }
 * };
 *
 * System.out.println(&quot;before:&quot;);
 * System.out.println(&quot;X=&quot; + Arrays.toString(x));
 * System.out.println(&quot;Y=&quot; + Arrays.toString(y));
 * System.out.println(&quot;Z=&quot; + Arrays.toString(z));
 *
 * GenericSorting.quickSort(0, X.length, comp, swapper);
 * // GenericSorting.mergeSort(0, X.length, comp, swapper);
 *
 * System.out.println(&quot;after:&quot;);
 * System.out.println(&quot;X=&quot; + Arrays.toString(x));
 * System.out.println(&quot;Y=&quot; + Arrays.toString(y));
 * System.out.println(&quot;Z=&quot; + Arrays.toString(z));
 * </pre>
 *
 * </td>
 * </table>
 * <h4>Sorting by multiple sorting criterias (primary, secondary, tertiary, ...)
 * </h4>
 * <p>
 * Assume again we have three arrays X, Y and Z. Now we want to sort all three
 * arrays, primarily by Y, secondarily by Z (if Y elements are equal). For
 * example, we have<br>
 * <tt>X=[6, 7, 8, 9], Y=[3.0, 2.0, 1.0, 3.0], Z=[5.0, 4.0, 4.0, 1.0]</tt>. The
 * output should be <tt><br>
 X=[8, 7, 9, 6], Y=[1.0, 2.0, 3.0, 3.0], Z=[4.0, 4.0, 1.0, 5.0]</tt>.
 * </p>
 * <p>
 * Here is how to solve the problem. All code in the above example stays the
 * same, except that we modify the comparison function as follows
 * </p>
 * <table>
 * <td class="PRE">
 *
 * <pre>
 * //compare by Y, if that doesn't help, reside to Z
 * IntComparator comp = new IntComparator() {
 *     public int compare(int a, int b) {
 *         if (y[a] == y[b])
 *             return z[a] == z[b] ? 0 : (z[a] &lt; z[b] ? -1 : 1);
 *         return y[a] &lt; y[b] ? -1 : 1;
 *     }
 * };
 * </pre>
 *
 * </td>
 * </table>
 *
 * <h4>Notes</h4>
 * <p>
 * </p>
 * <p>
 * Sorts involving floating point data and not involving comparators, like, for
 * example provided in the JDK {@link java.util.Arrays} and in the Colt
 * {@link cern.colt.Sorting} handle floating point numbers in special ways to
 * guarantee that NaN's are swapped to the end and -0.0 comes before 0.0.
 * Methods delegating to comparators cannot do this. They rely on the
 * comparator. Thus, if such boundary cases are an issue for the application at
 * hand, comparators explicitly need to implement -0.0 and NaN aware
 * comparisons. Remember: <tt>-0.0 < 0.0 == false</tt>,
 * <tt>(-0.0 == 0.0) == true</tt>, as well as
 * <tt>5.0 &lt; Double.NaN == false</tt>, <tt>5.0 &gt; Double.NaN == false</tt>.
 * Same for <tt>float</tt>.
 * <h4>Implementation</h4>
 * <p>
 * The quicksort is a derivative of the JDK 1.2 V1.26 algorithms (which are, in
 * turn, based on Bentley's and McIlroy's fine work). The mergesort is a
 * derivative of the JAL algorithms, with optimisations taken from the JDK
 * algorithms. Both quick and merge sort are "in-place", i.e. do not allocate
 * temporary memory (helper arrays). Mergesort is <i>stable</i> (by definition),
 * while quicksort is not. A stable sort is, for example, helpful, if matrices
 * are sorted successively by multiple columns. It preserves the relative
 * position of equal elements.
 *
 * @see java.util.Arrays
 * @see cern.colt.Sorting
 * @see cern.colt.matrix.tdouble.algo.DoubleSorting
 *
 * @author wolfgang.hoschek@cern.ch
 * @version 1.0, 03-Jul-99
 */
class GenericSorting protected () extends AnyRef
