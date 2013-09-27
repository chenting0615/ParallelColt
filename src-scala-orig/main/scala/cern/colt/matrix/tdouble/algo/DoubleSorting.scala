package cern.colt.matrix.tdouble.algo

import cern.colt.function.tint.IntComparator
import cern.colt.matrix.AbstractFormatter
import cern.colt.matrix.tdouble.DoubleFactory2D
import cern.colt.matrix.tdouble.DoubleFactory3D
import cern.colt.matrix.tdouble.StrideMatrix1D
import cern.colt.matrix.tdouble.StrideMatrix2D
import cern.colt.matrix.tdouble.DoubleMatrix3D
import cern.colt.matrix.tdouble.impl.DenseMatrix1D
import DoubleSorting._
//remove if not needed
import scala.collection.JavaConversions._

object DoubleSorting {

  /**
   * A prefabricated quicksort.
   */
  val quickSort = new DoubleSorting()

  /**
   * A prefabricated mergesort.
   */
  val mergeSort = new DoubleSorting() {

    /**
     *
     */
    private val serialVersionUID = 1L

    protected def runSort(a: Array[Int],
        fromIndex: Int,
        toIndex: Int,
        c: IntComparator) {
      cern.colt.Sorting.mergeSort(a, fromIndex, toIndex, c)
    }

    protected def runSort(fromIndex: Int,
        toIndex: Int,
        c: IntComparator,
        swapper: cern.colt.Swapper) {
      cern.colt.GenericSorting.mergeSort(fromIndex, toIndex, c, swapper)
    }
  }

  /**
   * Compare two values, one of which is assumed to be Double.NaN
   */
  private def compareNaN(a: Double, b: Double): Int = {
    if (a != a) {
      if (b != b) return 0 else return 1
    }
    -1
  }

  /**
   * Demonstrates advanced sorting. Sorts by sum of row.
   */
  def zdemo1() {
    val sort = quickSort
    val matrix = DoubleFactory2D.dense.descending(4, 3)
    val comp = new DoubleMatrix1DComparator() {

      def compare(a: StrideMatrix1D, b: StrideMatrix1D): Int = {
        var as = a.zSum()
        var bs = b.zSum()
        return if (as < bs) -1 else if (as == bs) 0 else 1
      }
    }
    println("unsorted:" + matrix)
    println("sorted  :" + sort.sort(matrix, comp))
  }

  /**
   * Demonstrates advanced sorting. Sorts by sum of slice.
   */
  def zdemo2() {
    val sort = quickSort
    val matrix = DoubleFactory3D.dense.descending(4, 3, 2)
    val comp = new DoubleMatrix2DComparator() {

      def compare(a: StrideMatrix2D, b: StrideMatrix2D): Int = {
        var as = a.zSum()
        var bs = b.zSum()
        return if (as < bs) -1 else if (as == bs) 0 else 1
      }
    }
    println("unsorted:" + matrix)
    println("sorted  :" + sort.sort(matrix, comp))
  }

  /**
   * Demonstrates advanced sorting. Sorts by sinus of cell values.
   */
  def zdemo3() {
    val sort = quickSort
    val values = Array(0.5f, 1.5f, 2.5f, 3.5f)
    val matrix = new DenseMatrix1D(values)
    val comp = new cern.colt.function.tdouble.PrimitiveComparator() {

      def compare(a: Double, b: Double): Int = {
        var as = Math.sin(a)
        var bs = Math.sin(b)
        return if (as < bs) -1 else if (as == bs) 0 else 1
      }
    }
    println("unsorted:" + matrix)
    val sorted = sort.sort(matrix, comp)
    println("sorted  :" + sorted)
    sorted.assign(cern.jet.math.tdouble.DoubleFunctions.sin)
    println("sined  :" + sorted)
  }

  /**
   * Demonstrates applying functions.
   */
  protected def zdemo4() {
    val values1 = Array(0, 1, 2, 3)
    val values2 = Array(0, 2, 4, 6)
    val matrix1 = new DenseMatrix1D(values1)
    val matrix2 = new DenseMatrix1D(values2)
    println("m1:" + matrix1)
    println("m2:" + matrix2)
    matrix1.assign(matrix2, cern.jet.math.tdouble.DoubleFunctions.pow)
    println("applied:" + matrix1)
  }

  /**
   * Demonstrates sorting with precomputation of aggregates (median and sum of
   * logarithms).
   */
  def zdemo5(rows: Int, columns: Int, print: Boolean) {
    val sort = quickSort
    println("\n\n")
    System.out.print("now initializing... ")
    val timer = new cern.colt.Timer().start()
    val F = cern.jet.math.tdouble.DoubleFunctions.functions
    var A = cern.colt.matrix.tdouble.DoubleFactory2D.dense.make(rows, columns)
    A.assign(new cern.jet.random.tdouble.engine.DRand())
    timer.stop().display()
    val B = A.like()
    timer.reset().start()
    System.out.print("now copying... ")
    B.assign(A)
    timer.stop().display()
    timer.reset().start()
    System.out.print("now copying subrange... ")
    B.viewPart(0, 0, rows, columns).assign(A.viewPart(0, 0, rows, columns))
    timer.stop().display()
    timer.reset().start()
    System.out.print("now copying selected... ")
    B.viewSelection(null, null).assign(A.viewSelection(null, null))
    timer.stop().display()
    System.out.print("now sorting - quick version with precomputation... ")
    timer.reset().start()
    A = sort.sort(A, hep.aida.tdouble.bin.DoubleBinFunctions1D.median)
    timer.stop().display()
    if (print) {
      val r = Math.min(rows, 5)
      val funs = Array(hep.aida.tdouble.bin.DoubleBinFunctions1D.median, hep.aida.tdouble.bin.DoubleBinFunctions1D.sumLog, hep.aida.tdouble.bin.DoubleBinFunctions1D.geometricMean)
      val rowNames = Array.ofDim[String](r)
      val columnNames = Array.ofDim[String](columns)
      var i = columns
      while (i >= 0) columnNames(i) = Integer toString i
      var i = r
      while (i >= 0) rowNames(i) = Integer toString i
      println("first part of sorted result = \n" +
        new cern.colt.matrix.tdouble.algo.DoubleFormatter("%G")
        .toTitleString(A.viewPart(0, 0, r, columns), rowNames, columnNames, null, null, null, funs))
    }
    System.out.print("now sorting - slow version... ")
    A = B
    val fun = new cern.colt.matrix.tdouble.algo.DoubleMatrix1DComparator() {

      def compare(x: StrideMatrix1D, y: StrideMatrix1D): Int = {
        var a = cern.colt.matrix.tdouble.algo.DoubleStatistic.bin(x)
          .median()
        var b = cern.colt.matrix.tdouble.algo.DoubleStatistic.bin(y)
          .median()
        return if (a < b) -1 else if ((a == b)) 0 else 1
      }
    }
    timer.reset().start()
    A = sort.sort(A, fun)
    timer.stop().display()
  }

  /**
   * Demonstrates advanced sorting. Sorts by sum of row.
   */
  def zdemo6() {
    val values = Array(Array(3, 7, 0), Array(2, 1, 0), Array(2, 2, 0), Array(1, 8, 0), Array(2, 5, 0), Array(7, 0, 0), Array(2, 3, 0), Array(1, 0, 0), Array(4, 0, 0), Array(2, 0, 0))
    val A = DoubleFactory2D.dense.make(values)
    var B: StrideMatrix2D = null
    var C: StrideMatrix2D = null
    println("\n\nunsorted:" + A)
    B = quickSort.sort(A, 1)
    C = quickSort.sort(B, 0)
    println("quick sorted  :" + C)
    B = mergeSort.sort(A, 1)
    C = mergeSort.sort(B, 0)
    println("merge sorted  :" + C)
  }

  /**
   * Demonstrates sorting with precomputation of aggregates, comparing
   * mergesort with quicksort.
   */
  def zdemo7(rows: Int, columns: Int, print: Boolean) {
    println("\n\n")
    println("now initializing... ")
    val F = cern.jet.math.tdouble.DoubleFunctions.functions
    val A = cern.colt.matrix.tdouble.DoubleFactory2D.dense.make(rows, columns)
    A.assign(new cern.jet.random.tdouble.engine.DRand())
    val v1 = A.viewColumn(0).toArray()
    val v2 = A.viewColumn(0).toArray()
    System.out.print("now quick sorting... ")
    val timer = new cern.colt.Timer().start()
    quickSort.sort(A, 0)
    timer.stop().display()
    System.out.print("now merge sorting... ")
    timer.reset().start()
    mergeSort.sort(A, 0)
    timer.stop().display()
    System.out.print("now quick sorting with simple aggregation... ")
    timer.reset().start()
    quickSort.sort(A, v1)
    timer.stop().display()
    System.out.print("now merge sorting with simple aggregation... ")
    timer.reset().start()
    mergeSort.sort(A, v2)
    timer.stop().display()
  }

  def zdemo8(size: Int) {
    println("\n\n")
    println("now initializing... ")
    val F = cern.jet.math.tdouble.DoubleFunctions.functions
    val A = cern.colt.matrix.tdouble.DoubleFactory1D.dense.random(size)
    System.out.print("now quick sorting... ")
    val timer = new cern.colt.Timer().start()
    quickSort.sort(A)
    timer.stop().display()
    System.out.print("now merge sorting... ")
    timer.reset().start()
    mergeSort.sort(A)
    timer.stop().display()
  }

  def main(args: Array[String]) {
    edu.emory.mathcs.utils.ConcurrencyUtils.setNumberOfThreads(2)
    zdemo8(10000000)
    System.exit(0)
  }
}

/**
 * Matrix quicksorts and mergesorts. Use idioms like
 * <tt>Sorting.quickSort.sort(...)</tt> and <tt>Sorting.mergeSort.sort(...)</tt>
 * .
 * <p>
 * This is another case demonstrating one primary goal of this library:
 * Delivering easy to use, yet very efficient APIs. The sorts return convenient
 * <i>sort views</i>. This enables the usage of algorithms which scale well with
 * the problem size: For example, sorting a 1000000 x 10000 or a 1000000 x 100 x
 * 100 matrix performs just as fast as sorting a 1000000 x 1 matrix. This is so,
 * because internally the algorithms only move around integer indexes, they do
 * not physically move around entire rows or slices. The original matrix is left
 * unaffected.
 * <p>
 * The quicksort is a derivative of the JDK 1.2 V1.26 algorithms (which are, in
 * turn, based on Bentley's and McIlroy's fine work). The mergesort is a
 * derivative of the JAL algorithms, with optimisations taken from the JDK
 * algorithms. Mergesort is <i>stable</i> (by definition), while quicksort is
 * not. A stable sort is, for example, helpful, if matrices are sorted
 * successively by multiple columns. It preserves the relative position of equal
 * elements.
 *
 * @see cern.colt.GenericSorting
 * @see cern.colt.Sorting
 * @see java.util.Arrays
 *
 * @author wolfgang.hoschek@cern.ch
 * @version 1.1, 25/May/2000
 *
 * @author Piotr Wendykier (piotr.wendykier@gmail.com)
 *
 */
@SerialVersionUID(1L)
class DoubleSorting protected () extends cern.colt.PersistentObject {

  protected def runSort(a: Array[Int],
      fromIndex: Int,
      toIndex: Int,
      c: IntComparator) {
    cern.colt.Sorting.parallelQuickSort(a, fromIndex, toIndex, c)
  }

  protected def runSort(fromIndex: Int,
      toIndex: Int,
      c: IntComparator,
      swapper: cern.colt.Swapper) {
    cern.colt.GenericSorting.quickSort(fromIndex, toIndex, c, swapper)
  }

  /**
   * Sorts the vector into ascending order, according to the <i>natural
   * ordering</i>. The returned view is backed by this matrix, so changes in
   * the returned view are reflected in this matrix, and vice-versa. To sort
   * ranges use sub-ranging views. To sort descending, use flip views ...
   * <p>
   * <b>Example:</b>
   * <table border="1" cellspacing="0">
   * <tr nowrap>
   * <td valign="top"><tt> 7, 1, 3, 1<br>
   </tt></td>
   * <td valign="top">
   * <p>
   * <tt> ==&gt; 1, 1, 3, 7<br>
   The vector IS NOT SORTED.<br>
   The new VIEW IS SORTED.</tt>
   * </p>
   * </td>
   * </tr>
   * </table>
   *
   * @param vector
   *            the vector to be sorted.
   * @return a new sorted vector (matrix) view. <b>Note that the original
   *         matrix is left unaffected.</b>
   */
  def sort(vector: StrideMatrix1D): StrideMatrix1D = vector.viewSelection(sortIndex(vector))

  /**
   * Sorts indexes of the <code>vector</code> into ascending order.
   *
   * @param vector
   * @return sorted indexes
   */
  def sortIndex(vector: StrideMatrix1D): Array[Int] = {
    val indexes = Array.ofDim[Int](vector.size.toInt)
    var i = indexes.length
    while (i >= 0) indexes(i) = i
    var comp: IntComparator = null
    if (vector.isInstanceOf[DenseMatrix1D]) {
      val velems = vector.elements().asInstanceOf[Array[Double]]
      val zero = vector.index(0).toInt
      val stride = vector.stride()
      comp = new IntComparator() {

        def compare(a: Int, b: Int): Int = {
          var idxa = zero + a * stride
          var idxb = zero + b * stride
          var av = velems(idxa)
          var bv = velems(idxb)
          if (av != av || bv != bv) return compareNaN(av, bv)
          return if (av < bv) -1 else (if (av == bv) 0 else 1)
        }
      }
    } else {
      comp = new IntComparator() {

        def compare(a: Int, b: Int): Int = {
          var av = vector.getQuick(a)
          var bv = vector.getQuick(b)
          if (av != av || bv != bv) return compareNaN(av, bv)
          return if (av < bv) -1 else (if (av == bv) 0 else 1)
        }
      }
    }
    runSort(indexes, 0, indexes.length, comp)
    indexes
  }

  /**
   * Multithreaded method that sorts indexes of the <code>vector</code>
   * according to the comparator <code>comp</code>.
   *
   * @param vector
   * @param comp
   * @return sorted indexes
   */
  def sortIndex(vector: StrideMatrix1D, comp: IntComparator): Array[Int] = {
    val indexes = Array.ofDim[Int](vector.size.toInt)
    var i = indexes.length
    while (i >= 0) indexes(i) = i
    runSort(indexes, 0, indexes.length, comp)
    indexes
  }

  /**
   * Sorts the vector into ascending order, according to the order induced by
   * the specified comparator. The returned view is backed by this matrix, so
   * changes in the returned view are reflected in this matrix, and
   * vice-versa. The algorithm compares two cells at a time, determinining
   * whether one is smaller, equal or larger than the other. To sort ranges
   * use sub-ranging views. To sort descending, use flip views ...
   * <p>
   * <b>Example:</b>
   *
   * <pre>
   * // sort by sinus of cells
   * DoubleComparator comp = new DoubleComparator() {
   *     public int compare(double a, double b) {
   *         double as = Math.sin(a);
   *         double bs = Math.sin(b);
   *         return as &lt; bs ? -1 : as == bs ? 0 : 1;
   *     }
   * };
   * sorted = quickSort(vector, comp);
   * </pre>
   *
   * @param vector
   *            the vector to be sorted.
   * @param c
   *            the comparator to determine the order.
   * @return a new matrix view sorted as specified. <b>Note that the original
   *         vector (matrix) is left unaffected.</b>
   */
  def sort(vector: StrideMatrix1D, c: cern.colt.function.tdouble.PrimitiveComparator): StrideMatrix1D = {
    vector.viewSelection(sortIndex(vector, c))
  }

  /**
   * Sorts indexes of the <code>vector</code> according to the comparator
   * <code>c</code>.
   *
   * @param vector
   * @param c
   * @return sorted indexes
   */
  def sortIndex(vector: StrideMatrix1D, c: cern.colt.function.tdouble.PrimitiveComparator): Array[Int] = {
    val indexes = Array.ofDim[Int](vector.size.toInt)
    var i = indexes.length
    while (i >= 0) indexes(i) = i
    var comp: IntComparator = null
    if (vector.isInstanceOf[DenseMatrix1D]) {
      val velems = vector.elements().asInstanceOf[Array[Double]]
      val zero = vector.index(0).toInt
      val stride = vector.stride()
      comp = new IntComparator() {

        def compare(a: Int, b: Int): Int = {
          var idxa = zero + a * stride
          var idxb = zero + b * stride
          return c.compare(velems(idxa), velems(idxb))
        }
      }
    } else {
      comp = new IntComparator() {

        def compare(a: Int, b: Int): Int = {
          return c.compare(vector.getQuick(a), vector.getQuick(b))
        }
      }
    }
    runSort(indexes, 0, indexes.length, comp)
    indexes
  }

  /**
   * Sorts the matrix rows into ascending order, according to the <i>natural
   * ordering</i> of the matrix values in the virtual column
   * <tt>aggregates</tt>; Particularly efficient when comparing expensive
   * aggregates, because aggregates need not be recomputed time and again, as
   * is the case for comparator based sorts. Essentially, this algorithm makes
   * expensive comparisons cheap. Normally each element of <tt>aggregates</tt>
   * is a summary measure of a row. Speedup over comparator based sorting =
   * <tt>2*log(rows)</tt>, on average. For this operation, quicksort is
   * usually faster.
   * <p>
   * The returned view is backed by this matrix, so changes in the returned
   * view are reflected in this matrix, and vice-versa. To sort ranges use
   * sub-ranging views. To sort columns by rows, use dice views. To sort
   * descending, use flip views ...
   * <p>
   * <b>Example:</b> Each aggregate is the sum of a row
   * <table border="1" * cellspacing="0">
   * <tr nowrap>
   * <td valign="top"><tt>4 x 2 matrix: <br>
   1, 1<br>
   5, 4<br>
   3, 0<br>
   4, 4 <br>
   </tt></td>
   * <td align="left" valign="top"> <tt>aggregates=<br>
   2<br>
   9<br>
   3<br>
   8<br>
   ==></tt></td>
   * <td valign="top">
   * <p>
   * <tt>4 x 2 matrix:<br>
   1, 1<br>
   3, 0<br>
   4, 4<br>
   5, 4</tt><br>
   * The matrix IS NOT SORTED.<br>
   * The new VIEW IS SORTED.
   * </p>
   * </td>
   * </tr>
   * </table>
   *
   * <table>
   * <td class="PRE">
   *
   * <pre>
   * // sort 10000 x 1000 matrix by sum of logarithms in a row (i.e. by geometric mean)
   * DoubleMatrix2D matrix = new DenseDoubleMatrix2D(10000, 1000);
   * matrix.assign(new cern.jet.random.engine.MersenneTwister()); // initialized randomly
   * cern.jet.math.Functions F = cern.jet.math.Functions.functions; // alias for convenience
   *
   * // THE QUICK VERSION (takes some 3 secs)
   * // aggregates[i] = Sum(log(row));
   * double[] aggregates = new double[matrix.rows()];
   * for (int i = matrix.rows(); --i &gt;= 0;)
   *     aggregates[i] = matrix.viewRow(i).aggregate(F.plus, F.log);
   * DoubleMatrix2D sorted = quickSort(matrix, aggregates);
   *
   * // THE SLOW VERSION (takes some 90 secs)
   * DoubleMatrix1DComparator comparator = new DoubleMatrix1DComparator() {
   *     public int compare(DoubleMatrix1D x, DoubleMatrix1D y) {
   *         double a = x.aggregate(F.plus, F.log);
   *         double b = y.aggregate(F.plus, F.log);
   *         return a &lt; b ? -1 : a == b ? 0 : 1;
   *     }
   * };
   * DoubleMatrix2D sorted = quickSort(matrix, comparator);
   * </pre>
   *
   * </td>
   * </table>
   *
   * @param matrix
   *            the matrix to be sorted.
   * @param aggregates
   *            the values to sort on. (As a side effect, this array will also
   *            get sorted).
   * @return a new matrix view having rows sorted. <b>Note that the original
   *         matrix is left unaffected.</b>
   * @throws IndexOutOfBoundsException
   *             if <tt>aggregates.length != matrix.rows()</tt>.
   */
  def sort(matrix: StrideMatrix2D, aggregates: Array[Double]): StrideMatrix2D = {
    val rows = matrix.rows()
    if (aggregates.length != rows) throw new IndexOutOfBoundsException("aggregates.length != matrix.rows()")
    val indexes = Array.ofDim[Int](rows)
    var i = rows
    while (i >= 0) indexes(i) = i
    val comp = new cern.colt.function.tint.IntComparator() {

      def compare(x: Int, y: Int): Int = {
        var a = aggregates(x)
        var b = aggregates(y)
        if (a != a || b != b) return compareNaN(a, b)
        return if (a < b) -1 else if ((a == b)) 0 else 1
      }
    }
    val swapper = new cern.colt.Swapper() {

      def swap(x: Int, y: Int) {
        var t1: Int = 0
        var t2: Double = 0.0
        t1 = indexes(x)
        indexes(x) = indexes(y)
        indexes(y) = t1
        t2 = aggregates(x)
        aggregates(x) = aggregates(y)
        aggregates(y) = t2
      }
    }
    runSort(0, rows, comp, swapper)
    matrix.viewSelection(indexes, null)
  }

  /**
   * Sorts the matrix rows into ascending order, according to the <i>natural
   * ordering</i> of the matrix values in the given column. The returned view
   * is backed by this matrix, so changes in the returned view are reflected
   * in this matrix, and vice-versa. To sort ranges use sub-ranging views. To
   * sort columns by rows, use dice views. To sort descending, use flip views
   * ...
   * <p>
   * <b>Example:</b>
   * <table border="1" cellspacing="0">
   * <tr nowrap>
   * <td valign="top"><tt>4 x 2 matrix: <br>
   7, 6<br>
   5, 4<br>
   3, 2<br>
   1, 0 <br>
   </tt></td>
   * <td align="left" valign="top">
   * <p>
   * <tt>column = 0;<br>
   view = quickSort(matrix,column);<br>
   System.out.println(view); </tt><tt><br>
   ==> </tt>
   * </p>
   * </td>
   * <td valign="top">
   * <p>
   * <tt>4 x 2 matrix:<br>
   1, 0<br>
   3, 2<br>
   5, 4<br>
   7, 6</tt><br>
   * The matrix IS NOT SORTED.<br>
   * The new VIEW IS SORTED.
   * </p>
   * </td>
   * </tr>
   * </table>
   *
   * @param matrix
   *            the matrix to be sorted.
   * @param column
   *            the index of the column inducing the order.
   * @return a new matrix view having rows sorted by the given column. <b>Note
   *         that the original matrix is left unaffected.</b>
   * @throws IndexOutOfBoundsException
   *             if <tt>column < 0 || column >= matrix.columns()</tt>.
   */
  def sort(matrix: StrideMatrix2D, column: Int): StrideMatrix2D = {
    if (column < 0 || column >= matrix.columns()) throw new IndexOutOfBoundsException("column=" + column + ", matrix=" + AbstractFormatter.shape(matrix))
    val rowIndexes = Array.ofDim[Int](matrix.rows())
    var i = rowIndexes.length
    while (i >= 0) rowIndexes(i) = i
    val col = matrix.viewColumn(column)
    val comp = new IntComparator() {

      def compare(a: Int, b: Int): Int = {
        var av = col.getQuick(a)
        var bv = col.getQuick(b)
        if (av != av || bv != bv) return compareNaN(av, bv)
        return if (av < bv) -1 else (if (av == bv) 0 else 1)
      }
    }
    runSort(rowIndexes, 0, rowIndexes.length, comp)
    matrix.viewSelection(rowIndexes, null)
  }

  /**
   * Sorts the matrix rows according to the order induced by the specified
   * comparator. The returned view is backed by this matrix, so changes in the
   * returned view are reflected in this matrix, and vice-versa. The algorithm
   * compares two rows (1-d matrices) at a time, determinining whether one is
   * smaller, equal or larger than the other. To sort ranges use sub-ranging
   * views. To sort columns by rows, use dice views. To sort descending, use
   * flip views ...
   * <p>
   * <b>Example:</b>
   *
   * <pre>
   * // sort by sum of values in a row
   * DoubleMatrix1DComparator comp = new DoubleMatrix1DComparator() {
   *     public int compare(DoubleMatrix1D a, DoubleMatrix1D b) {
   *         double as = a.zSum();
   *         double bs = b.zSum();
   *         return as &lt; bs ? -1 : as == bs ? 0 : 1;
   *     }
   * };
   * sorted = quickSort(matrix, comp);
   * </pre>
   *
   * @param matrix
   *            the matrix to be sorted.
   * @param c
   *            the comparator to determine the order.
   * @return a new matrix view having rows sorted as specified. <b>Note that
   *         the original matrix is left unaffected.</b>
   */
  def sort(matrix: StrideMatrix2D, c: DoubleMatrix1DComparator): StrideMatrix2D = {
    val rowIndexes = Array.ofDim[Int](matrix.rows())
    var i = rowIndexes.length
    while (i >= 0) rowIndexes(i) = i
    val views = Array.ofDim[StrideMatrix1D](matrix.rows())
    var i = views.length
    while (i >= 0) views(i) = matrix.viewRow(i)
    val comp = new IntComparator() {

      def compare(a: Int, b: Int): Int = return c.compare(views(a), views(b))
    }
    runSort(rowIndexes, 0, rowIndexes.length, comp)
    matrix.viewSelection(rowIndexes, null)
  }

  /**
   * Sorts the matrix rows into ascending order, according to the <i>natural
   * ordering</i> of the values computed by applying the given aggregation
   * function to each row; Particularly efficient when comparing expensive
   * aggregates, because aggregates need not be recomputed time and again, as
   * is the case for comparator based sorts. Essentially, this algorithm makes
   * expensive comparisons cheap. Normally <tt>aggregates</tt> defines a
   * summary measure of a row. Speedup over comparator based sorting =
   * <tt>2*log(rows)</tt>, on average.
   * <p>
   * The returned view is backed by this matrix, so changes in the returned
   * view are reflected in this matrix, and vice-versa. To sort ranges use
   * sub-ranging views. To sort columns by rows, use dice views. To sort
   * descending, use flip views ...
   * <p>
   * <b>Example:</b> Each aggregate is the sum of a row
   * <table border="1" * cellspacing="0">
   * <tr nowrap>
   * <td valign="top"><tt>4 x 2 matrix: <br>
   1, 1<br>
   5, 4<br>
   3, 0<br>
   4, 4 <br>
   </tt></td>
   * <td align="left" valign="top"> <tt>aggregates=<br>
   hep.aida.bin.BinFunctions1D.sum<br>
   ==></tt></td>
   * <td valign="top">
   * <p>
   * <tt>4 x 2 matrix:<br>
   1, 1<br>
   3, 0<br>
   4, 4<br>
   5, 4</tt><br>
   * The matrix IS NOT SORTED.<br>
   * The new VIEW IS SORTED.
   * </p>
   * </td>
   * </tr>
   * </table>
   *
   * <table>
   * <td class="PRE">
   *
   * <pre>
   * // sort 10000 x 1000 matrix by median or by sum of logarithms in a row (i.e. by geometric mean)
   * DoubleMatrix2D matrix = new DenseDoubleMatrix2D(10000, 1000);
   * matrix.assign(new cern.jet.random.engine.MersenneTwister()); // initialized randomly
   * cern.jet.math.Functions F = cern.jet.math.Functions.functions; // alias for convenience
   *
   * // THE QUICK VERSION (takes some 10 secs)
   * DoubleMatrix2D sorted = quickSort(matrix, hep.aida.bin.BinFunctions1D.median);
   * //DoubleMatrix2D sorted = quickSort(matrix,hep.aida.bin.BinFunctions1D.sumOfLogarithms);
   *
   * // THE SLOW VERSION (takes some 300 secs)
   * DoubleMatrix1DComparator comparator = new DoubleMatrix1DComparator() {
   *     public int compare(DoubleMatrix1D x, DoubleMatrix1D y) {
   *         double a = cern.colt.matrix.doublealgo.Statistic.bin(x).median();
   *         double b = cern.colt.matrix.doublealgo.Statistic.bin(y).median();
   *         // double a = x.aggregate(F.plus,F.log);
   *         // double b = y.aggregate(F.plus,F.log);
   *         return a &lt; b ? -1 : a == b ? 0 : 1;
   *     }
   * };
   * DoubleMatrix2D sorted = quickSort(matrix, comparator);
   * </pre>
   *
   * </td>
   * </table>
   *
   * @param matrix
   *            the matrix to be sorted.
   * @param aggregate
   *            the function to sort on; aggregates values in a row.
   * @return a new matrix view having rows sorted. <b>Note that the original
   *         matrix is left unaffected.</b>
   */
  def sort(matrix: StrideMatrix2D, aggregate: hep.aida.tdouble.bin.DoubleBinFunction1D): StrideMatrix2D = {
    val tmp = matrix.like(1, matrix.rows())
    val func = Array(aggregate)
    DoubleStatistic.aggregate(matrix.viewDice(), func, tmp)
    val aggr = tmp.viewRow(0).toArray()
    sort(matrix, aggr)
  }

  /**
   * Sorts the matrix slices into ascending order, according to the <i>natural
   * ordering</i> of the matrix values in the given <tt>[row,column]</tt>
   * position. The returned view is backed by this matrix, so changes in the
   * returned view are reflected in this matrix, and vice-versa. To sort
   * ranges use sub-ranging views. To sort by other dimensions, use dice
   * views. To sort descending, use flip views ...
   * <p>
   * The algorithm compares two 2-d slices at a time, determinining whether
   * one is smaller, equal or larger than the other. Comparison is based on
   * the cell <tt>[row,column]</tt> within a slice. Let <tt>A</tt> and
   * <tt>B</tt> be two 2-d slices. Then we have the following rules
   * <ul>
   * <li><tt>A &lt;  B  iff A.get(row,column) &lt;  B.get(row,column)</tt>
   * <li><tt>A == B iff A.get(row,column) == B.get(row,column)</tt>
   * <li><tt>A &gt;  B  iff A.get(row,column) &gt;  B.get(row,column)</tt>
   * </ul>
   *
   * @param matrix
   *            the matrix to be sorted.
   * @param row
   *            the index of the row inducing the order.
   * @param column
   *            the index of the column inducing the order.
   * @return a new matrix view having slices sorted by the values of the slice
   *         view <tt>matrix.viewRow(row).viewColumn(column)</tt>. <b>Note
   *         that the original matrix is left unaffected.</b>
   * @throws IndexOutOfBoundsException
   *             if
   *             <tt>row < 0 || row >= matrix.rows() || column < 0 || column >= matrix.columns()</tt>
   *             .
   */
  def sort(matrix: DoubleMatrix3D, row: Int, column: Int): DoubleMatrix3D = {
    if (row < 0 || row >= matrix.rows()) throw new IndexOutOfBoundsException("row=" + row + ", matrix=" + AbstractFormatter.shape(matrix))
    if (column < 0 || column >= matrix.columns()) throw new IndexOutOfBoundsException("column=" + column + ", matrix=" + AbstractFormatter.shape(matrix))
    val sliceIndexes = Array.ofDim[Int](matrix.slices())
    var i = sliceIndexes.length
    while (i >= 0) sliceIndexes(i) = i
    val sliceView = matrix.viewRow(row).viewColumn(column)
    val comp = new IntComparator() {

      def compare(a: Int, b: Int): Int = {
        var av = sliceView.getQuick(a)
        var bv = sliceView.getQuick(b)
        if (av != av || bv != bv) return compareNaN(av, bv)
        return if (av < bv) -1 else (if (av == bv) 0 else 1)
      }
    }
    runSort(sliceIndexes, 0, sliceIndexes.length, comp)
    matrix.viewSelection(sliceIndexes, null, null)
  }

  /**
   * Sorts the matrix slices according to the order induced by the specified
   * comparator. The returned view is backed by this matrix, so changes in the
   * returned view are reflected in this matrix, and vice-versa. The algorithm
   * compares two slices (2-d matrices) at a time, determinining whether one
   * is smaller, equal or larger than the other. To sort ranges use
   * sub-ranging views. To sort by other dimensions, use dice views. To sort
   * descending, use flip views ...
   * <p>
   * <b>Example:</b>
   *
   * <pre>
   * // sort by sum of values in a slice
   * DoubleMatrix2DComparator comp = new DoubleMatrix2DComparator() {
   *     public int compare(DoubleMatrix2D a, DoubleMatrix2D b) {
   *         double as = a.zSum();
   *         double bs = b.zSum();
   *         return as &lt; bs ? -1 : as == bs ? 0 : 1;
   *     }
   * };
   * sorted = quickSort(matrix, comp);
   * </pre>
   *
   * @param matrix
   *            the matrix to be sorted.
   * @param c
   *            the comparator to determine the order.
   * @return a new matrix view having slices sorted as specified. <b>Note that
   *         the original matrix is left unaffected.</b>
   */
  def sort(matrix: DoubleMatrix3D, c: DoubleMatrix2DComparator): DoubleMatrix3D = {
    val sliceIndexes = Array.ofDim[Int](matrix.slices())
    var i = sliceIndexes.length
    while (i >= 0) sliceIndexes(i) = i
    val views = Array.ofDim[StrideMatrix2D](matrix.slices())
    var i = views.length
    while (i >= 0) views(i) = matrix.viewSlice(i)
    val comp = new IntComparator() {

      def compare(a: Int, b: Int): Int = return c.compare(views(a), views(b))
    }
    runSort(sliceIndexes, 0, sliceIndexes.length, comp)
    matrix.viewSelection(sliceIndexes, null, null)
  }
}
