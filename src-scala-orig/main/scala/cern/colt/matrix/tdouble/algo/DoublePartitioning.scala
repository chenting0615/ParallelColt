package cern.colt.matrix.tdouble.algo

import cern.colt.Swapper
import cern.colt.function.tint.IntComparator
import cern.colt.matrix.tdouble.StrideMatrix1D
import cern.colt.matrix.tdouble.StrideMatrix2D
import DoublePartitioning._
//remove if not needed
import scala.collection.JavaConversions._

object DoublePartitioning {

  /**
   * Same as
   * {@link cern.colt.Partitioning#partition(int[],int,int,int[],int,int,int[])}
   * except that it <i>synchronously</i> partitions the rows of the given
   * matrix by the values of the given matrix column; This is essentially the
   * same as partitioning a list of composite objects by some instance
   * variable; In other words, two entire rows of the matrix are swapped,
   * whenever two column values indicate so.
   * <p>
   * Let's say, a "row" is an "object" (tuple, d-dimensional point). A
   * "column" is the list of "object" values of a given variable (field,
   * dimension). A "matrix" is a list of "objects" (tuples, points).
   * <p>
   * Now, rows (objects, tuples) are partially sorted according to their
   * values in one given variable (dimension). Two entire rows of the matrix
   * are swapped, whenever two column values indicate so.
   * <p>
   * Note that arguments are not checked for validity.
   * <p>
   * <b>Example:</b>
   * <table border="1" cellspacing="0">
   * <tr nowrap>
   * <td valign="top"><tt>8 x 3 matrix:<br>
   23, 22, 21<br>
   20, 19, 18<br>
   17, 16, 15<br>
   14, 13, 12<br>
   11, 10, 9<br>
   8,  7,  6<br>
   5,  4,  3<br>
   2,  1,  0 </tt></td>
   * <td align="left" valign="top">
   * <p>
   * <tt>column = 0;<br>
   rowIndexes = {0,1,2,..,matrix.rows()-1};
   rowFrom = 0;<br>
   rowTo = matrix.rows()-1;<br>
   splitters = {5,10,12}<br>
   c = 0; <br>
   d = splitters.length-1;<br>
   partition(matrix,rowIndexes,rowFrom,rowTo,column,splitters,c,d,splitIndexes);<br>
   ==><br>
   splitIndexes == {0, 2, 3}<br>
   rowIndexes == {7, 6, 5, 4, 0, 1, 2, 3}</tt>
   * </p>
   * </td>
   * <td valign="top">The matrix IS NOT REORDERED.<br>
   * Here is how it would look<br>
   * like, if it would be reordered<br>
   * accoring to <tt>rowIndexes</tt>.<br>
   * <tt>8 x 3 matrix:<br>
   2,  1,  0<br>
   5,  4,  3<br>
   8,  7,  6<br>
   11, 10, 9<br>
   23, 22, 21<br>
   20, 19, 18<br>
   17, 16, 15<br>
   14, 13, 12 </tt></td>
   * </tr>
   * </table>
   *
   * @param matrix
   *            the matrix to be partitioned.
   * @param rowIndexes
   *            the index of the i-th row; is modified by this method to
   *            reflect partitioned indexes.
   * @param rowFrom
   *            the index of the first row (inclusive).
   * @param rowTo
   *            the index of the last row (inclusive).
   * @param column
   *            the index of the column to partition on.
   * @param splitters
   *            the values at which the rows shall be split into intervals.
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
   *            a list into which this method fills the indexes of rows
   *            delimiting intervals. Upon return
   *            <tt>splitIndexes[splitFrom..splitTo]</tt> will be set
   *            accordingly. Therefore, must satisfy
   *            <tt>splitIndexes.length >= splitters.length</tt>.
   */
  def partition(matrix: StrideMatrix2D,
      rowIndexes: Array[Int],
      rowFrom: Int,
      rowTo: Int,
      column: Int,
      splitters: Array[Double],
      splitFrom: Int,
      splitTo: Int,
      splitIndexes: Array[Int]) {
    if (rowFrom < 0 || rowTo >= matrix.rows() || rowTo >= rowIndexes.length) throw new IllegalArgumentException()
    if (column < 0 || column >= matrix.columns()) throw new IllegalArgumentException()
    if (splitFrom < 0 || splitTo >= splitters.length) throw new IllegalArgumentException()
    if (splitIndexes.length < splitters.length) throw new IllegalArgumentException()
    val g = rowIndexes
    val swapper = new Swapper() {

      def swap(b: Int, c: Int) {
        var tmp = g(b)
        g(b) = g(c)
        g(c) = tmp
      }
    }
    val columnView = matrix.viewColumn(column)
    val comp = new IntComparator() {

      def compare(a: Int, b: Int): Int = {
        var av = splitters(a)
        var bv = columnView.getQuick(g(b))
        return if (av < bv) -1 else (if (av == bv) 0 else 1)
      }
    }
    val comp2 = new IntComparator() {

      def compare(a: Int, b: Int): Int = {
        var av = columnView.getQuick(g(a))
        var bv = columnView.getQuick(g(b))
        return if (av < bv) -1 else (if (av == bv) 0 else 1)
      }
    }
    val comp3 = new IntComparator() {

      def compare(a: Int, b: Int): Int = {
        var av = splitters(a)
        var bv = splitters(b)
        return if (av < bv) -1 else (if (av == bv) 0 else 1)
      }
    }
    cern.colt.Partitioning.genericPartition(rowFrom, rowTo, splitFrom, splitTo, splitIndexes, comp, comp2,
      comp3, swapper)
  }

  /**
   * Same as
   * {@link cern.colt.Partitioning#partition(int[],int,int,int[],int,int,int[])}
   * except that it <i>synchronously</i> partitions the rows of the given
   * matrix by the values of the given matrix column; This is essentially the
   * same as partitioning a list of composite objects by some instance
   * variable; In other words, two entire rows of the matrix are swapped,
   * whenever two column values indicate so.
   * <p>
   * Let's say, a "row" is an "object" (tuple, d-dimensional point). A
   * "column" is the list of "object" values of a given variable (field,
   * dimension). A "matrix" is a list of "objects" (tuples, points).
   * <p>
   * Now, rows (objects, tuples) are partially sorted according to their
   * values in one given variable (dimension). Two entire rows of the matrix
   * are swapped, whenever two column values indicate so.
   * <p>
   * Note that arguments are not checked for validity.
   * <p>
   * <b>Example:</b>
   * <table border="1" cellspacing="0">
   * <tr nowrap>
   * <td valign="top"><tt>8 x 3 matrix:<br>
   23, 22, 21<br>
   20, 19, 18<br>
   17, 16, 15<br>
   14, 13, 12<br>
   11, 10, 9<br>
   8,  7,  6<br>
   5,  4,  3<br>
   2,  1,  0 </tt></td>
   * <td align="left" valign="top"> <tt>column = 0;<br>
   splitters = {5,10,12}<br>
   partition(matrix,column,splitters,splitIndexes);<br>
   ==><br>
   splitIndexes == {0, 2, 3}</tt>
   * </p>
   * </td> <td valign="top">The matrix IS NOT REORDERED.<br>
   * The new VIEW IS REORDERED:<br>
   * <tt>8 x 3 matrix:<br>
   2,  1,  0<br>
   5,  4,  3<br>
   8,  7,  6<br>
   11, 10, 9<br>
   23, 22, 21<br>
   20, 19, 18<br>
   17, 16, 15<br>
   14, 13, 12 </tt></td> </tr> </table>
   *
   * @param matrix
   *            the matrix to be partitioned.
   * @param column
   *            the index of the column to partition on.
   * @param splitters
   *            the values at which the rows shall be split into intervals.
   *            Must be sorted ascending and must not contain multiple
   *            identical values. These preconditions are not checked; be sure
   *            that they are met.
   *
   * @param splitIndexes
   *            a list into which this method fills the indexes of rows
   *            delimiting intervals. Therefore, must satisfy
   *            <tt>splitIndexes.length >= splitters.length</tt>.
   *
   * @return a new matrix view having rows partitioned by the given column and
   *         splitters.
   */
  def partition(matrix: StrideMatrix2D,
      column: Int,
      splitters: Array[Double],
      splitIndexes: Array[Int]): StrideMatrix2D = {
    val rowFrom = 0
    val rowTo = matrix.rows() - 1
    val splitFrom = 0
    val splitTo = splitters.length - 1
    val rowIndexes = Array.ofDim[Int](matrix.rows())
    var i = rowIndexes.length
    while (i >= 0) rowIndexes(i) = i
    partition(matrix, rowIndexes, rowFrom, rowTo, column, splitters, splitFrom, splitTo, splitIndexes)
    val columnIndexes = Array.ofDim[Int](matrix.columns())
    var i = columnIndexes.length
    while (i >= 0) columnIndexes(i) = i
    matrix.viewSelection(rowIndexes, columnIndexes)
  }
}

/**
 * Given some interval boundaries, partitions matrices such that cell values
 * falling into an interval are placed next to each other.
 * <p>
 * <b>Performance</b>
 * <p>
 * Partitioning into two intervals is <tt>O( N )</tt>. Partitioning into k
 * intervals is <tt>O( N * log(k))</tt>. Constants factors are minimized.
 *
 * @see cern.colt.Partitioning "Partitioning arrays (provides more
 *      documentation)"
 *
 * @author wolfgang.hoschek@cern.ch
 * @version 1.0, 09/24/99
 */
class DoublePartitioning protected () extends AnyRef
