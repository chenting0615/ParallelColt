package cern.colt.matrix.tdouble

import cern.colt.matrix.tdouble.impl.DenseMatrix2D
import cern.colt.matrix.tdouble.impl.SparseDoubleMatrix2D
import cern.jet.math.tdouble.DoubleFunctions
import DoubleFactory2D._
//remove if not needed
import scala.collection.JavaConversions._

object DoubleFactory2D {

  /**
   * A factory producing dense matrices.
   */
  val dense = new DoubleFactory2D()

  /**
   * A factory producing sparse hash matrices.
   */
  val sparse = new DoubleFactory2D()

  /**
   * Checks whether the given array is rectangular, that is, whether all rows
   * have the same number of columns.
   *
   * @throws IllegalArgumentException
   *             if the array is not rectangular.
   */
  protected def checkRectangularShape(array: Array[Array[Double]]) {
    var columns = -1
    var row = array.length
    while (row >= 0) {
      if (array(row) != null) {
        if (columns == -1) columns = array(row).length
        if (array(row).length != columns) throw new IllegalArgumentException("All rows of array must have same number of columns.")
      }
    }
  }

  /**
   * Checks whether the given array is rectangular, that is, whether all rows
   * have the same number of columns.
   *
   * @throws IllegalArgumentException
   *             if the array is not rectangular.
   */
  protected def checkRectangularShape(array: Array[Array[StrideMatrix2D]]) {
    var columns = -1
    var row = array.length
    while (row >= 0) {
      if (array(row) != null) {
        if (columns == -1) columns = array(row).length
        if (array(row).length != columns) throw new IllegalArgumentException("All rows of array must have same number of columns.")
      }
    }
  }
}

/**
 * Factory for convenient construction of 2-d matrices holding <tt>double</tt>
 * cells. Also provides convenient methods to compose (concatenate) and
 * decompose (split) matrices from/to constituent blocks. </p>
 * <p>
 * &nbsp;
 * </p>
 * <table border="0" cellspacing="0">
 * <tr align="left" valign="top">
 * <td><i>Construction</i></td>
 * <td>Use idioms like <tt>DoubleFactory2D.dense.make(4,4)</tt> to construct
 * dense matrices, <tt>DoubleFactory2D.sparse.make(4,4)</tt> to construct sparse
 * matrices.</td>
 * </tr>
 * <tr align="left" valign="top">
 * <td><i> Construction with initial values </i></td>
 * <td>Use other <tt>make</tt> methods to construct matrices with given initial
 * values.</td>
 * </tr>
 * <tr align="left" valign="top">
 * <td><i> Appending rows and columns </i></td>
 * <td>Use methods {@link #appendColumns(DoubleMatrix2D,DoubleMatrix2D)
 * appendColumns}, {@link #appendColumns(DoubleMatrix2D,DoubleMatrix2D)
 * appendRows} and {@link #repeat(DoubleMatrix2D,int,int) repeat} to append rows
 * and columns.</td>
 * </tr>
 * <tr align="left" valign="top">
 * <td><i> General block matrices </i></td>
 * <td>Use methods {@link #compose(DoubleMatrix2D[][]) compose} and
 * {@link #decompose(DoubleMatrix2D[][],DoubleMatrix2D) decompose} to work with
 * general block matrices.</td>
 * </tr>
 * <tr align="left" valign="top">
 * <td><i> Diagonal matrices </i></td>
 * <td>Use methods {@link #diagonal(DoubleMatrix1D) diagonal(vector)},
 * {@link #diagonal(DoubleMatrix2D) diagonal(matrix)} and {@link #identity(int)
 * identity} to work with diagonal matrices.</td>
 * </tr>
 * <tr align="left" valign="top">
 * <td><i> Diagonal block matrices </i></td>
 * <td>Use method
 * {@link #composeDiagonal(DoubleMatrix2D,DoubleMatrix2D,DoubleMatrix2D)
 * composeDiagonal} to work with diagonal block matrices.</td>
 * </tr>
 * <tr align="left" valign="top">
 * <td><i>Random</i></td>
 * <td>Use methods {@link #random(int,int) random} and
 * {@link #sample(int,int,double,double) sample} to construct random matrices.</td>
 * </tr>
 * </table>
 * <p>
 * &nbsp;
 * </p>
 * <p>
 * If the factory is used frequently it might be useful to streamline the
 * notation. For example by aliasing:
 * </p>
 * <table>
 * <td class="PRE">
 *
 * <pre>
 *  DoubleFactory2D F = DoubleFactory2D.dense;
 *  F.make(4,4);
 *  F.descending(10,20);
 *  F.random(4,4);
 *  ...
 * </pre>
 *
 * </td>
 * </table>
 *
 * @author wolfgang.hoschek@cern.ch
 * @version 1.0, 09/24/99
 *
 * @author Piotr Wendykier (piotr.wendykier@gmail.com)
 */
@SerialVersionUID(1L)
class DoubleFactory2D protected () extends cern.colt.PersistentObject {

  /**
   * C = A||b; Constructs a new matrix which is the column-wise concatenation
   * of two other matrices.
   *
   * <pre>
   *   0 1 2
   *   3 4 5
   *   appendColumn
   *   6
   *   8
   *   --&gt;
   *   0 1 2 6
   *   3 4 5 8
   *
   * </pre>
   */
  def appendColumn(A: StrideMatrix2D, b: StrideMatrix1D): StrideMatrix2D = {
    if (b.size > A.rows()) b = b.viewPart(0, A.rows()) else if (b.size < A.rows()) A = A.viewPart(0,
      0, b.size.toInt, A.columns())
    val ac = A.columns()
    val bc = 1
    val r = A.rows()
    val matrix = make(r, ac + bc)
    matrix.viewPart(0, 0, r, ac).assign(A)
    matrix.viewColumn(ac).assign(b)
    matrix
  }

  /**
   * C = A||B; Constructs a new matrix which is the column-wise concatenation
   * of two other matrices.
   *
   * <pre>
   * 	 0 1 2
   * 	 3 4 5
   * 	 appendColumns
   * 	 6 7
   * 	 8 9
   * 	 --&gt;
   * 	 0 1 2 6 7
   * 	 3 4 5 8 9
   *
   * </pre>
   */
  def appendColumns(A: StrideMatrix2D, B: StrideMatrix2D): StrideMatrix2D = {
    if (B.rows() > A.rows()) B = B.viewPart(0, 0, A.rows(), B.columns()) else if (B.rows() < A.rows()) A = A.viewPart(0,
      0, B.rows(), A.columns())
    val ac = A.columns()
    val bc = B.columns()
    val r = A.rows()
    val matrix = make(r, ac + bc)
    matrix.viewPart(0, 0, r, ac).assign(A)
    matrix.viewPart(0, ac, r, bc).assign(B)
    matrix
  }

  /**
   * C = A||b; Constructs a new matrix which is the row-wise concatenation of
   * two other matrices.
   *
   * <pre>
   *   0 1
   *   2 3
   *   4 5
   *   appendRow
   *   6 7
   *   --&gt;
   *   0 1
   *   2 3
   *   4 5
   *   6 7
   *
   * </pre>
   */
  def appendRow(A: StrideMatrix2D, b: StrideMatrix1D): StrideMatrix2D = {
    if (b.size > A.columns()) b = b.viewPart(0, A.columns()) else if (b.size < A.columns()) A = A.viewPart(0,
      0, A.rows(), b.size.toInt)
    val ar = A.rows()
    val br = 1
    val c = A.columns()
    val matrix = make(ar + br, c)
    matrix.viewPart(0, 0, ar, c).assign(A)
    matrix.viewRow(ar).assign(b)
    matrix
  }

  /**
   * C = A||B; Constructs a new matrix which is the row-wise concatenation of
   * two other matrices.
   *
   * <pre>
   * 	 0 1
   * 	 2 3
   * 	 4 5
   * 	 appendRows
   * 	 6 7
   * 	 8 9
   * 	 --&gt;
   * 	 0 1
   * 	 2 3
   * 	 4 5
   * 	 6 7
   * 	 8 9
   *
   * </pre>
   */
  def appendRows(A: StrideMatrix2D, B: StrideMatrix2D): StrideMatrix2D = {
    if (B.columns() > A.columns()) B = B.viewPart(0, 0, B.rows(), A.columns()) else if (B.columns() < A.columns()) A = A.viewPart(0,
      0, A.rows(), B.columns())
    val ar = A.rows()
    val br = B.rows()
    val c = A.columns()
    val matrix = make(ar + br, c)
    matrix.viewPart(0, 0, ar, c).assign(A)
    matrix.viewPart(ar, 0, br, c).assign(B)
    matrix
  }

  /**
   * Constructs a matrix with cells having ascending values. For debugging
   * purposes. Example:
   *
   * <pre>
   * 	 0 1 2
   * 	 3 4 5
   *
   * </pre>
   */
  def ascending(rows: Int, columns: Int): StrideMatrix2D = {
    descending(rows, columns).assign(DoubleFunctions.chain(DoubleFunctions.neg, DoubleFunctions.minus(columns * rows)))
  }

  /**
   * Constructs a block matrix made from the given parts. The inverse to
   * method {@link #decompose(DoubleMatrix2D[][], DoubleMatrix2D)}.
   * <p>
   * All matrices of a given column within <tt>parts</tt> must have the same
   * number of columns. All matrices of a given row within <tt>parts</tt> must
   * have the same number of rows. Otherwise an
   * <tt>IllegalArgumentException</tt> is thrown. Note that <tt>null</tt>s
   * within <tt>parts[row,col]</tt> are an exception to this rule: they are
   * ignored. Cells are copied. Example:
   * <table border="1" cellspacing="0">
   * <tr align="left" valign="top">
   * <td><tt>Code</tt></td>
   * <td><tt>Result</tt></td>
   * </tr>
   * <tr align="left" valign="top">
   * <td>
   *
   * <pre>
   * DoubleMatrix2D[][] parts1 = { { null, make(2, 2, 1), null }, { make(4, 4, 2), null, make(4, 3, 3) },
   *         { null, make(2, 2, 4), null } };
   * System.out.println(compose(parts1));
   * </pre>
   *
   * </td>
   * <td><tt>8&nbsp;x&nbsp;9&nbsp;matrix<br>
   0&nbsp;0&nbsp;0&nbsp;0&nbsp;1&nbsp;1&nbsp;0&nbsp;0&nbsp;0<br>
   0&nbsp;0&nbsp;0&nbsp;0&nbsp;1&nbsp;1&nbsp;0&nbsp;0&nbsp;0<br>
   2&nbsp;2&nbsp;2&nbsp;2&nbsp;0&nbsp;0&nbsp;3&nbsp;3&nbsp;3<br>
   2&nbsp;2&nbsp;2&nbsp;2&nbsp;0&nbsp;0&nbsp;3&nbsp;3&nbsp;3<br>
   2&nbsp;2&nbsp;2&nbsp;2&nbsp;0&nbsp;0&nbsp;3&nbsp;3&nbsp;3<br>
   2&nbsp;2&nbsp;2&nbsp;2&nbsp;0&nbsp;0&nbsp;3&nbsp;3&nbsp;3<br>
   0&nbsp;0&nbsp;0&nbsp;0&nbsp;4&nbsp;4&nbsp;0&nbsp;0&nbsp;0<br>
   0&nbsp;0&nbsp;0&nbsp;0&nbsp;4&nbsp;4&nbsp;0&nbsp;0&nbsp;0</tt></td>
   * </tr>
   * <tr align="left" valign="top">
   * <td>
   *
   * <pre>
   * DoubleMatrix2D[][] parts3 = { { identity(3), null, }, { null, identity(3).viewColumnFlip() },
   *         { identity(3).viewRowFlip(), null } };
   * System.out.println(&quot;\n&quot; + make(parts3));
   * </pre>
   *
   * </td>
   * <td><tt>9&nbsp;x&nbsp;6&nbsp;matrix<br>
   1&nbsp;0&nbsp;0&nbsp;0&nbsp;0&nbsp;0<br>
   0&nbsp;1&nbsp;0&nbsp;0&nbsp;0&nbsp;0<br>
   0&nbsp;0&nbsp;1&nbsp;0&nbsp;0&nbsp;0<br>
   0&nbsp;0&nbsp;0&nbsp;0&nbsp;0&nbsp;1<br>
   0&nbsp;0&nbsp;0&nbsp;0&nbsp;1&nbsp;0<br>
   0&nbsp;0&nbsp;0&nbsp;1&nbsp;0&nbsp;0<br>
   0&nbsp;0&nbsp;1&nbsp;0&nbsp;0&nbsp;0<br>
   0&nbsp;1&nbsp;0&nbsp;0&nbsp;0&nbsp;0<br>
   1&nbsp;0&nbsp;0&nbsp;0&nbsp;0&nbsp;0 </tt></td>
   * </tr>
   * <tr align="left" valign="top">
   * <td>
   *
   * <pre>
   * DoubleMatrix2D A = ascending(2, 2);
   * DoubleMatrix2D B = descending(2, 2);
   * DoubleMatrix2D _ = null;
   *
   * DoubleMatrix2D[][] parts4 = { { A, _, A, _ }, { _, A, _, B } };
   * System.out.println(&quot;\n&quot; + make(parts4));
   * </pre>
   *
   * </td>
   * <td><tt>4&nbsp;x&nbsp;8&nbsp;matrix<br>
   1&nbsp;2&nbsp;0&nbsp;0&nbsp;1&nbsp;2&nbsp;0&nbsp;0<br>
   3&nbsp;4&nbsp;0&nbsp;0&nbsp;3&nbsp;4&nbsp;0&nbsp;0<br>
   0&nbsp;0&nbsp;1&nbsp;2&nbsp;0&nbsp;0&nbsp;3&nbsp;2<br>
   0&nbsp;0&nbsp;3&nbsp;4&nbsp;0&nbsp;0&nbsp;1&nbsp;0 </tt></td>
   * </tr>
   * <tr align="left" valign="top">
   * <td>
   *
   * <pre>
   * DoubleMatrix2D[][] parts2 = { { null, make(2, 2, 1), null }, { make(4, 4, 2), null, make(4, 3, 3) },
   *         { null, make(2, 3, 4), null } };
   * System.out.println(&quot;\n&quot; + Factory2D.make(parts2));
   * </pre>
   *
   * </td>
   * <td><tt>IllegalArgumentException<br>
   A[0,1].columns != A[2,1].columns<br>
   (2 != 3)</tt></td>
   * </tr>
   * </table>
   *
   * @throws IllegalArgumentException
   *             subject to the conditions outlined above.
   */
  def compose(parts: Array[Array[StrideMatrix2D]]): StrideMatrix2D = {
    checkRectangularShape(parts)
    val rows = parts.length
    var columns = 0
    if (parts.length > 0) columns = parts(0).length
    val empty = make(0, 0)
    if (rows == 0 || columns == 0) return empty
    val maxWidths = Array.ofDim[Int](columns)
    var column = columns
    while (column >= 0) {
      var maxWidth = 0
      var row = rows
      while (row >= 0) {
        val part = parts(row)(column)
        if (part != null) {
          val width = part.columns()
          if (maxWidth > 0 && width > 0 && width != maxWidth) throw new IllegalArgumentException("Different number of columns.")
          maxWidth = Math.max(maxWidth, width)
        }
      }
      maxWidths(column) = maxWidth
    }
    val maxHeights = Array.ofDim[Int](rows)
    var row = rows
    while (row >= 0) {
      var maxHeight = 0
      var column = columns
      while (column >= 0) {
        val part = parts(row)(column)
        if (part != null) {
          val height = part.rows()
          if (maxHeight > 0 && height > 0 && height != maxHeight) throw new IllegalArgumentException("Different number of rows.")
          maxHeight = Math.max(maxHeight, height)
        }
      }
      maxHeights(row) = maxHeight
    }
    var resultRows = 0
    var row = rows
    while (row >= 0) resultRows += maxHeights(row)
    var resultCols = 0
    var column = columns
    while (column >= 0) resultCols += maxWidths(column)
    val matrix = make(resultRows, resultCols)
    var r = 0
    for (row <- 0 until rows) {
      var c = 0
      for (column <- 0 until columns) {
        val part = parts(row)(column)
        if (part != null) {
          matrix.viewPart(r, c, part.rows(), part.columns()).assign(part)
        }
        c += maxWidths(column)
      }
      r += maxHeights(row)
    }
    matrix
  }

  /**
   * Constructs a bidiagonal block matrix from the given parts. The
   * concatenation has the form
   *
   * <pre>
   *   A 0 0
   *   0 B 0
   *   0 0 C
   *
   * </pre>
   *
   * from the given parts. Cells are copied.
   *
   * @param A
   *            bidiagonal matrix
   * @param B
   *            bidiagonal matrix
   * @return bidiagonal matrix
   */
  def composeBidiagonal(A: StrideMatrix2D, B: StrideMatrix2D): StrideMatrix2D = {
    val ar = A.rows()
    val ac = A.columns()
    val br = B.rows()
    val bc = B.columns()
    val sum = make(ar + br - 1, ac + bc)
    sum.viewPart(0, 0, ar, ac).assign(A)
    sum.viewPart(ar - 1, ac, br, bc).assign(B)
    sum
  }

  /**
   * Constructs a diagonal block matrix from the given parts (the <i>direct
   * sum</i> of two matrices). That is the concatenation
   *
   * <pre>
   * 	 A 0
   * 	 0 B
   *
   * </pre>
   *
   * (The direct sum has <tt>A.rows()+B.rows()</tt> rows and
   * <tt>A.columns()+B.columns()</tt> columns). Cells are copied.
   *
   * @return a new matrix which is the direct sum.
   */
  def composeDiagonal(A: StrideMatrix2D, B: StrideMatrix2D): StrideMatrix2D = {
    val ar = A.rows()
    val ac = A.columns()
    val br = B.rows()
    val bc = B.columns()
    val sum = make(ar + br, ac + bc)
    sum.viewPart(0, 0, ar, ac).assign(A)
    sum.viewPart(ar, ac, br, bc).assign(B)
    sum
  }

  /**
   * Constructs a diagonal block matrix from the given parts. The
   * concatenation has the form
   *
   * <pre>
   * 	 A 0 0
   * 	 0 B 0
   * 	 0 0 C
   *
   * </pre>
   *
   * from the given parts. Cells are copied.
   */
  def composeDiagonal(A: StrideMatrix2D, B: StrideMatrix2D, C: StrideMatrix2D): StrideMatrix2D = {
    val diag = make(A.rows() + B.rows() + C.rows(), A.columns() + B.columns() + C.columns())
    diag.viewPart(0, 0, A.rows(), A.columns()).assign(A)
    diag.viewPart(A.rows(), A.columns(), B.rows(), B.columns())
      .assign(B)
    diag.viewPart(A.rows() + B.rows(), A.columns() + B.columns(), C.rows(), C.columns())
      .assign(C)
    diag
  }

  /**
   * Splits a block matrix into its constituent blocks; Copies blocks of a
   * matrix into the given parts. The inverse to method
   * {@link #compose(DoubleMatrix2D[][])}.
   * <p>
   * All matrices of a given column within <tt>parts</tt> must have the same
   * number of columns. All matrices of a given row within <tt>parts</tt> must
   * have the same number of rows. Otherwise an
   * <tt>IllegalArgumentException</tt> is thrown. Note that <tt>null</tt>s
   * within <tt>parts[row,col]</tt> are an exception to this rule: they are
   * ignored. Cells are copied. Example:
   * <table border="1" cellspacing="0">
   * <tr align="left" valign="top">
   * <td><tt>Code</tt></td>
   * <td><tt>matrix</tt></td>
   * <td><tt>--&gt; parts </tt></td>
   * </tr>
   * <tr align="left" valign="top">
   * <td>
   *
   * <pre>
   * 	 DoubleMatrix2D matrix = ... ;
   * 	 DoubleMatrix2D _ = null;
   * 	 DoubleMatrix2D A,B,C,D;
   * 	 A = make(2,2); B = make (4,4);
   * 	 C = make(4,3); D = make (2,2);
   * 	 DoubleMatrix2D[][] parts =
   * 	 {
   * 	    { _, A, _ },
   * 	    { B, _, C },
   * 	    { _, D, _ }
   * 	 };
   * 	 decompose(parts,matrix);
   * 	 System.out.println(&quot;\nA = &quot;+A);
   * 	 System.out.println(&quot;\nB = &quot;+B);
   * 	 System.out.println(&quot;\nC = &quot;+C);
   * 	 System.out.println(&quot;\nD = &quot;+D);
   *
   * </pre>
   *
   * </td>
   * <td><tt>8&nbsp;x&nbsp;9&nbsp;matrix<br>
   9&nbsp;9&nbsp;9&nbsp;9&nbsp;1&nbsp;1&nbsp;9&nbsp;9&nbsp;9<br>
   9&nbsp;9&nbsp;9&nbsp;9&nbsp;1&nbsp;1&nbsp;9&nbsp;9&nbsp;9<br>
   2&nbsp;2&nbsp;2&nbsp;2&nbsp;9&nbsp;9&nbsp;3&nbsp;3&nbsp;3<br>
   2&nbsp;2&nbsp;2&nbsp;2&nbsp;9&nbsp;9&nbsp;3&nbsp;3&nbsp;3<br>
   2&nbsp;2&nbsp;2&nbsp;2&nbsp;9&nbsp;9&nbsp;3&nbsp;3&nbsp;3<br>
   2&nbsp;2&nbsp;2&nbsp;2&nbsp;9&nbsp;9&nbsp;3&nbsp;3&nbsp;3<br>
   9&nbsp;9&nbsp;9&nbsp;9&nbsp;4&nbsp;4&nbsp;9&nbsp;9&nbsp;9<br>
   9&nbsp;9&nbsp;9&nbsp;9&nbsp;4&nbsp;4&nbsp;9&nbsp;9&nbsp;9</tt></td>
   * <td>
   * <p>
   * <tt>A = 2&nbsp;x&nbsp;2&nbsp;matrix<br>
   1&nbsp;1<br>
   1&nbsp;1</tt>
   * </p>
   * <p>
   * <tt>B = 4&nbsp;x&nbsp;4&nbsp;matrix<br>
   2&nbsp;2&nbsp;2&nbsp;2<br>
   2&nbsp;2&nbsp;2&nbsp;2<br>
   2&nbsp;2&nbsp;2&nbsp;2<br>
   2&nbsp;2&nbsp;2&nbsp;2</tt>
   * </p>
   * <p>
   * <tt>C = 4&nbsp;x&nbsp;3&nbsp;matrix<br>
   3&nbsp;3&nbsp;3<br>
   3&nbsp;3&nbsp;3<br>
   </tt><tt>3&nbsp;3&nbsp;3<br>
   </tt><tt>3&nbsp;3&nbsp;3</tt>
   * </p>
   * <p>
   * <tt>D = 2&nbsp;x&nbsp;2&nbsp;matrix<br>
   4&nbsp;4<br>
   4&nbsp;4</tt>
   * </p>
   * </td>
   * </tr>
   * </table>
   *
   * @throws IllegalArgumentException
   *             subject to the conditions outlined above.
   */
  def decompose(parts: Array[Array[StrideMatrix2D]], matrix: StrideMatrix2D) {
    checkRectangularShape(parts)
    val rows = parts.length
    var columns = 0
    if (parts.length > 0) columns = parts(0).length
    if (rows == 0 || columns == 0) return
    val maxWidths = Array.ofDim[Int](columns)
    var column = columns
    while (column >= 0) {
      var maxWidth = 0
      var row = rows
      while (row >= 0) {
        val part = parts(row)(column)
        if (part != null) {
          val width = part.columns()
          if (maxWidth > 0 && width > 0 && width != maxWidth) throw new IllegalArgumentException("Different number of columns.")
          maxWidth = Math.max(maxWidth, width)
        }
      }
      maxWidths(column) = maxWidth
    }
    val maxHeights = Array.ofDim[Int](rows)
    var row = rows
    while (row >= 0) {
      var maxHeight = 0
      var column = columns
      while (column >= 0) {
        val part = parts(row)(column)
        if (part != null) {
          val height = part.rows()
          if (maxHeight > 0 && height > 0 && height != maxHeight) throw new IllegalArgumentException("Different number of rows.")
          maxHeight = Math.max(maxHeight, height)
        }
      }
      maxHeights(row) = maxHeight
    }
    var resultRows = 0
    var row = rows
    while (row >= 0) resultRows += maxHeights(row)
    var resultCols = 0
    var column = columns
    while (column >= 0) resultCols += maxWidths(column)
    if (matrix.rows() < resultRows || matrix.columns() < resultCols) throw new IllegalArgumentException("Parts larger than matrix.")
    var r = 0
    for (row <- 0 until rows) {
      var c = 0
      for (column <- 0 until columns) {
        val part = parts(row)(column)
        if (part != null) {
          part.assign(matrix.viewPart(r, c, part.rows(), part.columns()))
        }
        c += maxWidths(column)
      }
      r += maxHeights(row)
    }
  }

  /**
   * Demonstrates usage of this class.
   */
  def demo1() {
    println("\n\n")
    val parts1 = Array(Array(null, make(2, 2, 1), null), Array(make(4, 4, 2), null, make(4, 3, 3)), Array(null, make(2,
      2, 4), null))
    println("\n" + compose(parts1))
    val parts3 = Array(Array(identity(3), null), Array(null, identity(3).viewColumnFlip()), Array(identity(3).viewRowFlip(), null))
    println("\n" + compose(parts3))
    val A = ascending(2, 2)
    val B = descending(2, 2)
    val _: StrideMatrix2D = null
    val parts4 = Array(Array(A, _, A, _), Array(_, A, _, B))
    println("\n" + compose(parts4))
  }

  /**
   * Demonstrates usage of this class.
   */
  def demo2() {
    println("\n\n")
    var matrix: StrideMatrix2D = null
    var A: StrideMatrix2D = null
    var B: StrideMatrix2D = null
    var C: StrideMatrix2D = null
    var D: StrideMatrix2D = null
    val _: StrideMatrix2D = null
    A = make(2, 2, 1)
    B = make(4, 4, 2)
    C = make(4, 3, 3)
    D = make(2, 2, 4)
    val parts1 = Array(Array(_, A, _), Array(B, _, C), Array(_, D, _))
    matrix = compose(parts1)
    println("\n" + matrix)
    A.assign(9)
    B.assign(9)
    C.assign(9)
    D.assign(9)
    decompose(parts1, matrix)
    println(A)
    println(B)
    println(C)
    println(D)
  }

  /**
   * Constructs a matrix with cells having descending values. For debugging
   * purposes. Example:
   *
   * <pre>
   * 	 5 4 3
   * 	 2 1 0
   *
   * </pre>
   */
  def descending(rows: Int, columns: Int): StrideMatrix2D = {
    val matrix = make(rows, columns)
    var v = 0
    var row = rows
    while (row >= 0) {
      var column = columns
      while (column >= 0) {
        matrix.setQuick(row, column, v += 1)
      }
    }
    matrix
  }

  /**
   * Constructs a new diagonal matrix whose diagonal elements are the elements
   * of <tt>vector</tt>. Cells values are copied. The new matrix is not a
   * view. Example:
   *
   * <pre>
   * 	 5 4 3 --&gt;
   * 	 5 0 0
   * 	 0 4 0
   * 	 0 0 3
   *
   * </pre>
   *
   * @return a new matrix.
   */
  def diagonal(vector: Array[Double]): StrideMatrix2D = {
    val size = vector.length
    val diag = make(size, size)
    for (i <- 0 until size) {
      diag.setQuick(i, i, vector(i))
    }
    diag
  }

  /**
   * Constructs a new diagonal matrix whose diagonal elements are the elements
   * of <tt>vector</tt>. Cells values are copied. The new matrix is not a
   * view. Example:
   *
   * <pre>
   * 	 5 4 3 --&gt;
   * 	 5 0 0
   * 	 0 4 0
   * 	 0 0 3
   *
   * </pre>
   *
   * @return a new matrix.
   */
  def diagonal(vector: StrideMatrix1D): StrideMatrix2D = {
    val size = vector.size.toInt
    val diag = make(size, size)
    var i = size
    while (i >= 0) {
      diag.setQuick(i, i, vector.getQuick(i))
    }
    diag
  }

  /**
   * Constructs a new vector consisting of the diagonal elements of <tt>A</tt>
   * . Cells values are copied. The new vector is not a view. Example:
   *
   * <pre>
   * 	 5 0 0 9
   * 	 0 4 0 9
   * 	 0 0 3 9
   * 	 --&gt; 5 4 3
   *
   * </pre>
   *
   * @param A
   *            the matrix, need not be square.
   * @return a new vector.
   */
  def diagonal(A: StrideMatrix2D): StrideMatrix1D = {
    val min = Math.min(A.rows(), A.columns())
    val diag = make1D(min)
    var i = min
    while (i >= 0) {
      diag.setQuick(i, A.getQuick(i, i))
    }
    diag
  }

  /**
   * Constructs an identity matrix (having ones on the diagonal and zeros
   * elsewhere).
   */
  def identity(rowsAndColumns: Int): StrideMatrix2D = {
    val matrix = make(rowsAndColumns, rowsAndColumns)
    var i = rowsAndColumns
    while (i >= 0) {
      matrix.setQuick(i, i, 1)
    }
    matrix
  }

  /**
   * Construct a matrix from a one-dimensional column-major packed array, ala
   * Fortran. Has the form
   * <tt>matrix.get(row,column) == values[row + column*rows]</tt>. The values
   * are copied.
   *
   * @param values
   *            One-dimensional array of doubles, packed by columns (ala
   *            Fortran).
   * @param rows
   *            the number of rows.
   * @exception IllegalArgumentException
   *                <tt>values.length</tt> must be a multiple of <tt>rows</tt>
   *                .
   */
  def make(values: Array[Double], rows: Int): StrideMatrix2D = {
    val columns = (if (rows != 0) values.length / rows else 0)
    if (rows * columns != values.length) throw new IllegalArgumentException("Array length must be a multiple of m.")
    val matrix = make(rows, columns)
    for (row <- 0 until rows; column <- 0 until columns) {
      matrix.setQuick(row, column, values(row + column * rows))
    }
    matrix
  }

  /**
   * Constructs a matrix with the given cell values. <tt>values</tt> is
   * required to have the form <tt>values[row][column]</tt> and have exactly
   * the same number of columns in every row.
   * <p>
   * The values are copied. So subsequent changes in <tt>values</tt> are not
   * reflected in the matrix, and vice-versa.
   *
   * @param values
   *            The values to be filled into the new matrix.
   * @throws IllegalArgumentException
   *             if
   *             <tt>for any 1 &lt;= row &lt; values.length: values[row].length != values[row-1].length</tt>
   *             .
   */
  def make(values: Array[Array[Double]]): StrideMatrix2D = {
    if (this == sparse) new SparseDoubleMatrix2D(values) else new DenseMatrix2D(values)
  }

  /**
   * Constructs a matrix with the given shape, each cell initialized with
   * zero.
   */
  def make(rows: Int, columns: Int): StrideMatrix2D = {
    if (this == sparse) {
      new SparseDoubleMatrix2D(rows, columns)
    } else {
      new DenseMatrix2D(rows, columns)
    }
  }

  /**
   * Constructs a matrix with the given shape, each cell initialized with the
   * given value.
   */
  def make(rows: Int, columns: Int, initialValue: Double): StrideMatrix2D = {
    if (initialValue == 0) return make(rows, columns)
    make(rows, columns).assign(initialValue)
  }

  /**
   * Constructs a matrix with uniformly distributed values in <tt>(0,1)</tt>
   * (exclusive).
   */
  def random(rows: Int, columns: Int): StrideMatrix2D = {
    make(rows, columns).assign(cern.jet.math.tdouble.DoubleFunctions.random())
  }

  /**
   * C = A||A||..||A; Constructs a new matrix which is duplicated both along
   * the row and column dimension. Example:
   *
   * <pre>
   * 	 0 1
   * 	 2 3
   * 	 repeat(2,3) --&gt;
   * 	 0 1 0 1 0 1
   * 	 2 3 2 3 2 3
   * 	 0 1 0 1 0 1
   * 	 2 3 2 3 2 3
   *
   * </pre>
   */
  def repeat(A: StrideMatrix2D, rowRepeat: Int, columnRepeat: Int): StrideMatrix2D = {
    val r = A.rows()
    val c = A.columns()
    val matrix = make(r * rowRepeat, c * columnRepeat)
    var i = rowRepeat
    while (i >= 0) {
      var j = columnRepeat
      while (j >= 0) {
        matrix.viewPart(r * i, c * j, r, c).assign(A)
      }
    }
    matrix
  }

  /**
   * Modifies the given matrix to be a randomly sampled matrix. Randomly picks
   * exactly <tt>Math.round(rows*columns*nonZeroFraction)</tt> cells and
   * initializes them to <tt>value</tt>, all the rest will be initialized to
   * zero. Note that this is not the same as setting each cell with
   * probability <tt>nonZeroFraction</tt> to <tt>value</tt>. Note: The random
   * seed is a constant.
   *
   * @throws IllegalArgumentException
   *             if <tt>nonZeroFraction < 0 || nonZeroFraction > 1</tt>.
   * @see cern.jet.random.tdouble.sampling.DoubleRandomSampler
   */
  def sample(matrix: StrideMatrix2D, value: Double, nonZeroFraction: Double): StrideMatrix2D = {
    val rows = matrix.rows()
    val columns = matrix.columns()
    val epsilon = 1e-09
    if (nonZeroFraction < 0 - epsilon || nonZeroFraction > 1 + epsilon) throw new IllegalArgumentException()
    if (nonZeroFraction < 0) nonZeroFraction = 0
    if (nonZeroFraction > 1) nonZeroFraction = 1
    matrix.assign(0)
    val size = rows * columns
    val n = Math.round(size * nonZeroFraction).toInt
    if (n == 0) return matrix
    val sampler = new cern.jet.random.tdouble.sampling.DoubleRandomSamplingAssistant(n, size, new cern.jet.random.tdouble.engine.DoubleMersenneTwister())
    for (i <- 0 until size if sampler.sampleNextElement()) {
      val row = (i / columns)
      val column = (i % columns)
      matrix.setQuick(row, column, value)
    }
    matrix
  }

  /**
   * Constructs a randomly sampled matrix with the given shape. Randomly picks
   * exactly <tt>Math.round(rows*columns*nonZeroFraction)</tt> cells and
   * initializes them to <tt>value</tt>, all the rest will be initialized to
   * zero. Note that this is not the same as setting each cell with
   * probability <tt>nonZeroFraction</tt> to <tt>value</tt>. Note: The random
   * seed is a constant.
   *
   * @throws IllegalArgumentException
   *             if <tt>nonZeroFraction < 0 || nonZeroFraction > 1</tt>.
   * @see cern.jet.random.tdouble.sampling.DoubleRandomSampler
   */
  def sample(rows: Int,
      columns: Int,
      value: Double,
      nonZeroFraction: Double): StrideMatrix2D = {
    val matrix = make(rows, columns)
    sample(matrix, value, nonZeroFraction)
    matrix
  }

  /**
   * Constructs a 1d matrix of the right dynamic type.
   */
  protected def make1D(size: Int): StrideMatrix1D = make(0, 0).like1D(size)
}
