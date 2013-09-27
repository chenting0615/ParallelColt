package cern.colt.matrix.tdouble.impl

import cern.colt.function.tdouble.Function3
import cern.colt.list.tdouble.DoubleArrayList
import cern.colt.list.tint.IntArrayList
import cern.colt.matrix.tdouble.StrideMatrix1D
import cern.colt.matrix.tdouble.StrideMatrix2D
import cern.jet.math.tdouble.DoubleFunctions
import edu.emory.mathcs.utils.ConcurrencyUtils
import java.util.Arrays
import java.util.concurrent.Future
import SparseRCDoubleMatrix2D._
//remove if not needed
import scala.collection.JavaConversions._

object SparseRCDoubleMatrix2D {

  private def searchFromTo(list: Array[Int],
      key: Int,
      from: Int,
      to: Int): Int = {
    while (from <= to) {
      if (list(from) == key) {
        return from
      } else {
        from += 1
      }
    }
    -(from + 1)
  }
}

/**
 * Sparse row-compressed 2-d matrix holding <tt>double</tt> elements. First see
 * the <a href="package-summary.html">package summary</a> and javadoc <a
 * href="package-tree.html">tree view</a> to get the broad picture.
 * <p>
 * <b>Implementation:</b>
 * <p>
 * Internally uses the standard sparse row-compressed format<br>
 * Note that this implementation is not synchronized.
 * <p>
 * <b>Memory requirements:</b>
 * <p>
 * Cells that
 * <ul>
 * <li>are never set to non-zero values do not use any memory.
 * <li>switch from zero to non-zero state do use memory.
 * <li>switch back from non-zero to zero state also do use memory. Their memory
 * is <i>not</i> automatically reclaimed (because of the lists vs. arrays).
 * Reclamation can be triggered via {@link #trimToSize()}.
 * </ul>
 * <p>
 * <tt>memory [bytes] = 4*rows + 12 * nonZeros</tt>. <br>
 * Where <tt>nonZeros = cardinality()</tt> is the number of non-zero cells.
 * Thus, a 1000 x 1000 matrix with 1000000 non-zero cells consumes 11.5 MB. The
 * same 1000 x 1000 matrix with 1000 non-zero cells consumes 15 KB.
 * <p>
 * <b>Time complexity:</b>
 * <p>
 * Getting a cell value takes time<tt> O(log nzr)</tt> where <tt>nzr</tt> is the
 * number of non-zeros of the touched row. This is usually quick, because
 * typically there are only few nonzeros per row. So, in practice, get has
 * <i>expected</i> constant time. Setting a cell value takes <i> </i>worst-case
 * time <tt>O(nz)</tt> where <tt>nzr</tt> is the total number of non-zeros in
 * the matrix. This can be extremely slow, but if you traverse coordinates
 * properly (i.e. upwards), each write is done much quicker:
 * <table>
 * <td class="PRE">
 *
 * <pre>
 * // rather quick
 * matrix.assign(0);
 * for (int row = 0; row &lt; rows; row++) {
 *     for (int column = 0; column &lt; columns; column++) {
 *         if (someCondition)
 *             matrix.setQuick(row, column, someValue);
 *     }
 * }
 *
 * // poor
 * matrix.assign(0);
 * for (int row = rows; --row &gt;= 0;) {
 *     for (int column = columns; --column &gt;= 0;) {
 *         if (someCondition)
 *             matrix.setQuick(row, column, someValue);
 *     }
 * }
 * </pre>
 *
 * </td>
 * </table>
 * If for whatever reasons you can't iterate properly, consider to create an
 * empty dense matrix, store your non-zeros in it, then call
 * <tt>sparse.assign(dense)</tt>. Under the circumstances, this is still rather
 * quick.
 * <p>
 * Fast iteration over non-zeros can be done via {@link #forEachNonZero}, which
 * supplies your function with row, column and value of each nonzero. Although
 * the internally implemented version is a bit more sophisticated, here is how a
 * quite efficient user-level matrix-vector multiplication could look like:
 * <table>
 * <td class="PRE">
 *
 * <pre>
 * // Linear algebraic y = A * x
 * A.forEachNonZero(new cern.colt.function.IntIntDoubleFunction() {
 *     public double apply(int row, int column, double value) {
 *         y.setQuick(row, y.getQuick(row) + value * x.getQuick(column));
 *         return value;
 *     }
 * });
 * </pre>
 *
 * </td>
 * </table>
 * <p>
 * Here is how a a quite efficient user-level combined scaling operation could
 * look like:
 * <table>
 * <td class="PRE">
 *
 * <pre>
 * // Elementwise A = A + alpha*B
 * B.forEachNonZero(new cern.colt.function.IntIntDoubleFunction() {
 *     public double apply(int row, int column, double value) {
 *         A.setQuick(row, column, A.getQuick(row, column) + alpha * value);
 *         return value;
 *     }
 * });
 * </pre>
 *
 * </td>
 * </table>
 * Method
 * {@link #assign(DoubleMatrix2D,cern.colt.tdouble.DoubleDoubleFunction)}
 * does just that if you supply
 * {@link cern.jet.math.tdouble.DoubleFunctions#plusMultSecond} as argument.
 *
 *
 * @author wolfgang.hoschek@cern.ch
 * @version 0.9, 04/14/2000
 */
@SerialVersionUID(1L)
class SparseRCDoubleMatrix2D(rows: Int, columns: Int, nzmax: Int) extends WrapperMatrix2D(null) {

  protected var rowPointers: Array[Int] = new Array[Int](rows + 1)

  protected var columnIndexes: Array[Int] = new Array[Int](nzmax)

  protected var values: Array[Double] = new Array[Double](nzmax)

  protected var columnIndexesSorted: Boolean = false

  try {
    setUp(rows, columns)
  } catch {
    case exc: IllegalArgumentException => if ("matrix too large" != exc.getMessage) throw exc
  }

  /**
   * Constructs a matrix with a copy of the given values. <tt>values</tt> is
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
  def this(values: Array[Array[Double]]) {
    this(values.length, if (values.length == 0) 0 else values(0).length)
    assign(values)
  }

  /**
   * Constructs a matrix with a given number of rows and columns. All entries
   * are initially <tt>0</tt>.
   *
   * @param rows
   *            the number of rows the matrix shall have.
   * @param columns
   *            the number of columns the matrix shall have.
   * @throws IllegalArgumentException
   *             if
   *             <tt>rows<0 || columns<0 || (double)columns*rows > Integer.MAX_VALUE</tt>
   *             .
   */
  def this(rows: Int, columns: Int) {
    this(rows, columns, Math.min(10l * rows, Integer.MAX_VALUE).toInt)
  }

  /**
   * Constructs a matrix with indexes given in the coordinate format and
   * single value.
   *
   * @param rows
   *            the number of rows the matrix shall have.
   * @param columns
   *            the number of columns the matrix shall have.
   * @param rowIndexes
   *            row indexes
   * @param columnIndexes
   *            column indexes
   * @param value
   *            numerical value, cannot be zero
   * @param removeDuplicates
   *            if true, then duplicates (if any) are removed
   * @param sortColumnIndexes
   *            if true, then column indexes are sorted
   */
  def this(rows: Int,
      columns: Int,
      rowIndexes: Array[Int],
      columnIndexes: Array[Int],
      value: Double,
      removeDuplicates: Boolean,
      sortColumnIndexes: Boolean) {
    super(null)
    try {
      setUp(rows, columns)
    } catch {
      case exc: IllegalArgumentException => if ("matrix too large" != exc.getMessage) throw exc
    }
    if (rowIndexes.length != columnIndexes.length) {
      throw new IllegalArgumentException("rowIndexes.length != columnIndexes.length")
    }
    if (value == 0) {
      throw new IllegalArgumentException("value cannot be 0")
    }
    val nz = Math.max(rowIndexes.length, 1)
    this.columnIndexes = Array.ofDim[Int](nz)
    this.values = Array.ofDim[Double](nz)
    this.rowPointers = Array.ofDim[Int](rows + 1)
    val w = Array.ofDim[Int](rows)
    val r: Int = 0
    for (k <- 0 until nz) {
      w(rowIndexes(k)) += 1
    }
    cumsum(this.rowPointers, w, rows)
    for (k <- 0 until nz) {
      this.columnIndexes(r = w(rowIndexes(k)) += 1) = columnIndexes(k)
      this.values(r) = value
    }
    if (removeDuplicates) {
      removeDuplicates()
    }
    if (sortColumnIndexes) {
      sortColumnIndexes()
    }
  }

  /**
   * Constructs a matrix with indexes and values given in the coordinate
   * format.
   *
   * @param rows
   *            the number of rows the matrix shall have.
   * @param columns
   *            the number of columns the matrix shall have.
   * @param rowIndexes
   *            row indexes
   * @param columnIndexes
   *            column indexes
   * @param values
   *            numerical values
   * @param removeDuplicates
   *            if true, then duplicates (if any) are removed
   * @param removeZeroes
   *            if true, then zeroes (if any) are removed
   * @param sortColumnIndexes
   *            if true, then column indexes are sorted
   */
  def this(rows: Int,
      columns: Int,
      rowIndexes: Array[Int],
      columnIndexes: Array[Int],
      values: Array[Double],
      removeDuplicates: Boolean,
      removeZeroes: Boolean,
      sortColumnIndexes: Boolean) {
    super(null)
    try {
      setUp(rows, columns)
    } catch {
      case exc: IllegalArgumentException => if ("matrix too large" != exc.getMessage) throw exc
    }
    if (rowIndexes.length != columnIndexes.length) {
      throw new IllegalArgumentException("rowIndexes.length != columnIndexes.length")
    } else if (rowIndexes.length != values.length) {
      throw new IllegalArgumentException("rowIndexes.length != values.length")
    }
    val nz = Math.max(rowIndexes.length, 1)
    this.columnIndexes = Array.ofDim[Int](nz)
    this.values = Array.ofDim[Double](nz)
    this.rowPointers = Array.ofDim[Int](rows + 1)
    val w = Array.ofDim[Int](rows)
    val r: Int = 0
    for (k <- 0 until nz) {
      w(rowIndexes(k)) += 1
    }
    cumsum(this.rowPointers, w, rows)
    for (k <- 0 until nz) {
      this.columnIndexes(r = w(rowIndexes(k)) += 1) = columnIndexes(k)
      this.values(r) = values(k)
    }
    if (removeZeroes) {
      removeZeroes()
    }
    if (removeDuplicates) {
      removeDuplicates()
    }
    if (sortColumnIndexes) {
      sortColumnIndexes()
    }
  }

  /**
   * Constructs a matrix with given parameters. The arrays are not copied.
   *
   * @param rows
   *            the number of rows the matrix shall have.
   * @param columns
   *            the number of columns the matrix shall have.
   * @param rowPointers
   *            row pointers
   * @param columnIndexes
   *            column indexes
   * @param values
   *            numerical values
   */
  def this(rows: Int,
      columns: Int,
      rowPointers: Array[Int],
      columnIndexes: Array[Int],
      values: Array[Double]) {
    super(null)
    try {
      setUp(rows, columns)
    } catch {
      case exc: IllegalArgumentException => if ("matrix too large" != exc.getMessage) throw exc
    }
    if (rowPointers.length != rows + 1) {
      throw new IllegalArgumentException("rowPointers.length != rows + 1")
    }
    this.rowPointers = rowPointers
    this.columnIndexes = columnIndexes
    this.values = values
  }

  def assign(function: cern.colt.function.tdouble.Function1): StrideMatrix2D = {
    if (function.isInstanceOf[cern.jet.math.tdouble.DoubleMult]) {
      val alpha = function.asInstanceOf[cern.jet.math.tdouble.DoubleMult].multiplicator
      if (alpha == 1) return this
      if (alpha == 0) return assign(0)
      if (alpha != alpha) return assign(alpha)
      val nz = cardinality()
      for (j <- 0 until nz) {
        values(j) *= alpha
      }
    } else {
      forEachNonZero(new cern.colt.function.tdouble.Function3() {

        def apply(i: Int, j: Int, value: Double): Double = return function.apply(value)
      })
    }
    this
  }

  def assign(value: Double): StrideMatrix2D = {
    if (value == 0) {
      Arrays.fill(rowPointers, 0)
      Arrays.fill(columnIndexes, 0)
      Arrays.fill(values, 0)
    } else {
      val nnz = cardinality()
      for (i <- 0 until nnz) {
        values(i) = value
      }
    }
    this
  }

  def assign(source: StrideMatrix2D): StrideMatrix2D = {
    if (source == this) return this
    checkShape(source)
    if (source.isInstanceOf[SparseRCDoubleMatrix2D]) {
      val other = source.asInstanceOf[SparseRCDoubleMatrix2D]
      System.arraycopy(other.rowPointers, 0, rowPointers, 0, rows + 1)
      val nzmax = other.columnIndexes.length
      if (columnIndexes.length < nzmax) {
        columnIndexes = Array.ofDim[Int](nzmax)
        values = Array.ofDim[Double](nzmax)
      }
      System.arraycopy(other.columnIndexes, 0, columnIndexes, 0, nzmax)
      System.arraycopy(other.values, 0, values, 0, nzmax)
      columnIndexesSorted = other.columnIndexesSorted
    } else if (source.isInstanceOf[SparseCCDoubleMatrix2D]) {
      val other = source.asInstanceOf[SparseCCDoubleMatrix2D].getTranspose
      rowPointers = other.getColumnPointers
      columnIndexes = other.getRowIndexes
      values = other.getValues
      columnIndexesSorted = true
    } else {
      assign(0)
      source.forEachNonZero(new cern.colt.function.tdouble.Function3() {

        def apply(i: Int, j: Int, value: Double): Double = {
          setQuick(i, j, value)
          return value
        }
      })
    }
    this
  }

  def assign(y: StrideMatrix2D, function: cern.colt.function.tdouble.DoubleDoubleFunction): StrideMatrix2D = {
    checkShape(y)
    if ((y.isInstanceOf[SparseRCDoubleMatrix2D]) &&
      (function == cern.jet.math.tdouble.DoubleFunctions.plus)) {
      val yy = y.asInstanceOf[SparseRCDoubleMatrix2D]
      val rowPointersY = yy.rowPointers
      val columnIndexesY = yy.columnIndexes
      val valuesY = yy.values
      val rowPointersC = Array.ofDim[Int](rows + 1)
      val cnz = Math.max(columnIndexes.length, Math.min(Integer.MAX_VALUE, rowPointers(rows).toLong + rowPointersY(rows).toLong).toInt)
      val columnIndexesC = Array.ofDim[Int](cnz)
      val valuesC = Array.ofDim[Double](cnz)
      val nrow = rows
      val ncol = columns
      val nzmax = valuesC.length
      if (function == cern.jet.math.tdouble.DoubleFunctions.plus) {
        var kc = 0
        rowPointersC(0) = kc
        var j1: Int = 0
        var j2: Int = 0
        for (i <- 0 until nrow) {
          var ka = rowPointers(i)
          var kb = rowPointersY(i)
          val kamax = rowPointers(i + 1) - 1
          val kbmax = rowPointersY(i + 1) - 1
          while (ka <= kamax || kb <= kbmax) {
            j1 = if (ka <= kamax) columnIndexes(ka) else ncol + 1
            j2 = if (kb <= kbmax) columnIndexesY(kb) else ncol + 1
            if (j1 == j2) {
              valuesC(kc) = values(ka) + valuesY(kb)
              columnIndexesC(kc) = j1
              ka += 1
              kb += 1
              kc += 1
            } else if (j1 < j2) {
              columnIndexesC(kc) = j1
              valuesC(kc) = values(ka)
              ka += 1
              kc += 1
            } else if (j1 > j2) {
              columnIndexesC(kc) = j2
              valuesC(kc) = valuesY(kb)
              kb += 1
              kc += 1
            }
            if (kc >= nzmax) {
              throw new IllegalArgumentException("The number of elements in C exceeds nzmax")
            }
          }
          rowPointersC(i + 1) = kc
        }
        this.rowPointers = rowPointersC
        this.columnIndexes = columnIndexesC
        this.values = valuesC
        return this
      }
    }
    if (function.isInstanceOf[cern.jet.math.tdouble.DoublePlusMultSecond]) {
      val alpha = function.asInstanceOf[cern.jet.math.tdouble.DoublePlusMultSecond].multiplicator
      if (alpha == 0) return this
      y.forEachNonZero(new cern.colt.function.tdouble.Function3() {

        def apply(i: Int, j: Int, value: Double): Double = {
          setQuick(i, j, getQuick(i, j) + alpha * value)
          return value
        }
      })
      return this
    }
    if (function.isInstanceOf[cern.jet.math.tdouble.DoublePlusMultFirst]) {
      val alpha = function.asInstanceOf[cern.jet.math.tdouble.DoublePlusMultFirst].multiplicator
      if (alpha == 0) return assign(y)
      y.forEachNonZero(new cern.colt.function.tdouble.Function3() {

        def apply(i: Int, j: Int, value: Double): Double = {
          setQuick(i, j, alpha * getQuick(i, j) + value)
          return value
        }
      })
      return this
    }
    if (function == cern.jet.math.tdouble.DoubleFunctions.mult) {
      var i = rows
      while (i >= 0) {
        val low = rowPointers(i)
        var k = rowPointers(i + 1)
        while (k >= low) {
          val j = columnIndexes(k)
          values(k) *= y.getQuick(i, j)
          if (values(k) == 0) remove(i, j)
        }
      }
      return this
    }
    if (function == cern.jet.math.tdouble.DoubleFunctions.div) {
      var i = rows
      while (i >= 0) {
        val low = rowPointers(i)
        var k = rowPointers(i + 1)
        while (k >= low) {
          val j = columnIndexes(k)
          values(k) /= y.getQuick(i, j)
          if (values(k) == 0) remove(i, j)
        }
      }
      return this
    }
    super.assign(y, function)
  }

  def cardinality(): Int = rowPointers(rows)

  def forEachNonZero(function: cern.colt.function.tdouble.Function3): StrideMatrix2D = {
    var i = rows
    while (i >= 0) {
      val low = rowPointers(i)
      var k = rowPointers(i + 1)
      while (k >= low) {
        val j = columnIndexes(k)
        val value = values(k)
        val r = function.apply(i, j, value)
        if (r != value) values(k) = r
      }
    }
    this
  }

  override def forEachNonZeroInRow(rowIdx: Int, function: Function3): StrideMatrix2D = {
    super.forEachNonZeroInRow(rowIdx, function)
  }

  override def forEachNonZeroInColumn(colIdx: Int, function: Function3): StrideMatrix2D = {
    super.forEachNonZeroInColumn(colIdx, function)
  }

  /**
   * Returns a new matrix that has the same elements as this matrix, but is in
   * a column-compressed form. This method creates a new object (not a view),
   * so changes in the returned matrix are NOT reflected in this matrix.
   *
   * @return this matrix in a column-compressed form
   */
  def getColumnCompressed(): SparseCCDoubleMatrix2D = {
    val tr = getTranspose
    val cc = new SparseCCDoubleMatrix2D(rows, columns)
    cc.dcs.i = tr.columnIndexes
    cc.dcs.p = tr.rowPointers
    cc.dcs.x = tr.values
    cc.dcs.nzmax = tr.values.length
    cc.rowIndexesSorted = true
    cc
  }

  /**
   * Returns column indexes
   *
   * @return column indexes
   */
  def getColumnIndexes(): Array[Int] = columnIndexes

  /**
   * Returns a new matrix that has the same elements as this matrix, but is in
   * a dense form. This method creates a new object (not a view), so changes
   * in the returned matrix are NOT reflected in this matrix.
   *
   * @return this matrix in a dense form
   */
  def getDense(): DenseMatrix2D = {
    val dense = new DenseMatrix2D(rows, columns)
    forEachNonZero(new cern.colt.function.tdouble.Function3() {

      def apply(i: Int, j: Int, value: Double): Double = {
        dense.setQuick(i, j, getQuick(i, j))
        return value
      }
    })
    dense
  }

  def getQuick(row: Int, column: Int): Double = {
    synchronized {
      val k = searchFromTo(columnIndexes, column, rowPointers(row), rowPointers(row + 1) - 1)
      var v = 0
      if (k >= 0) v = values(k)
      v
    }
  }

  /**
   * Returns row pointers
   *
   * @return row pointers
   */
  def getRowPointers(): Array[Int] = rowPointers

  /**
   * Returns a new matrix that is the transpose of this matrix. This method
   * creates a new object (not a view), so changes in the returned matrix are
   * NOT reflected in this matrix.
   *
   * @return the transpose of this matrix
   */
  def getTranspose(): SparseRCDoubleMatrix2D = {
    val nnz = rowPointers(rows)
    val w = Array.ofDim[Int](columns)
    val rowPointersT = Array.ofDim[Int](columns + 1)
    val columnIndexesT = Array.ofDim[Int](nnz)
    val valuesT = Array.ofDim[Double](nnz)
    for (p <- 0 until nnz) {
      w(columnIndexes(p)) += 1
    }
    cumsum(rowPointersT, w, columns)
    val q: Int = 0
    for (j <- 0 until rows) {
      val high = rowPointers(j + 1)
      for (p <- rowPointers(j) until high) {
        columnIndexesT(q = w(columnIndexes(p)) += 1) = j
        valuesT(q) = values(p)
      }
    }
    val T = new SparseRCDoubleMatrix2D(columns, rows)
    T.rowPointers = rowPointersT
    T.columnIndexes = columnIndexesT
    T.values = valuesT
    T
  }

  /**
   * Returns numerical values
   *
   * @return numerical values
   */
  def getValues(): Array[Double] = values

  /**
   * Returns true if column indexes are sorted, false otherwise
   *
   * @return true if column indexes are sorted, false otherwise
   */
  def hasColumnIndexesSorted(): Boolean = columnIndexesSorted

  def like(rows: Int, columns: Int): StrideMatrix2D = {
    new SparseRCDoubleMatrix2D(rows, columns)
  }

  def like1D(size: Int): StrideMatrix1D = new SparseDoubleMatrix1D(size)

  /**
   * Removes (sums) duplicate entries (if any}
   */
  def removeDuplicates() {
    val nz = 0
    var q: Int = 0
    var i: Int = 0
    val w = Array.ofDim[Int](columns)
    i = 0
    while (i < columns) {w(i) = -1i += 1
    }
    for (j <- 0 until rows) {
      q = nz
      for (p <- rowPointers(j) until rowPointers(j + 1)) {
        i = columnIndexes(p)
        if (w(i) >= q) {
          values(w(i)) += values(p)
        } else {
          w(i) = nz
          columnIndexes(nz) = i
          values(nz += 1) = values(p)
        }
      }
      rowPointers(j) = q
    }
    rowPointers(rows) = nz
  }

  /**
   * Removes zero entries (if any)
   */
  def removeZeroes() {
    val nz = 0
    val eps = Math.pow(2, -52)
    for (j <- 0 until rows) {
      var p = rowPointers(j)
      rowPointers(j) = nz
      while (p < rowPointers(j + 1)) {
        if (Math.abs(values(p)) > eps) {
          values(nz) = values(p)
          columnIndexes(nz += 1) = columnIndexes(p)
        }
        p += 1
      }
    }
    rowPointers(rows) = nz
  }

  def setQuick(row: Int, column: Int, value: Double) {
    synchronized {
      var k = searchFromTo(columnIndexes, column, rowPointers(row), rowPointers(row + 1) - 1)
      if (k >= 0) {
        if (value == 0) remove(row, k) else values(k) = value
        return
      }
      if (value != 0) {
        k = -k - 1
        insert(row, column, k, value)
      }
    }
  }

  /**
   * Sorts column indexes
   */
  def sortColumnIndexes() {
    var T = getTranspose
    this.rows = T.rows
    this.columns = T.columns
    this.columnIndexes = T.columnIndexes
    this.rowPointers = T.rowPointers
    this.values = T.values
    T = getTranspose
    this.rows = T.rows
    this.columns = T.columns
    this.columnIndexes = T.columnIndexes
    this.rowPointers = T.rowPointers
    this.values = T.values
    columnIndexesSorted = true
  }

  override def toString(): String = {
    val builder = new StringBuilder()
    builder.append(rows).append(" x ").append(columns).append(" sparse matrix, nnz = ")
      .append(cardinality())
      .append('\n')
    for (i <- 0 until rows) {
      val high = rowPointers(i + 1)
      for (j <- rowPointers(i) until high) {
        builder.append('(').append(i).append(',').append(columnIndexes(j))
          .append(')')
          .append('\t')
          .append(values(j))
          .append('\n')
      }
    }
    builder.toString
  }

  def trimToSize() {
    realloc(0)
  }

  def zMult(y: StrideMatrix1D,
      z: StrideMatrix1D,
      alpha: Double,
      beta: Double,
      transposeA: Boolean): StrideMatrix1D = {
    val rowsA = if (transposeA) columns else rows
    val columnsA = if (transposeA) rows else columns
    val ignore = (z == null || !transposeA)
    if (z == null) z = new DenseMatrix1D(rowsA)
    if (!(y.isInstanceOf[DenseMatrix1D] && z.isInstanceOf[DenseMatrix1D])) {
      return super.zMult(y, z, alpha, beta, transposeA)
    }
    if (columnsA != y.size || rowsA > z.size) throw new IllegalArgumentException("Incompatible args: " +
      ((if (transposeA) viewDice() else this).toShapeString()) +
      ", " +
      y.toShapeString() +
      ", " +
      z.toShapeString())
    val zz = z.asInstanceOf[DenseMatrix1D]
    val elementsZ = zz.elements
    val strideZ = zz.stride()
    val zeroZ = z.index(0).toInt
    val yy = y.asInstanceOf[DenseMatrix1D]
    val elementsY = yy.elements
    val strideY = yy.stride()
    val zeroY = y.index(0).toInt
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if (transposeA) {
      if ((!ignore) && (beta != 1.0)) z.assign(cern.jet.math.tdouble.DoubleFunctions.mult(beta))
      if ((nthreads > 1) &&
        (cardinality() >= ConcurrencyUtils.getThreadsBeginN_2D)) {
        nthreads = 2
        val futures = Array.ofDim[Future](nthreads)
        val result = Array.ofDim[Double](rowsA)
        val k = rows / nthreads
        for (j <- 0 until nthreads) {
          val firstRow = j * k
          val lastRow = if ((j == nthreads - 1)) rows else firstRow + k
          val threadID = j
          futures(j) = ConcurrencyUtils.submit(new Runnable() {

            def run() {
              if (threadID == 0) {
                for (i <- firstRow until lastRow) {
                  var high = rowPointers(i + 1)
                  var yElem = alpha * elementsY(zeroY + strideY * i)
                  for (k <- rowPointers(i) until high) {
                    var j = columnIndexes(k)
                    elementsZ(zeroZ + strideZ * j) += values(k) * yElem
                  }
                }
              } else {
                for (i <- firstRow until lastRow) {
                  var high = rowPointers(i + 1)
                  var yElem = alpha * elementsY(zeroY + strideY * i)
                  for (k <- rowPointers(i) until high) {
                    var j = columnIndexes(k)
                    result(j) += values(k) * yElem
                  }
                }
              }
            }
          })
        }
        ConcurrencyUtils.waitForCompletion(futures)
        val rem = rowsA % 10
        var j = rem
        while (j < rowsA) {
          elementsZ(zeroZ + j * strideZ) += result(j)
          elementsZ(zeroZ + (j + 1) * strideZ) += result(j + 1)
          elementsZ(zeroZ + (j + 2) * strideZ) += result(j + 2)
          elementsZ(zeroZ + (j + 3) * strideZ) += result(j + 3)
          elementsZ(zeroZ + (j + 4) * strideZ) += result(j + 4)
          elementsZ(zeroZ + (j + 5) * strideZ) += result(j + 5)
          elementsZ(zeroZ + (j + 6) * strideZ) += result(j + 6)
          elementsZ(zeroZ + (j + 7) * strideZ) += result(j + 7)
          elementsZ(zeroZ + (j + 8) * strideZ) += result(j + 8)
          elementsZ(zeroZ + (j + 9) * strideZ) += result(j + 9)
          j += 10
        }
        for (j <- 0 until rem) {
          elementsZ(zeroZ + j * strideZ) += result(j)
        }
      } else {
        for (i <- 0 until rows) {
          val high = rowPointers(i + 1)
          val yElem = alpha * elementsY(zeroY + strideY * i)
          for (k <- rowPointers(i) until high) {
            val j = columnIndexes(k)
            elementsZ(zeroZ + strideZ * j) += values(k) * yElem
          }
        }
      }
      return z
    }
    if ((nthreads > 1) &&
      (cardinality() >= ConcurrencyUtils.getThreadsBeginN_2D)) {
      nthreads = Math.min(nthreads, rows)
      val futures = Array.ofDim[Future](nthreads)
      val k = rows / nthreads
      for (j <- 0 until nthreads) {
        val firstRow = j * k
        val lastRow = if ((j == nthreads - 1)) rows else firstRow + k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            var zidx = zeroZ + firstRow * strideZ
            var k = rowPointers(firstRow)
            if (beta == 0.0) {
              for (i <- firstRow until lastRow) {
                var sum = 0
                var high = rowPointers(i + 1)
                while (k + 10 < high) {
                  var ind = k + 9
                  sum += values(ind) *
                    elementsY(zeroY + strideY * columnIndexes(ind -= 1)) +
                    values(ind) *
                    elementsY(zeroY + strideY * columnIndexes(ind -= 1)) +
                    values(ind) *
                    elementsY(zeroY + strideY * columnIndexes(ind -= 1)) +
                    values(ind) *
                    elementsY(zeroY + strideY * columnIndexes(ind -= 1)) +
                    values(ind) *
                    elementsY(zeroY + strideY * columnIndexes(ind -= 1)) +
                    values(ind) *
                    elementsY(zeroY + strideY * columnIndexes(ind -= 1)) +
                    values(ind) *
                    elementsY(zeroY + strideY * columnIndexes(ind -= 1)) +
                    values(ind) *
                    elementsY(zeroY + strideY * columnIndexes(ind -= 1)) +
                    values(ind) *
                    elementsY(zeroY + strideY * columnIndexes(ind -= 1)) +
                    values(ind) * elementsY(zeroY + strideY * columnIndexes(ind))
                  k += 10
                }
                while (k < high) {
                  sum += values(k) * elementsY(columnIndexes(k))
                  k += 1
                }
                elementsZ(zidx) = alpha * sum
                zidx += strideZ
              }
            } else {
              for (i <- firstRow until lastRow) {
                var sum = 0
                var high = rowPointers(i + 1)
                while (k + 10 < high) {
                  var ind = k + 9
                  sum += values(ind) *
                    elementsY(zeroY + strideY * columnIndexes(ind -= 1)) +
                    values(ind) *
                    elementsY(zeroY + strideY * columnIndexes(ind -= 1)) +
                    values(ind) *
                    elementsY(zeroY + strideY * columnIndexes(ind -= 1)) +
                    values(ind) *
                    elementsY(zeroY + strideY * columnIndexes(ind -= 1)) +
                    values(ind) *
                    elementsY(zeroY + strideY * columnIndexes(ind -= 1)) +
                    values(ind) *
                    elementsY(zeroY + strideY * columnIndexes(ind -= 1)) +
                    values(ind) *
                    elementsY(zeroY + strideY * columnIndexes(ind -= 1)) +
                    values(ind) *
                    elementsY(zeroY + strideY * columnIndexes(ind -= 1)) +
                    values(ind) *
                    elementsY(zeroY + strideY * columnIndexes(ind -= 1)) +
                    values(ind) * elementsY(zeroY + strideY * columnIndexes(ind))
                  k += 10
                }
                while (k < high) {
                  sum += values(k) * elementsY(columnIndexes(k))
                  k += 1
                }
                elementsZ(zidx) = alpha * sum + beta * elementsZ(zidx)
                zidx += strideZ
              }
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
    } else {
      var zidx = zeroZ
      var k = rowPointers(0)
      if (beta == 0.0) {
        for (i <- 0 until rows) {
          var sum = 0
          val high = rowPointers(i + 1)
          while (k + 10 < high) {
            val ind = k + 9
            sum += values(ind) *
              elementsY(zeroY + strideY * columnIndexes(ind -= 1)) +
              values(ind) *
              elementsY(zeroY + strideY * columnIndexes(ind -= 1)) +
              values(ind) *
              elementsY(zeroY + strideY * columnIndexes(ind -= 1)) +
              values(ind) *
              elementsY(zeroY + strideY * columnIndexes(ind -= 1)) +
              values(ind) *
              elementsY(zeroY + strideY * columnIndexes(ind -= 1)) +
              values(ind) *
              elementsY(zeroY + strideY * columnIndexes(ind -= 1)) +
              values(ind) *
              elementsY(zeroY + strideY * columnIndexes(ind -= 1)) +
              values(ind) *
              elementsY(zeroY + strideY * columnIndexes(ind -= 1)) +
              values(ind) *
              elementsY(zeroY + strideY * columnIndexes(ind -= 1)) +
              values(ind) * elementsY(zeroY + strideY * columnIndexes(ind))
            k += 10
          }
          while (k < high) {
            sum += values(k) * elementsY(columnIndexes(k))
            k += 1
          }
          elementsZ(zidx) = alpha * sum
          zidx += strideZ
        }
      } else {
        for (i <- 0 until rows) {
          var sum = 0
          val high = rowPointers(i + 1)
          while (k + 10 < high) {
            val ind = k + 9
            sum += values(ind) *
              elementsY(zeroY + strideY * columnIndexes(ind -= 1)) +
              values(ind) *
              elementsY(zeroY + strideY * columnIndexes(ind -= 1)) +
              values(ind) *
              elementsY(zeroY + strideY * columnIndexes(ind -= 1)) +
              values(ind) *
              elementsY(zeroY + strideY * columnIndexes(ind -= 1)) +
              values(ind) *
              elementsY(zeroY + strideY * columnIndexes(ind -= 1)) +
              values(ind) *
              elementsY(zeroY + strideY * columnIndexes(ind -= 1)) +
              values(ind) *
              elementsY(zeroY + strideY * columnIndexes(ind -= 1)) +
              values(ind) *
              elementsY(zeroY + strideY * columnIndexes(ind -= 1)) +
              values(ind) *
              elementsY(zeroY + strideY * columnIndexes(ind -= 1)) +
              values(ind) * elementsY(zeroY + strideY * columnIndexes(ind))
            k += 10
          }
          while (k < high) {
            sum += values(k) * elementsY(columnIndexes(k))
            k += 1
          }
          elementsZ(zidx) = alpha * sum + beta * elementsZ(zidx)
          zidx += strideZ
        }
      }
    }
    z
  }

  def zMult(B: StrideMatrix2D,
      C: StrideMatrix2D,
      alpha: Double,
      beta: Double,
      transposeA: Boolean,
      transposeB: Boolean): StrideMatrix2D = {
    var rowsA = rows
    var columnsA = columns
    if (transposeA) {
      rowsA = columns
      columnsA = rows
    }
    var rowsB = B.rows()
    var columnsB = B.columns()
    if (transposeB) {
      rowsB = B.columns()
      columnsB = B.rows()
    }
    val p = columnsB
    val ignore = (C == null)
    if (C == null) {
      C = if (B.isInstanceOf[SparseRCDoubleMatrix2D]) new SparseRCDoubleMatrix2D(rowsA, p, (rowsA * p)) else new DenseMatrix2D(rowsA,
        p)
    }
    if (rowsB != columnsA) throw new IllegalArgumentException("Matrix2D inner dimensions must agree:" + toShapeString() +
      ", " +
      (if (transposeB) B.viewDice() else B).toShapeString())
    if (C.rows() != rowsA || C.columns() != p) throw new IllegalArgumentException("Incompatible result matrix: " + toShapeString() + ", " +
      (if (transposeB) B.viewDice() else B).toShapeString() +
      ", " +
      C.toShapeString())
    if (this == C || B == C) throw new IllegalArgumentException("Matrices must not be identical")
    if (!ignore && beta != 1.0) {
      C.assign(cern.jet.math.tdouble.DoubleFunctions.mult(beta))
    }
    if ((B.isInstanceOf[DenseMatrix2D]) && (C.isInstanceOf[DenseMatrix2D])) {
      var AA: SparseRCDoubleMatrix2D = null
      AA = if (transposeA) getTranspose else this
      var BB: DenseMatrix2D = null
      BB = if (transposeB) B.viewDice().asInstanceOf[DenseMatrix2D] else B.asInstanceOf[DenseMatrix2D]
      val CC = C.asInstanceOf[DenseMatrix2D]
      val rowPointersA = AA.rowPointers
      val columnIndexesA = AA.columnIndexes
      val valuesA = AA.values
      for (ii <- 0 until rowsA) {
        val highA = rowPointersA(ii + 1)
        for (ka <- rowPointersA(ii) until highA) {
          val scal = valuesA(ka) * alpha
          val jj = columnIndexesA(ka)
          CC.viewRow(ii).assign(BB.viewRow(jj), DoubleFunctions.plusMultSecond(scal))
        }
      }
    } else if ((B.isInstanceOf[SparseRCDoubleMatrix2D]) && (C.isInstanceOf[SparseRCDoubleMatrix2D])) {
      var AA: SparseRCDoubleMatrix2D = null
      var BB: SparseRCDoubleMatrix2D = null
      val CC = C.asInstanceOf[SparseRCDoubleMatrix2D]
      AA = if (transposeA) getTranspose else this
      BB = if (transposeB) B.asInstanceOf[SparseRCDoubleMatrix2D].getTranspose else B.asInstanceOf[SparseRCDoubleMatrix2D]
      val rowPointersA = AA.rowPointers
      val columnIndexesA = AA.columnIndexes
      val valuesA = AA.values
      val rowPointersB = BB.rowPointers
      val columnIndexesB = BB.columnIndexes
      val valuesB = BB.values
      val rowPointersC = CC.rowPointers
      val columnIndexesC = CC.columnIndexes
      val valuesC = CC.values
      val nzmax = valuesC.length
      val iw = Array.ofDim[Int](columnsB + 1)
      for (i <- 0 until iw.length) {
        iw(i) = -1
      }
      var len = -1
      for (ii <- 0 until rowsA) {
        val highA = rowPointersA(ii + 1)
        for (ka <- rowPointersA(ii) until highA) {
          val scal = valuesA(ka) * alpha
          val jj = columnIndexesA(ka)
          val highB = rowPointersB(jj + 1)
          for (kb <- rowPointersB(jj) until highB) {
            val jcol = columnIndexesB(kb)
            val jpos = iw(jcol)
            if (jpos == -1) {
              len += 1
              if (len >= nzmax) {
                throw new IllegalArgumentException("The max number of nonzero elements in C is too small.")
              }
              columnIndexesC(len) = jcol
              iw(jcol) = len
              valuesC(len) = scal * valuesB(kb)
            } else {
              valuesC(jpos) += scal * valuesB(kb)
            }
          }
        }
        for (k <- rowPointersC(ii) until len + 1) {
          iw(columnIndexesC(k)) = -1
        }
        rowPointersC(ii + 1) = len + 1
      }
    } else {
      if (transposeB) {
        B = B.viewDice()
      }
      val Brows = Array.ofDim[StrideMatrix1D](columnsA)
      var i = columnsA
      while (i >= 0) Brows(i) = B.viewRow(i)
      val Crows = Array.ofDim[StrideMatrix1D](rowsA)
      var i = rowsA
      while (i >= 0) Crows(i) = C.viewRow(i)
      val fun = cern.jet.math.tdouble.DoublePlusMultSecond.plusMult(0)
      val columnIndexesA = columnIndexes
      val valuesA = values
      var i = rows
      while (i >= 0) {
        val low = rowPointers(i)
        var k = rowPointers(i + 1)
        while (k >= low) {
          val j = columnIndexesA(k)
          fun.multiplicator = valuesA(k) * alpha
          if (!transposeA) Crows(i).assign(Brows(j), fun) else Crows(j).assign(Brows(i), fun)
        }
      }
    }
    C
  }

  private def cumsum(p: Array[Int], c: Array[Int], n: Int): Double = {
    var nz = 0
    var nz2 = 0
    for (k <- 0 until n) {
      p(k) = nz
      nz += c(k)
      nz2 += c(k)
      c(k) = p(k)
    }
    p(n) = nz
    (nz2)
  }

  private def realloc(nzmax: Int) {
    if (nzmax <= 0) nzmax = rowPointers(rows)
    val columnIndexesNew = Array.ofDim[Int](nzmax)
    var length = Math.min(nzmax, columnIndexes.length)
    System.arraycopy(columnIndexes, 0, columnIndexesNew, 0, length)
    columnIndexes = columnIndexesNew
    val valuesNew = Array.ofDim[Double](nzmax)
    length = Math.min(nzmax, values.length)
    System.arraycopy(values, 0, valuesNew, 0, length)
    values = valuesNew
  }

  protected def getStorageMatrix(): StrideMatrix2D = this

  protected def insert(row: Int,
      column: Int,
      index: Int,
      value: Double) {
    val columnIndexesList = new IntArrayList(columnIndexes)
    columnIndexesList.setSizeRaw(rowPointers(rows))
    val valuesList = new DoubleArrayList(values)
    valuesList.setSizeRaw(rowPointers(rows))
    columnIndexesList.beforeInsert(index, column)
    valuesList.beforeInsert(index, value)
    var i = rowPointers.length
    while (i > row) rowPointers(i) += 1
    columnIndexes = columnIndexesList.elements()
    values = valuesList.elements()
  }

  protected def remove(row: Int, index: Int) {
    val columnIndexesList = new IntArrayList(columnIndexes)
    columnIndexesList.setSizeRaw(rowPointers(rows))
    val valuesList = new DoubleArrayList(values)
    valuesList.setSizeRaw(rowPointers(rows))
    columnIndexesList.remove(index)
    valuesList.remove(index)
    var i = rowPointers.length
    while (i > row) rowPointers(i) -= 1
    columnIndexes = columnIndexesList.elements()
    values = valuesList.elements()
  }
}
