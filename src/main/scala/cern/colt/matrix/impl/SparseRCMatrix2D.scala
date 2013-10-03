package cern.colt.matrix.impl

import java.util.Arrays
import SparseRCMatrix2D._
import cern.colt.matrix.Matrix2D
import cern.colt.list.impl.ArrayList

object SparseRCMatrix2D {

  private def searchFromTo(list: Array[Int], key: Int, from_p: Int, to: Int): Int = {
    var from = from_p
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
 * Reclamation can be triggered via trimToSize().
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
 * Fast iteration over non-zeros can be done via forEachNonZero(), which
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
 *
 * @author wolfgang.hoschek@cern.ch
 * @version 0.9, 04/14/2000
 */
@specialized(Double)
@SerialVersionUID(1L)
class SparseRCMatrix2D[T: Manifest](rows_p: Int, columns_p: Int, nzmax: Int) extends WrapperMatrix2D[T](null) {

  protected var rowPointers: Array[Int] = new Array[Int](rows_p + 1)

  protected var columnIndexes: Array[Int] = new Array[Int](nzmax)

  protected var values: Array[T] = new Array[T](nzmax)

  protected var columnIndexesSorted: Boolean = false

  try {
    setUp(rows_p, columns_p)
  } catch {
    case exc: IllegalArgumentException => if ("matrix too large" != exc.getMessage) throw exc
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
  def this(values: Array[Array[T]]) {
    this(values.length, if (values.length == 0) 0 else values(0).length)
    assign(values)
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
   * @param sortColumnIndexes_p
   *            if true, then column indexes are sorted
   */
  def this(rows: Int,
      columns: Int,
      rowIndexes: Array[Int],
      columnIndexes: Array[Int],
      values: Array[T],
      sortColumnIndexes_p: Boolean) {

    this(rows, columns, Math.max(rowIndexes.length, 1))
    if (rowIndexes.length != columnIndexes.length) {
      throw new IllegalArgumentException("rowIndexes.length != columnIndexes.length")
    } else if (rowIndexes.length != values.length) {
      throw new IllegalArgumentException("rowIndexes.length != values.length")
    }
    val nz = nzmax
    this.columnIndexes = Array.ofDim[Int](nz)
    this.values = Array.ofDim[T](nz)
    this.rowPointers = Array.ofDim[Int](rows + 1)
    val w = Array.ofDim[Int](rows)
    var r: Int = 0
    for (k <- 0 until nz) {
      w(rowIndexes(k)) += 1
    }
    cumsum(this.rowPointers, w, rows)
    for (k <- 0 until nz) {
      r = w(rowIndexes(k))
      w(rowIndexes(k)) += 1
      this.columnIndexes(r) = columnIndexes(k)
      this.values(r) = values(k)
    }
    if (sortColumnIndexes_p) {
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
      values: Array[T]) {
    this(rows, columns)
    if (rowPointers.length != rows + 1) {
      throw new IllegalArgumentException("rowPointers.length != rows + 1")
    }
    this.rowPointers = rowPointers
    this.columnIndexes = columnIndexes
    this.values = values
  }

  override def assignConstant(value: T) = {
    if (value == 0) {
      Arrays.fill(rowPointers, 0)
      Arrays.fill(columnIndexes, 0)
      for(i <- 0 until values.length)
        values(i) = value
    } else {
      val nnz = numNonZero.toInt
      for (i <- 0 until nnz) {
        values(i) = value
      }
    }
    this
  }

  override def assign(source: Matrix2D[T]): SparseRCMatrix2D[T] = {
    if (source == this) return this
    checkShape(source)
    if (source.isInstanceOf[SparseRCMatrix2D[T]]) {
      val other = source.asInstanceOf[SparseRCMatrix2D[T]]
      System.arraycopy(other.rowPointers, 0, rowPointers, 0, rows + 1)
      val nzmax = other.columnIndexes.length
      if (columnIndexes.length < nzmax) {
        columnIndexes = Array.ofDim[Int](nzmax)
        values = Array.ofDim[T](nzmax)
      }
      System.arraycopy(other.columnIndexes, 0, columnIndexes, 0, nzmax)
      System.arraycopy(other.values, 0, values, 0, nzmax)
      columnIndexesSorted = other.columnIndexesSorted
    } else if (source.isInstanceOf[SparseCCMatrix2D[T]]) {
      val other = source.asInstanceOf[SparseCCMatrix2D[T]].getTranspose
      rowPointers = other.getColumnPointers
      columnIndexes = other.getRowIndexes
      values = other.getValues
      columnIndexesSorted = true
    } else {
      assignConstant(0.asInstanceOf[T])
      source.forEachNonZero(new Function3[Int, Int, T, T]() {
        def apply(i: Int, j: Int, value: T): T = {
          setQuick(i, j, value)
          value
        }
      })
    }
    this
  }

  override def numNonZero = rowPointers(rows)

  override def forEachNonZero(function: Function3[Int, Int, T, T]) = {
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

  /**
   * Returns a new matrix that has the same elements as this matrix, but is in
   * a column-compressed form. This method creates a new object (not a view),
   * so changes in the returned matrix are NOT reflected in this matrix.
   *
   * @return this matrix in a column-compressed form
   */
  def getColumnCompressed: SparseCCMatrix2D[T] = {
    val tr = getTranspose
    val cc = new SparseCCMatrix2D[T](rows, columns, tr.columnIndexes, tr.rowPointers, tr.values, true)
    cc
  }

  /**
   * Returns column indexes
   *
   * @return column indexes
   */
  def getColumnIndexes: Array[Int] = columnIndexes

  /**
   * Returns a new matrix that has the same elements as this matrix, but is in
   * a dense form. This method creates a new object (not a view), so changes
   * in the returned matrix are NOT reflected in this matrix.
   *
   * @return this matrix in a dense form
   */
  def getDense: DenseMatrix2D[T] = {
    val dense = new DenseMatrix2D[T](rows, columns)
    forEachNonZero(new Function3[Int, Int, T, T]() {
      def apply(i: Int, j: Int, value: T): T = {
        dense.setQuick(i, j, value)
        value
      }
    })
    dense
  }

  override def getQuick(row: Int, column: Int): T = {
    val k = searchFromTo(columnIndexes, column, rowPointers(row), rowPointers(row + 1) - 1)
    if (k >= 0) values(k) else 0.asInstanceOf[T]
  }

  /**
   * Returns row pointers
   *
   * @return row pointers
   */
  def getRowPointers: Array[Int] = rowPointers

  /**
   * Returns a new matrix that is the transpose of this matrix. This method
   * creates a new object (not a view), so changes in the returned matrix are
   * NOT reflected in this matrix.
   *
   * @return the transpose of this matrix
   */
  def getTranspose: SparseRCMatrix2D[T] = {
    val nnz = rowPointers(rows)
    val w = Array.ofDim[Int](columns)
    val rowPointersT = Array.ofDim[Int](columns + 1)
    val columnIndexesT = Array.ofDim[Int](nnz)
    val valuesT = Array.ofDim[T](nnz)
    for (p <- 0 until nnz) {
      w(columnIndexes(p)) += 1
    }
    cumsum(rowPointersT, w, columns)
    var q: Int = 0
    for (j <- 0 until rows) {
      val high = rowPointers(j + 1)
      for (p <- rowPointers(j) until high) {
        q = w(columnIndexes(p))
        w(columnIndexes(p)) += 1
        columnIndexesT(q) = j
        valuesT(q) = values(p)
      }
    }
    val T = new SparseRCMatrix2D[T](columns, rows)
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
  def getValues: Array[T] = values

  /**
   * Returns true if column indexes are sorted, false otherwise
   *
   * @return true if column indexes are sorted, false otherwise
   */
  def hasColumnIndexesSorted: Boolean = columnIndexesSorted

  override def like2D(rows: Int, columns: Int) = new SparseRCMatrix2D(rows, columns)

  override def like1D(size: Int) = new SparseHashMatrix1D[T](size)

  /**
   * Removes (sums) duplicate entries (if any}
   */
  def removeDuplicates() {
    var nz = 0
    var q: Int = 0
    var i: Int = 0
    val w = Array.ofDim[Int](columns)
    i = 0
    while (i < columns) {w(i) = -1; i += 1
    }
    for (j <- 0 until rows) {
      q = nz
      for (p <- rowPointers(j) until rowPointers(j + 1)) {
        i = columnIndexes(p)
        if (w(i) >= q) {
          values.asInstanceOf[Array[Double]](w(i)) += values(p).asInstanceOf[Double]
        } else {
          w(i) = nz
          columnIndexes(nz) = i
          values(nz) = values(p)
          nz += 1
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
    var nz = 0
    val eps = Math.pow(2, -52)
    for (j <- 0 until rows) {
      var p = rowPointers(j)
      rowPointers(j) = nz
      while (p < rowPointers(j + 1)) {
        if (Math.abs(values(p).asInstanceOf[Double]) > eps) {
          values(nz) = values(p)
          columnIndexes(nz) = columnIndexes(p)
          nz += 1
        }
        p += 1
      }
    }
    rowPointers(rows) = nz
  }

  override def setQuick(row: Int, column: Int, value: T) {
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

  /**
   * Sorts column indexes
   */
  def sortColumnIndexes() {
    var T = getTranspose
    this.rowsVar = T.rows
    this.columnsVar = T.columns
    this.columnIndexes = T.columnIndexes
    this.rowPointers = T.rowPointers
    this.values = T.values
    T = getTranspose
    this.rowsVar = T.rows
    this.columnsVar = T.columns
    this.columnIndexes = T.columnIndexes
    this.rowPointers = T.rowPointers
    this.values = T.values
    columnIndexesSorted = true
  }

  override def toString: String = {
    val builder = new StringBuilder()
    builder.append(rows).append(" x ").append(columns).append(" sparse matrix, nnz = ")
      .append(numNonZero)
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
    builder.toString()
  }

  override def trimToSize() {
    val nzmax = rowPointers(rows)
    val columnIndexesNew = Array.ofDim[Int](nzmax)
    var length = Math.min(nzmax, columnIndexes.length)
    System.arraycopy(columnIndexes, 0, columnIndexesNew, 0, length)
    columnIndexes = columnIndexesNew
    val valuesNew = Array.ofDim[T](nzmax)
    length = Math.min(nzmax, values.length)
    System.arraycopy(values, 0, valuesNew, 0, length)
    values = valuesNew
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
    nz2
  }

  protected def insert(row: Int, column: Int, index: Int, value: T) {
    val columnIndexesList = new ArrayList[Int](columnIndexes)
    columnIndexesList.setSize(rowPointers(rows))
    val valuesList = new ArrayList[Double](values.asInstanceOf[Array[Double]])
    valuesList.setSize(rowPointers(rows))
    columnIndexesList.set(index, column)
    valuesList.set(index, value.asInstanceOf[Double])
    var i = rowPointers.length-1
    while (i > row) {rowPointers(i) += 1; i -= 1}
    columnIndexes = columnIndexesList.elements()
    values = valuesList.elements().asInstanceOf[Array[T]]
  }

  protected def remove(row: Int, index: Int) {
    val columnIndexesList = new ArrayList[Int](columnIndexes)
    columnIndexesList.setSize(rowPointers(rows))
    val valuesList = new ArrayList[Double](values.asInstanceOf[Array[Double]])
    valuesList.setSize(rowPointers(rows))
    columnIndexesList.remove(index)
    valuesList.remove(index)
    var i = rowPointers.length-1
    while (i > row) {rowPointers(i) += 1; i -= 1}
    columnIndexes = columnIndexesList.elements()
    values = valuesList.elements().asInstanceOf[Array[T]]
  }
}
