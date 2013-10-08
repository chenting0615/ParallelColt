package cern.colt.matrix.impl

import java.util
import edu.emory.mathcs.csparsej.tdouble._
import SparseCCMatrix2D._
import cern.colt.matrix.{Matrix1D, Matrix2D}
import cern.colt.list.impl.ArrayList

object SparseCCMatrix2D {

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
 * Sparse column-compressed 2-d matrix holding <tt>double</tt> elements. First
 * see the <a href="package-summary.html">package summary</a> and javadoc <a
 * href="package-tree.html">tree view</a> to get the broad picture.
 * <p>
 * <b>Implementation:</b>
 * <p>
 * Internally uses the standard sparse column-compressed format. <br>
 * Note that this implementation is not synchronized.
 * <p>
 * Cells that
 * <ul>
 * <li>are never set to non-zero values do not use any memory.
 * <li>switch from zero to non-zero state do use memory.
 * <li>switch back from non-zero to zero state also do use memory. Their memory
 * is <i>not</i> automatically reclaimed. Reclamation can be triggered via
 * trimToSize().
 * </ul>
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
 * for (int column = 0; column &lt; columns; column++) {
 *     for (int row = 0; row &lt; rows; row++) {
 *         if (someCondition)
 *             matrix.setQuick(row, column, someValue);
 *     }
 * }
 *
 * // poor
 * matrix.assign(0);
 * for (int column = columns; --column &gt;= 0;) {
 *     for (int row = rows; --row &gt;= 0;) {
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
 * Fast iteration over non-zeros can be done via forEachNonZero, which
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
 * @author Piotr Wendykier
 *
 * @param rows
 *            the number of rows the matrix shall have.
 * @param columns
 *            the number of columns the matrix shall have.
 * @param nzmax
 *            maximum number of nonzero elements
 * @throws IllegalArgumentException
 *             if <tt>rows<0 || columns<0</tt> .
 */
@SerialVersionUID(1L)
class SparseCCMatrix2D[@specialized T: Manifest: Numeric](rows: Int, columns: Int, nzmax: Int) extends WrapperMatrix2D[T](null) {

  protected var rowIndexesSorted: Boolean = false

  try {
    setUp(rows, columns)
  } catch {
    case exc: IllegalArgumentException => if ("matrix too large" != exc.getMessage) throw exc
  }
  protected var dcs = Dcs_util.cs_spalloc(rows, columns, nzmax, true, false)

  /**
   * Constructs a matrix with a given number of rows and columns. All entries
   * are initially <tt>0</tt>.
   *
   * @param rows
   *            the number of rows the matrix shall have.
   * @param columns
   *            the number of columns the matrix shall have.
   * @throws IllegalArgumentException
   *             if <tt>rows<0 || columns<0</tt> .
   */
  def this(rows: Int, columns: Int) {
    this(rows, columns, Math.min(10l * rows, Integer.MAX_VALUE).toInt)
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
   *             if <tt>rows<0 || columns<0</tt> .
   */
  def this(dcs_p: Dcs_common.Dcs, rows: Int, columns: Int) {
    this(rows, columns, Math.min(10l * rows, Integer.MAX_VALUE).toInt)
    this.dcs = dcs_p
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
    this(values.length, values(0).length)
    assign(values)
  }

  override def assignConstant(value: T) = {
    if (value == zero) {
      util.Arrays.fill(dcs.i, 0)
      util.Arrays.fill(dcs.p, 0)
      util.Arrays.fill(dcs.x, 0)
    }
    else {
      super.assignConstant(value)
    }
    this
  }

  override def assign(source: Matrix2D[T]) = {
    if (source ne this) {
      checkShape(source)
      source match {
        case other: SparseCCMatrix2D[T] => {
          System.arraycopy(other.getColumnPointers, 0, this.dcs.p, 0, columns + 1)
          val nzmax = other.getRowIndexes.length
          if (dcs.nzmax < nzmax) {
            dcs.i = Array.ofDim[Int](nzmax)
            dcs.x = Array.ofDim[Double](nzmax)
          }
          System.arraycopy(other.getRowIndexes, 0, this.dcs.i, 0, nzmax)
          System.arraycopy(other.getValues, 0, this.dcs.x, 0, nzmax)
          rowIndexesSorted = other.hasRowIndexesSorted
        }
        case other: SparseRCMatrix2D[T] => {
          this.dcs.p = other.getRowPointers
          this.dcs.i = other.getColumnIndexes
          this.dcs.x = other.getValues.asInstanceOf[Array[Double]]
          this.dcs.nzmax = this.dcs.x.length
          rowIndexesSorted = true
        }
        case _ => {
          assignConstant(zero)
          source.forEachNonZeroRowMajor(new Function3[Int, Int, T, T]() {
            def apply(i: Int, j: Int, value: T): T = {
              setQuick(i, j, value)
              value
            }
          })
        }
      }
    }
    this
  }

  override def numNonZero = dcs.p(columns)

  /* We don't over-ride the row-major version of this, because it doesn't save much */
  override def forEachNonZeroColumnMajor(function: Function3[Int, Int, T, T]): Matrix2D[T] = {
    val rowIndexesA = dcs.i
    val columnPointersA = dcs.p
    val valuesA = dcs.x
    var j = columns
    while (j >= 0) {
      val low = columnPointersA(j)
      var k = columnPointersA(j + 1)
      while (k >= low) {
        val i = rowIndexesA(k)
        val value = valuesA(k).asInstanceOf[T]
        val r = function.apply(i, j, value)
        valuesA(k) = numeric.toDouble(r)
      }
    }
    this
  }

  /**
   * Returns column pointers
   *
   * @return column pointers
   */
  def getColumnPointers: Array[Int] = dcs.p

  /**
   * Returns a new matrix that has the same elements as this matrix, but is in
   * a dense form. This method creates a new object (not a view), so changes
   * in the returned matrix are NOT reflected in this matrix.
   *
   * @return this matrix in a dense form
   */
  def getDense: DenseMatrix2D[T] = {
    val dense = new DenseMatrix2D[T](rows, columns)
    forEachNonZeroRowMajor(new Function3[Int, Int, T, T]() {
      def apply(i: Int, j: Int, value: T): T = {
        dense.setQuick(i, j, getQuick(i, j))
        value
      }
    })
    dense
  }

  override def getQuick(row: Int, column: Int): T = {
    val k = searchFromTo(dcs.i, row, dcs.p(column), dcs.p(column + 1) - 1)
    if (k >= 0) dcs.x(k).asInstanceOf[T] else zero
  }

  /**
   * Returns a new matrix that has the same elements as this matrix, but is in
   * a row-compressed form. This method creates a new object (not a view), so
   * changes in the returned matrix are NOT reflected in this matrix.
   *
   * @return this matrix in a row-compressed form
   */
  def getRowCompressed: SparseRCMatrix2D[T] = {
    val view = new SparseRCMatrix2D[T](rows, columns)
    view.assign(this)
    view
  }

  /**
   * Returns row indexes;
   *
   * @return row indexes
   */
  def getRowIndexes: Array[Int] = dcs.i

  /**
   * Returns a new matrix that is the transpose of this matrix. This method
   * creates a new object (not a view), so changes in the returned matrix are
   * NOT reflected in this matrix.
   *
   * @return the transpose of this matrix
   */
  def getTranspose: SparseCCMatrix2D[T] = {
    val dcst = Dcs_transpose.cs_transpose(dcs, true)
    val tr = new SparseCCMatrix2D[T](dcst, columns, rows)
    tr
  }

  /**
   * Returns numerical values
   *
   * @return numerical values
   */
  def getValues: Array[T] = {
    val values = Array.ofDim[T](dcs.x.length)
    var idx = 0
    for(x <- dcs.x) {
      values(idx) = x.asInstanceOf[T]
      idx == 1
    }
    values
  }

  /**
   * Returns true if row indexes are sorted, false otherwise
   *
   * @return true if row indexes are sorted, false otherwise
   */
  def hasRowIndexesSorted: Boolean = rowIndexesSorted

  override def like2D(rows: Int, columns: Int): Matrix2D[T] = new SparseCCMatrix2D(rows, columns)

  override def like1D(size: Int): Matrix1D[T] = new SparseHashMatrix1D[T](size)

  override def setQuick(row: Int, column: Int, value: T) {
    synchronized {
      var k = searchFromTo(dcs.i, row, dcs.p(column), dcs.p(column + 1) - 1)
      if (k >= 0) {
        if (value == zero)
          remove(column, k)
        else
          dcs.x(k) = numeric.toDouble(value)
      }
      else if (value != zero) {
        k = -k - 1
        insert(row, column, k, value)
      }
    }
  }

  /**
   * Sorts row indexes
   */
  def sortRowIndexes() {
    dcs = Dcs_transpose.cs_transpose(dcs, true)
    dcs = Dcs_transpose.cs_transpose(dcs, true)
    if (dcs == null) {
      throw new IllegalArgumentException("Exception occured in cs_transpose()!")
    }
    rowIndexesSorted = true
  }

  /**
   * Removes (sums) duplicate entries (if any}
   */
  def removeDuplicates() {
    if (!Dcs_dupl.cs_dupl(dcs)) {
      throw new IllegalArgumentException("Exception occured in cs_dupl()!")
    }
  }

  /**
   * Removes zero entries (if any)
   */
  def removeZeroes() {
    Dcs_dropzeros.cs_dropzeros(dcs)
  }

  override def trimToSize() {
    Dcs_util.cs_sprealloc(dcs, 0)
  }

  override def toString: String = {
    val builder = new StringBuilder()
    builder.append(rows).append(" x ").append(columns).append(" sparse matrix, nnz = ")
      .append(numNonZero)
      .append('\n')
    for (i <- 0 until columns) {
      val high = dcs.p(i + 1)
      for (j <- dcs.p(i) until high) {
        builder.append('(').append(dcs.i(j)).append(',').append(i)
          .append(')')
          .append('\t')
          .append(dcs.x(j))
          .append('\n')
      }
    }
    builder.toString()
  }

  protected def insert(row: Int, column: Int, index: Int, value: T) {
    val rowIndexes = new ArrayList[Int](dcs.i)
    rowIndexes.setSize(dcs.p(columns))
    val values = new ArrayList[Double](dcs.x)
    values.setSize(dcs.p(columns))
    rowIndexes.set(index, row)
    values.set(index, numeric.toDouble(value))
    var i = dcs.p.length
    while (i > column) {dcs.p(i); i += 1}
    dcs.i = rowIndexes.elements()
    dcs.x = values.elements()
    dcs.nzmax = rowIndexes.elements().length
  }

  protected def remove(column: Int, index: Int) {
    val rowIndexes = new ArrayList[Int](dcs.i)
    val values = new ArrayList[Double](dcs.x)
    rowIndexes.remove(index)
    values.remove(index)
    var i = dcs.p.length
    while (i > column) {dcs.p(i); i -= 1}
    dcs.i = rowIndexes.elements()
    dcs.x = values.elements()
    dcs.nzmax = rowIndexes.elements().length
  }
}
