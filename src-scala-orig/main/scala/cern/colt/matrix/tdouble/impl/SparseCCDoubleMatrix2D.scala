package cern.colt.matrix.tdouble.impl

import java.util.Arrays
import java.util.concurrent.Future
import cern.colt.list.tdouble.DoubleArrayList
import cern.colt.list.tint.IntArrayList
import cern.colt.matrix.tdouble.StrideMatrix1D
import cern.colt.matrix.tdouble.StrideMatrix2D
import edu.emory.mathcs.csparsej.tdouble.Dcs_add
import edu.emory.mathcs.csparsej.tdouble.Dcs_cumsum
import edu.emory.mathcs.csparsej.tdouble.Dcs_dropzeros
import edu.emory.mathcs.csparsej.tdouble.Dcs_dupl
import edu.emory.mathcs.csparsej.tdouble.Dcs_multiply
import edu.emory.mathcs.csparsej.tdouble.Dcs_transpose
import edu.emory.mathcs.csparsej.tdouble.Dcs_util
import edu.emory.mathcs.csparsej.tdouble.Dcs_common.Dcs
import edu.emory.mathcs.utils.ConcurrencyUtils
import SparseCCDoubleMatrix2D._
//remove if not needed
import scala.collection.JavaConversions._

object SparseCCDoubleMatrix2D {

  private def searchFromTo(list: Array[Int],
      key: Int,
      from: Int,
      to: Int): Int = {
    while (from <= to) {
      if (list(from) == key) {
        return from
      } else {
        from += 1
        //continue
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
 * {@link #trimToSize()}.
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
 * @author Piotr Wendykier
 *
 */
@SerialVersionUID(1L)
class SparseCCDoubleMatrix2D(protected var dcs: Dcs) extends WrapperMatrix2D(null) {

  protected var rowIndexesSorted: Boolean = false

  try {
    setUp(dcs.m, dcs.n)
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
    this(values.length, values(0).length)
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
   * @param nzmax
   *            maximum number of nonzero elements
   * @throws IllegalArgumentException
   *             if <tt>rows<0 || columns<0</tt> .
   */
  def this(rows: Int, columns: Int, nzmax: Int) {
    super(null)
    try {
      setUp(rows, columns)
    } catch {
      case exc: IllegalArgumentException => if ("matrix too large" != exc.getMessage) throw exc
    }
    dcs = Dcs_util.cs_spalloc(rows, columns, nzmax, true, false)
  }

  /**
   * Constructs a matrix with indexes given in the coordinate format and a
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
   *            numerical value
   * @param removeDuplicates
   *            if true, then duplicates (if any) are removed
   * @param sortRowIndexes
   *            if true, then row indexes are sorted
   */
  def this(rows: Int,
      columns: Int,
      rowIndexes: Array[Int],
      columnIndexes: Array[Int],
      value: Double,
      removeDuplicates: Boolean,
      sortRowIndexes: Boolean) {
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
    dcs = Dcs_util.cs_spalloc(rows, columns, nz, true, false)
    val w = Array.ofDim[Int](columns)
    val Cp = dcs.p
    val Ci = dcs.i
    val Cx = dcs.x
    for (k <- 0 until nz) w(columnIndexes(k)) += 1
    Dcs_cumsum.cs_cumsum(Cp, w, columns)
    val p: Int = 0
    for (k <- 0 until nz) {
      Ci(p = w(columnIndexes(k)) += 1) = rowIndexes(k)
      if (Cx != null) Cx(p) = value
    }
    if (removeDuplicates) {
      if (!Dcs_dupl.cs_dupl(dcs)) {
        throw new IllegalArgumentException("Exception occured in cs_dupl()!")
      }
    }
    if (sortRowIndexes) {
      dcs = Dcs_transpose.cs_transpose(dcs, true)
      dcs = Dcs_transpose.cs_transpose(dcs, true)
      if (dcs == null) {
        throw new IllegalArgumentException("Exception occured in cs_transpose()!")
      }
      rowIndexesSorted = true
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
   * @param sortRowIndexes
   *            if true, then row indexes are sorted
   */
  def this(rows: Int,
      columns: Int,
      rowIndexes: Array[Int],
      columnIndexes: Array[Int],
      values: Array[Double],
      removeDuplicates: Boolean,
      removeZeroes: Boolean,
      sortRowIndexes: Boolean) {
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
    dcs = Dcs_util.cs_spalloc(rows, columns, nz, true, false)
    val w = Array.ofDim[Int](columns)
    val Cp = dcs.p
    val Ci = dcs.i
    val Cx = dcs.x
    for (k <- 0 until nz) w(columnIndexes(k)) += 1
    Dcs_cumsum.cs_cumsum(Cp, w, columns)
    val p: Int = 0
    for (k <- 0 until nz) {
      Ci(p = w(columnIndexes(k)) += 1) = rowIndexes(k)
      if (Cx != null) Cx(p) = values(k)
    }
    if (removeZeroes) {
      Dcs_dropzeros.cs_dropzeros(dcs)
    }
    if (removeDuplicates) {
      if (!Dcs_dupl.cs_dupl(dcs)) {
        throw new IllegalArgumentException("Exception occured in cs_dupl()!")
      }
    }
    if (sortRowIndexes) {
      dcs = Dcs_transpose.cs_transpose(dcs, true)
      dcs = Dcs_transpose.cs_transpose(dcs, true)
      if (dcs == null) {
        throw new IllegalArgumentException("Exception occured in cs_transpose()!")
      }
      rowIndexesSorted = true
    }
  }

  def assign(function: cern.colt.function.tdouble.Function1): StrideMatrix2D = {
    if (function.isInstanceOf[cern.jet.math.tdouble.DoubleMult]) {
      val alpha = function.asInstanceOf[cern.jet.math.tdouble.DoubleMult].multiplicator
      if (alpha == 1) return this
      if (alpha == 0) return assign(0)
      if (alpha != alpha) return assign(alpha)
      val valuesE = dcs.x
      val nz = cardinality()
      for (j <- 0 until nz) {
        valuesE(j) *= alpha
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
      Arrays.fill(dcs.i, 0)
      Arrays.fill(dcs.p, 0)
      Arrays.fill(dcs.x, 0)
    } else {
      val nnz = cardinality()
      for (i <- 0 until nnz) {
        dcs.x(i) = value
      }
    }
    this
  }

  def assign(source: StrideMatrix2D): StrideMatrix2D = {
    if (source == this) return this
    checkShape(source)
    if (source.isInstanceOf[SparseCCDoubleMatrix2D]) {
      val other = source.asInstanceOf[SparseCCDoubleMatrix2D]
      System.arraycopy(other.getColumnPointers, 0, this.dcs.p, 0, columns + 1)
      val nzmax = other.getRowIndexes.length
      if (dcs.nzmax < nzmax) {
        dcs.i = Array.ofDim[Int](nzmax)
        dcs.x = Array.ofDim[Double](nzmax)
      }
      System.arraycopy(other.getRowIndexes, 0, this.dcs.i, 0, nzmax)
      System.arraycopy(other.getValues, 0, this.dcs.x, 0, nzmax)
      rowIndexesSorted = other.rowIndexesSorted
    } else if (source.isInstanceOf[SparseRCDoubleMatrix2D]) {
      val other = source.asInstanceOf[SparseRCDoubleMatrix2D].getTranspose
      this.dcs.p = other.getRowPointers
      this.dcs.i = other.getColumnIndexes
      this.dcs.x = other.getValues
      this.dcs.nzmax = this.dcs.x.length
      rowIndexesSorted = true
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
    if ((y.isInstanceOf[SparseCCDoubleMatrix2D]) &&
      (function == cern.jet.math.tdouble.DoubleFunctions.plus)) {
      val yy = y.asInstanceOf[SparseCCDoubleMatrix2D]
      dcs = Dcs_add.cs_add(dcs, yy.dcs, 1, 1)
      return this
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
      val rowIndexesA = dcs.i
      val columnPointersA = dcs.p
      val valuesA = dcs.x
      var j = columns
      while (j >= 0) {
        val low = columnPointersA(j)
        var k = columnPointersA(j + 1)
        while (k >= low) {
          val i = rowIndexesA(k)
          valuesA(k) *= y.getQuick(i, j)
          if (valuesA(k) == 0) remove(i, j)
        }
      }
      return this
    }
    if (function == cern.jet.math.tdouble.DoubleFunctions.div) {
      val rowIndexesA = dcs.i
      val columnPointersA = dcs.p
      val valuesA = dcs.x
      var j = columns
      while (j >= 0) {
        val low = columnPointersA(j)
        var k = columnPointersA(j + 1)
        while (k >= low) {
          val i = rowIndexesA(k)
          valuesA(k) /= y.getQuick(i, j)
          if (valuesA(k) == 0) remove(i, j)
        }
      }
      return this
    }
    super.assign(y, function)
  }

  def cardinality(): Int = dcs.p(columns)

  def elements(): Dcs = dcs

  def forEachNonZero(function: cern.colt.function.tdouble.Function3): StrideMatrix2D = {
    val rowIndexesA = dcs.i
    val columnPointersA = dcs.p
    val valuesA = dcs.x
    var j = columns
    while (j >= 0) {
      val low = columnPointersA(j)
      var k = columnPointersA(j + 1)
      while (k >= low) {
        val i = rowIndexesA(k)
        val value = valuesA(k)
        val r = function.apply(i, j, value)
        valuesA(k) = r
      }
    }
    this
  }

  /**
   * Returns column pointers
   *
   * @return column pointers
   */
  def getColumnPointers(): Array[Int] = dcs.p

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
      val k = searchFromTo(dcs.i, row, dcs.p(column), dcs.p(column + 1) - 1)
      var v = 0
      if (k >= 0) v = dcs.x(k)
      v
    }
  }

  /**
   * Returns a new matrix that has the same elements as this matrix, but is in
   * a row-compressed form. This method creates a new object (not a view), so
   * changes in the returned matrix are NOT reflected in this matrix.
   *
   * @return this matrix in a row-compressed form
   */
  def getRowCompressed(): SparseRCDoubleMatrix2D = {
    val dcst = Dcs_transpose.cs_transpose(dcs, true)
    val rc = new SparseRCDoubleMatrix2D(rows, columns)
    rc.columnIndexes = dcst.i
    rc.rowPointers = dcst.p
    rc.values = dcst.x
    rc.columnIndexesSorted = true
    rc
  }

  /**
   * Returns row indexes;
   *
   * @return row indexes
   */
  def getRowIndexes(): Array[Int] = dcs.i

  /**
   * Returns a new matrix that is the transpose of this matrix. This method
   * creates a new object (not a view), so changes in the returned matrix are
   * NOT reflected in this matrix.
   *
   * @return the transpose of this matrix
   */
  def getTranspose(): SparseCCDoubleMatrix2D = {
    val dcst = Dcs_transpose.cs_transpose(dcs, true)
    val tr = new SparseCCDoubleMatrix2D(columns, rows)
    tr.dcs = dcst
    tr
  }

  /**
   * Returns numerical values
   *
   * @return numerical values
   */
  def getValues(): Array[Double] = dcs.x

  /**
   * Returns true if row indexes are sorted, false otherwise
   *
   * @return true if row indexes are sorted, false otherwise
   */
  def hasRowIndexesSorted(): Boolean = rowIndexesSorted

  def like(rows: Int, columns: Int): StrideMatrix2D = {
    new SparseCCDoubleMatrix2D(rows, columns)
  }

  def like1D(size: Int): StrideMatrix1D = new SparseDoubleMatrix1D(size)

  def setQuick(row: Int, column: Int, value: Double) {
    synchronized {
      var k = searchFromTo(dcs.i, row, dcs.p(column), dcs.p(column + 1) - 1)
      if (k >= 0) {
        if (value == 0) remove(column, k) else dcs.x(k) = value
        return
      }
      if (value != 0) {
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

  def trimToSize() {
    Dcs_util.cs_sprealloc(dcs, 0)
  }

  override def toString(): String = {
    val builder = new StringBuilder()
    builder.append(rows).append(" x ").append(columns).append(" sparse matrix, nnz = ")
      .append(cardinality())
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
    builder.toString
  }

  def zMult(y: StrideMatrix1D,
      z: StrideMatrix1D,
      alpha: Double,
      beta: Double,
      transposeA: Boolean): StrideMatrix1D = {
    val rowsA = if (transposeA) columns else rows
    val columnsA = if (transposeA) rows else columns
    val ignore = (z == null || transposeA)
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
    val zeroZ = zz.index(0).toInt
    val yy = y.asInstanceOf[DenseMatrix1D]
    val elementsY = yy.elements
    val strideY = yy.stride()
    val zeroY = yy.index(0).toInt
    val rowIndexesA = dcs.i
    val columnPointersA = dcs.p
    val valuesA = dcs.x
    var zidx = zeroZ
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if (!transposeA) {
      if ((!ignore) && (beta / alpha != 1.0)) {
        z.assign(cern.jet.math.tdouble.DoubleFunctions.mult(beta / alpha))
      }
      if ((nthreads > 1) &&
        (cardinality() >= ConcurrencyUtils.getThreadsBeginN_2D)) {
        nthreads = 2
        val futures = Array.ofDim[Future](nthreads)
        val result = Array.ofDim[Double](rowsA)
        val k = columns / nthreads
        for (j <- 0 until nthreads) {
          val firstColumn = j * k
          val lastColumn = if ((j == nthreads - 1)) columns else firstColumn + k
          val threadID = j
          futures(j) = ConcurrencyUtils.submit(new Runnable() {

            def run() {
              if (threadID == 0) {
                for (i <- firstColumn until lastColumn) {
                  var high = columnPointersA(i + 1)
                  var yElem = elementsY(zeroY + strideY * i)
                  for (k <- columnPointersA(i) until high) {
                    var j = rowIndexesA(k)
                    elementsZ(zeroZ + strideZ * j) += valuesA(k) * yElem
                  }
                }
              } else {
                for (i <- firstColumn until lastColumn) {
                  var high = columnPointersA(i + 1)
                  var yElem = elementsY(zeroY + strideY * i)
                  for (k <- columnPointersA(i) until high) {
                    var j = rowIndexesA(k)
                    result(j) += valuesA(k) * yElem
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
        for (i <- 0 until columns) {
          val high = columnPointersA(i + 1)
          val yElem = elementsY(zeroY + strideY * i)
          for (k <- columnPointersA(i) until high) {
            val j = rowIndexesA(k)
            elementsZ(zeroZ + strideZ * j) += valuesA(k) * yElem
          }
        }
      }
      if (alpha != 1.0) {
        z.assign(cern.jet.math.tdouble.DoubleFunctions.mult(alpha))
      }
    } else {
      if ((nthreads > 1) &&
        (cardinality() >= ConcurrencyUtils.getThreadsBeginN_2D)) {
        val futures = Array.ofDim[Future](nthreads)
        val k = columns / nthreads
        for (j <- 0 until nthreads) {
          val firstColumn = j * k
          val lastColumn = if ((j == nthreads - 1)) columns else firstColumn + k
          futures(j) = ConcurrencyUtils.submit(new Runnable() {

            def run() {
              var zidx = zeroZ + firstColumn * strideZ
              var k = dcs.p(firstColumn)
              for (i <- firstColumn until lastColumn) {
                var sum = 0
                var high = dcs.p(i + 1)
                while (k + 10 < high) {
                  var ind = k + 9
                  sum += valuesA(ind) * elementsY(zeroY + strideY * dcs.i(ind -= 1)) +
                    valuesA(ind) * elementsY(zeroY + strideY * dcs.i(ind -= 1)) +
                    valuesA(ind) * elementsY(zeroY + strideY * dcs.i(ind -= 1)) +
                    valuesA(ind) * elementsY(zeroY + strideY * dcs.i(ind -= 1)) +
                    valuesA(ind) * elementsY(zeroY + strideY * dcs.i(ind -= 1)) +
                    valuesA(ind) * elementsY(zeroY + strideY * dcs.i(ind -= 1)) +
                    valuesA(ind) * elementsY(zeroY + strideY * dcs.i(ind -= 1)) +
                    valuesA(ind) * elementsY(zeroY + strideY * dcs.i(ind -= 1)) +
                    valuesA(ind) * elementsY(zeroY + strideY * dcs.i(ind -= 1)) +
                    valuesA(ind) * elementsY(zeroY + strideY * dcs.i(ind -= 1))
                  k += 10
                }
                while (k < high) {
                  sum += valuesA(k) * elementsY(dcs.i(k))
                  k += 1
                }
                elementsZ(zidx) = alpha * sum + beta * elementsZ(zidx)
                zidx += strideZ
              }
            }
          })
        }
        ConcurrencyUtils.waitForCompletion(futures)
      } else {
        var k = dcs.p(0)
        for (i <- 0 until columns) {
          var sum = 0
          val high = dcs.p(i + 1)
          while (k + 10 < high) {
            val ind = k + 9
            sum += valuesA(ind) * elementsY(zeroY + strideY * dcs.i(ind -= 1)) +
              valuesA(ind) * elementsY(zeroY + strideY * dcs.i(ind -= 1)) +
              valuesA(ind) * elementsY(zeroY + strideY * dcs.i(ind -= 1)) +
              valuesA(ind) * elementsY(zeroY + strideY * dcs.i(ind -= 1)) +
              valuesA(ind) * elementsY(zeroY + strideY * dcs.i(ind -= 1)) +
              valuesA(ind) * elementsY(zeroY + strideY * dcs.i(ind -= 1)) +
              valuesA(ind) * elementsY(zeroY + strideY * dcs.i(ind -= 1)) +
              valuesA(ind) * elementsY(zeroY + strideY * dcs.i(ind -= 1)) +
              valuesA(ind) * elementsY(zeroY + strideY * dcs.i(ind -= 1)) +
              valuesA(ind) * elementsY(zeroY + strideY * dcs.i(ind -= 1))
            k += 10
          }
          while (k < high) {
            sum += valuesA(k) * elementsY(dcs.i(k))
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
      C = if (B.isInstanceOf[SparseCCDoubleMatrix2D]) new SparseCCDoubleMatrix2D(rowsA, p, (rowsA * p)) else new DenseMatrix2D(rowsA,
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
      var AA: SparseCCDoubleMatrix2D = null
      AA = if (transposeA) getTranspose else this
      var BB: DenseMatrix2D = null
      BB = if (transposeB) B.viewDice().asInstanceOf[DenseMatrix2D] else B.asInstanceOf[DenseMatrix2D]
      val CC = C.asInstanceOf[DenseMatrix2D]
      val columnPointersA = AA.dcs.p
      val rowIndexesA = AA.dcs.i
      val valuesA = AA.dcs.x
      val zeroB = BB.index(0, 0).toInt
      val rowStrideB = BB.rowStride()
      val columnStrideB = BB.columnStride()
      val elementsB = BB.elements
      val zeroC = CC.index(0, 0).toInt
      val rowStrideC = CC.rowStride()
      val columnStrideC = CC.columnStride()
      val elementsC = CC.elements
      for (jj <- 0 until columnsB; kk <- 0 until columnsA) {
        val high = columnPointersA(kk + 1)
        val yElem = elementsB(zeroB + kk * rowStrideB + jj * columnStrideB)
        for (ii <- columnPointersA(kk) until high) {
          val j = rowIndexesA(ii)
          elementsC(zeroC + j * rowStrideC + jj * columnStrideC) += valuesA(ii) * yElem
        }
      }
      if (alpha != 1.0) {
        C.assign(cern.jet.math.tdouble.DoubleFunctions.mult(alpha))
      }
    } else if ((B.isInstanceOf[SparseCCDoubleMatrix2D]) && (C.isInstanceOf[SparseCCDoubleMatrix2D])) {
      var AA: SparseCCDoubleMatrix2D = null
      AA = if (transposeA) getTranspose else this
      var BB = B.asInstanceOf[SparseCCDoubleMatrix2D]
      if (transposeB) {
        BB = BB.getTranspose
      }
      val CC = C.asInstanceOf[SparseCCDoubleMatrix2D]
      CC.dcs = Dcs_multiply.cs_multiply(AA.dcs, BB.dcs)
      if (CC.dcs == null) {
        throw new IllegalArgumentException("Exception occured in cs_multiply()")
      }
      if (alpha != 1.0) {
        CC.assign(cern.jet.math.tdouble.DoubleFunctions.mult(alpha))
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
      val rowIndexesA = dcs.i
      val columnPointersA = dcs.p
      val valuesA = dcs.x
      var i = columns
      while (i >= 0) {
        val low = columnPointersA(i)
        var k = columnPointersA(i + 1)
        while (k >= low) {
          val j = rowIndexesA(k)
          fun.multiplicator = valuesA(k) * alpha
          if (!transposeA) Crows(j).assign(Brows(i), fun) else Crows(i).assign(Brows(j), fun)
        }
      }
    }
    C
  }

  protected def getStorageMatrix(): StrideMatrix2D = this

  protected def insert(row: Int,
      column: Int,
      index: Int,
      value: Double) {
    val rowIndexes = new IntArrayList(dcs.i)
    rowIndexes.setSizeRaw(dcs.p(columns))
    val values = new DoubleArrayList(dcs.x)
    values.setSizeRaw(dcs.p(columns))
    rowIndexes.beforeInsert(index, row)
    values.beforeInsert(index, value)
    var i = dcs.p.length
    while (i > column) dcs.p(i) += 1
    dcs.i = rowIndexes.elements()
    dcs.x = values.elements()
    dcs.nzmax = rowIndexes.elements().length
  }

  protected def remove(column: Int, index: Int) {
    val rowIndexes = new IntArrayList(dcs.i)
    val values = new DoubleArrayList(dcs.x)
    rowIndexes.remove(index)
    values.remove(index)
    var i = dcs.p.length
    while (i > column) dcs.p(i) -= 1
    dcs.i = rowIndexes.elements()
    dcs.x = values.elements()
    dcs.nzmax = rowIndexes.elements().length
  }
}
