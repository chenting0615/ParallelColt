package cern.colt.matrix.tint.impl

import java.util.Arrays
import java.util.concurrent.Future
import cern.colt.list.tint.IntArrayList
import cern.colt.matrix.tint.IntMatrix1D
import cern.colt.matrix.tint.IntMatrix2D
import edu.emory.mathcs.utils.ConcurrencyUtils
import SparseCCIntMatrix2D._
//remove if not needed
import scala.collection.JavaConversions._

object SparseCCIntMatrix2D {

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
 * Sparse column-compressed 2-d matrix holding <tt>int</tt> elements. First see
 * the <a href="package-summary.html">package summary</a> and javadoc <a
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
 * A.forEachNonZero(new cern.colt.function.IntIntIntFunction() {
 *     public int apply(int row, int column, int value) {
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
 * B.forEachNonZero(new cern.colt.function.IntIntIntFunction() {
 *     public int apply(int row, int column, int value) {
 *         A.setQuick(row, column, A.getQuick(row, column) + alpha * value);
 *         return value;
 *     }
 * });
 * </pre>
 *
 * </td>
 * </table>
 * Method {@link #assign(IntMatrix2D,cern.colt.function.tint.IntIntFunction)}
 * does just that if you supply
 * {@link cern.jet.math.tint.IntFunctions#plusMultSecond} as argument.
 *
 *
 * @author Piotr Wendykier
 *
 */
@SerialVersionUID(1L)
class SparseCCIntMatrix2D(rows: Int, columns: Int, nzmax: Int) extends WrapperIntMatrix2D(null) {

  protected var columnPointers: Array[Int] = new Array[Int](columns + 1)

  protected var rowIndexes: Array[Int] = new Array[Int](nzmax)

  protected var values: Array[Int] = new Array[Int](nzmax)

  protected var rowIndexesSorted: Boolean = false

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
  def this(values: Array[Array[Int]]) {
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
      value: Int,
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
    this.rowIndexes = Array.ofDim[Int](nz)
    this.values = Array.ofDim[Int](nz)
    this.columnPointers = Array.ofDim[Int](columns + 1)
    val w = Array.ofDim[Int](columns)
    val r: Int = 0
    for (k <- 0 until nz) {
      w(columnIndexes(k)) += 1
    }
    cumsum(this.columnPointers, w, columns)
    for (k <- 0 until nz) {
      this.rowIndexes(r = w(columnIndexes(k)) += 1) = rowIndexes(k)
      this.values(r) = value
    }
    if (removeDuplicates) {
      removeDuplicates()
    }
    if (sortRowIndexes) {
      sortRowIndexes()
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
      values: Array[Int],
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
    this.rowIndexes = Array.ofDim[Int](nz)
    this.values = Array.ofDim[Int](nz)
    this.columnPointers = Array.ofDim[Int](columns + 1)
    val w = Array.ofDim[Int](columns)
    val r: Int = 0
    for (k <- 0 until nz) {
      w(columnIndexes(k)) += 1
    }
    cumsum(this.columnPointers, w, columns)
    for (k <- 0 until nz) {
      this.rowIndexes(r = w(columnIndexes(k)) += 1) = rowIndexes(k)
      this.values(r) = values(k)
    }
    if (removeDuplicates) {
      removeDuplicates()
    }
    if (sortRowIndexes) {
      sortRowIndexes()
    }
  }

  def assign(function: cern.colt.function.tint.IntFunction): IntMatrix2D = {
    if (function.isInstanceOf[cern.jet.math.tint.IntMult]) {
      val alpha = function.asInstanceOf[cern.jet.math.tint.IntMult].multiplicator
      if (alpha == 1) return this
      if (alpha == 0) return assign(0)
      if (alpha != alpha) return assign(alpha)
      val valuesE = values
      val nz = cardinality()
      for (j <- 0 until nz) {
        valuesE(j) *= alpha
      }
    } else {
      forEachNonZero(new cern.colt.function.tint.IntIntIntFunction() {

        def apply(i: Int, j: Int, value: Int): Int = return function.apply(value)
      })
    }
    this
  }

  def assign(value: Int): IntMatrix2D = {
    if (value == 0) {
      Arrays.fill(rowIndexes, 0)
      Arrays.fill(columnPointers, 0)
      Arrays.fill(values, 0)
    } else {
      val nnz = cardinality()
      for (i <- 0 until nnz) {
        values(i) = value
      }
    }
    this
  }

  def assign(source: IntMatrix2D): IntMatrix2D = {
    if (source == this) return this
    checkShape(source)
    if (source.isInstanceOf[SparseCCIntMatrix2D]) {
      val other = source.asInstanceOf[SparseCCIntMatrix2D]
      System.arraycopy(other.getColumnPointers, 0, columnPointers, 0, columns + 1)
      val nzmax = other.getRowIndexes.length
      if (rowIndexes.length < nzmax) {
        rowIndexes = Array.ofDim[Int](nzmax)
        values = Array.ofDim[Int](nzmax)
      }
      System.arraycopy(other.getRowIndexes, 0, rowIndexes, 0, nzmax)
      System.arraycopy(other.getValues, 0, values, 0, nzmax)
      rowIndexesSorted = other.rowIndexesSorted
    } else if (source.isInstanceOf[SparseRCIntMatrix2D]) {
      val other = source.asInstanceOf[SparseRCIntMatrix2D].getTranspose
      columnPointers = other.getRowPointers
      rowIndexes = other.getColumnIndexes
      values = other.getValues
      rowIndexesSorted = true
    } else {
      assign(0)
      source.forEachNonZero(new cern.colt.function.tint.IntIntIntFunction() {

        def apply(i: Int, j: Int, value: Int): Int = {
          setQuick(i, j, value)
          return value
        }
      })
    }
    this
  }

  def assign(y: IntMatrix2D, function: cern.colt.function.tint.IntIntFunction): IntMatrix2D = {
    checkShape(y)
    if ((y.isInstanceOf[SparseCCIntMatrix2D]) && (function == cern.jet.math.tint.IntFunctions.plus)) {
      val yy = y.asInstanceOf[SparseCCIntMatrix2D]
      var p: Int = 0
      var j: Int = 0
      var nz = 0
      var anz: Int = 0
      var Cp: Array[Int] = 0
      var Ci: Array[Int] = 0
      var Bp: Array[Int] = 0
      var m: Int = 0
      var n: Int = 0
      var bnz: Int = 0
      var w: Array[Int] = 0
      var x: Array[Int] = 0
      var Cx: Array[Int] = 0
      m = rows
      anz = columnPointers(columns)
      n = yy.columns
      Bp = yy.columnPointers
      bnz = Bp(n)
      w = Array.ofDim[Int](m)
      x = Array.ofDim[Int](m)
      val C = new SparseCCIntMatrix2D(m, n, anz + bnz)
      Cp = C.columnPointers
      Ci = C.rowIndexes
      Cx = C.values
      j = 0
      while (j < n) {
        Cp(j) = nz
        nz = scatter(this, j, 1, w, x, j + 1, C, nz)
        nz = scatter(yy, j, 1, w, x, j + 1, C, nz)
        p = Cp(j)
        while (p < nz) {Cx(p) = x(Ci(p))p += 1
        }
        j += 1
      }
      Cp(n) = nz
      rowIndexes = Ci
      columnPointers = Cp
      values = Cx
      return this
    }
    if (function.isInstanceOf[cern.jet.math.tint.IntPlusMultSecond]) {
      val alpha = function.asInstanceOf[cern.jet.math.tint.IntPlusMultSecond].multiplicator
      if (alpha == 0) return this
      y.forEachNonZero(new cern.colt.function.tint.IntIntIntFunction() {

        def apply(i: Int, j: Int, value: Int): Int = {
          setQuick(i, j, getQuick(i, j) + alpha * value)
          return value
        }
      })
      return this
    }
    if (function.isInstanceOf[cern.jet.math.tint.IntPlusMultFirst]) {
      val alpha = function.asInstanceOf[cern.jet.math.tint.IntPlusMultFirst].multiplicator
      if (alpha == 0) return assign(y)
      y.forEachNonZero(new cern.colt.function.tint.IntIntIntFunction() {

        def apply(i: Int, j: Int, value: Int): Int = {
          setQuick(i, j, alpha * getQuick(i, j) + value)
          return value
        }
      })
      return this
    }
    if (function == cern.jet.math.tint.IntFunctions.mult) {
      val rowIndexesA = rowIndexes
      val columnPointersA = columnPointers
      val valuesA = values
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
    if (function == cern.jet.math.tint.IntFunctions.div) {
      val rowIndexesA = rowIndexes
      val columnPointersA = columnPointers
      val valuesA = values
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

  def cardinality(): Int = columnPointers(columns)

  def forEachNonZero(function: cern.colt.function.tint.IntIntIntFunction): IntMatrix2D = {
    val rowIndexesA = rowIndexes
    val columnPointersA = columnPointers
    val valuesA = values
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
  def getColumnPointers(): Array[Int] = columnPointers

  /**
   * Returns a new matrix that has the same elements as this matrix, but is in
   * a dense form. This method creates a new object (not a view), so changes
   * in the returned matrix are NOT reflected in this matrix.
   *
   * @return this matrix in a dense form
   */
  def getDense(): DenseIntMatrix2D = {
    val dense = new DenseIntMatrix2D(rows, columns)
    forEachNonZero(new cern.colt.function.tint.IntIntIntFunction() {

      def apply(i: Int, j: Int, value: Int): Int = {
        dense.setQuick(i, j, getQuick(i, j))
        return value
      }
    })
    dense
  }

  def getQuick(row: Int, column: Int): Int = {
    synchronized {
      val k = searchFromTo(rowIndexes, row, columnPointers(column), columnPointers(column + 1) - 1)
      var v = 0
      if (k >= 0) v = values(k)
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
  def getRowCompressed(): SparseRCIntMatrix2D = {
    val tr = getTranspose
    val rc = new SparseRCIntMatrix2D(rows, columns)
    rc.columnIndexes = tr.rowIndexes
    rc.rowPointers = tr.columnPointers
    rc.values = tr.values
    rc.columnIndexesSorted = true
    rc
  }

  /**
   * Returns row indexes;
   *
   * @return row indexes
   */
  def getRowIndexes(): Array[Int] = rowIndexes

  /**
   * Returns a new matrix that is the transpose of this matrix. This method
   * creates a new object (not a view), so changes in the returned matrix are
   * NOT reflected in this matrix.
   *
   * @return the transpose of this matrix
   */
  def getTranspose(): SparseCCIntMatrix2D = {
    var p: Int = 0
    var q: Int = 0
    var j: Int = 0
    var Cp: Array[Int] = 0
    var Ci: Array[Int] = 0
    var n: Int = 0
    var m: Int = 0
    var Ap: Array[Int] = 0
    var Ai: Array[Int] = 0
    var w: Array[Int] = 0
    var Cx: Array[Int] = 0
    var Ax: Array[Int] = 0
    m = rows
    n = columns
    Ap = columnPointers
    Ai = rowIndexes
    Ax = values
    val C = new SparseCCIntMatrix2D(columns, rows, Ai.length)
    w = Array.ofDim[Int](m)
    Cp = C.columnPointers
    Ci = C.rowIndexes
    Cx = C.values
    p = 0
    while (p < Ap(n)) {w(Ai(p)) += 1p += 1
    }
    cumsum(Cp, w, m)
    j = 0
    while (j < n) {
      p = Ap(j)
      while (p < Ap(j + 1)) {
        Ci(q = w(Ai(p)) += 1) = j
        Cx(q) = Ax(p)
        p += 1
      }
      j += 1
    }
    C
  }

  /**
   * Returns numerical values
   *
   * @return numerical values
   */
  def getValues(): Array[Int] = values

  /**
   * Returns true if row indexes are sorted, false otherwise
   *
   * @return true if row indexes are sorted, false otherwise
   */
  def hasRowIndexesSorted(): Boolean = rowIndexesSorted

  def like(rows: Int, columns: Int): IntMatrix2D = new SparseCCIntMatrix2D(rows, columns)

  def like1D(size: Int): IntMatrix1D = new SparseIntMatrix1D(size)

  def setQuick(row: Int, column: Int, value: Int) {
    synchronized {
      var k = searchFromTo(rowIndexes, row, columnPointers(column), columnPointers(column + 1) - 1)
      if (k >= 0) {
        if (value == 0) remove(column, k) else values(k) = value
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
    var tr = getTranspose
    tr = tr.getTranspose
    columnPointers = tr.columnPointers
    rowIndexes = tr.rowIndexes
    values = tr.values
    rowIndexesSorted = true
  }

  /**
   * Removes (sums) duplicate entries (if any}
   */
  def removeDuplicates() {
    var i: Int = 0
    var j: Int = 0
    var p: Int = 0
    var q: Int = 0
    var nz = 0
    var n: Int = 0
    var m: Int = 0
    var Ap: Array[Int] = 0
    var Ai: Array[Int] = 0
    var w: Array[Int] = 0
    var Ax: Array[Int] = 0
    m = rows
    n = columns
    Ap = columnPointers
    Ai = rowIndexes
    Ax = values
    w = Array.ofDim[Int](m)
    i = 0
    while (i < m) {w(i) = -1i += 1
    }
    j = 0
    while (j < n) {
      q = nz
      p = Ap(j)
      while (p < Ap(j + 1)) {
        i = Ai(p)
        if (w(i) >= q) {
          Ax(w(i)) += Ax(p)
        } else {
          w(i) = nz
          Ai(nz) = i
          Ax(nz += 1) = Ax(p)
        }
        p += 1
      }
      Ap(j) = q
      j += 1
    }
    Ap(n) = nz
  }

  /**
   * Removes zero entries (if any)
   */
  def removeZeroes() {
    var j: Int = 0
    var p: Int = 0
    var nz = 0
    var n: Int = 0
    var Ap: Array[Int] = 0
    var Ai: Array[Int] = 0
    var Ax: Array[Int] = 0
    n = columns
    Ap = columnPointers
    Ai = rowIndexes
    Ax = values
    j = 0
    while (j < n) {
      p = Ap(j)
      Ap(j) = nz
      while (p < Ap(j + 1)) {
        if (Ax(p) != 0) {
          Ax(nz) = Ax(p)
          Ai(nz += 1) = Ai(p)
        }
        p += 1
      }
      j += 1
    }
    Ap(n) = nz
  }

  def trimToSize() {
    realloc(0)
  }

  override def toString(): String = {
    val builder = new StringBuilder()
    builder.append(rows).append(" x ").append(columns).append(" sparse matrix, nnz = ")
      .append(cardinality())
      .append('\n')
    for (i <- 0 until columns) {
      val high = columnPointers(i + 1)
      for (j <- columnPointers(i) until high) {
        builder.append('(').append(rowIndexes(j)).append(',')
          .append(i)
          .append(')')
          .append('\t')
          .append(values(j))
          .append('\n')
      }
    }
    builder.toString
  }

  def zMult(y: IntMatrix1D,
      z: IntMatrix1D,
      alpha: Int,
      beta: Int,
      transposeA: Boolean): IntMatrix1D = {
    val rowsA = if (transposeA) columns else rows
    val columnsA = if (transposeA) rows else columns
    val ignore = (z == null || transposeA)
    if (z == null) z = new DenseIntMatrix1D(rowsA)
    if (!(y.isInstanceOf[DenseIntMatrix1D] && z.isInstanceOf[DenseIntMatrix1D])) {
      return super.zMult(y, z, alpha, beta, transposeA)
    }
    if (columnsA != y.size || rowsA > z.size) throw new IllegalArgumentException("Incompatible args: " +
      ((if (transposeA) viewDice() else this).toShapeString()) +
      ", " +
      y.toShapeString() +
      ", " +
      z.toShapeString())
    val zz = z.asInstanceOf[DenseIntMatrix1D]
    val elementsZ = zz.elements
    val strideZ = zz.stride()
    val zeroZ = zz.index(0).toInt
    val yy = y.asInstanceOf[DenseIntMatrix1D]
    val elementsY = yy.elements
    val strideY = yy.stride()
    val zeroY = yy.index(0).toInt
    val rowIndexesA = rowIndexes
    val columnPointersA = columnPointers
    val valuesA = values
    var zidx = zeroZ
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if (!transposeA) {
      if ((!ignore) && (beta != 1)) {
        z.assign(cern.jet.math.tint.IntFunctions.mult(beta))
      }
      if ((nthreads > 1) &&
        (cardinality() >= ConcurrencyUtils.getThreadsBeginN_2D)) {
        nthreads = 2
        val futures = Array.ofDim[Future](nthreads)
        val result = Array.ofDim[Int](rowsA)
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
                    elementsZ(zeroZ + strideZ * j) += alpha * valuesA(k) * yElem
                  }
                }
              } else {
                for (i <- firstColumn until lastColumn) {
                  var high = columnPointersA(i + 1)
                  var yElem = elementsY(zeroY + strideY * i)
                  for (k <- columnPointersA(i) until high) {
                    var j = rowIndexesA(k)
                    result(j) += alpha * valuesA(k) * yElem
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
            elementsZ(zeroZ + strideZ * j) += alpha * valuesA(k) * yElem
          }
        }
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
              var k = columnPointers(firstColumn)
              for (i <- firstColumn until lastColumn) {
                var sum = 0
                var high = columnPointers(i + 1)
                while (k + 10 < high) {
                  var ind = k + 9
                  sum += valuesA(ind) * elementsY(zeroY + strideY * rowIndexes(ind -= 1)) +
                    valuesA(ind) * elementsY(zeroY + strideY * rowIndexes(ind -= 1)) +
                    valuesA(ind) * elementsY(zeroY + strideY * rowIndexes(ind -= 1)) +
                    valuesA(ind) * elementsY(zeroY + strideY * rowIndexes(ind -= 1)) +
                    valuesA(ind) * elementsY(zeroY + strideY * rowIndexes(ind -= 1)) +
                    valuesA(ind) * elementsY(zeroY + strideY * rowIndexes(ind -= 1)) +
                    valuesA(ind) * elementsY(zeroY + strideY * rowIndexes(ind -= 1)) +
                    valuesA(ind) * elementsY(zeroY + strideY * rowIndexes(ind -= 1)) +
                    valuesA(ind) * elementsY(zeroY + strideY * rowIndexes(ind -= 1)) +
                    valuesA(ind) * elementsY(zeroY + strideY * rowIndexes(ind -= 1))
                  k += 10
                }
                while (k < high) {
                  sum += valuesA(k) * elementsY(rowIndexes(k))
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
        var k = columnPointers(0)
        for (i <- 0 until columns) {
          var sum = 0
          val high = columnPointers(i + 1)
          while (k + 10 < high) {
            val ind = k + 9
            sum += valuesA(ind) * elementsY(zeroY + strideY * rowIndexes(ind -= 1)) +
              valuesA(ind) * elementsY(zeroY + strideY * rowIndexes(ind -= 1)) +
              valuesA(ind) * elementsY(zeroY + strideY * rowIndexes(ind -= 1)) +
              valuesA(ind) * elementsY(zeroY + strideY * rowIndexes(ind -= 1)) +
              valuesA(ind) * elementsY(zeroY + strideY * rowIndexes(ind -= 1)) +
              valuesA(ind) * elementsY(zeroY + strideY * rowIndexes(ind -= 1)) +
              valuesA(ind) * elementsY(zeroY + strideY * rowIndexes(ind -= 1)) +
              valuesA(ind) * elementsY(zeroY + strideY * rowIndexes(ind -= 1)) +
              valuesA(ind) * elementsY(zeroY + strideY * rowIndexes(ind -= 1)) +
              valuesA(ind) * elementsY(zeroY + strideY * rowIndexes(ind -= 1))
            k += 10
          }
          while (k < high) {
            sum += valuesA(k) * elementsY(rowIndexes(k))
            k += 1
          }
          elementsZ(zidx) = alpha * sum + beta * elementsZ(zidx)
          zidx += strideZ
        }
      }
    }
    z
  }

  def zMult(B: IntMatrix2D,
      C: IntMatrix2D,
      alpha: Int,
      beta: Int,
      transposeA: Boolean,
      transposeB: Boolean): IntMatrix2D = {
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
    var p = columnsB
    val ignore = (C == null)
    if (C == null) {
      C = if (B.isInstanceOf[SparseCCIntMatrix2D]) new SparseCCIntMatrix2D(rowsA, p, (rowsA * p)) else new DenseIntMatrix2D(rowsA,
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
      C.assign(cern.jet.math.tint.IntFunctions.mult(beta))
    }
    if ((B.isInstanceOf[DenseIntMatrix2D]) && (C.isInstanceOf[DenseIntMatrix2D])) {
      var AA: SparseCCIntMatrix2D = null
      AA = if (transposeA) getTranspose else this
      var BB: DenseIntMatrix2D = null
      BB = if (transposeB) B.viewDice().asInstanceOf[DenseIntMatrix2D] else B.asInstanceOf[DenseIntMatrix2D]
      val CC = C.asInstanceOf[DenseIntMatrix2D]
      val columnPointersA = AA.columnPointers
      val rowIndexesA = AA.rowIndexes
      val valuesA = AA.values
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
        C.assign(cern.jet.math.tint.IntFunctions.mult(alpha))
      }
    } else if ((B.isInstanceOf[SparseCCIntMatrix2D]) && (C.isInstanceOf[SparseCCIntMatrix2D])) {
      var AA: SparseCCIntMatrix2D = null
      AA = if (transposeA) getTranspose else this
      var BB = B.asInstanceOf[SparseCCIntMatrix2D]
      if (transposeB) {
        BB = BB.getTranspose
      }
      val CC = C.asInstanceOf[SparseCCIntMatrix2D]
      var j: Int = 0
      var nz = 0
      var Cp: Array[Int] = 0
      var Ci: Array[Int] = 0
      var Bp: Array[Int] = 0
      var m: Int = 0
      var n: Int = 0
      var w: Array[Int] = 0
      var Bi: Array[Int] = 0
      var x: Array[Int] = 0
      var Bx: Array[Int] = 0
      var Cx: Array[Int] = 0
      m = rowsA
      n = columnsB
      Bp = BB.columnPointers
      Bi = BB.rowIndexes
      Bx = BB.values
      w = Array.ofDim[Int](m)
      x = Array.ofDim[Int](m)
      Cp = CC.columnPointers
      Ci = CC.rowIndexes
      Cx = CC.values
      j = 0
      while (j < n) {
        var nzmaxC = CC.rowIndexes.length
        if (nz + m > nzmaxC) {
          nzmaxC = 2 * nzmaxC + m
          val rowIndexesNew = Array.ofDim[Int](nzmaxC)
          System.arraycopy(Ci, 0, rowIndexesNew, 0, Ci.length)
          Ci = rowIndexesNew
          val valuesNew = Array.ofDim[Int](nzmaxC)
          System.arraycopy(Cx, 0, valuesNew, 0, Cx.length)
          Cx = valuesNew
        }
        Cp(j) = nz
        p = Bp(j)
        while (p < Bp(j + 1)) {
          nz = scatter(AA, Bi(p), Bx(p), w, x, j + 1, CC, nz)
          p += 1
        }
        p = Cp(j)
        while (p < nz) {Cx(p) = x(Ci(p))p += 1
        }
        j += 1
      }
      Cp(n) = nz
      if (alpha != 1.0) {
        CC.assign(cern.jet.math.tint.IntFunctions.mult(alpha))
      }
    } else {
      if (transposeB) {
        B = B.viewDice()
      }
      val Brows = Array.ofDim[IntMatrix1D](columnsA)
      var i = columnsA
      while (i >= 0) Brows(i) = B.viewRow(i)
      val Crows = Array.ofDim[IntMatrix1D](rowsA)
      var i = rowsA
      while (i >= 0) Crows(i) = C.viewRow(i)
      val fun = cern.jet.math.tint.IntPlusMultSecond.plusMult(0)
      val rowIndexesA = rowIndexes
      val columnPointersA = columnPointers
      val valuesA = values
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

  protected def getStorageMatrix(): IntMatrix2D = this

  protected def insert(row: Int,
      column: Int,
      index: Int,
      value: Int) {
    val rowIndexesList = new IntArrayList(rowIndexes)
    rowIndexesList.setSizeRaw(columnPointers(columns))
    val valuesList = new IntArrayList(values)
    valuesList.setSizeRaw(columnPointers(columns))
    rowIndexesList.beforeInsert(index, row)
    valuesList.beforeInsert(index, value)
    var i = columnPointers.length
    while (i > column) columnPointers(i) += 1
    rowIndexes = rowIndexesList.elements()
    values = valuesList.elements()
  }

  protected def remove(column: Int, index: Int) {
    val rowIndexesList = new IntArrayList(rowIndexes)
    val valuesList = new IntArrayList(values)
    rowIndexesList.remove(index)
    valuesList.remove(index)
    var i = columnPointers.length
    while (i > column) columnPointers(i) -= 1
    rowIndexes = rowIndexesList.elements()
    values = valuesList.elements()
  }

  private def cumsum(p: Array[Int], c: Array[Int], n: Int): Int = {
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
    if (nzmax <= 0) nzmax = columnPointers(columns)
    val rowIndexesNew = Array.ofDim[Int](nzmax)
    var length = Math.min(nzmax, rowIndexes.length)
    System.arraycopy(rowIndexes, 0, rowIndexesNew, 0, length)
    rowIndexes = rowIndexesNew
    val valuesNew = Array.ofDim[Int](nzmax)
    length = Math.min(nzmax, values.length)
    System.arraycopy(values, 0, valuesNew, 0, length)
    values = valuesNew
  }

  private def scatter(A: SparseCCIntMatrix2D,
      j: Int,
      beta: Int,
      w: Array[Int],
      x: Array[Int],
      mark: Int,
      C: SparseCCIntMatrix2D,
      nz: Int): Int = {
    var i: Int = 0
    var p: Int = 0
    var Ap: Array[Int] = 0
    var Ai: Array[Int] = 0
    var Ci: Array[Int] = 0
    var Ax: Array[Int] = null
    Ap = A.columnPointers
    Ai = A.rowIndexes
    Ax = A.values
    Ci = C.rowIndexes
    p = Ap(j)
    while (p < Ap(j + 1)) {
      i = Ai(p)
      if (w(i) < mark) {
        w(i) = mark
        Ci(nz += 1) = i
        if (x != null) x(i) = beta * Ax(p)
      } else if (x != null) x(i) += beta * Ax(p)
      p += 1
    }
    nz
  }
}
