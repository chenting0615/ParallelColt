package cern.colt.matrix.impl

import java.util
import cern.colt.list.impl.ArrayList
import cern.colt.matrix.Matrix2D

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
@SerialVersionUID(1L)
abstract class SparsePointerIndexMatrix2D[T: Manifest: Numeric](rowMajor: Boolean, rows_p: Int, columns_p: Int, nzmax: Int) extends AbstractMatrix2D[T] {

  protected var majorDimPointers = Array.ofDim[Int](if (rowMajor) rows_p + 1 else columns_p + 1)

  protected var minorDimIndexes = new ArrayList[Int](nzmax)

  protected var values = new ArrayList[T](nzmax)

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
  def this(rowMajor: Boolean, rows: Int, columns: Int) {
    this(rowMajor, rows, columns, Math.min(rows.toLong * columns.toLong / 10l, Integer.MAX_VALUE.toLong).toInt)
  }

  override def isRowMajor = rowMajor

  override def assignConstant(value: T) = {
    if (value == numeric.zero) {
      util.Arrays.fill(majorDimPointers, 0)
      minorDimIndexes.clear()
      values.clear()
    }
    else if (rowMajor) {
      for(row <- 0 until rowsVar; col <- 0 until columnsVar)
        setQuick(row, col, value)
    }
    else {
      for(col <- 0 until columnsVar; row <- 0 until rowsVar)
        setQuick(row, col, value)
    }
    this
  }

  override def numNonZero = majorDimPointers( if (rowMajor) rowsVar else columnsVar )

  override def forEachNonZeroRowMajor(function: Function3[Int, Int, T, T]) = {
    if (rowMajor) {
      /* We don't over-ride the column-major version of this, because it doesn't save much */
      for(rowIdx <- 0 until rowsVar) {
        for(colIdxIdx <- majorDimPointers(rowIdx) until majorDimPointers(rowIdx+1)) {
          val colIdx = minorDimIndexes.getQuick(colIdxIdx)
          val oldValue = values.getQuick(colIdxIdx)
          val newValue = function.apply(rowIdx, colIdx, oldValue)
          if (newValue != oldValue)
            values.setQuick(colIdxIdx, newValue)
        }
      }
      this
    }
    else
      super.forEachNonZeroRowMajor(function)
  }

  override def forEachNonZeroColumnMajor(function: Function3[Int, Int, T, T]) = {
    if (! rowMajor) {
      /* We don't over-ride the row-major version of this, because it doesn't save much */
      for(colIdx <- 0 until columnsVar) {
        for(rowIdxIdx <- majorDimPointers(colIdx) until majorDimPointers(colIdx+1)) {
          val rowIdx = minorDimIndexes.getQuick(rowIdxIdx)
          val oldValue = values.getQuick(rowIdxIdx)
          val newValue = function.apply(rowIdx, colIdx, oldValue)
          if (newValue != oldValue)
            values.setQuick(rowIdxIdx, newValue)
        }
      }
      this
    }
    else
      super.forEachNonZeroColumnMajor(function)
  }

  /**
   * Returns a new matrix that has the same elements as this matrix, but is in
   * a dense form. This method creates a new object (not a view), so changes
   * in the returned matrix are NOT reflected in this matrix.
   *
   * @return this matrix in a dense form
   */
  def getDense: DenseMatrix2D[T] = {
    val dense = new DenseMatrix2D[T](rows, columns)
    if (rowMajor) {
      forEachNonZeroRowMajor(new Function3[Int, Int, T, T]() {
        def apply(r: Int, c: Int, value: T): T = {
          dense.setQuick(r, c, value)
          value
        }
      })
    }
    else {
      forEachNonZeroColumnMajor(new Function3[Int, Int, T, T]() {
        def apply(r: Int, c: Int, value: T): T = {
          dense.setQuick(r, c, value)
          value
        }
      })
    }
    dense
  }

  private def searchForIndex(major: Int, minor: Int): Int = {
    // TODO: Use a binary search?
    val firstIdx = majorDimPointers(major)
    val nextRowIdx = majorDimPointers(major + 1)
    for(idx <- firstIdx until nextRowIdx) {
      val valueIdx = minorDimIndexes.getQuick(idx)
      if (valueIdx == minor)
        return idx
      else if (valueIdx > minor)
        return -(idx + 1)
    }
    -(nextRowIdx + 1)
  }

  override def getQuick(row: Int, column: Int): T = {
    var major = 0
    var minor = 0
    if (rowMajor) {
      major = row
      minor = column
    }
    else {
      major = column
      minor = row
    }
    val idx = searchForIndex(major, minor)
    if (idx >= 0) values.getQuick(idx) else numeric.zero
  }

  override def setQuick(row: Int, column: Int, value: T) {
    var major = 0
    var minor = 0
    if (rowMajor) {
      major = row
      minor = column
    }
    else {
      major = column
      minor = row
    }
    var idx = searchForIndex(major, minor)
    if (idx >= 0) {
      if (value == numeric.zero) {
        minorDimIndexes.remove(idx)
        values.remove(idx)
        for(i <- major+1 until majorDimPointers.length) {
          majorDimPointers(i) -= 1
        }
      }
      else
        values.setQuick(idx, value)
    }
    else if (value != numeric.zero) {
      idx = -idx - 1
      minorDimIndexes.beforeInsert(idx, minor)
      values.beforeInsert(idx, value)
      for(i <- major+1 until majorDimPointers.length) {
        majorDimPointers(i) += 1
      }
    }
  }

  /**
   * Returns a new matrix that is the transpose of this matrix. This method
   * creates a new object (not a view), so changes in the returned matrix are
   * NOT reflected in this matrix.
   *
   * @return the transpose of this matrix
   */
  def getTranspose: Matrix2D[T] = {
    val T = like2D(columns, rows)
    T.assign(this.viewTranspose())
    T
  }

  override def like1D(size: Int) = new SparseHashMatrix1D[T](size)

  protected def copyOther(other: SparsePointerIndexMatrix2D[T]) {
    if (rowMajor != other.isRowMajor)
      throw new IllegalArgumentException("Cannot copy matrices when one is row-major and one is column-major")
    minorDimIndexes = other.minorDimIndexes.copy()
    majorDimPointers = other.majorDimPointers.clone()
    values = other.values.copy()
  }

  override def copy() = {
    val c = clone().asInstanceOf[SparsePointerIndexMatrix2D[T]]
    c.majorDimPointers = majorDimPointers.clone()
    c.minorDimIndexes = minorDimIndexes.copy()
    c.values = values.copy()
    c
  }

  override def toString: String = {
    val builder = new StringBuilder()
    builder.append(rows).append(" x ").append(columns).append(" sparse matrix, nnz = ")
      .append(numNonZero)
      .append('\n')
    var major = 0
    var minor = 0
    if (rowMajor) {
      major = rows
      minor = columns
    }
    else {
      major = columns
      minor = rows
    }
    for (i <- 0 until major) {
      val high = majorDimPointers(i + 1)
      for (j <- majorDimPointers(i) until high) {
        builder.append('(')
        if (rowMajor)
          builder.append(i).append(',').append(minorDimIndexes.getQuick(j))
        else
          builder.append(minorDimIndexes.getQuick(j)).append(',').append(i)

        builder.append(')')
          .append('\t')
          .append(values.getQuick(j))
          .append('\n')
      }
    }
    builder.toString()
  }

  override def trimToSize() {
    minorDimIndexes.trimToSize()
    values.trimToSize()
  }
}
