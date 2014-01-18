package cern.colt.matrix.impl

import cern.colt.matrix.{Matrix1D, Matrix2D}

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
class SparseRCMatrix2D[T: Manifest: Numeric](rows: Int, columns: Int, nzmax: Int) extends SparsePointerIndexMatrix2D[T](true, rows, columns, nzmax) {

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

  def numeric = implicitly[Numeric[T]]

  override def assign(source: Matrix2D[T]): SparseRCMatrix2D[T] = {
    if (source eq this) return this
    checkShape(source)
    source match {
      case other: SparseRCMatrix2D[T] => {
        copyOther(other)
      }
      case _ => {
        assignConstant(numeric.zero)
        source.forEachNonZeroRowMajor(new Function3[Int, Int, T, T]() {
          def apply(r: Int, c: Int, value: T): T = {
            setQuick(r, c, value)
            value
          }
        })
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
    val cc = new SparseCCMatrix2D[T](rowsVar, columnsVar)
    cc.assign(this)
    cc
  }

  override def like2D(rows: Int, columns: Int) = new SparseRCMatrix2D(rows, columns)

  def toArray: Array[Array[T]] = {
    val values = Array.ofDim[T](rows, columns)
    toArray(values)
  }

  def viewRow(row: Int): Matrix1D[T] = {
    checkRow(row)
    new WrappedRowMatrix1D[T](this, row)
  }

  def viewColumn(column: Int): Matrix1D[T] = {
    checkColumn(column)
    new WrappedColumnMatrix1D[T](this, column)
  }

  override def viewColumnFlip(): Matrix2D[T] = {
    if (columnsVar == 0) {
      this
    }
    else {
      val view = new WrapperMatrix2D[T](this) {
        override protected def remapIndexes(row: Int, column: Int): Tuple2[Int, Int] = (row, columns - 1 - column)
      }
      view
    }
  }

  override def viewTranspose(): Matrix2D[T] = {
    val view = new WrapperMatrix2D[T](this, columnsVar, rowsVar) {
      override protected def remapIndexes(row: Int, column: Int): Tuple2[Int, Int] = (column, row)
    }
    view
  }

  override def viewPart(boxRow: Int, boxColumn: Int, height: Int, width: Int): Matrix2D[T] = {
    checkBox(boxRow, boxColumn, height, width)
    val view = new WrapperMatrix2D[T](this, height, width) {
      override protected def remapIndexes(row: Int, column: Int): Tuple2[Int, Int] = (row+boxRow, column+boxColumn)
    }
    view
  }

  override def viewRowFlip(): Matrix2D[T] = {
    if (rowsVar == 0) {
      this
    }
    else {
      val view = new WrapperMatrix2D[T](this) {
        override protected def remapIndexes(row: Int, column: Int): Tuple2[Int, Int] = (rowsVar - 1 - row, column)
      }
      view
    }
  }

  override def viewSelection(rowIndexes: Array[Int], columnIndexes: Array[Int]): Matrix2D[T] = {
    checkRowIndexes(rowIndexes)
    checkColumnIndexes(columnIndexes)
    val viewRowsVar = if (rowIndexes == null) rowsVar else rowIndexes.length
    val viewColumnsVar = if (columnIndexes == null) columnsVar else columnIndexes.length
    val view = new WrapperMatrix2D(this, viewRowsVar, viewColumnsVar) {
      override protected def remapIndexes(row: Int, column: Int): Tuple2[Int, Int] = {
        (if (rowIndexes == null) row else rowIndexes(row), if (columnIndexes == null) column else columnIndexes(column))
      }
    }
    view
  }

  override def viewStrides(rowStride: Int, columnStride: Int): Matrix2D[T] = {
    if (rowStride <= 0 || columnStride <= 0) throw new IndexOutOfBoundsException("illegal stride")
    val viewRowsVar = if (rowsVar != 0) (rowsVar - 1) / rowStride + 1 else rowsVar
    val viewColumnsVar = if (columnsVar != 0) (columnsVar - 1) / columnStride + 1 else columnsVar
    val view = new WrapperMatrix2D(this, viewRowsVar, viewColumnsVar) {
      override protected def remapIndexes(row: Int, column: Int): Tuple2[Int, Int] = {
        (rowStride * row, columnStride * column)
      }
    }
    view
  }
}
