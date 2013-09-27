package cern.colt.matrix.impl

import cern.colt.matrix._
import it.unimi.dsi.fastutil.ints.IntArrayList

/**
 * Dense 2-d matrix holding a specific type elements in an array. First see the <a
 * href="package-summary.html">package summary</a> and javadoc <a
 * href="package-tree.html">tree view</a> to get the broad picture.
 * <p>
 * <b>Implementation:</b>
 * <p>
 * Internally holds one single contigous one-dimensional array, addressed in row
 * major. Note that this implementation is not synchronized.
 * <p>
 * <b>Time complexity:</b>
 * <p>
 * <tt>O(1)</tt> (i.e. constant time) for the basic operations <tt>get</tt>,
 * <tt>getQuick</tt>, <tt>set</tt>, <tt>setQuick</tt> and <tt>size</tt>,
 * <p>
 * Cells are internally addressed in row-major. Applications demanding utmost
 * speed can exploit this fact. Setting/getting values in a loop row-by-row is
 * quicker than column-by-column. Thus
 *
 * <pre>
 * for (int row = 0; row &lt; rows; row++) {
 *     for (int column = 0; column &lt; columns; column++) {
 *         matrix.setQuick(row, column, someValue);
 *     }
 * }
 * </pre>
 *
 * is quicker than
 *
 * <pre>
 * for (int column = 0; column &lt; columns; column++) {
 *     for (int row = 0; row &lt; rows; row++) {
 *         matrix.setQuick(row, column, someValue);
 *     }
 * }
 * </pre>
 *
 * @author wolfgang.hoschek@cern.ch
 * @version 1.0, 09/24/99
 *
 * @author Piotr Wendykier (piotr.wendykier@gmail.com)
 *
 */
@specialized
@SerialVersionUID(1L)
class DenseMatrix2D[T: Manifest](rows: Int, columns: Int) extends StrideMatrix2D[T] {

  protected var elementsVar: Array[T] = Array.ofDim[T](rows * columns)

  setUp(rows, columns)

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
   * Constructs a matrix with the given parameters.
   *
   * @param rows
   *            the number of rows the matrix shall have.
   * @param columns
   *            the number of columns the matrix shall have.
   * @param elements
   *            the cells.
   * @param rowZero
   *            the position of the first element.
   * @param columnZero
   *            the position of the first element.
   * @param rowStride
   *            the number of elements between two rows, i.e.
   *            <tt>index(i+1,j)-index(i,j)</tt>.
   * @param columnStride
   *            the number of elements between two columns, i.e.
   *            <tt>index(i,j+1)-index(i,j)</tt>.
   * @param isView
   *            if true then a matrix view is constructed
   * @throws IllegalArgumentException
   *             if
   *             <tt>rows<0 || columns<0 || (double)columns*rows > Integer.MAX_VALUE</tt>
   *             or flip's are illegal.
   */
  def this(rows: Int, columns: Int, elements: Array[T], rowZero: Int, columnZero: Int, rowStride: Int, columnStride: Int, isView: Boolean) {
    this(rows, columns)
    setUp(rows, columns, rowZero, columnZero, rowStride, columnStride)
    this.elementsVar = elements
    this.isNoView = !isView
  }

  override def assignConstant(value: T) = {
    if (this.isNoView) {
      for(i <- 0 until elementsVar.length) elementsVar(i) = value
    }
    else {
      for (r <- 0 until rowsVar) {
        for (c <- 0 until columnsVar) {
          this.elementsVar(toRawIndex(r, c)) = value
        }
      }
    }
    this
  }

  override def assign(values: Array[T]): DenseMatrix2D[T] = {
    if (values.length < size)
      throw new IllegalArgumentException("Must have same length: length=" + values.length + " rows()*columns()=" + rows * columns)
    if (this.isNoView) {
      System.arraycopy(values, 0, this.elementsVar, 0, values.length)
    }
    else {
      var idx = 0
      for (r <- 0 until rowsVar; c <- 0 until columnsVar) {
        elementsVar(toRawIndex(r, c)) = values(idx)
        idx += 1
      }
    }
    this
  }

  override def assign(values: Array[Array[T]]) = {
    if (values.length != rows)
      throw new IllegalArgumentException("Must have same number of rows: rows=" + values.length + "rows()=" + rows)
    for (r <- 0 until rowsVar) {
      val currentRow = values(r)
      if (currentRow.length != columnsVar)
        throw new IllegalArgumentException("Must have same number of columns in every row: columns=" + currentRow.length + "columns()=" + columns)
      for (c <- 0 until columnsVar) {
        elementsVar(toRawIndex(r, c)) = currentRow(c)
      }
    }
    this
  }

  override def assign(source: Matrix2D[T]): DenseMatrix2D[T] = {
    if (source == this) return this
    checkShape(source)
    if (! source.isInstanceOf[DenseMatrix2D[T]]) {
      super.assign(source)
      return this
    }

    var other = source.asInstanceOf[DenseMatrix2D[T]]
    if (this.isNoView && other.isNoView && isRowMajor == other.isRowMajor) {
      System.arraycopy(other.elementsVar, 0, this.elementsVar, 0, this.elementsVar.length)
      return this
    }

    if (haveSharedCells(other)) {
      val c = other.copy()
      if (! c.isInstanceOf[DenseMatrix2D[T]]) {
        super.assign(other)
        return this
      }
      other = c.asInstanceOf[DenseMatrix2D[T]]
    }

    val elementsOther = other.elementsVar
    for (r <- 0 until rowsVar) {
      for (c <- 0 until columnsVar) {
        elementsVar(toRawIndex(r, c)) = elementsOther(other.toRawIndex(r, c))
      }
    }
    this
  }

  override def numNonZero: Long = {
    var cardinality = 0
    for (r <- 0 until rowsVar) {
      for (c <- 0 until columnsVar) {
        if (elementsVar(toRawIndex(r, c)) != 0) cardinality += 1
      }
    }
    cardinality
  }

  def getQuick(row: Int, column: Int): T = {
    elementsVar(toRawIndex(row, column))
  }

  def like2D(rows: Int, columns: Int): StrideMatrix2D[T] = new DenseMatrix2D[T](rows, columns)

  def like1D(size: Int): StrideMatrix1D[T] = new DenseMatrix1D[T](size)

  def setQuick(row: Int, column: Int, value: T) {
    elementsVar(toRawIndex(row, column)) = value
  }

  override def toArray: Array[Array[T]] = {
    val values = Array.ofDim[T](rows, columns)
    toArray(values)
  }

  override def vectorize() = {
    val vector = Array.ofDim[T](rowsVar*columnsVar)
    var idx = 0
    for (c <- 0 until columnsVar) {
      for (r <- 0 until rowsVar) {
        vector(idx) = elementsVar(toRawIndex(r, c))
        idx += 1
      }
    }
    new DenseMatrix1D[T](rowsVar*columnsVar, vector, 0, 1, isView=false)
  }

  /**
   * Constructs and returns a 2-dimensional array containing the cell values.
   * The returned array <tt>values</tt> has the form
   * <tt>values[row][column]</tt> and has the same number of rows and columns
   * as the receiver.
   * <p>
   * The values are copied. So subsequent changes in <tt>values</tt> are not
   * reflected in the matrix, and vice-versa.
   *
   * @return an array filled with the values of the cells.
   */
  override def toArray(values: Array[Array[T]]): Array[Array[T]] = {
    if (values.length != rowsVar)
      throw new IllegalArgumentException("Must have same length rows: length=" + values.length + " rows()=" + rowsVar)
    for (r <- 0 until rowsVar) {
      val currentRow = values(r)
      if (currentRow.length != columnsVar)
        throw new IllegalArgumentException("Must have same length columns: length=" + currentRow.length + " columns()=" + columnsVar)
      for (c <- 0 until columnsVar) {
        currentRow(c) = elementsVar(toRawIndex(r, c))
      }
    }
    values
  }

  /**
   * Constructs and returns a new <i>slice view</i> representing the rows of
   * the given column. The returned view is backed by this matrix, so changes
   * in the returned view are reflected in this matrix, and vice-versa. To
   * obtain a slice view on subranges, construct a sub-ranging view (
   * <tt>viewPart(...)</tt>), then apply this method to the sub-range view.
   * <p>
   * <b>Example:</b>
   * <table border="0">
   * <tr nowrap>
   * <td valign="top">2 x 3 matrix: <br>
   * 1, 2, 3<br>
   * 4, 5, 6</td>
   * <td>viewColumn(0) ==></td>
   * <td valign="top">Matrix1D of size 2:<br>
   * 1, 4</td>
   * </tr>
   * </table>
   *
   * @param column
   *            the column to fix.
   * @return a new slice view.
   * @throws IndexOutOfBoundsException
   *             if <tt>column < 0 || column >= columns()</tt>.
   * @see #viewRow(int)
   */
  def viewColumn(column: Int): StrideMatrix1D[T] = {
    checkColumn(column)
    val viewSize = this.rowsVar
    val viewZero = toRawIndex(0, column)
    val viewStride = this.rowStrideVar
    new DenseMatrix1D[T](viewSize, elementsVar, viewZero, viewStride, isView=true)
  }

  /**
   * Constructs and returns a new <i>slice view</i> representing the columns
   * of the given row. The returned view is backed by this matrix, so changes
   * in the returned view are reflected in this matrix, and vice-versa. To
   * obtain a slice view on subranges, construct a sub-ranging view (
   * <tt>viewPart(...)</tt>), then apply this method to the sub-range view.
   * <p>
   * <b>Example:</b>
   * <table border="0">
   * <tr nowrap>
   * <td valign="top">2 x 3 matrix: <br>
   * 1, 2, 3<br>
   * 4, 5, 6</td>
   * <td>viewRow(0) ==></td>
   * <td valign="top">Matrix1D of size 3:<br>
   * 1, 2, 3</td>
   * </tr>
   * </table>
   *
   * @param row
   *            the row to fix.
   * @return a new slice view.
   * @throws IndexOutOfBoundsException
   *             if <tt>row < 0 || row >= rows()</tt>.
   * @see #viewColumn(int)
   */
  def viewRow(row: Int): StrideMatrix1D[T] = {
    checkRow(row)
    val viewSize = this.columnsVar
    val viewZero = toRawIndex(row, 0)
    val viewStride = this.columnStrideVar
    new DenseMatrix1D[T](viewSize, elementsVar, viewZero, viewStride, isView=true)
  }

  /**
   * Constructs and returns a new <i>selection view</i> that is a matrix
   * holding all <b>rows</b> matching the given condition. Applies the
   * condition to each row and takes only those row where
   * <tt>condition.apply(viewRow(i))</tt> yields <tt>true</tt>.
   * <p>
   * <b>Example:</b> <br>
   *
   * <pre>
   * // extract and view all rows which have a value &lt; threshold in the first column (representing &quot;age&quot;)
   * final double threshold = 16;
   * matrix.viewRowSelection(
   * new DoubleMatrix1DProcedure() {
   * public final boolean apply(DoubleMatrix1D m) { return m.get(0) &lt; threshold; }
   * }
   * );
   *
   * // extract and view all columns with RMS &lt; threshold
   * // The RMS (Root-Mean-Square) is a measure of the average &quot;size&quot; of the elements of a data sequence.
   * matrix = 0 1 2 3
   * final double threshold = 0.5;
   * matrix.viewRowSelection(
   * new DoubleMatrix1DProcedure() {
   * public final boolean apply(DoubleMatrix1D m) { return Math.sqrt(m.aggregate(F.plus,F.square) / m.size()) &lt; threshold; }
   * }
   * );
   *
   * </pre>
   *
   * For further examples, see the <a
   * href="package-summary.html#FunctionObjects">package doc</a>. The returned
   * view is backed by this matrix, so changes in the returned view are
   * reflected in this matrix, and vice-versa.
   *
   * @param condition
   * The condition to be matched.
   * @return the new view.
   */
  override def viewRowSelection(condition: Matrix1DProcedure[T]): Matrix2D[T] = {
    val matches = new IntArrayList()
    for (i <- 0 until rowsVar) if (condition.apply(viewRow(i))) matches.add(i)
    viewSelection(matches.toIntArray, null)
  }

  /**
   * Constructs and returns a new <i>selection view</i> that is a matrix
   * holding all <b>columns</b> matching the given condition. Applies the
   * condition to each column and takes only those where
   * <tt>condition.apply(viewColumn(i))</tt> yields <tt>true</tt>.
   * <p>
   * <b>Example:</b> <br>
   *
   * <pre>
   * // extract and view all columns which have a value &lt; threshold in the first row (representing &quot;age&quot;)
   * final double threshold = 16;
   * matrix.viewColumnSelection(
   * new DoubleMatrix1DProcedure() {
   * public final boolean apply(DoubleMatrix1D m) { return m.get(0) &lt; threshold; }
   * }
   * );
   *
   * // extract and view all columns with RMS &lt; threshold
   * // The RMS (Root-Mean-Square) is a measure of the average &quot;size&quot; of the elements of a data sequence.
   * matrix = 0 1 2 3
   * final double threshold = 0.5;
   * matrix.viewColumnSelection(
   * new DoubleMatrix1DProcedure() {
   * public final boolean apply(DoubleMatrix1D m) { return Math.sqrt(m.aggregate(F.plus,F.square) / m.size()) &lt; threshold; }
   * }
   * );
   *
   * </pre>
   *
   * For further examples, see the <a
   * href="package-summary.html#FunctionObjects">package doc</a>. The returned
   * view is backed by this matrix, so changes in the returned view are
   * reflected in this matrix, and vice-versa.
   *
   * @param condition
   * The condition to be matched.
   * @return the new view.
   */
  override def viewColumnSelection(condition: Matrix1DProcedure[T]): Matrix2D[T] = {
      val matches = new IntArrayList()
      for (i <- 0 until columnsVar) if (condition.apply(viewColumn(i))) matches.add(i)
      viewSelection(null, matches.toIntArray)
    }
}
