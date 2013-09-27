package cern.colt.matrix.tdouble.impl

import cern.colt.matrix.tdouble.{Matrix1DProcedure, StrideMatrix1D, StrideMatrix2D}
import cern.colt.matrix._
import cern.colt.function.Procedure3
import it.unimi.dsi.fastutil.ints.IntArrayList

/**
 * Dense 2-d matrix holding <tt>double</tt> elements. First see the <a
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
@SerialVersionUID(102L)
class DenseDoubleMatrix2D[T](rows: Int, columns: Int) extends StrideMatrix2D[T] {

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

  override def assign(value: T): StrideMatrix2D[T] = {
    // TODO: If isNoView rowStride == columnStride == 1 && columnZeroVar == 0 then just use the array directly
    var idx = index(0, 0).toInt
    for (r <- 0 until rows by rowStrideVar) {
      for (i <- idx until columnsVar by columnStrideVar) {
        this.elementsVar(i) = value
      }
    }
    this
  }

  def assign(values: Array[T]): StrideMatrix2D[T] = {
    if (values.length < size)
      throw new IllegalArgumentException("Must have same length: length=" + values.length + " rows()*columns()=" + rows() * columns())
    if (this.isNoView) {
      System.arraycopy(values, 0, this.elementsVar, 0, values.length)
    } else {
      var idxOther = 0
      var idx = index(0, 0).toInt
      for (r <- 0 until rowsVar by rowStrideVar) {
        for (i <- idx until columnsVar by columnStrideVar) {
          elementsVar(i) = values(idxOther)
          idxOther += 1
        }
      }
    }
    this
  }

  override def assign(values: Array[Array[T]]): StrideMatrix2D[T] = {
    if (values.length != rows)
      throw new IllegalArgumentException("Must have same number of rows: rows=" + values.length + "rows()=" + rows())
    if (this.isNoView) {
      var i = 0
      for (r <- 0 until rowsVar) {
        val currentRow = values(r)
        if (currentRow.length != columnsVar)
          throw new IllegalArgumentException("Must have same number of columns in every row: columns=" + currentRow.length + "columns()=" + columns())
        System.arraycopy(currentRow, 0, this.elementsVar, i, columnsVar)
        i += columnsVar
      }
    } else {
      var idx = index(0, 0).toInt
      for (r <- 0 until rowsVar) {
        val currentRow = values(r)
        if (currentRow.length != columnsVar)
          throw new IllegalArgumentException("Must have same number of columns in every row: columns=" + currentRow.length + "columns()=" + columns())
        var c = 0
        for (i <- idx until columnsVar by columnStrideVar) {
          elementsVar(i) = currentRow(c)
          c += 1
        }
      }
    }
    this
  }

  def assign(source: StrideMatrix2D[T]): StrideMatrix2D[T] = {
    if (! source.isInstanceOf[DenseMatrix2D[T]]) {
      super.assign(source)
      return this
    }

    var other = source.asInstanceOf[DenseMatrix2D[T]]
    if (other == this) return this
    checkShape(other)
    if (this.isNoView && other.isNoView) {
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
    if (elementsVar == null || elementsOther == null) throw new InternalError()
    val columnsOther = other.columnsVar
    val columnStrideOther = other.columnStrideVar
    val rowStrideOther = other.rowStrideVar
    var idx = index(0, 0).toInt
    var idxOther = other.index(0, 0).toInt
    for (r <- 0 until rowsVar) {
      for (i <- idx until columnsVar by columnStrideVar; j <- idxOther until columnsOther by columnStrideOther) {
        elementsVar(i) = elementsOther(j)
      }
      idx += rowStride
      idxOther += rowStrideOther
    }
    this
  }

  override def numNonZero: Long = {
    var cardinality = 0
    var idx = index(0, 0).toInt
    for (r <- 0 until rowsVar by rowStrideVar) {
      for (i <- idx until columnsVar by columnStrideVar) {
        if (elementsVar(i) != 0) cardinality += 1
      }
    }
    cardinality
  }

  def getQuick(row: Int, column: Int): T = {
    elementsVar(rowZeroVar + row * rowStrideVar + columnZeroVar + column * columnStrideVar)
  }

  override def index(row: Int, column: Int): Long = {
    rowZeroVar + row * rowStride + columnZeroVar + column * columnStrideVar
  }

  def like2D(rows: Int, columns: Int): StrideMatrix2D[T] = new DenseMatrix2D[T](rows, columns)

  def like1D(size: Int): StrideMatrix1D[T] = new DenseMatrix1D[T](size)

  def setQuick(row: Int, column: Int, value: T) {
    elementsVar(rowZeroVar + row * rowStrideVar + columnZeroVar + column * columnStrideVar) = value
  }

  override def toArray: Array[Array[T]] = {
    val values = Array.ofDim[T](rows, columns)
    toArray(values)
  }

  def vectorize(): StrideMatrix1D[T] = {
    val zero = index(0, 0).toInt
    val elementsOther = Array.ofDim[T](rowsVar*columnsVar)
    var idxOther = 0
    for (c <- 0 until columnsVar) {
      var idx = zero + c * columnStrideVar
      for (r <- 0 until rowsVar by rowStrideVar) {
        elementsOther(idxOther) = elementsVar(idx)
        idxOther += 1
      }
    }
    new DenseMatrix1D[T](rowsVar*columnsVar, elementsOther, 0, 1, isView=false)
  }

  override protected def like1D(size: Int, zero: Int, stride: Int): StrideMatrix1D = {
    new DenseMatrix1D[T](size, null, zero, stride, false)
  }

  protected def viewSelectionLike(rowOffsets: Array[Int], columnOffsets: Array[Int]): StrideMatrix2D[T] = {
    // TODO: Implement
    //new SelectedDenseDoubleMatrix2D(this.elementsVar, rowOffsets, columnOffsets, 0)
    null
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
  def toArray(values: Array[Array[T]]): Array[Array[T]] = {
    if (values.length != rowsVar)
      throw new IllegalArgumentException("Must have same length rows: length=" + values.length + " rows()=" + rowsVar)
    var idx = index(0, 0).toInt
    for (r <- 0 until rowsVar by rowStrideVar) {
      val currentRow = values(r)
      if (currentRow.length != columnsVar)
        throw new IllegalArgumentException("Must have same length columns: length=" + currentRow.length + " columns()=" + columnsVar)
      for (i <- idx until columnsVar by columnStrideVar; c <- 0 until currentRow.length) {
        currentRow(c) = elementsVar(i)
      }
    }
    values
  }

  /**
   * @return Return the ParallelStrategy object used by this matrix.
   *         The ParallelStrategy manages the division of matrix operations into
   *         rows/columns.
   */
  def getParallelStrategy: ParallelStrategy = null

  def setParallelStrategy(s: Any) {}

  /**
   * @return Returns true if this matrix uses a sparse representation for storing cell values
   */
  def isSparse: Boolean = false

  /**
   * @return Return the MatrixFactory which can produce more matrices like this one.
   */
  def getFactory: MatrixFactory = null

  protected def setFactory(f: MatrixFactory) {}

  /**
   * @return Return the algebra object with matrix operations for use with this
   *         matrix.
   */
  def getAlgebra: MatrixAlgebra = null

  def setAlgebra(m: MatrixAlgebra) {}

  /**
   * @return Returns true if this matrix stores cells in row-major order.  Returns false if this
   *         matrix stores cells in column-major order.
   */
  def isRowMajor: Boolean = true

  abstract class AbstractIterator2D extends IndexIterator2D[T] {
    var rowIdx = -1
    var colIdx = columnsVar+1
    var maxRowIdx = rowsVar
    var maxColumnIdx = columnsVar

    protected def checkIndex(): Boolean

    private def incrIndex() {
      if (colIdx < maxColumnIdx-1)
        colIdx += 1
      else if (rowIdx < maxRowIdx-1) {
        rowIdx += 1
        colIdx = 0
      }
    }

    def hasNext: Boolean = {
      while(rowIdx < maxRowIdx-1 || (rowIdx == maxRowIdx-1 && colIdx < maxColumnIdx-1)) {
        incrIndex()
        if (checkIndex())
          return true
      }
      false
    }

    def next(): T = getQuick(rowIdx, colIdx)

    def row: Int = rowIdx

    def column: Int = colIdx
  }

  /**
   * Returns an iterator that can traverse all non-zero values in the matrix.
   */
  def iteratorNonZeros: IndexIterator2D[T] = new AbstractIterator2D {
    protected def checkIndex(): Boolean = getQuick(rowIdx, colIdx) != 0
  }

  /**
   * Returns an iterator that can traverse all non-zero values in the matrix
   * which return true from the given condition.
   */
  def iteratorNonZeros(condition: Procedure3[Int, Int, T]): IndexIterator2D[T] = new AbstractIterator2D {
    protected def checkIndex(): Boolean = {
      val value = getQuick(rowIdx, colIdx)
      value != 0 && condition(rowIdx, colIdx, value)
    }
  }

  /**
   * Returns an iterator that can traverse all values in the matrix.
   */
  def iteratorAllCells: IndexIterator2D[T] = new AbstractIterator2D {
    protected def checkIndex(): Boolean = true
  }

  /**
   * Returns an iterator that can traverse all values in the matrix
   * which return true from the given condition.
   */
  def iterator(condition: Procedure3[Int, Int, T]): IndexIterator2D[T] = new AbstractIterator2D {
    protected def checkIndex(): Boolean = {
      val value = getQuick(rowIdx, colIdx)
      condition(rowIdx, colIdx, value)
    }
  }

  /**
   * Returns an iterator that can traverse all values in the given row of the matrix
   */
  def iteratorNonZerosForRow(row: Int): IndexIterator2D[T] = new AbstractIterator2D {
    rowIdx = row
    maxRowIdx = math.min(row+1, rowsVar)

    protected def checkIndex(): Boolean = {
      val value = getQuick(rowIdx, colIdx)
      value != 0
    }
  }

  /**
   * Returns an iterator that can traverse all values in the given row of the matrix
   */
  def iteratorNonZerosForColumn(column: Int): IndexIterator2D[T] = new AbstractIterator2D {
    colIdx = column
    maxColumnIdx = math.min(column+1, columnsVar)

    protected def checkIndex(): Boolean = {
      val value = getQuick(rowIdx, colIdx)
      value != 0
    }
  }

  protected def forEachNonZero(iter: IndexIterator2D[T], function: Function3[Int, Int, T, T]): Matrix2D[T] = {
    while( iter.hasNext ) {
      val value = iter.next()
      val newValue = function(iter.row, iter.column, value)
      if (newValue != value)
        setQuick(iter.row, iter.column, newValue)
    }
    this
  }


  /**
   * Assigns the result of a function to each <i>non-zero</i> cell;
   * <tt>x[row,col] = function(x[row,col])</tt>. Use this method for fast
   * special-purpose iteration. If you want to modify another matrix instead
   * of <tt>this</tt> (i.e. work in read-only mode), simply return the input
   * value unchanged.
   *
   * Parameters to function are as follows: <tt>first==row</tt>,
   * <tt>second==column</tt>, <tt>third==nonZeroValue</tt>.
   *
   * @param function
   * a function object taking as argument the current non-zero
   * cell's row, column and value.
   * @return <tt>this</tt> (for convenience only).
   */
  override def forEachNonZero(function: Function3[Int, Int, T, T]): Matrix2D[T] = {
    forEachNonZero(iteratorNonZeros, function)
  }

  /**
   * Assigns the result of a function to each <i>non-zero</i> cell in the given row;
   * <tt>x[row,col] = function(x[row,col])</tt>. Use this method for fast
   * special-purpose iteration. If you want to modify another matrix instead
   * of <tt>this</tt> (i.e. work in read-only mode), simply return the input
   * value unchanged.
   *
   * Parameters to function are as follows: <tt>first==row</tt>,
   * <tt>second==column</tt>, <tt>third==nonZeroValue</tt>.
   *
   * @param function
   * a function object taking as argument the current non-zero
   * cell's row, column and value.
   * @return <tt>this</tt> (for convenience only).
   */
  override def forEachNonZeroInRow(rowIdx: Int, function: Function3[Int, Int, T, T]): Matrix2D[T] = {
    forEachNonZero(iteratorNonZerosForRow(rowIdx), function)
  }

  /**
   * Assigns the result of a function to each <i>non-zero</i> cell in the given column;
   * <tt>x[row,col] = function(x[row,col])</tt>. Use this method for fast
   * special-purpose iteration. If you want to modify another matrix instead
   * of <tt>this</tt> (i.e. work in read-only mode), simply return the input
   * value unchanged.
   *
   * Parameters to function are as follows: <tt>first==row</tt>,
   * <tt>second==column</tt>, <tt>third==nonZeroValue</tt>.
   *
   * @param function
   * a function object taking as argument the current non-zero
   * cell's row, column and value.
   * @return <tt>this</tt> (for convenience only).
   */
  override def forEachNonZeroInColumn(colIdx: Int, function: Function3[Int, Int, T, T]): Matrix2D[T] = {
    forEachNonZero(iteratorNonZerosForColumn(colIdx), function)
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
    val viewZero = index(0, column).toInt
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
    val viewZero = index(row, 0).toInt
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
  def viewRowSelection(condition: Matrix1DProcedure[T]): Matrix2D[T] = {
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
  def viewColumnSelection(condition: Matrix1DProcedure[T]): Matrix2D[T] = {
      val matches = new IntArrayList()
      for (i <- 0 until columnsVar) if (condition.apply(viewColumn(i))) matches.add(i)
      viewSelection(null, matches.toIntArray)
    }
}
