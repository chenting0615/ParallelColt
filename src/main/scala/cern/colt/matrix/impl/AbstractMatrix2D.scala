package cern.colt.matrix.impl

import it.unimi.dsi.fastutil.ints.IntArrayList
import cern.colt.function.Procedure3
import cern.colt.matrix._

/**
 * Abstract base class for 2-d matrices holding objects or primitive data types
 * such as <code>int</code>, <code>double</code>, etc. First see the <a
 * href="package-summary.html">package summary</a> and javadoc <a
 * href="package-tree.html">tree view</a> to get the broad picture.
 * <p>
 * <b>Note that this implementation is not synchronized.</b>
 *
 * @author wolfgang.hoschek@cern.ch
 * @version 1.0, 09/24/99
 */
@specialized
@SerialVersionUID(1L)
abstract class AbstractMatrix2D[T: Manifest] extends Matrix2D[T] {

  /**
   the number of colums and rows this matrix (view) has
   */
  protected var columnsVar: Int = 0

  protected var rowsVar: Int = 0

  protected def setUp(rows: Int, columns: Int) {
    rowsVar = rows
    columnsVar = columns
  }

  /**
   * Returns the number of columns.
   */
  def columns: Int = columnsVar

  /**
   * Returns the number of rows.
   */
  def rows: Int = rowsVar

  /**
   * Returns the number of cells which is <tt>rows()*columns()</tt>.
   */
  override def size: Long = rowsVar * columnsVar

  /**
   * Checks whether the receiver contains the given box and throws an
   * exception, if necessary.
   *
   * @throws IndexOutOfBoundsException
   *             if
   *             <tt>column<0 || width<0 || column+width>columns() || row<0 || height<0 || row+height>rows()</tt>
   */
  def checkBox(row: Int, column: Int, height: Int, width: Int) {
    if (column < 0 || width < 0 || column + width > columnsVar ||
      row < 0 || height < 0 || row + height > rowsVar) {

      throw new IndexOutOfBoundsException(toShapeString + ", column:" + column + ", row:" + row + " ,width:" + width + ", height:" + height)
    }
  }

  /**
   * Sanity check for operations requiring a column index to be within bounds.
   *
   * @throws IndexOutOfBoundsException
   *             if <tt>column < 0 || column >= columns()</tt>.
   */
  def checkColumn(column: Int) {
    if (column < 0 || column >= columnsVar)
      throw new IndexOutOfBoundsException("Attempted to access " + toShapeString + " at column=" + column)
  }

  /**
   * Sanity check for row size to be within bounds.
   *
   * @throws IndexOutOfBoundsException
   *             if <tt>rowIdx != rows()</tt>.
   */
  def checkRowShape(rowIdx: Int) {
    if (rowIdx != rowsVar)
      throw new IllegalArgumentException("Incompatible dimensions: " + toShapeString + " and rows=" + rowIdx)
  }

  /**
   * Sanity check for column size to be within bounds.
   *
   * @throws IndexOutOfBoundsException
   *             if <tt>colIdx != columns()</tt>.
   */
  def checkColumnShape(colIdx: Int) {
    if (colIdx != columnsVar)
      throw new IllegalArgumentException("Incompatible dimensions: " + toShapeString + " and columns=" + colIdx)
  }

  /**
   * Sanity check for operations requiring a row index to be within bounds.
   *
   * @throws IndexOutOfBoundsException
   *             if <tt>row < 0 || row >= rows()</tt>.
   */
  def checkRow(row: Int) {
    if (row < 0 || row >= rowsVar)
      throw new IndexOutOfBoundsException("Attempted to access " + toShapeString + " at row=" + row)
  }

  /**
   * Checks whether indexes are legal and throws an exception, if necessary.
   *
   * @throws IndexOutOfBoundsException
   *             if <tt>! (0 <= indexes[i] < columns())</tt> for any
   *             i=0..indexes.length()-1.
   */
  protected def checkColumnIndexes(indexes: Array[Int]) {
    for(i <- 0 until indexes.length) {
      val index = indexes(i)
      if (index < 0 || index >= columnsVar) checkColumn(index)
    }
  }

  /**
   * Checks whether indexes are legal and throws an exception, if necessary.
   *
   * @throws IndexOutOfBoundsException
   *             if <tt>! (0 <= indexes[i] < rows())</tt> for any
   *             i=0..indexes.length()-1.
   */
  def checkRowIndexes(indexes: Array[Int]) {
    for(i <- 0 until indexes.length) {
      val index = indexes(i)
      if (index < 0 || index >= rowsVar) checkRow(index)
    }
  }

  /**
   * Sanity check for operations requiring two matrices with the same number
   * of columns and rows.
   *
   * @throws IllegalArgumentException
   *             if <tt>columns() != B.columns() || rows() != B.rows()</tt>.
   */
  def checkShape(B: Matrix2D[T]) {
    if (columnsVar != B.columns || rowsVar != B.rows)
      throw new IllegalArgumentException("Incompatible dimensions: " + toShapeString + " and " + B.toShapeString)
  }

  /**
   * Sanity check for operations requiring matrices with the same number of
   * columns and rows.
   *
   * @throws IllegalArgumentException
   *             if
   *             <tt>columns() != B.columns() || rows() != B.rows() || columns() != C.columns() || rows() != C.rows()</tt>
   *             .
   */
  def checkShape(B: Matrix2D[T], C: Matrix2D[T]) {
    if (columnsVar != B.columns || rowsVar != B.rows || columnsVar != C.columns || rowsVar != C.rows)
      throw new IllegalArgumentException("Incompatible dimensions: " + toShapeString + ", " + B.toShapeString + ", " + C.toShapeString)
  }

  /**
   * Sets all cells to the value specified by <tt>value</tt>.
   *
   * @param value
   *            the value to be filled into the cells.
   * @return <tt>this</tt> (for convenience only).
   */
  def assignConstant(value: T) = {
    for (r <- 0 until rowsVar; c <- 0 until columnsVar) {
      setQuick(r, c, value)
    }
    this
  }

  /**
   * Sets all cells to the state specified by <tt>values</tt>. <tt>values</tt>
   * is required to have the form <tt>values[row*column]</tt> and elements
   * have to be stored in a row-wise order.
   * <p>
   * The values are copied. So subsequent changes in <tt>values</tt> are not
   * reflected in the matrix, and vice-versa.
   *
   * @param values
   *            the values to be filled into the cells.
   * @return <tt>this</tt> (for convenience only).
   * @throws IllegalArgumentException
   *             if <tt>values.length != rows()*columns()</tt>.
   */
  def assign(values: Array[T]) = {
    if (values.length < size)
      throw new IllegalArgumentException("Must have same length: length=" + values.length + " rows()*columns()=" + rows * columns)
    var idx = 0
    for (r <- 0 until rowsVar; c <- 0 until columnsVar) {
      setQuick(r, c, values(idx))
      idx += 1
    }
    this
  }

  /**
   * Sets all cells to the state specified by <tt>values</tt>. <tt>values</tt>
   * is required to have the form <tt>values[row][column]</tt> and have
   * exactly the same number of rows and columns as the receiver.
   * <p>
   * The values are copied. So subsequent changes in <tt>values</tt> are not
   * reflected in the matrix, and vice-versa.
   *
   * @param values
   *            the values to be filled into the cells.
   * @return <tt>this</tt> (for convenience only).
   * @throws IllegalArgumentException
   *             if
   *             <tt>values.length != rows() || for any 0 &lt;= row &lt; rows(): values[row].length != columns()</tt>
   *             .
   */
  def assign(values: Array[Array[T]]) = {
    if (values.length < rowsVar)
      throw new IllegalArgumentException("Must have same number of rows: rows=" + values.length + "rows()=" + rows)
    for (r <- 0 until rowsVar) {
      val currentRow = values(r)
      if (currentRow.length < columnsVar)
        throw new IllegalArgumentException("Must have same number of columns in every row: columns=" + currentRow.length + "columns()=" + columns)
      for (c <- 0 until columnsVar) {
        setQuick(r, c, currentRow(c))
      }
    }
    this
  }

  /**
   * Replaces all cell values of the receiver with the values of another
   * matrix. Both matrices must have the same number of rows and columns. If
   * both matrices share the same cells (as is the case if they are views
   * derived from the same matrix) and intersect in an ambiguous way, then
   * replaces <i>as if</i> using an intermediate auxiliary deep copy of
   * <tt>other</tt>.
   *
   * @param other
   *            the source matrix to copy from (may be identical to the
   *            receiver).
   * @return <tt>this</tt> (for convenience only).
   * @throws IllegalArgumentException
   *             if
   *             <tt>columns() != other.columns() || rows() != other.rows()</tt>
   */
  def assign(other: Matrix2D[T]) = {
    if (other != this) {
      checkShape(other)
      val source = if (haveSharedCells(other)) other.copy() else other
      for (r <- 0 until rowsVar; c <- 0 until columnsVar) {
        setQuick(r, c, source.getQuick(r, c))
      }
    }
    this
  }

  /**
   * Returns the number of cells having non-zero values; ignores tolerance.
   *
   * @return cardinality
   */
  def numNonZero: Long = {
    var cardinality = 0
    for (r <- 0 until rowsVar; c <- 0 until columnsVar)
      if (getQuick(r, c) != 0) cardinality += 1
    cardinality
  }

  /**
   * Returns whether all cells are equal to the given value.
   *
   * @param value
   *            the value to test against.
   * @return <tt>true</tt> if all cells are equal to the given value,
   *         <tt>false</tt> otherwise.
   */
  override def everyCellEquals(value: T): Boolean = {
    for (r <- 0 until rowsVar; c <- 0 until columnsVar)
      if (getQuick(r, c) != value) return false
    true
  }

  /**
   * Compares this object against the specified object. The result is
   * <code>true</code> if and only if the argument is not <code>null</code>
   * and is at least a <code>Matrix2D</code> object that has the same
   * number of columns and rows as the receiver and has exactly the same
   * values at the same coordinates.
   *
   * @param obj
   *            the object to compare with.
   * @return <code>true</code> if the objects are the same; <code>false</code>
   *         otherwise.
   */
  override def equals(obj: Any): Boolean = {
    if (this == obj) return true
    if (obj == null) return false
    if (! obj.isInstanceOf[Matrix2D[T]]) return false
    val other = obj.asInstanceOf[Matrix2D[T]]
    if (other.rows != rowsVar || other.columns != columnsVar) return false
    for (r <- 0 until rowsVar; c <- 0 until columnsVar)
      if (getQuick(r, c) != other.getQuick(r, c)) return false

    true
  }

  /**
   * Constructs and returns a deep copy of the receiver.
   * <p>
   * <b>Note that the returned matrix is an independent deep copy.</b> The
   * returned matrix is not backed by this matrix, so changes in the returned
   * matrix are not reflected in this matrix, and vice-versa.
   *
   * @return a deep copy of the receiver.
   */
  def copy() = like2D(rowsVar, columnsVar).assign(this)

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
   *            a function object taking as argument the current non-zero
   *            cell's row, column and value.
   * @return <tt>this</tt> (for convenience only).
   */
  def forEachNonZero(function: Function3[Int, Int, T, T]): Matrix2D[T] = {
    for (r <- 0 until rows; c <- 0 until columns) {
      val value = getQuick(r, c)
      if (value != 0) {
        val a = function.apply(r, c, value)
        if (a != value) setQuick(r, c, a)
      }
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
   *            a function object taking as argument the current non-zero
   *            cell's row, column and value.
   * @return <tt>this</tt> (for convenience only).
   */
  def forEachNonZeroInRow(rowIdx: Int, function: Function3[Int, Int, T, T]): Matrix2D[T] = {
    for (c <- 0 until columns) {
      val value = getQuick(rowIdx, c)
      if (value != 0) {
        val a = function.apply(rowIdx, c, value)
        if (a != value) setQuick(rowIdx, c, a)
      }
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
   *            a function object taking as argument the current non-zero
   *            cell's row, column and value.
   * @return <tt>this</tt> (for convenience only).
   */
  def forEachNonZeroInColumn(colIdx: Int, function: Function3[Int, Int, T, T]): Matrix2D[T] = {
    for (r <- 0 until rows) {
      val value = getQuick(r, colIdx)
      if (value != 0) {
        val a = function.apply(r, colIdx, value)
        if (a != value) setQuick(r, colIdx, a)
      }
    }
    this
  }

  /**
   * Returns a string representation of the receiver's shape.
   */
  def toShapeString: String = AbstractFormatter.shape(this)

  /**
   * Constructs and returns a new <i>selection view</i> that is a matrix
   * holding all <b>rows</b> matching the given condition. Applies the
   * condition to each row and takes only those row where
   * <tt>condition.apply(viewRow(i))</tt> yields <tt>true</tt>. To match
   * columns, use a dice view.
   * <p>
   * <b>Example:</b> <br>
   *
   * <pre>
   * 	 // extract and view all rows which have a value &lt; threshold in the first column (representing &quot;age&quot;)
   * 	 final double threshold = 16;
   * 	 matrix.viewSelection(
   * 	    new DoubleMatrix1DProcedure() {
   * 	       public final boolean apply(DoubleMatrix1D m) { return m.get(0) &lt; threshold; }
   * 	    }
   * 	 );
   *
   * 	 // extract and view all rows with RMS &lt; threshold
   * 	 // The RMS (Root-Mean-Square) is a measure of the average &quot;size&quot; of the elements of a data sequence.
   * 	 matrix = 0 1 2 3
   * 	 final double threshold = 0.5;
   * 	 matrix.viewSelection(
   * 	    new DoubleMatrix1DProcedure() {
   * 	       public final boolean apply(DoubleMatrix1D m) { return Math.sqrt(m.aggregate(F.plus,F.square) / m.size()) &lt; threshold; }
   * 	    }
   * 	 );
   *
   * </pre>
   *
   * For further examples, see the <a
   * href="package-summary.html#FunctionObjects">package doc</a>. The returned
   * view is backed by this matrix, so changes in the returned view are
   * reflected in this matrix, and vice-versa.
   *
   * @param condition
   *            The condition to be matched.
   * @return the new view.
   */
  def viewSelection(condition: Matrix1DProcedure[T]): Matrix2D[T] = {
    val matches = new IntArrayList()
    for (i <- 0 until rows if condition.apply(viewRow(i))) matches.add(i)
    viewSelection(matches.toIntArray, null)
  }

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
  def viewRowSelection(condition: Matrix1DProcedure[T]) = {
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
  def viewColumnSelection(condition: Matrix1DProcedure[T]) = {
    val matches = new IntArrayList()
    for (i <- 0 until columnsVar) if (condition.apply(viewColumn(i))) matches.add(i)
    viewSelection(null, matches.toIntArray)
  }

  /**
   * Returns a vector obtained by stacking the columns of the matrix on top of
   * one another.
   *
   * @return a vector of columns of this matrix.
   */
  def vectorize(): Matrix1D[T] = {
    val view = like1D(size.toInt)
    forEachNonZero(new Function3[Int, Int, T, T]() {
      def apply(r: Int, c: Int, value: T) = {
        view.setQuick(r*columnsVar+c, value)
        value
      }
    })
    view
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
  def toArray: Array[Array[T]] = {
    val values = Array.ofDim[T](rows, columns)
    toArray(values)
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
  def toArray(values: Array[Array[T]]) = {
    checkRowShape(values.length)
    for (row <- 0 until rowsVar) {
      val currentRow = values(row)
      checkColumnShape(currentRow.length)
      for(column <- 0 until columnsVar)
        currentRow(column) = getQuick(row, column)
    }
    values
  }

  /**
   * @return Return the ParallelStrategy object used by this matrix.
   *         The ParallelStrategy manages the division of matrix operations into
   *         rows/columns.
   */
  def getParallelStrategy: ParallelStrategy = null

  def setParallelStrategy(s: ParallelStrategy) {}

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

}
