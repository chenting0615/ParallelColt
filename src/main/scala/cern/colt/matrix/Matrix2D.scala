package cern.colt.matrix

import cern.colt.function.{Matrix1DProcedure, Procedure3}

/**
 * Trait for 2-d matrices holding objects or primitive data types
 * such as <code>double</code>, <code>it</code>, etc. First see the <a
 * href="package-summary.html">package summary</a> and javadoc <a
 * href="package-tree.html">tree view</a> to get the broad picture.
 *
 * @author wolfgang.hoschek@cern.ch
 * @version 1.0, 09/24/99
 */
@SerialVersionUID(1L)
trait Matrix2D[@specialized T] extends Matrix[T] {

  def numeric: Numeric[T]

  /**
   * @return Returns true if this matrix stores cells in row-major order.  Returns false if this
   *         matrix stores cells in column-major order.
   */
  def isRowMajor: Boolean

  /**
   * Can the cell at the given row, column be set to a non-zero value.
   * @param row The cell row
   * @param column The cell column
   * @return Returns true if the cell can be set to any value.  Returns false if the cell value will always be zero,
   *         as in diagonal matrices.
   */
  def canSetCellAt(row: Int, column: Int): Boolean

  /**
   * TODO: Should we remove the ()?
   *
   * Returns the number of columns.
   */
  def columns: Int

  /**
   * TODO: Should we remove the ()?
   *
   * Returns the number of rows.
   */
  def rows: Int

  /**
   * Returns the number of cells in the matrix, i.e. <tt>rows*columns</tt>.
   */
  def size: Long = rows * columns

  /**
   * Returns the number of cells having non-zero values; ignores any tolerance.
   *
   * @return Number of non-zero cells
   */
  def numNonZero: Long

  /**
   * Returns the matrix cell value at coordinate <tt>[row,column]</tt>.
   *
   * @param row
   *            the index of the row-coordinate.
   * @param column
   *            the index of the column-coordinate.
   * @return the value of the specified cell.
   * @throws IndexOutOfBoundsException
   *             if
   *             <tt>column&lt;0 || column&gt;=columns || row&lt;0 || row&gt;=rows</tt>
   */
  def get(row: Int, column: Int): T = {
    checkColumn(column)
    checkRow(row)
    getQuick(row, column)
  }

  /**
   * Returns the matrix cell value at coordinate <tt>[row,column]</tt>.
   *
   * <p>
   * Provided with invalid parameters this method may return invalid objects
   * without throwing any exception. <b>You should only use this method when
   * you are absolutely sure that the coordinate is within bounds.</b>
   * Precondition (unchecked):
   * <tt>0 &lt;= column &lt; columns && 0 &lt;= row &lt; rows</tt>.
   *
   * @param row
   *            the index of the row-coordinate.
   * @param column
   *            the index of the column-coordinate.
   * @return the value at the specified coordinate.
   */
  def getQuick(row: Int, column: Int): T

  /**
   * Sets the matrix cell at coordinate <tt>[row,column]</tt> to the specified
   * value.
   *
   * @param row
   *            the index of the row-coordinate.
   * @param column
   *            the index of the column-coordinate.
   * @param value
   *            the value to be filled into the specified cell.
   * @throws IndexOutOfBoundsException
   *             if
   *             <tt>column&lt;0 || column&gt;=columns || row&lt;0 || row&gt;=rows</tt>
   */
  def set(row: Int, column: Int, value: T) {
    checkColumn(column)
    checkRow(row)
    setQuick(row, column, value)
  }

  /**
   * Sets the matrix cell at coordinate <tt>[row,column]</tt> to the specified
   * value.
   *
   * <p>
   * Provided with invalid parameters this method may access illegal indexes
   * without throwing any exception. <b>You should only use this method when
   * you are absolutely sure that the coordinate is within bounds.</b>
   * Precondition (unchecked):
   * <tt>0 &lt;= column &lt; columns && 0 &lt;= row &lt; rows</tt>.
   *
   * @param row
   *            the index of the row-coordinate.
   * @param column
   *            the index of the column-coordinate.
   * @param value
   *            the value to be filled into the specified cell.
   */
  def setQuick(row: Int, column: Int, value: T): Unit

  /**
   * Sets all cells to the state specified by <tt>value</tt>.
   *
   * @param value
   *            the value to be filled into the cells.
   * @return <tt>this</tt> (for convenience only).
   */
  def assignConstant(value: T): Matrix2D[T]

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
   *             if <tt>values.length != rows*columns</tt>.
   */
  def assign(values: Array[T]): Matrix2D[T]

  /**
   * Replaces all cell values of the receiver with the values of an array.
   * The array and the matrix must have the same dimensions.
   *
   * @param other
   *            the source array to copy from.
   * @return <tt>this</tt> (for convenience only).
   * @throws IllegalArgumentException
   *             if
   *             <tt>columns != other(0).length || rows != other.length</tt>
   */
  def assign(other: Array[Array[T]]): Matrix2D[T]

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
   *             <tt>columns != other.columns || rows != other.rows</tt>
   */
  def assign(other: Matrix2D[T]): Matrix2D[T]

  /**
   * Returns an iterator that can traverse all non-zero values in the matrix.
   */
  def iteratorNonZeros(byRows: Boolean): IndexIterator2D[T]

  /**
   * Returns an iterator that can traverse all non-zero values in the given row of the matrix.
   */
  def iteratorNonZerosInRow(row: Int): IndexIterator2D[T]

  /**
   * Returns an iterator that can traverse all non-zero values in the given column of the matrix.
   */
  def iteratorNonZerosInColumn(column: Int): IndexIterator2D[T]

  /**
   * Returns an iterator that can traverse all non-zero values in the matrix
   * which return true from the given condition.
   */
  def iteratorNonZeros(byRows: Boolean, condition: Procedure3[Int, Int, T]): IndexIterator2D[T]

  /**
   * Returns an iterator that can traverse all values in the matrix.
   */
  def iteratorAllCells: IndexIterator2D[T]

  /**
   * Returns an iterator that can traverse all values in the matrix
   * which return true from the given condition.
   */
  def iterator(condition: Procedure3[Int, Int, T]): IndexIterator2D[T]

  /**
   * Assigns the result of a function to each <i>non-zero</i> cell;
   * <tt>x[row,col] = function(x[row,col])</tt>. Use this method for fast
   * special-purpose iteration. If you want to modify another matrix instead
   * of <tt>this</tt> (i.e. work in read-only mode), simply return the input
   * value unchanged.
   *
   * The matrix is traversed in row-major order.
   *
   * Parameters to function are as follows: <tt>first==row</tt>,
   * <tt>second==column</tt>, <tt>third==nonZeroValue</tt>.
   *
   * @param function
   *            a function object taking as argument the current non-zero
   *            cell's row, column and value.
   * @return <tt>this</tt> (for convenience only).
   */
  def forEachNonZeroRowMajor(function: Function3[Int, Int, T, T]): Matrix2D[T]

  /**
   * Assigns the result of a function to each <i>non-zero</i> cell;
   * <tt>x[row,col] = function(x[row,col])</tt>. Use this method for fast
   * special-purpose iteration. If you want to modify another matrix instead
   * of <tt>this</tt> (i.e. work in read-only mode), simply return the input
   * value unchanged.
   *
   * The matrix is traversed in column-major order.
   *
   * Parameters to function are as follows: <tt>first==row</tt>,
   * <tt>second==column</tt>, <tt>third==nonZeroValue</tt>.
   *
   * @param function
   *            a function object taking as argument the current non-zero
   *            cell's row, column and value.
   * @return <tt>this</tt> (for convenience only).
   */
  def forEachNonZeroColumnMajor(function: Function3[Int, Int, T, T]): Matrix2D[T]

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
   *            a function object taking as argument the current non-zero
   *            cell's row, column and value.
   * @return <tt>this</tt> (for convenience only).
   */
  def forEachNonZeroInRow(rowIdx: Int, function: Function3[Int, Int, T, T]): Matrix2D[T]

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
   *            a function object taking as argument the current non-zero
   *            cell's row, column and value.
   * @return <tt>this</tt> (for convenience only).
   */
  def forEachNonZeroInColumn(colIdx: Int, function: Function3[Int, Int, T, T]): Matrix2D[T]

  /**
   * Constructs and returns a new <i>dice (transposition) view</i>; Swaps
   * axes; example: 3 x 4 matrix --> 4 x 3 matrix. The view has both
   * dimensions exchanged; what used to be columns become rows, what used to
   * be rows become columns. In other words:
   * <tt>view.get(row,column)==this.get(column,row)</tt>. This is a zero-copy
   * transposition, taking O(1), i.e. constant time. The returned view is
   * backed by this matrix, so changes in the returned view are reflected in
   * this matrix, and vice-versa. Use idioms like
   * <tt>result = viewTranspose(A).copy()</tt> to generate an independent
   * transposed matrix.
   * <p>
   * <b>Example:</b>
   * <table border="0">
   * <tr nowrap>
   * <td valign="top">2 x 3 matrix: <br>
   * 1, 2, 3<br>
   * 4, 5, 6</td>
   * <td>transpose ==></td>
   * <td valign="top">3 x 2 matrix:<br>
   * 1, 4 <br>
   * 2, 5 <br>
   * 3, 6</td>
   * <td>transpose ==></td>
   * <td valign="top">2 x 3 matrix: <br>
   * 1, 2, 3<br>
   * 4, 5, 6</td>
   * </tr>
   * </table>
   *
   * @return a new dice view.
   */
  def viewTranspose(): Matrix2D[T]

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
   *             if <tt>column < 0 || column >= columns</tt>.
   * @see #viewRow(int)
   */
  def viewColumn(column: Int): Matrix1D[T]

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
   *             if <tt>row < 0 || row >= rows</tt>.
   * @see #viewColumn(int)
   */
  def viewRow(row: Int): Matrix1D[T]

  /**
   * Constructs and returns a new <i>sub-range view</i> that is a
   * <tt>height x width</tt> sub matrix starting at <tt>[row,column]</tt>.
   *
   * Operations on the returned view can only be applied to the restricted
   * range. Any attempt to access coordinates not contained in the view will
   * throw an <tt>IndexOutOfBoundsException</tt>.
   * <p>
   * <b>Note that the view is really just a range restriction:</b> The
   * returned matrix is backed by this matrix, so changes in the returned
   * matrix are reflected in this matrix, and vice-versa.
   * <p>
   * The view contains the cells from <tt>[row,column]</tt> to
   * <tt>[row+height-1,column+width-1]</tt>, all inclusive. and has
   * <tt>view.rows == height; view.columns == width;</tt>. A view's legal
   * coordinates are again zero based, as usual. In other words, legal
   * coordinates of the view range from <tt>[0,0]</tt> to
   * <tt>[view.rows-1==height-1,view.columns-1==width-1]</tt>. As usual,
   * any attempt to access a cell at a coordinate
   * <tt>column&lt;0 || column&gt;=view.columns || row&lt;0 || row&gt;=view.rows</tt>
   * will throw an <tt>IndexOutOfBoundsException</tt>.
   *
   * @param row
   *            The index of the row-coordinate.
   * @param column
   *            The index of the column-coordinate.
   * @param height
   *            The height of the box.
   * @param width
   *            The width of the box.
   * @throws IndexOutOfBoundsException
   *             if
   *             <tt>column<0 || width<0 || column+width>columns || row<0 || height<0 || row+height>rows</tt>
   * @return the new view.
   *
   */
  def viewPart(row: Int, column: Int, height: Int, width: Int): Matrix2D[T]

  /**
   * Constructs and returns a new <i>flip view</i> along the column axis. What
   * used to be column <tt>0</tt> is now column <tt>columns-1</tt>, ...,
   * what used to be column <tt>columns-1</tt> is now column <tt>0</tt>. The
   * returned view is backed by this matrix, so changes in the returned view
   * are reflected in this matrix, and vice-versa.
   * <p>
   * <b>Example:</b>
   * <table border="0">
   * <tr nowrap>
   * <td valign="top">2 x 3 matrix: <br>
   * 1, 2, 3<br>
   * 4, 5, 6</td>
   * <td>columnFlip ==></td>
   * <td valign="top">2 x 3 matrix:<br>
   * 3, 2, 1 <br>
   * 6, 5, 4</td>
   * <td>columnFlip ==></td>
   * <td valign="top">2 x 3 matrix: <br>
   * 1, 2, 3<br>
   * 4, 5, 6</td>
   * </tr>
   * </table>
   *
   * @return a new flip view.
   * @see #viewRowFlip()
   */
  def viewColumnFlip(): Matrix2D[T]

  /**
   * Constructs and returns a new <i>flip view</i> along the row axis. What
   * used to be row <tt>0</tt> is now row <tt>rows-1</tt>, ..., what used to
   * be row <tt>rows-1</tt> is now row <tt>0</tt>. The returned view is
   * backed by this matrix, so changes in the returned view are reflected in
   * this matrix, and vice-versa.
   * <p>
   * <b>Example:</b>
   * <table border="0">
   * <tr nowrap>
   * <td valign="top">2 x 3 matrix: <br>
   * 1, 2, 3<br>
   * 4, 5, 6</td>
   * <td>rowFlip ==></td>
   * <td valign="top">2 x 3 matrix:<br>
   * 4, 5, 6 <br>
   * 1, 2, 3</td>
   * <td>rowFlip ==></td>
   * <td valign="top">2 x 3 matrix: <br>
   * 1, 2, 3<br>
   * 4, 5, 6</td>
   * </tr>
   * </table>
   *
   * @return a new flip view.
   * @see #viewColumnFlip()
   */
  def viewRowFlip(): Matrix2D[T]

  /**
   * TODO: Do we need/want this?
   *
   * Constructs and returns a new <i>stride view</i> which is a sub matrix
   * consisting of every i-th cell. More specifically, the view has
   * <tt>this.rows/rowStride</tt> rows and
   * <tt>this.columns/columnStride</tt> columns holding cells
   * <tt>this.get(i*rowStride,j*columnStride)</tt> for all
   * <tt>i = 0..rows/rowStride - 1, j = 0..columns/columnStride - 1</tt>.
   * The returned view is backed by this matrix, so changes in the returned
   * view are reflected in this matrix, and vice-versa.
   *
   * @param rowStride
   *            the row step factor.
   * @param columnStride
   *            the column step factor.
   * @return a new view.
   * @throws IndexOutOfBoundsException
   *             if <tt>rowStride<=0 || columnStride<=0</tt>.
   */
  def viewStrides(rowStride: Int, columnStride: Int): Matrix2D[T]

  /**
   * Constructs and returns a new <i>selection view</i> that is a matrix
   * holding all <b>rows</b> matching the given condition. Applies the
   * condition to each row and takes only those row where
   * <tt>condition.apply(viewRow(i))</tt> yields <tt>true</tt>.
   * <p>
   * <b>Example:</b> <br>
   *
   * <pre>
   * 	 // extract and view all rows which have a value &lt; threshold in the first column (representing &quot;age&quot;)
   * 	 final double threshold = 16;
   * 	 matrix.viewRowSelection(
   * 	    new DoubleMatrix1DProcedure() {
   * 	       public final boolean apply(DoubleMatrix1D m) { return m.get(0) &lt; threshold; }
   * 	    }
   * 	 );
   *
   * 	 // extract and view all columns with RMS &lt; threshold
   * 	 // The RMS (Root-Mean-Square) is a measure of the average &quot;size&quot; of the elements of a data sequence.
   * 	 matrix = 0 1 2 3
   * 	 final double threshold = 0.5;
   * 	 matrix.viewRowSelection(
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
  def viewRowSelection(condition: Matrix1DProcedure[T]): Matrix2D[T]

  /**
   * Constructs and returns a new <i>selection view</i> that is a matrix
   * holding all <b>columns</b> matching the given condition. Applies the
   * condition to each column and takes only those where
   * <tt>condition.apply(viewColumn(i))</tt> yields <tt>true</tt>.
   * <p>
   * <b>Example:</b> <br>
   *
   * <pre>
   * 	 // extract and view all columns which have a value &lt; threshold in the first row (representing &quot;age&quot;)
   * 	 final double threshold = 16;
   * 	 matrix.viewColumnSelection(
   * 	    new DoubleMatrix1DProcedure() {
   * 	       public final boolean apply(DoubleMatrix1D m) { return m.get(0) &lt; threshold; }
   * 	    }
   * 	 );
   *
   * 	 // extract and view all columns with RMS &lt; threshold
   * 	 // The RMS (Root-Mean-Square) is a measure of the average &quot;size&quot; of the elements of a data sequence.
   * 	 matrix = 0 1 2 3
   * 	 final double threshold = 0.5;
   * 	 matrix.viewColumnSelection(
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
  def viewColumnSelection(condition: Matrix1DProcedure[T]): Matrix2D[T]

  /**
   * Constructs and returns a new <i>selection view</i> that is a matrix
   * holding the intersection of the indicated rows and columns. It holds
   * <tt>view.rows == rowIndexes.length, view.columns == columnIndexes.length</tt>
   * and <tt>view.get(i,j) == this.get(rowIndexes[i],columnIndexes[j])</tt>.
   * Indexes can occur multiple times and can be in arbitrary order.
   * <p>
   * <b>Example:</b>
   *
   * <pre>
   * 	 this = 2 x 3 matrix:
   * 	 1, 2, 3
   * 	 4, 5, 6
   * 	 rowIndexes     = (0,1)
   * 	 columnIndexes  = (1,0,1,0)
   * 	 --&gt;
   * 	 view = 2 x 4 matrix:
   * 	 2, 1, 2, 1
   * 	 5, 4, 5, 4
   *
   * </pre>
   *
   * Note that modifying the index arguments after this call has returned has
   * no effect on the view. The returned view is backed by this matrix, so
   * changes in the returned view are reflected in this matrix, and
   * vice-versa.
   * <p>
   * To indicate "all" rows or "all columns", simply set the respective
   * parameter to null
   *
   * @param rowIndexes
   *            The rows of the cells that shall be visible in the new view.
   *            To indicate that <i>all</i> rows shall be visible, simply set
   *            this parameter to <tt>null</tt>.
   * @param columnIndexes
   *            The columns of the cells that shall be visible in the new
   *            view. To indicate that <i>all</i> columns shall be visible,
   *            simply set this parameter to <tt>null</tt>.
   * @return the new view.
   * @throws IndexOutOfBoundsException
   *             if <tt>!(0 <= rowIndexes[i] < rows)</tt> for any
   *             <tt>i=0..rowIndexes.length()-1</tt>.
   *             if <tt>!(0 <= colIndexes[i] < columns)</tt> for any
   *             <tt>i=0..colIndexes.length()-1</tt>.
   */
  def viewSelection(rowIndexes: Array[Int], columnIndexes: Array[Int]): Matrix2D[T]

  /**
   * Construct and returns a new empty matrix <i>of the same dynamic type</i>
   * as the receiver, having the same number of rows and columns. For example,
   * if the receiver is an instance of type <tt>DenseDoubleMatrix2D</tt> the
   * new matrix must also be of type <tt>DenseDoubleMatrix2D</tt>, if the
   * receiver is an instance of type <tt>SparseDoubleMatrix2D</tt> the new
   * matrix must also be of type <tt>SparseDoubleMatrix2D</tt>, etc. In
   * general, the new matrix should have internal parametrization as similar
   * as possible.
   *
   * @return a new empty matrix of the same dynamic type.
   */
  def like(): Matrix2D[T] = like2D(rows, columns)

  /**
   * Constructs and returns a deep copy of the receiver.
   * <p>
   * <b>Note that the returned matrix is an independent deep copy.</b> The
   * returned matrix is not backed by this matrix, so changes in the returned
   * matrix are not reflected in this matrix, and vice-versa.
   *
   * @return a deep copy of the receiver.
   */
  def copy(): Matrix2D[T]

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
  def toArray: Array[Array[T]]

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
  def toArray(values: Array[Array[T]]): Array[Array[T]]

  /**
   * Returns a vector obtained by stacking the columns of the matrix on top of
   * one another.
   *
   * @return a vector of columns of this matrix.
   */
  def vectorize(): Matrix1D[T]

  /**
   * Returns a string representation of the matrix's shape.
   */
  def toShapeString: String = rows + " x " + columns + " matrix"


  /**
   * Sanity check for operations requiring a column index to be within bounds.
   *
   * @throws IndexOutOfBoundsException
   *             if <tt>column < 0 || column >= columns</tt>.
   */
  def checkColumn(column: Int)

  /**
   * Sanity check for operations requiring a row index to be within bounds.
   *
   * @throws IndexOutOfBoundsException
   *             if <tt>row < 0 || row >= rows</tt>.
   */
  def checkRow(row: Int)

  /**
   * Sanity check for row size to be within bounds.
   *
   * @throws IllegalArgumentException
   *             if <tt>rowSize != rows</tt>.
   */
  def checkRowShape(rowSize: Int)

  /**
   * Sanity check for column size to be within bounds.
   *
   * @throws IllegalArgumentException
   *             if <tt>columnSize != columns</tt>.
   */
  def checkColumnShape(columnSize: Int)

  /**
   * Sanity check for operations requiring two matrices with the same number
   * of columns and rows.
   *
   * @throws IllegalArgumentException
   *             if <tt>columns != B.columns || rows != B.rows</tt>.
   */
  def checkShape(B: Matrix2D[T])

  /**
   * Sanity check for operations requiring matrices with the same number of
   * columns and rows.
   *
   * @throws IllegalArgumentException
   *             if
   *             <tt>columns != B.columns || rows != B.rows || columns != C.columns || rows != C.rows</tt>
   *             .
   */
  def checkShape(B: Matrix2D[T], C: Matrix2D[T])
}
