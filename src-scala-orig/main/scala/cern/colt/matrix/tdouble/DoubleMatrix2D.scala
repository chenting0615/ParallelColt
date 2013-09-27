package cern.colt.matrix.tdouble

import cern.colt.matrix.{Matrix2D, AbstractMatrix2D}
import it.unimi.dsi.fastutil.ints.IntArrayList

/**
 * Abstract base class for 2-d matrices holding <tt>double</tt> elements. First
 * see the <a href="package-summary.html">package summary</a> and javadoc <a
 * href="package-tree.html">tree view</a> to get the broad picture.
 * <p>
 * A matrix has a number of rows and columns, which are assigned upon instance
 * construction - The matrix's size is then <tt>rows()*columns()</tt>. Elements
 * are accessed via <tt>[row,column]</tt> coordinates. Legal coordinates range
 * from <tt>[0,0]</tt> to <tt>[rows()-1,columns()-1]</tt>. Any attempt to access
 * an element at a coordinate
 * <tt>column&lt;0 || column&gt;=columns() || row&lt;0 || row&gt;=rows()</tt>
 * will throw an <tt>IndexOutOfBoundsException</tt>.
 * <p>
 * <b>Note</b> that this implementation is not synchronized.
 *
 * @author wolfgang.hoschek@cern.ch
 * @version 1.0, 09/24/99
 *
 * @author Piotr Wendykier (piotr.wendykier@gmail.com)
 *
 */
@specialized
@SerialVersionUID(1L)
abstract class DoubleMatrix2D[T] protected () extends AbstractMatrix2D[T] {

  /**
   * Sets all cells to the value specified by <tt>value</tt>.
   *
   * @param value
   *            the value to be filled into the cells.
   * @return <tt>this</tt> (for convenience only).
   */
  def assign(value: T): StrideMatrix2D[T] = {
    for (r <- 0 until rows; c <- 0 until columns) {
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
  def assign(values: Array[T], zero: Int, stride: Int): StrideMatrix2D[T] = {
    var idx = zero
    for (r <- 0 until rows; c <- 0 until columns) {
      setQuick(r, c, values(idx))
      idx += stride
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
  def assign(values: Array[Array[T]]): Matrix2D[T] = {
    if (values.length < rows())
      throw new IllegalArgumentException("Must have same number of rows: rows=" + values.length + "rows()=" + rows())
    for (r <- 0 until rows) {
      val currentRow = values(r)
      if (currentRow.length < columns)
        throw new IllegalArgumentException("Must have same number of columns in every row: columns=" + currentRow.length + "columns()=" + columns())
      for (c <- 0 until columns) {
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
  def assign(other: Matrix2D[T]): Matrix2D[T] = {
    if (other == this) return this
    checkShape(other)
    var source = if (haveSharedCells(other)) other.copy() else other
    for (r <- 0 until rows; c <- 0 until columns) {
      setQuick(r, c, source.getQuick(r, c))
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
    for (r <- 0 until rows; c <- 0 until columns if getQuick(r, c) != 0) cardinality += 1
    cardinality
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
  def copy(): StrideMatrix2D[T] = like().assign(this).asInstanceOf[StrideMatrix2D[T]]

  /**
   * Returns whether all cells are equal to the given value.
   *
   * @param value
   *            the value to test against.
   * @return <tt>true</tt> if all cells are equal to the given value,
   *         <tt>false</tt> otherwise.
   */
  override def everyCellEquals(value: T): Boolean = {
    for (r <- 0 until rows(); c <- 0 until columns()) if (getQuick(r, c) != value) return false
    true
  }

  /**
   * Compares this object against the specified object. The result is
   * <code>true</code> if and only if the argument is not <code>null</code>
   * and is at least a <code>DoubleMatrix2D</code> object that has the same
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
    if (! obj.isInstanceOf[StrideMatrix2D]) return false
    val other = obj.asInstanceOf[StrideMatrix2D]
    if (other.rows() != rows() || other.columns() != columns()) return false
    for (r <- 0 until rows(); c <- 0 until columns()) if (getQuick(r, c) != other.getQuick(r, c)) return false
    true
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
   * Constructs and returns a new <i>flip view</i> along the column axis. What
   * used to be column <tt>0</tt> is now column <tt>columns()-1</tt>, ...,
   * what used to be column <tt>columns()-1</tt> is now column <tt>0</tt>. The
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
  def viewColumnFlip(): StrideMatrix2D[T] = {
    view().vColumnFlip().asInstanceOf[StrideMatrix2D[T]]
  }

  /**
   * Constructs and returns a new <i>dice (transposition) view</i>; Swaps
   * axes; example: 3 x 4 matrix --> 4 x 3 matrix. The view has both
   * dimensions exchanged; what used to be columns become rows, what used to
   * be rows become columns. In other words:
   * <tt>view.get(row,column)==this.get(column,row)</tt>. This is a zero-copy
   * transposition, taking O(1), i.e. constant time. The returned view is
   * backed by this matrix, so changes in the returned view are reflected in
   * this matrix, and vice-versa. Use idioms like
   * <tt>result = viewDice(A).copy()</tt> to generate an independent
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
  def viewTranspose(): StrideMatrix2D[T] = {
    view().vDice().asInstanceOf[StrideMatrix2D[T]]
  }

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
   * <tt>view.rows() == height; view.columns() == width;</tt>. A view's legal
   * coordinates are again zero based, as usual. In other words, legal
   * coordinates of the view range from <tt>[0,0]</tt> to
   * <tt>[view.rows()-1==height-1,view.columns()-1==width-1]</tt>. As usual,
   * any attempt to access a cell at a coordinate
   * <tt>column&lt;0 || column&gt;=view.columns() || row&lt;0 || row&gt;=view.rows()</tt>
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
   *             <tt>column<0 || width<0 || column+width>columns() || row<0 || height<0 || row+height>rows()</tt>
   * @return the new view.
   *
   */
  def viewPart(row: Int, column: Int, height: Int, width: Int): StrideMatrix2D[T] = {
    view().vPart(row, column, height, width).asInstanceOf[StrideMatrix2D[T]]
  }

  /**
   * Constructs and returns a new <i>flip view</i> along the row axis. What
   * used to be row <tt>0</tt> is now row <tt>rows()-1</tt>, ..., what used to
   * be row <tt>rows()-1</tt> is now row <tt>0</tt>. The returned view is
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
  def viewRowFlip(): StrideMatrix2D[T] = {
    view().vRowFlip().asInstanceOf[StrideMatrix2D[T]]
  }

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
  def viewSelection(condition: Matrix1DProcedure[T]): StrideMatrix2D[T] = {
    val matches = new IntArrayList()
    for (i <- 0 until rows if condition.apply(viewRow(i))) matches.add(i)
    viewSelection(matches.toIntArray, null)
  }

  /**
   * Constructs and returns a new <i>selection view</i> that is a matrix
   * holding the indicated cells. There holds
   * <tt>view.rows() == rowIndexes.length, view.columns() == columnIndexes.length</tt>
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
   * parameter
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
   *             if <tt>!(0 <= rowIndexes[i] < rows())</tt> for any
   *             <tt>i=0..rowIndexes.length()-1</tt>.
   */
  def viewSelection(rowIndexes: Array[Int], columnIndexes: Array[Int]): StrideMatrix2D[T] = {
    var rowOffsets: Array[Int] = null
    var columnOffsets: Array[Int] = null
    if (rowIndexes != null) {
      checkRowIndexes(rowIndexes)
      rowOffsets = Array.ofDim[Int](rowIndexes.length)
      for (i <- 0 until rowIndexes.length) {
        rowOffsets(i) = _rowOffset(_rowRank(rowIndexes(i)))
      }
    }
    if (columnIndexes != null) {
      checkColumnIndexes(columnIndexes)
      columnOffsets = Array.ofDim[Int](columnIndexes.length)
      for (i <- 0 until columnIndexes.length) {
        columnOffsets(i) = _columnOffset(_columnRank(columnIndexes(i)))
      }
    }
    viewSelectionLike(rowOffsets, columnOffsets)
  }

  protected def viewSelectionLike(rowOffsets: Array[Int], columnOffsets: Array[Int]): StrideMatrix2D[T]

  /**
   * Constructs and returns a new <i>stride view</i> which is a sub matrix
   * consisting of every i-th cell. More specifically, the view has
   * <tt>this.rows()/rowStride</tt> rows and
   * <tt>this.columns()/columnStride</tt> columns holding cells
   * <tt>this.get(i*rowStride,j*columnStride)</tt> for all
   * <tt>i = 0..rows()/rowStride - 1, j = 0..columns()/columnStride - 1</tt>.
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
  def viewStrides(rowStride: Int, columnStride: Int): StrideMatrix2D[T] = {
    view().vStrides(rowStride, columnStride).asInstanceOf[StrideMatrix2D[T]]
  }

  /**
   * Returns the content of this matrix if it is a wrapper; or <tt>this</tt>
   * otherwise. Override this method in wrappers.
   * TODO: Need to implement this
   */
  override protected def getStorageMatrix: Matrix2D[T] = this

  /**
   * Returns <tt>true</tt> if both matrices share at least one identical cell.
   */
  protected def haveSharedCells(other: StrideMatrix2D): Boolean = {
    if (other == null) return false
    if (this == other) return true
    if (getStorageMatrix == other.getStorageMatrix) return true
    getStorageMatrix.haveSharedCells(other.getStorageMatrix)
  }

  /**
   * Constructs and returns a new view equal to the receiver. The view is a
   * shallow clone. Calls <code>clone()</code> and casts the result.
   * <p>
   * <b>Note that the view is not a deep copy.</b> The returned matrix is
   * backed by this matrix, so changes in the returned matrix are reflected in
   * this matrix, and vice-versa.
   * <p>
   * Use {@link #copy()} to construct an independent deep copy rather than a
   * new view.
   *
   * @return a new view of the receiver.
   */
  override protected def view(): StrideMatrix2D[T] = clone().asInstanceOf[StrideMatrix2D[T]]
}
