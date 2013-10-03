package cern.colt.matrix.impl

import cern.colt.matrix.Matrix

/**
 * Abstract base class for 2-d matrices holding specific type of elements, supporting offsets and strides.
 * First see the <a href="package-summary.html">package summary</a> and javadoc <a
 * href="package-tree.html">tree view</a> to get the broad picture.
 * <p>
 * A matrix has a number of rows and columns, which are assigned upon instance
 * construction - The matrix's size is then <tt>rows*columns</tt>. Elements
 * are accessed via <tt>[row,column]</tt> coordinates. Legal coordinates range
 * from <tt>[0,0]</tt> to <tt>[rows-1,columns-1]</tt>. Any attempt to access
 * an element at a coordinate
 * <tt>column&lt;0 || column&gt;=columns || row&lt;0 || row&gt;=rows</tt>
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
abstract class StrideMatrix2D[T: Manifest] protected () extends AbstractMatrix2D[T] {

  /**
   * the number of elements between two rows, i.e.
   * <tt>index(i+1,j,k) - index(i,j,k)</tt>.
   */
  protected var rowStrideVar: Int = 0

  /**
   * the number of elements between two columns, i.e.
   * <tt>index(i,j+1,k) - index(i,j,k)</tt>.
   */
  protected var columnStrideVar: Int = 0

  /**
   the index of the first element
   */
  protected var rowZeroVar: Int = 0

  protected var columnZeroVar: Int = 0

  private var storageMatrix: Matrix[T] = this

  /**
   * Returns the column stride.
   */
  def columnStride(): Int = columnStrideVar

  /**
   * Returns the position of the given coordinate within the (virtual or
   * non-virtual) internal 1-dimensional array.
   *
   * @param row
   *            the index of the row-coordinate.
   * @param column
   *            the index of the column-coordinate.
   */
  protected def toRawIndex(row: Int, column: Int): Int = {
    rowZeroVar + row * rowStrideVar + columnZeroVar + column * columnStrideVar
  }

  /**
   * Returns the row stride.
   */
  def rowStride(): Int = rowStrideVar

  /**
   * Sets up a matrix with a given number of rows and columns.
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
  override protected def setUp(rows: Int, columns: Int) {
    setUp(rows, columns, 0, 0, columns, 1)
  }

  /**
   * Sets up a matrix with a given number of rows and columns and the given
   * strides.
   *
   * @param rows
   *            the number of rows the matrix shall have.
   * @param columns
   *            the number of columns the matrix shall have.
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
   * @throws IllegalArgumentException
   *             if
   *             <tt>rows<0 || columns<0 || (double)columns*rows > Integer.MAX_VALUE</tt>
   *             or flip's are illegal.
   */
  protected def setUp(rows: Int, columns: Int, rowZero: Int, columnZero: Int, rowStride: Int, columnStride: Int) {
    if (rows < 0 || columns < 0)
      throw new IllegalArgumentException("negative size")
    this.rowsVar = rows
    this.columnsVar = columns
    this.rowZeroVar = rowZero
    this.columnZeroVar = columnZero
    this.rowStrideVar = rowStride
    this.columnStrideVar = columnStride
    this.isNoView = true
    if (columns.toLong * rows > Integer.MAX_VALUE)
      throw new IllegalArgumentException("matrix too large")
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
   *             if <tt>values.length != rows*columns</tt>.
   */
  def assign(values: Array[T], zero: Int, stride: Int) = {
    val requiredLen = size * stride + zero - stride + 1
    if (values.length < requiredLen)
      throw new IllegalArgumentException("Length too short (" + values.length + "), required rows*columns*stride+zero-stride+1=" + requiredLen)
    var idx = zero
    for (r <- 0 until rowsVar; c <- 0 until columnsVar) {
      setQuick(r, c, values(idx))
      idx += stride
    }
    this
  }

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
  def viewColumnFlip(): StrideMatrix2D[T] = {
    view().vColumnFlip()
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
  def viewTranspose() = {
    view().vDice()
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
  def viewPart(row: Int, column: Int, height: Int, width: Int) = {
    view().vPart(row, column, height, width)
  }

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
  def viewRowFlip() = {
    view().vRowFlip()
  }

  /**
   * Constructs and returns a new <i>selection view</i> that is a matrix
   * holding the indicated cells. There holds
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
   *             if <tt>!(0 <= rowIndexes[i] < rows)</tt> for any
   *             <tt>i=0..rowIndexes.length()-1</tt>.
   */
  def viewSelection(rowIndexes: Array[Int], columnIndexes: Array[Int]): AbstractMatrix2D[T] = {
    val viewRows = if (rowIndexes != null) rowsVar else rowIndexes.length
    val viewColumns = if (columnIndexes != null) columnsVar else columnIndexes.length
    val view = new WrapperMatrix2D[T](this, viewRows, viewColumns) {
      override def remapIndexes(row: Int, column: Int) = {
        (if (rowIndexes == null) row else rowIndexes(row), if (columnIndexes == null) column else columnIndexes(column))
      }
    }
    view
  }

  /**
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
  def viewStrides(rowStride: Int, columnStride: Int) = {
    view().vStrides(rowStride, columnStride)
  }

  /**
   * Returns the content of this matrix if it is a wrapper; or <tt>this</tt>
   * otherwise. Override this method in wrappers.
   * TODO: Need to implement this
   */
  override def getStorageMatrix = storageMatrix


  /**
   * Returns <tt>true</tt> if both matrices share at least one identical cell.
   */
  def haveSharedCells(other: StrideMatrix2D[T]): Boolean = {
    if (other == null) return false
    if (this == other) return true
    val m = getStorageMatrix
    if (m == null) return false
    val otherM = other.getStorageMatrix
    if (otherM == null) return false
    if (m == otherM) return true
    if (m == this || otherM == this)
      false
    else
      m.haveSharedCells(otherM)
  }

  /**
   * Constructs and returns a new view equal to the receiver. The view is a
   * shallow clone. Calls <code>clone()</code> and casts the result.
   * <p>
   * <b>Note that the view is not a deep copy.</b> The returned matrix is
   * backed by this matrix, so changes in the returned matrix are reflected in
   * this matrix, and vice-versa.
   * <p>
   * Use copy() to construct an independent deep copy rather than a
   * new view.
   *
   * @return a new view of the receiver.
   */
  protected def view() = {
    val v = clone().asInstanceOf[StrideMatrix2D[T]]
    v.storageMatrix = this.getStorageMatrix
    v.isNoView = false
    v
  }

  /**
   * Self modifying version of viewColumnFlip().
   */
  protected def vColumnFlip() = {
    if (columnsVar > 0) {
      columnZeroVar += (columnsVar - 1) * columnStrideVar
      columnStrideVar = -columnStrideVar
      this.isNoView = false
    }
    this
  }

  /**
   * Self modifying version of viewTranspose().
   */
  protected def vDice() = {
    var tmp: Int = 0
    tmp = rowsVar
    rowsVar = columnsVar
    columnsVar = tmp
    tmp = rowStrideVar
    rowStrideVar = columnStrideVar
    columnStrideVar = tmp
    tmp = rowZeroVar
    rowZeroVar = columnZeroVar
    columnZeroVar = tmp
    this.isNoView = false
    this
  }

  /**
   * Self modifying version of viewPart().
   *
   * @throws IndexOutOfBoundsException
   *             if
   *             <tt>column<0 || width<0 || column+width>columns || row<0 || height<0 || row+height>rows</tt>
   */
  protected def vPart(row: Int, column: Int, height: Int, width: Int) = {
    checkBox(row, column, height, width)
    this.rowZeroVar += this.rowStride * row
    this.columnZeroVar += this.columnStride * column
    this.rowsVar = height
    this.columnsVar = width
    this.isNoView = false
    this
  }

  /**
   * Self modifying version of viewRowFlip().
   */
  protected def vRowFlip() = {
    if (rowsVar > 0) {
      rowZeroVar += (rowsVar - 1) * rowStrideVar
      rowStrideVar = -rowStrideVar
      this.isNoView = false
    }
    this
  }

  /**
   * Self modifying version of viewStrides().
   *
   * @throws IndexOutOfBoundsException
   *             if <tt>rowStride<=0 || columnStride<=0</tt>.
   */
  protected def vStrides(rowStride: Int, columnStride: Int) = {
    if (rowStride <= 0 || columnStride <= 0)
      throw new IndexOutOfBoundsException("illegal strides: " + rowStride + ", " + columnStride)
    this.rowStrideVar *= rowStride
    this.columnStrideVar *= columnStride
    if (this.rowsVar != 0) this.rowsVar = (this.rowsVar - 1) / rowStrideVar + 1
    if (this.columnsVar != 0) this.columnsVar = (this.columnsVar - 1) / columnStrideVar + 1
    this.isNoView = false
    this
  }
}
