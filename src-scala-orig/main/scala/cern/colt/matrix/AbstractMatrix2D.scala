package cern.colt.matrix

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
abstract class AbstractMatrix2D[T] extends Matrix2D[T] {

  /**
   the number of colums and rows this matrix (view) has
   */
  protected var columnsVar: Int = 0

  protected var rowsVar: Int = 0

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

  /**
   * Returns the position of the given absolute rank within the (virtual or
   * non-virtual) internal 1-dimensional array. Default implementation.
   * Override, if necessary.
   *
   * @param absRank
   *            the absolute rank of the element.
   * @return the position.
   */
  protected def _columnOffset(absRank: Int): Int = absRank

  /**
   * Returns the absolute rank of the given relative rank.
   *
   * @param rank
   *            the relative rank of the element.
   * @return the absolute rank of the element.
   */
  protected def _columnRank(rank: Int): Int = columnZeroVar + rank * columnStrideVar

  /**
   * Returns the position of the given absolute rank within the (virtual or
   * non-virtual) internal 1-dimensional array. Default implementation.
   * Override, if necessary.
   *
   * @param absRank
   *            the absolute rank of the element.
   * @return the position.
   */
  protected def _rowOffset(absRank: Int): Int = absRank

  /**
   * Returns the absolute rank of the given relative rank.
   *
   * @param rank
   *            the relative rank of the element.
   * @return the absolute rank of the element.
   */
  protected def _rowRank(rank: Int): Int = rowZeroVar + rank * rowStrideVar

  /**
   * Checks whether the receiver contains the given box and throws an
   * exception, if necessary.
   *
   * @throws IndexOutOfBoundsException
   *             if
   *             <tt>column<0 || width<0 || column+width>columns() || row<0 || height<0 || row+height>rows()</tt>
   */
  protected def checkBox(row: Int, column: Int, height: Int, width: Int) {
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
  protected def checkColumn(column: Int) {
    if (column < 0 || column >= columnsVar) throw new IndexOutOfBoundsException("Attempted to access " + toShapeString + " at column=" + column)
  }

  /**
   * Checks whether indexes are legal and throws an exception, if necessary.
   *
   * @throws IndexOutOfBoundsException
   *             if <tt>! (0 <= indexes[i] < columns())</tt> for any
   *             i=0..indexes.length()-1.
   */
  protected def checkColumnIndexes(indexes: Array[Int]) {
    var i = indexes.length
    while (i >= 0) {
      val index = indexes(i)
      if (index < 0 || index >= columnsVar) checkColumn(index)
      i -= 1
    }
  }

  /**
   * Sanity check for operations requiring a row index to be within bounds.
   *
   * @throws IndexOutOfBoundsException
   *             if <tt>row < 0 || row >= rows()</tt>.
   */
  protected def checkRow(row: Int) {
    if (row < 0 || row >= rowsVar) throw new IndexOutOfBoundsException("Attempted to access " + toShapeString + " at row=" + row)
  }

  /**
   * Checks whether indexes are legal and throws an exception, if necessary.
   *
   * @throws IndexOutOfBoundsException
   *             if <tt>! (0 <= indexes[i] < rows())</tt> for any
   *             i=0..indexes.length()-1.
   */
  protected def checkRowIndexes(indexes: Array[Int]) {
    var i = indexes.length
    while (i >= 0) {
      val index = indexes(i)
      if (index < 0 || index >= rows) checkRow(index)
      i -= 1
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
    if (columnsVar != B.columns() || rowsVar != B.rows())
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
    if (columnsVar != B.columns() || rowsVar != B.rows() || columnsVar != C.columns() || rowsVar != C.rows())
      throw new IllegalArgumentException("Incompatible dimensions: " + toShapeString + ", " + B.toShapeString + ", " + C.toShapeString)
  }

  /**
   * Returns the number of columns.
   */
  def columns(): Int = columnsVar

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
  protected def index(row: Int, column: Int): Long = {
    _rowOffset(_rowRank(row)) + _columnOffset(_columnRank(column))
  }

  /**
   * Returns the number of rows.
   */
  def rows(): Int = rowsVar

  /**
   * Returns the row stride.
   */
  def rowStride(): Int = rowStrideVar

  def view(): AbstractMatrix2D[T] = {
    clone().asInstanceOf[AbstractMatrix2D[T]]
  }

  protected def like1D(size: Int, zero: Int, stride: Int): AbstractMatrix1D[T]

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
  protected def setUp(rows: Int, columns: Int) {
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
  protected def setUp(rows: Int,
      columns: Int,
      rowZero: Int,
      columnZero: Int,
      rowStride: Int,
      columnStride: Int) {
    if (rows < 0 || columns < 0) throw new IllegalArgumentException("negative size")
    this.rowsVar = rows
    this.columnsVar = columns
    this.rowZeroVar = rowZero
    this.columnZeroVar = columnZero
    this.rowStrideVar = rowStride
    this.columnStrideVar = columnStride
    this.isNoView = true
    if (columns.toDouble * rows > Integer.MAX_VALUE) throw new IllegalArgumentException("matrix too large")
  }

  /**
   * Returns the number of cells which is <tt>rows()*columns()</tt>.
   */
  override def size: Long = rowsVar * columnsVar

  /**
   * Returns a string representation of the receiver's shape.
   */
  def toShapeString: String = AbstractFormatter.shape(this)

  /**
   * Self modifying version of viewColumnFlip().
   */
  protected def vColumnFlip(): AbstractMatrix2D[T] = {
    if (columns > 0) {
      columnZeroVar += (columnsVar - 1) * columnStrideVar
      columnStrideVar = -columnStrideVar
      this.isNoView = false
    }
    this
  }

  /**
   * Self modifying version of viewDice().
   */
  protected def vDice(): AbstractMatrix2D[T] = {
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
   *             <tt>column<0 || width<0 || column+width>columns() || row<0 || height<0 || row+height>rows()</tt>
   */
  protected def vPart(row: Int,
      column: Int,
      height: Int,
      width: Int): AbstractMatrix2D[T] = {
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
  protected def vRowFlip(): AbstractMatrix2D[T] = {
    if (rows > 0) {
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
  protected def vStrides(rowStride: Int, columnStride: Int): AbstractMatrix2D[T] = {
    if (rowStride <= 0 || columnStride <= 0) throw new IndexOutOfBoundsException("illegal strides: " + rowStride + ", " + columnStride)
    this.rowStride *= rowStride
    this.columnStride *= columnStride
    if (this.rows != 0) this.rowsVar = (this.rowsVar - 1) / rowStrideVar + 1
    if (this.columns != 0) this.columnsVar = (this.columnsVar - 1) / columnStrideVar + 1
    this.isNoView = false
    this
  }
}
