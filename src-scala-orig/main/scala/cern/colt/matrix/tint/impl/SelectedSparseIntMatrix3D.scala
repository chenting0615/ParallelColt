package cern.colt.matrix.tint.impl

import cern.colt.map.tlong.AbstractLongIntMap
import cern.colt.matrix.AbstractMatrix3D
import cern.colt.matrix.tint.IntMatrix1D
import cern.colt.matrix.tint.IntMatrix2D
import cern.colt.matrix.tint.IntMatrix3D
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Selection view on sparse 3-d matrices holding <tt>int</tt> elements. First
 * see the <a href="package-summary.html">package summary</a> and javadoc <a
 * href="package-tree.html">tree view</a> to get the broad picture.
 * <p>
 * <b>Implementation:</b>
 * <p>
 * Objects of this class are typically constructed via <tt>viewIndexes</tt>
 * methods on some source matrix. The interface introduced in abstract super
 * classes defines everything a user can do. From a user point of view there is
 * nothing special about this class; it presents the same functionality with the
 * same signatures and semantics as its abstract superclass(es) while
 * introducing no additional functionality. Thus, this class need not be visible
 * to users. By the way, the same principle applies to concrete DenseXXX and
 * SparseXXX classes: they presents the same functionality with the same
 * signatures and semantics as abstract superclass(es) while introducing no
 * additional functionality. Thus, they need not be visible to users, either.
 * Factory methods could hide all these concrete types.
 * <p>
 * This class uses no delegation. Its instances point directly to the data. Cell
 * addressing overhead is is 1 additional int addition and 3 additional array
 * index accesses per get/set.
 * <p>
 * Note that this implementation is not synchronized.
 * <p>
 * <b>Memory requirements:</b>
 * <p>
 * <tt>memory [bytes] = 4*(sliceIndexes.length+rowIndexes.length+columnIndexes.length)</tt>
 * . Thus, an index view with 100 x 100 x 100 indexes additionally uses 8 KB.
 * <p>
 * <b>Time complexity:</b>
 * <p>
 * Depends on the parent view holding cells.
 * <p>
 *
 * @author wolfgang.hoschek@cern.ch
 * @version 1.0, 09/24/99
 */
@SerialVersionUID(1L)
class SelectedSparseIntMatrix3D protected (protected var elements: AbstractLongIntMap, 
    protected var sliceOffsets: Array[Int], 
    protected var rowOffsets: Array[Int], 
    protected var columnOffsets: Array[Int], 
    protected var offset: Int) extends IntMatrix3D {

  val slices = sliceOffsets.length

  val rows = rowOffsets.length

  val columns = columnOffsets.length

  setUp(slices, rows, columns)

  this.isNoView = false

  def elements(): AbstractLongIntMap = elements

  /**
   * Returns the matrix cell value at coordinate <tt>[slice,row,column]</tt>.
   *
   * <p>
   * Provided with invalid parameters this method may return invalid objects
   * without throwing any exception. <b>You should only use this method when
   * you are absolutely sure that the coordinate is within bounds.</b>
   * Precondition (unchecked):
   * <tt>slice&lt;0 || slice&gt;=slices() || row&lt;0 || row&gt;=rows() || column&lt;0 || column&gt;=column()</tt>.
   *
   * @param slice
   *            the index of the slice-coordinate.
   * @param row
   *            the index of the row-coordinate.
   * @param column
   *            the index of the column-coordinate.
   * @return the value at the specified coordinate.
   */
  def getQuick(slice: Int, row: Int, column: Int): Int = {
    elements.get(offset.toLong + 
      sliceOffsets(sliceZero + slice * sliceStride).toLong + 
      rowOffsets(rowZero + row * rowStride).toLong + 
      columnOffsets(columnZero + column * columnStride).toLong)
  }

  /**
   * Returns the position of the given coordinate within the (virtual or
   * non-virtual) internal 1-dimensional array.
   *
   * @param slice
   *            the index of the slice-coordinate.
   * @param row
   *            the index of the row-coordinate.
   * @param column
   *            the index of the third-coordinate.
   */
  def index(slice: Int, row: Int, column: Int): Long = {
    this.offset.toLong + 
      sliceOffsets(sliceZero + slice * sliceStride).toLong + 
      rowOffsets(rowZero + row * rowStride).toLong + 
      columnOffsets(columnZero + column * columnStride).toLong
  }

  /**
   * Construct and returns a new empty matrix <i>of the same dynamic type</i>
   * as the receiver, having the specified number of slices, rows and columns.
   * For example, if the receiver is an instance of type
   * <tt>DenseIntMatrix3D</tt> the new matrix must also be of type
   * <tt>DenseIntMatrix3D</tt>, if the receiver is an instance of type
   * <tt>SparseIntMatrix3D</tt> the new matrix must also be of type
   * <tt>SparseIntMatrix3D</tt>, etc. In general, the new matrix should have
   * internal parametrization as similar as possible.
   *
   * @param slices
   *            the number of slices the matrix shall have.
   * @param rows
   *            the number of rows the matrix shall have.
   * @param columns
   *            the number of columns the matrix shall have.
   * @return a new empty matrix of the same dynamic type.
   */
  def like(slices: Int, rows: Int, columns: Int): IntMatrix3D = {
    new SparseIntMatrix3D(slices, rows, columns)
  }

  def like2D(rows: Int, columns: Int): IntMatrix2D = new SparseIntMatrix2D(rows, columns)

  /**
   * Sets the matrix cell at coordinate <tt>[slice,row,column]</tt> to the
   * specified value.
   *
   * <p>
   * Provided with invalid parameters this method may access illegal indexes
   * without throwing any exception. <b>You should only use this method when
   * you are absolutely sure that the coordinate is within bounds.</b>
   * Precondition (unchecked):
   * <tt>slice&lt;0 || slice&gt;=slices() || row&lt;0 || row&gt;=rows() || column&lt;0 || column&gt;=column()</tt>.
   *
   * @param slice
   *            the index of the slice-coordinate.
   * @param row
   *            the index of the row-coordinate.
   * @param column
   *            the index of the column-coordinate.
   * @param value
   *            the value to be filled into the specified cell.
   */
  def setQuick(slice: Int, 
      row: Int, 
      column: Int, 
      value: Int) {
    val index = offset.toLong + 
      sliceOffsets(sliceZero + slice * sliceStride).toLong + 
      rowOffsets(rowZero + row * rowStride).toLong + 
      columnOffsets(columnZero + column * columnStride).toLong
    if (value == 0) this.elements.removeKey(index) else this.elements.put(index, value)
  }

  /**
   * Returns a vector obtained by stacking the columns of each slice of the
   * matrix on top of one another.
   *
   * @return
   */
  def vectorize(): IntMatrix1D = {
    throw new IllegalArgumentException("This method is not supported.")
  }

  /**
   * Constructs and returns a new 2-dimensional <i>slice view</i> representing
   * the slices and rows of the given column. The returned view is backed by
   * this matrix, so changes in the returned view are reflected in this
   * matrix, and vice-versa.
   * <p>
   * To obtain a slice view on subranges, construct a sub-ranging view (
   * <tt>view().part(...)</tt>), then apply this method to the sub-range view.
   * To obtain 1-dimensional views, apply this method, then apply another
   * slice view (methods <tt>viewColumn</tt>, <tt>viewRow</tt>) on the
   * intermediate 2-dimensional view. To obtain 1-dimensional views on
   * subranges, apply both steps.
   *
   * @param column
   *            the index of the column to fix.
   * @return a new 2-dimensional slice view.
   * @throws IndexOutOfBoundsException
   *             if <tt>column < 0 || column >= columns()</tt>.
   * @see #viewSlice(int)
   * @see #viewRow(int)
   */
  def viewColumn(column: Int): IntMatrix2D = {
    checkColumn(column)
    val viewRows = this.slices
    val viewColumns = this.rows
    val viewRowZero = sliceZero
    val viewColumnZero = rowZero
    val viewOffset = this.offset + _columnOffset(_columnRank(column))
    val viewRowStride = this.sliceStride
    val viewColumnStride = this.rowStride
    val viewRowOffsets = this.sliceOffsets
    val viewColumnOffsets = this.rowOffsets
    new SelectedSparseIntMatrix2D(viewRows, viewColumns, this.elements, viewRowZero, viewColumnZero, 
      viewRowStride, viewColumnStride, viewRowOffsets, viewColumnOffsets, viewOffset)
  }

  /**
   * Constructs and returns a new 2-dimensional <i>slice view</i> representing
   * the slices and columns of the given row. The returned view is backed by
   * this matrix, so changes in the returned view are reflected in this
   * matrix, and vice-versa.
   * <p>
   * To obtain a slice view on subranges, construct a sub-ranging view (
   * <tt>view().part(...)</tt>), then apply this method to the sub-range view.
   * To obtain 1-dimensional views, apply this method, then apply another
   * slice view (methods <tt>viewColumn</tt>, <tt>viewRow</tt>) on the
   * intermediate 2-dimensional view. To obtain 1-dimensional views on
   * subranges, apply both steps.
   *
   * @param row
   *            the index of the row to fix.
   * @return a new 2-dimensional slice view.
   * @throws IndexOutOfBoundsException
   *             if <tt>row < 0 || row >= row()</tt>.
   * @see #viewSlice(int)
   * @see #viewColumn(int)
   */
  def viewRow(row: Int): IntMatrix2D = {
    checkRow(row)
    val viewRows = this.slices
    val viewColumns = this.columns
    val viewRowZero = sliceZero
    val viewColumnZero = columnZero
    val viewOffset = this.offset + _rowOffset(_rowRank(row))
    val viewRowStride = this.sliceStride
    val viewColumnStride = this.columnStride
    val viewRowOffsets = this.sliceOffsets
    val viewColumnOffsets = this.columnOffsets
    new SelectedSparseIntMatrix2D(viewRows, viewColumns, this.elements, viewRowZero, viewColumnZero, 
      viewRowStride, viewColumnStride, viewRowOffsets, viewColumnOffsets, viewOffset)
  }

  /**
   * Constructs and returns a new 2-dimensional <i>slice view</i> representing
   * the rows and columns of the given slice. The returned view is backed by
   * this matrix, so changes in the returned view are reflected in this
   * matrix, and vice-versa.
   * <p>
   * To obtain a slice view on subranges, construct a sub-ranging view (
   * <tt>view().part(...)</tt>), then apply this method to the sub-range view.
   * To obtain 1-dimensional views, apply this method, then apply another
   * slice view (methods <tt>viewColumn</tt>, <tt>viewRow</tt>) on the
   * intermediate 2-dimensional view. To obtain 1-dimensional views on
   * subranges, apply both steps.
   *
   * @param slice
   *            the index of the slice to fix.
   * @return a new 2-dimensional slice view.
   * @throws IndexOutOfBoundsException
   *             if <tt>slice < 0 || slice >= slices()</tt>.
   * @see #viewRow(int)
   * @see #viewColumn(int)
   */
  def viewSlice(slice: Int): IntMatrix2D = {
    checkSlice(slice)
    val viewRows = this.rows
    val viewColumns = this.columns
    val viewRowZero = rowZero
    val viewColumnZero = columnZero
    val viewOffset = this.offset + _sliceOffset(_sliceRank(slice))
    val viewRowStride = this.rowStride
    val viewColumnStride = this.columnStride
    val viewRowOffsets = this.rowOffsets
    val viewColumnOffsets = this.columnOffsets
    new SelectedSparseIntMatrix2D(viewRows, viewColumns, this.elements, viewRowZero, viewColumnZero, 
      viewRowStride, viewColumnStride, viewRowOffsets, viewColumnOffsets, viewOffset)
  }

  /**
   * Returns the position of the given absolute rank within the (virtual or
   * non-virtual) internal 1-dimensional array. Default implementation.
   * Override, if necessary.
   *
   * @param rank
   *            the absolute rank of the element.
   * @return the position.
   */
  protected def _columnOffset(absRank: Int): Int = columnOffsets(absRank)

  /**
   * Returns the position of the given absolute rank within the (virtual or
   * non-virtual) internal 1-dimensional array. Default implementation.
   * Override, if necessary.
   *
   * @param rank
   *            the absolute rank of the element.
   * @return the position.
   */
  protected def _rowOffset(absRank: Int): Int = rowOffsets(absRank)

  /**
   * Returns the position of the given absolute rank within the (virtual or
   * non-virtual) internal 1-dimensional array. Default implementation.
   * Override, if necessary.
   *
   * @param rank
   *            the absolute rank of the element.
   * @return the position.
   */
  protected def _sliceOffset(absRank: Int): Int = sliceOffsets(absRank)

  /**
   * Returns <tt>true</tt> if both matrices share common cells. More formally,
   * returns <tt>true</tt> if <tt>other != null</tt> and at least one of the
   * following conditions is met
   * <ul>
   * <li>the receiver is a view of the other matrix
   * <li>the other matrix is a view of the receiver
   * <li><tt>this == other</tt>
   * </ul>
   */
  protected def haveSharedCellsRaw(other: IntMatrix3D): Boolean = {
    if (other.isInstanceOf[SelectedSparseIntMatrix3D]) {
      val otherMatrix = other.asInstanceOf[SelectedSparseIntMatrix3D]
      return this.elements == otherMatrix.elements
    } else if (other.isInstanceOf[SparseIntMatrix3D]) {
      val otherMatrix = other.asInstanceOf[SparseIntMatrix3D]
      return this.elements == otherMatrix.elements
    }
    false
  }

  /**
   * Construct and returns a new 2-d matrix <i>of the corresponding dynamic
   * type</i>, sharing the same cells. For example, if the receiver is an
   * instance of type <tt>DenseIntMatrix3D</tt> the new matrix must also be of
   * type <tt>DenseIntMatrix2D</tt>, if the receiver is an instance of type
   * <tt>SparseIntMatrix3D</tt> the new matrix must also be of type
   * <tt>SparseIntMatrix2D</tt>, etc.
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
   * @return a new matrix of the corresponding dynamic type.
   */
  protected def like2D(rows: Int, 
      columns: Int, 
      rowZero: Int, 
      columnZero: Int, 
      rowStride: Int, 
      columnStride: Int): IntMatrix2D = throw new InternalError()

  /**
   * Sets up a matrix with a given number of slices and rows.
   *
   * @param slices
   *            the number of slices the matrix shall have.
   * @param rows
   *            the number of rows the matrix shall have.
   * @param columns
   *            the number of columns the matrix shall have.
   * @throws IllegalArgumentException
   *             if <tt>(int)rows*slices > Integer.MAX_VALUE</tt>.
   */
  protected def setUp(slices: Int, rows: Int, columns: Int) {
    super.setUp(slices, rows, columns)
    this.sliceStride = 1
    this.rowStride = 1
    this.columnStride = 1
    this.offset = 0
  }

  /**
   * Self modifying version of viewDice().
   *
   * @throws IllegalArgumentException
   *             if some of the parameters are equal or not in range 0..2.
   */
  protected def vDice(axis0: Int, axis1: Int, axis2: Int): AbstractMatrix3D = {
    super.vDice(axis0, axis1, axis2)
    val offsets = Array.ofDim[IntInt,](3)
    offsets(0) = this.sliceOffsets
    offsets(1) = this.rowOffsets
    offsets(2) = this.columnOffsets
    this.sliceOffsets = offsets(axis0)
    this.rowOffsets = offsets(axis1)
    this.columnOffsets = offsets(axis2)
    this
  }

  /**
   * Construct and returns a new selection view.
   *
   * @param sliceOffsets
   *            the offsets of the visible elements.
   * @param rowOffsets
   *            the offsets of the visible elements.
   * @param columnOffsets
   *            the offsets of the visible elements.
   * @return a new view.
   */
  protected def viewSelectionLike(sliceOffsets: Array[Int], rowOffsets: Array[Int], columnOffsets: Array[Int]): IntMatrix3D = {
    new SelectedSparseIntMatrix3D(this.elements, sliceOffsets, rowOffsets, columnOffsets, this.offset)
  }
}
