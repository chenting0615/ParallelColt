package cern.colt.matrix.tdouble.impl

import cern.colt.map.tdouble.AbstractLongDoubleMap
import cern.colt.matrix.AbstractMatrix2D
import cern.colt.matrix.tdouble.StrideMatrix1D
import cern.colt.matrix.tdouble.StrideMatrix2D
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Selection view on sparse 2-d matrices holding <tt>double</tt> elements. First
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
 * addressing overhead is 1 additional int addition and 2 additional array index
 * accesses per get/set.
 * <p>
 * Note that this implementation is not synchronized.
 * <p>
 * <b>Memory requirements:</b>
 * <p>
 * <tt>memory [bytes] = 4*(rowIndexes.length+columnIndexes.length)</tt>. Thus,
 * an index view with 1000 x 1000 indexes additionally uses 8 KB.
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
class SelectedSparseDoubleMatrix2D protected (rows: Int,
    columns: Int,
    protected var elements: AbstractLongDoubleMap,
    rowZero: Int,
    columnZero: Int,
    rowStride: Int,
    columnStride: Int,
    protected var rowOffsets: Array[Int],
    protected var columnOffsets: Array[Int],
    protected var offset: Int) extends StrideMatrix2D {

  setUp(rows, columns, rowZero, columnZero, rowStride, columnStride)

  this.isNoView = false

  /**
   * Constructs a matrix view with the given parameters.
   *
   * @param elements
   *            the cells.
   * @param rowOffsets
   *            The row offsets of the cells that shall be visible.
   * @param columnOffsets
   *            The column offsets of the cells that shall be visible.
   * @param offset
   */
  protected def this(elements: AbstractLongDoubleMap,
      rowOffsets: Array[Int],
      columnOffsets: Array[Int],
      offset: Int) {
    this(rowOffsets.length, columnOffsets.length, elements, 0, 0, 1, 1, rowOffsets, columnOffsets, offset)
  }

  def elements(): AbstractLongDoubleMap = elements

  /**
   * Returns the matrix cell value at coordinate <tt>[row,column]</tt>.
   *
   * <p>
   * Provided with invalid parameters this method may return invalid objects
   * without throwing any exception. <b>You should only use this method when
   * you are absolutely sure that the coordinate is within bounds.</b>
   * Precondition (unchecked):
   * <tt>0 &lt;= column &lt; columns() && 0 &lt;= row &lt; rows()</tt>.
   *
   * @param row
   *            the index of the row-coordinate.
   * @param column
   *            the index of the column-coordinate.
   * @return the value at the specified coordinate.
   */
  def getQuick(row: Int, column: Int): Double = {
    elements.get(offset.toLong + rowOffsets(rowZero + row * rowStride).toLong +
      columnOffsets(columnZero + column * columnStride).toLong)
  }

  /**
   * Returns the position of the given coordinate within the (virtual or
   * non-virtual) internal 1-dimensional array.
   *
   * @param row
   *            the index of the row-coordinate.
   * @param column
   *            the index of the column-coordinate.
   */
  def index(row: Int, column: Int): Long = {
    this.offset.toLong + rowOffsets(rowZero + row * rowStride).toLong +
      columnOffsets(columnZero + column * columnStride).toLong
  }

  /**
   * Construct and returns a new empty matrix <i>of the same dynamic type</i>
   * as the receiver, having the specified number of rows and columns. For
   * example, if the receiver is an instance of type
   * <tt>DenseDoubleMatrix2D</tt> the new matrix must also be of type
   * <tt>DenseDoubleMatrix2D</tt>, if the receiver is an instance of type
   * <tt>SparseDoubleMatrix2D</tt> the new matrix must also be of type
   * <tt>SparseDoubleMatrix2D</tt>, etc. In general, the new matrix should
   * have internal parametrization as similar as possible.
   *
   * @param rows
   *            the number of rows the matrix shall have.
   * @param columns
   *            the number of columns the matrix shall have.
   * @return a new empty matrix of the same dynamic type.
   */
  def like(rows: Int, columns: Int): StrideMatrix2D = new SparseDoubleMatrix2D(rows, columns)

  /**
   * Construct and returns a new 1-d matrix <i>of the corresponding dynamic
   * type</i>, entirelly independent of the receiver. For example, if the
   * receiver is an instance of type <tt>DenseDoubleMatrix2D</tt> the new
   * matrix must be of type <tt>DenseDoubleMatrix1D</tt>, if the receiver is
   * an instance of type <tt>SparseDoubleMatrix2D</tt> the new matrix must be
   * of type <tt>SparseDoubleMatrix1D</tt>, etc.
   *
   * @param size
   *            the number of cells the matrix shall have.
   * @return a new matrix of the corresponding dynamic type.
   */
  def like1D(size: Int): StrideMatrix1D = new SparseDoubleMatrix1D(size)

  /**
   * Sets the matrix cell at coordinate <tt>[row,column]</tt> to the specified
   * value.
   *
   * <p>
   * Provided with invalid parameters this method may access illegal indexes
   * without throwing any exception. <b>You should only use this method when
   * you are absolutely sure that the coordinate is within bounds.</b>
   * Precondition (unchecked):
   * <tt>0 &lt;= column &lt; columns() && 0 &lt;= row &lt; rows()</tt>.
   *
   * @param row
   *            the index of the row-coordinate.
   * @param column
   *            the index of the column-coordinate.
   * @param value
   *            the value to be filled into the specified cell.
   */
  def setQuick(row: Int, column: Int, value: Double) {
    val index = offset.toLong + rowOffsets(rowZero + row * rowStride).toLong +
      columnOffsets(columnZero + column * columnStride).toLong
    if (value == 0) this.elements.removeKey(index) else this.elements.put(index, value)
  }

  /**
   * Returns a vector obtained by stacking the columns of the matrix on top of
   * one another.
   *
   * @return
   */
  def vectorize(): StrideMatrix1D = {
    val v = new SparseDoubleMatrix1D(size.toInt)
    var idx = 0
    for (c <- 0 until columns; r <- 0 until rows) {
      v.setQuick(idx += 1, getQuick(c, r))
    }
    v
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
   * @param the
   *            column to fix.
   * @return a new slice view.
   * @throws IllegalArgumentException
   *             if <tt>column < 0 || column >= columns()</tt>.
   * @see #viewRow(int)
   */
  def viewColumn(column: Int): StrideMatrix1D = {
    checkColumn(column)
    val viewSize = this.rows
    val viewZero = this.rowZero
    val viewStride = this.rowStride
    val viewOffsets = this.rowOffsets
    val viewOffset = this.offset + _columnOffset(_columnRank(column))
    new SelectedSparseDoubleMatrix1D(viewSize, this.elements, viewZero, viewStride, viewOffsets, viewOffset)
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
   * @param the
   *            row to fix.
   * @return a new slice view.
   * @throws IndexOutOfBoundsException
   *             if <tt>row < 0 || row >= rows()</tt>.
   * @see #viewColumn(int)
   */
  def viewRow(row: Int): StrideMatrix1D = {
    checkRow(row)
    val viewSize = this.columns
    val viewZero = columnZero
    val viewStride = this.columnStride
    val viewOffsets = this.columnOffsets
    val viewOffset = this.offset + _rowOffset(_rowRank(row))
    new SelectedSparseDoubleMatrix1D(viewSize, this.elements, viewZero, viewStride, viewOffsets, viewOffset)
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
   * Returns <tt>true</tt> if both matrices share common cells. More formally,
   * returns <tt>true</tt> if <tt>other != null</tt> and at least one of the
   * following conditions is met
   * <ul>
   * <li>the receiver is a view of the other matrix
   * <li>the other matrix is a view of the receiver
   * <li><tt>this == other</tt>
   * </ul>
   */
  protected def haveSharedCellsRaw(other: StrideMatrix2D): Boolean = {
    if (other.isInstanceOf[SelectedSparseDoubleMatrix2D]) {
      val otherMatrix = other.asInstanceOf[SelectedSparseDoubleMatrix2D]
      return this.elements == otherMatrix.elements
    } else if (other.isInstanceOf[SparseDoubleMatrix2D]) {
      val otherMatrix = other.asInstanceOf[SparseDoubleMatrix2D]
      return this.elements == otherMatrix.elements
    }
    false
  }

  /**
   * Construct and returns a new 1-d matrix <i>of the corresponding dynamic
   * type</i>, sharing the same cells. For example, if the receiver is an
   * instance of type <tt>DenseDoubleMatrix2D</tt> the new matrix must be of
   * type <tt>DenseDoubleMatrix1D</tt>, if the receiver is an instance of type
   * <tt>SparseDoubleMatrix2D</tt> the new matrix must be of type
   * <tt>SparseDoubleMatrix1D</tt>, etc.
   *
   * @param size
   *            the number of cells the matrix shall have.
   * @param zero
   *            the index of the first element.
   * @param stride
   *            the number of indexes between any two elements, i.e.
   *            <tt>index(i+1)-index(i)</tt>.
   * @return a new matrix of the corresponding dynamic type.
   */
  protected def like1D(size: Int, zero: Int, stride: Int): StrideMatrix1D = throw new InternalError()

  /**
   * Sets up a matrix with a given number of rows and columns.
   *
   * @param rows
   *            the number of rows the matrix shall have.
   * @param columns
   *            the number of columns the matrix shall have.
   * @throws IllegalArgumentException
   *             if <tt>(double)columns*rows > Integer.MAX_VALUE</tt>.
   */
  protected def setUp(rows: Int, columns: Int) {
    super.setUp(rows, columns)
    this.rowStride = 1
    this.columnStride = 1
    this.offset = 0
  }

  /**
   * Self modifying version of viewDice().
   */
  protected def vDice(): AbstractMatrix2D = {
    super.vDice()
    val tmp = rowOffsets
    rowOffsets = columnOffsets
    columnOffsets = tmp
    this.isNoView = false
    this
  }

  /**
   * Construct and returns a new selection view.
   *
   * @param rowOffsets
   *            the offsets of the visible elements.
   * @param columnOffsets
   *            the offsets of the visible elements.
   * @return a new view.
   */
  protected def viewSelectionLike(rowOffsets: Array[Int], columnOffsets: Array[Int]): StrideMatrix2D = {
    new SelectedSparseDoubleMatrix2D(this.elements, rowOffsets, columnOffsets, this.offset)
  }
}
