package cern.colt.matrix.tint.impl

import cern.colt.map.tlong.AbstractLongIntMap
import cern.colt.matrix.tint.IntMatrix1D
import cern.colt.matrix.tint.IntMatrix2D
import cern.colt.matrix.tint.IntMatrix3D
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Selection view on sparse 1-d matrices holding <tt>int</tt> elements. First
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
 * to users. By the way, the same principle applies to concrete DenseXXX,
 * SparseXXX classes: they presents the same functionality with the same
 * signatures and semantics as abstract superclass(es) while introducing no
 * additional functionality. Thus, they need not be visible to users, either.
 * Factory methods could hide all these concrete types.
 * <p>
 * This class uses no delegation. Its instances point directly to the data. Cell
 * addressing overhead is 1 additional array index access per get/set.
 * <p>
 * Note that this implementation is not synchronized.
 * <p>
 * <b>Memory requirements:</b>
 * <p>
 * <tt>memory [bytes] = 4*indexes.length</tt>. Thus, an index view with 1000
 * indexes additionally uses 4 KB.
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
class SelectedSparseIntMatrix1D protected (size: Int, 
    protected var elements: AbstractLongIntMap, 
    zero: Int, 
    stride: Int, 
    protected var offsets: Array[Int], 
    protected var offset: Int) extends IntMatrix1D {

  setUp(size, zero, stride)

  this.isNoView = false

  /**
   * Constructs a matrix view with the given parameters.
   *
   * @param elements
   *            the cells.
   * @param indexes
   *            The indexes of the cells that shall be visible.
   */
  protected def this(elements: AbstractLongIntMap, offsets: Array[Int]) {
    this(offsets.length, elements, 0, 1, offsets, 0)
  }

  def elements(): AbstractLongIntMap = {
    throw new IllegalArgumentException("This method is not supported.")
  }

  /**
   * Returns the matrix cell value at coordinate <tt>index</tt>.
   *
   * <p>
   * Provided with invalid parameters this method may return invalid objects
   * without throwing any exception. <b>You should only use this method when
   * you are absolutely sure that the coordinate is within bounds.</b>
   * Precondition (unchecked): <tt>index&lt;0 || index&gt;=size()</tt>.
   *
   * @param index
   *            the index of the cell.
   * @return the value of the specified cell.
   */
  def getQuick(index: Int): Int = {
    elements.get(offset.toLong + offsets(zero + index * stride).toLong)
  }

  /**
   * Returns the position of the element with the given relative rank within
   * the (virtual or non-virtual) internal 1-dimensional array. You may want
   * to override this method for performance.
   *
   * @param rank
   *            the rank of the element.
   */
  def index(rank: Int): Long = {
    offset.toLong + offsets(zero + rank * stride).toLong
  }

  /**
   * Construct and returns a new empty matrix <i>of the same dynamic type</i>
   * as the receiver, having the specified size. For example, if the receiver
   * is an instance of type <tt>DenseIntMatrix1D</tt> the new matrix must also
   * be of type <tt>DenseIntMatrix1D</tt>, if the receiver is an instance of
   * type <tt>SparseIntMatrix1D</tt> the new matrix must also be of type
   * <tt>SparseIntMatrix1D</tt>, etc. In general, the new matrix should have
   * internal parametrization as similar as possible.
   *
   * @param size
   *            the number of cell the matrix shall have.
   * @return a new empty matrix of the same dynamic type.
   */
  def like(size: Int): IntMatrix1D = new SparseIntMatrix1D(size)

  /**
   * Construct and returns a new 2-d matrix <i>of the corresponding dynamic
   * type</i>, entirelly independent of the receiver. For example, if the
   * receiver is an instance of type <tt>DenseIntMatrix1D</tt> the new matrix
   * must be of type <tt>DenseIntMatrix2D</tt>, if the receiver is an instance
   * of type <tt>SparseIntMatrix1D</tt> the new matrix must be of type
   * <tt>SparseIntMatrix2D</tt>, etc.
   *
   * @param rows
   *            the number of rows the matrix shall have.
   * @param columns
   *            the number of columns the matrix shall have.
   * @return a new matrix of the corresponding dynamic type.
   */
  def like2D(rows: Int, columns: Int): IntMatrix2D = new SparseIntMatrix2D(rows, columns)

  def reshape(rows: Int, columns: Int): IntMatrix2D = {
    throw new IllegalArgumentException("This method is not supported.")
  }

  def reshape(slices: Int, rows: Int, columns: Int): IntMatrix3D = {
    throw new IllegalArgumentException("This method is not supported.")
  }

  /**
   * Sets the matrix cell at coordinate <tt>index</tt> to the specified value.
   *
   * <p>
   * Provided with invalid parameters this method may access illegal indexes
   * without throwing any exception. <b>You should only use this method when
   * you are absolutely sure that the coordinate is within bounds.</b>
   * Precondition (unchecked): <tt>index&lt;0 || index&gt;=size()</tt>.
   *
   * @param index
   *            the index of the cell.
   * @param value
   *            the value to be filled into the specified cell.
   */
  def setQuick(index: Int, value: Int) {
    val i = offset.toLong + offsets(zero + index * stride).toLong
    if (value == 0) this.elements.removeKey(i) else this.elements.put(i, value)
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
  protected def _offset(absRank: Int): Int = offsets(absRank)

  /**
   * Returns <tt>true</tt> if both matrices share at least one identical cell.
   */
  protected def haveSharedCellsRaw(other: IntMatrix1D): Boolean = {
    if (other.isInstanceOf[SelectedSparseIntMatrix1D]) {
      val otherMatrix = other.asInstanceOf[SelectedSparseIntMatrix1D]
      return this.elements == otherMatrix.elements
    } else if (other.isInstanceOf[SparseIntMatrix1D]) {
      val otherMatrix = other.asInstanceOf[SparseIntMatrix1D]
      return this.elements == otherMatrix.elements
    }
    false
  }

  /**
   * Sets up a matrix with a given number of cells.
   *
   * @param size
   *            the number of cells the matrix shall have.
   */
  protected def setUp(size: Int) {
    super.setUp(size)
    this.stride = 1
    this.offset = 0
  }

  /**
   * Construct and returns a new selection view.
   *
   * @param offsets
   *            the offsets of the visible elements.
   * @return a new view.
   */
  protected def viewSelectionLike(offsets: Array[Int]): IntMatrix1D = {
    new SelectedSparseIntMatrix1D(this.elements, offsets)
  }
}
