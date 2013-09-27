package cern.colt.matrix.tdouble.impl

import cern.colt.function.tdouble.Function2
import cern.colt.matrix.tdouble.StrideMatrix1D
import cern.colt.matrix.tdouble.StrideMatrix2D
import cern.colt.matrix.tdouble.DoubleMatrix3D
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Selection view on dense 1-d matrices holding <tt>double</tt> elements. First
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
 *
 * @author Piotr Wendykier (piotr.wendykier@gmail.com)
 * @version 1.1, 08/22/2007
 */
@SerialVersionUID(1L)
class SelectedDenseDoubleMatrix1D protected (size: Int,
    protected var elements: Array[Double],
    zero: Int,
    stride: Int,
    protected var offsets: Array[Int],
    protected var offset: Int) extends StrideMatrix1D {

  setUp(size, zero, stride)

  this.isNoView = false

  /**
   * Constructs a matrix view with the given parameters.
   *
   * @param elements
   *            the cells.
   * @param offsets
   *            The indexes of the cells that shall be visible.
   */
  protected def this(elements: Array[Double], offsets: Array[Int]) {
    this(offsets.length, elements, 0, 1, offsets, 0)
  }

  def elements(): Array[Double] = elements

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
  def getQuick(index: Int): Double = {
    elements(offset + offsets(zero + index * stride))
  }

  /**
   * Returns the position of the element with the given relative rank within
   * the (virtual or non-virtual) internal 1-dimensional array. You may want
   * to override this method for performance.
   *
   * @param rank
   *            the rank of the element.
   */
  def index(rank: Int): Long = offset + offsets(zero + rank * stride)

  /**
   * Construct and returns a new empty matrix <i>of the same dynamic type</i>
   * as the receiver, having the specified size. For example, if the receiver
   * is an instance of type <tt>DenseDoubleMatrix1D</tt> the new matrix must
   * also be of type <tt>DenseDoubleMatrix1D</tt>, if the receiver is an
   * instance of type <tt>SparseDoubleMatrix1D</tt> the new matrix must also
   * be of type <tt>SparseDoubleMatrix1D</tt>, etc. In general, the new matrix
   * should have internal parametrization as similar as possible.
   *
   * @param size
   *            the number of cell the matrix shall have.
   * @return a new empty matrix of the same dynamic type.
   */
  def like(size: Int): StrideMatrix1D = new DenseMatrix1D(size)

  /**
   * Construct and returns a new 2-d matrix <i>of the corresponding dynamic
   * type</i>, entirelly independent of the receiver. For example, if the
   * receiver is an instance of type <tt>DenseDoubleMatrix1D</tt> the new
   * matrix must be of type <tt>DenseDoubleMatrix2D</tt>, if the receiver is
   * an instance of type <tt>SparseDoubleMatrix1D</tt> the new matrix must be
   * of type <tt>SparseDoubleMatrix2D</tt>, etc.
   *
   * @param rows
   *            the number of rows the matrix shall have.
   * @param columns
   *            the number of columns the matrix shall have.
   * @return a new matrix of the corresponding dynamic type.
   */
  def like2D(rows: Int, columns: Int): StrideMatrix2D = new DenseMatrix2D(rows, columns)

  def reshape(rows: Int, columns: Int): StrideMatrix2D = {
    if (rows * columns != size) {
      throw new IllegalArgumentException("rows*columns != size")
    }
    val M = new DenseMatrix2D(rows, columns)
    val elementsOther = M.elements().asInstanceOf[Array[Double]]
    val zeroOther = M.index(0, 0).toInt
    val rowStrideOther = M.rowStride()
    val colStrideOther = M.columnStride()
    var idxOther: Int = 0
    val idx = 0
    for (c <- 0 until columns) {
      idxOther = zeroOther + c * colStrideOther
      for (r <- 0 until rows) {
        elementsOther(idxOther) = getQuick(idx += 1)
        idxOther += rowStrideOther
      }
    }
    M
  }

  def reshape(slices: Int, rows: Int, columns: Int): DoubleMatrix3D = {
    if (slices * rows * columns != size) {
      throw new IllegalArgumentException("slices*rows*columns != size")
    }
    val M = new DenseDoubleMatrix3D(slices, rows, columns)
    val elementsOther = M.elements().asInstanceOf[Array[Double]]
    val zeroOther = M.index(0, 0, 0).toInt
    val sliceStrideOther = M.sliceStride()
    val rowStrideOther = M.rowStride()
    val colStrideOther = M.columnStride()
    var idxOther: Int = 0
    val idx = 0
    for (s <- 0 until slices; c <- 0 until columns) {
      idxOther = zeroOther + s * sliceStrideOther + c * colStrideOther
      for (r <- 0 until rows) {
        elementsOther(idxOther) = getQuick(idx += 1)
        idxOther += rowStrideOther
      }
    }
    M
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
  def setQuick(index: Int, value: Double) {
    elements(offset + offsets(zero + index * stride)) = value
  }

  override def forEachNonZero(f: Function2): StrideMatrix1D = {
    for (i <- 0 until offsets.length) {
      val oldValue = getQuick(i)
      val newValue = f.apply(i, oldValue)
      if (newValue != oldValue) setQuick(i, newValue)
    }
    this
  }

  override def forEachNegativeValue(f: Function2): StrideMatrix1D = {
    for (i <- 0 until offsets.length) {
      val oldValue = getQuick(i)
      if (oldValue < 0) {
        val newValue = f.apply(i, oldValue)
        if (newValue != oldValue) setQuick(i, newValue)
      }
    }
    this
  }

  override def forEachPositiveValue(f: Function2): StrideMatrix1D = {
    for (i <- 0 until offsets.length) {
      val oldValue = getQuick(i)
      if (oldValue > 0) {
        val newValue = f.apply(i, oldValue)
        if (newValue != oldValue) setQuick(i, newValue)
      }
    }
    this
  }

  /**
   * Returns the position of the given absolute rank within the (virtual or
   * non-virtual) internal 1-dimensional array. Default implementation.
   * Override, if necessary.
   *
   * @param absRank
   *            the absolute rank of the element.
   * @return the position.
   */
  protected def _offset(absRank: Int): Int = offsets(absRank)

  /**
   * Returns <tt>true</tt> if both matrices share at least one identical cell.
   */
  protected def haveSharedCellsRaw(other: StrideMatrix1D): Boolean = {
    if (other.isInstanceOf[SelectedDenseDoubleMatrix1D]) {
      val otherMatrix = other.asInstanceOf[SelectedDenseDoubleMatrix1D]
      return this.elements == otherMatrix.elements
    } else if (other.isInstanceOf[DenseMatrix1D]) {
      val otherMatrix = other.asInstanceOf[DenseMatrix1D]
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
  protected def viewSelectionLike(offsets: Array[Int]): StrideMatrix1D = {
    new SelectedDenseDoubleMatrix1D(this.elements, offsets)
  }
}
