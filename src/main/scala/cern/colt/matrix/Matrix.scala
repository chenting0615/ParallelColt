package cern.colt.matrix

/**
 * Trait for arbitrary-dimensional matrices holding objects or
 * primitive data types such as <code>double</code>, <code>int</code>, etc. First
 * see the <a href="package-summary.html">package summary</a> and javadoc <a
 * href="package-tree.html">tree view</a> to get the broad picture.
 * <p>
 * <b>Note that this implementation is not synchronized.</b>
 *
 * @author wolfgang.hoschek@cern.ch
 * @version 1.0, 09/24/99
 */
@SerialVersionUID(1L)
trait Matrix[T] extends cern.colt.PersistentObject {

  protected var isNoView: Boolean = true

  /**
   * TODO: This returns a Long because rowsXcolumns can be >= 2**32.  However, this doesn't make sense
   * for 1D matrices.  Should we put this in Matrix1D and Matrix2D instead?
   *
   * Returns the number of cells in the matrix.
   */
  def size: Long

  /**
   * Returns the number of cells having non-zero values; ignores any tolerance.
   *
   * @return Number of non-zero cells
   */
  def numNonZero: Long

  /**
   * Returns whether the receiver is a view/wrapper or not.
   * TODO: Should this be a reference to the underlying storage matrix
   */
  def isView: Boolean = !this.isNoView

  def setIsView(isView: Boolean) {
    this.isNoView = ! isView
  }

  /**
   * @return Returns true if this matrix uses a sparse representation for storing cell values
   */
  def isSparse: Boolean

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
  def like1D(size: Int): Matrix1D[T]

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
  def like2D(rows: Int, columns: Int): Matrix2D[T]

  /**
   * Ensures that the receiver can hold at least the specified number of
   * non-zero (non-null) cells without needing to allocate new internal
   * memory. If necessary, allocates new internal memory and increases the
   * capacity of the receiver.
   * <p>
   * This default implementation does nothing. Override this method if
   * necessary.
   *
   * //@param minNonZeros
   *            the desired minimum number of non-zero (non-null) cells.
   */
/*
  def ensureCapacity(minNonZeros: Int) {
  }
*/

  /**
   * Releases any superfluous internal memory. An application can use this
   * operation to minimize the storage of the receiver.
   * <p>
   * This default implementation does nothing. Override this method if
   * necessary.
   */
  def trimToSize() {
  }

  /**
   * Returns <tt>true</tt> if both matrices share at least one identical cell
   * in storage space.
   */
  def haveSharedCells(other: Matrix[T]): Boolean = {
    if (other == null) return false
    if (this eq other) return true
    val thisStorage = getStorageMatrix
    val otherStorage = other.getStorageMatrix
    if ((otherStorage eq null) || (thisStorage eq null)) return false
    if (thisStorage eq otherStorage) return true
    if ((this eq thisStorage) && (other eq otherStorage)) return false
    thisStorage.haveSharedCells(otherStorage)
  }

  /**
   * Returns the matrix holding the storage for this matrix.  If this is a wrapper
   * or view, it will return the underlying matrix; or <tt>this</tt>
   * otherwise. Override this method in wrappers.
   */
  def getStorageMatrix: Matrix[T] = this

  def setStorageMatrix(m: Matrix[T]) {}

  /**
   * @return Return the MatrixFactory which can produce more matrices like this one.
   * TODO: Should this be available as a type class instead? Since it is based on the
   * creation context, instead of operation-calling context, probably not.
   */
  def getFactory: MatrixFactory

  protected def setFactory(f: MatrixFactory): Unit

  /**
   * @return Return the ParallelStrategy object used by this matrix.
   *         The ParallelStrategy manages the division of matrix operations into
   *         rows/columns.
   * TODO: Should this be available as a type class instead?
   * TODO: Is there a way to make the implicit type class lookup resolve to this
   * method?
   */
  def getParallelStrategy: ParallelStrategy

  def setParallelStrategy(s: ParallelStrategy): Unit

  /**
   * Compares this object against the specified object. The result is
   * <code>true</code> if and only if the argument is not <code>null</code>
   * and is at least a <code>Matrix</code> of the same dimensions (rows, columns, etc.)
   * and type [T] as the receiver and has exactly the same values at
   * the same coordinates.
   *
   * @param obj
   *            the object to compare with.
   * @return <code>true</code> if the objects are the same; <code>false</code>
   *         otherwise.
   */
  def equals(obj: Any): Boolean

  /**
   * Compares this object against the specified object. The result is
   * <code>true</code> if and only if the argument is not <code>null</code>
   * and is  a <code>Matrix</code> of the same dimensions (rows, columns, etc.)
   * and type [T] as the receiver and has the same values at
   * the same coordinates within the given tolerance.
   *
   * @param other
   *            the object to compare with.
   * @return <code>true</code> if the objects are the same within the given tolerance <code>false</code>
   *         otherwise.
   */
  def equals(other: Matrix[T], tolerance: Double): Boolean

  /**
   * Returns whether all cells are equal to the given value.
   *
   * @param value
   *            the value to test against.
   * @return <tt>true</tt> if all cells are equal to the given value,
   *         <tt>false</tt> otherwise.
   */
  def everyCellEquals(value: T): Boolean

  /**
   * Returns whether all cells are equal to the given value.
   *
   * @param value
   *            the value to test against, within the given tolerance.
   * @return <tt>true</tt> if all cells are equal to the given value,
   *         <tt>false</tt> otherwise.
   */
  def everyCellEquals(value: T, tolerance: Double): Boolean

  /**
   * Returns a string representation of the receiver's shape.
   */
  def toShapeString: String

  /**
   * Returns a string of the matrix type, size, and content using default formatting.
   */
  override def toString: String
}
