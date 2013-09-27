package cern.colt.matrix

/**
 * Abstract base class for 1-d matrices (aka <i>vectors</i>) holding objects or
 * primitive data types such as <code>int</code>, <code>double</code>, etc.
 * First see the <a href="package-summary.html">package summary</a> and javadoc
 * <a href="package-tree.html">tree view</a> to get the broad picture.
 * <p>
 * <b>Note that this implementation is not synchronized.</b>
 *
 * @author wolfgang.hoschek@cern.ch
 * @version 1.0, 09/24/99
 */
@specialized
@SerialVersionUID(1L)
abstract class AbstractMatrix1D[T] extends Matrix1D[T] {

  def resetZeroAndStride(zero: Int, stride: Int) {
    // TODO: Check zero and stride consistency
    zeroVar = zero
    strideVar = stride
  }

  /**
   the number of cells this matrix (view) has
   */
  protected var sizeVar: Int = 0

  /**
   the index of the first element
   */
  protected var zeroVar: Int = 0

  /**
   * the number of indexes between any two elements, i.e.
   * <tt>index(i+1) - index(i)</tt>.
   */
  protected var strideVar: Int = 0

  /**
   * Returns the position of the given absolute rank within the (virtual or
   * non-virtual) internal 1-dimensional array. Default implementation.
   * Override, if necessary.
   *
   * @param absRank
   *            the absolute rank of the element.
   * @return the position.
   */
  protected def _offset(absRank: Int): Int = absRank

  /**
   * Returns the absolute rank of the given relative rank.
   *
   * @param rank
   *            the relative rank of the element.
   * @return the absolute rank of the element.
   */
  protected def _rank(rank: Int): Int = zeroVar + rank * strideVar

  /**
   * Sanity check for operations requiring an index to be within bounds.
   *
   * @throws IndexOutOfBoundsException
   *             if <tt>index < 0 || index >= size()</tt>.
   */
  override protected def checkIndex(index: Int) {
    if (index < 0 || index >= sizeVar)
      throw new IndexOutOfBoundsException("Attempted to access " + toShapeString + " at index=" + index)
  }

  /**
   * Checks whether indexes are legal and throws an exception, if necessary.
   *
   * @throws IndexOutOfBoundsException
   *             if <tt>! (0 <= indexes[i] < size())</tt> for any
   *             i=0..indexes.length()-1.
   */
  override protected def checkIndexes(indexes: Array[Int]) {
    var i = indexes.length
    while (i >= 0) {
      val index = indexes(i)
      if (index < 0 || index >= sizeVar)
        throw new IndexOutOfBoundsException("Index " + i + ": Attempted to access " + toShapeString + " at index=" + index)
      i -= 1
    }
  }

  /**
   * Checks whether the receiver contains the given range and throws an
   * exception, if necessary.
   *
   * @throws IndexOutOfBoundsException
   *             if <tt>index<0 || index+width>size()</tt>.
   */
  override protected def checkRange(index: Int, width: Int) {
    if (index < 0 || index + width > sizeVar)
      throw new IndexOutOfBoundsException("index: " + index + ", width: " + width + ", size=" + sizeVar)
  }

  /**
   * Sanity check for operations requiring two matrices with the same size.
   *
   * @throws IllegalArgumentException
   *             if <tt>size() != B.size()</tt>.
   */
  override def checkSize(B: Matrix1D[T]) {
    if (sizeVar != B.size)
      throw new IllegalArgumentException("Incompatible sizes: " + toShapeString + " and " + B.toShapeString)
  }

  /**
   * Returns the position of the element with the given relative rank within
   * the (virtual or non-virtual) internal 1-dimensional array. You may want
   * to override this method for performance.
   *
   * @param rank
   *            the rank of the element.
   */
  def index(rank: Int): Long = _offset(_rank(rank))

  /**
   * Sets up a matrix with a given number of cells.
   *
   * @param size
   *            the number of cells the matrix shall have.
   * @throws IllegalArgumentException
   *             if <tt>size<0</tt>.
   */
  protected def setUp(size: Int) {
    setUp(size, 0, 1)
  }

  /**
   * Sets up a matrix with the given parameters.
   *
   * @param size
   *            the number of elements the matrix shall have.
   * @param zero
   *            the index of the first element.
   * @param stride
   *            the number of indexes between any two elements, i.e.
   *            <tt>index(i+1)-index(i)</tt>.
   * @throws IllegalArgumentException
   *             if <tt>size<0</tt>.
   */
  protected def setUp(size: Int, zero: Int, stride: Int) {
    if (size < 0) throw new IllegalArgumentException("negative size")
    this.sizeVar = size
    this.zeroVar = zero
    this.strideVar = stride
    this.isNoView = true
  }

  /**
   * Returns the number of cells.
   */
  override def size: Long = sizeVar

  /**
   * Returns the stride.
   */
  def stride: Int = strideVar

  /**
   * Self modifying version of viewFlip(). What used to be index <tt>0</tt> is
   * now index <tt>size()-1</tt>, ..., what used to be index <tt>size()-1</tt>
   * is now index <tt>0</tt>.
   */
  protected def vFlip(): AbstractMatrix1D[T] = {
    if (size > 0) {
      this.zeroVar += (this.sizeVar - 1) * this.strideVar
      this.strideVar = -this.strideVar
      this.isNoView = false
    }
    this
  }

  /**
   * Self modifying version of viewPart().
   *
   * @throws IndexOutOfBoundsException
   *             if <tt>index<0 || index+width>size()</tt>.
   */
  protected def vPart(index: Int, width: Int): AbstractMatrix1D[T] = {
    checkRange(index, width)
    this.zeroVar += this.strideVar * index
    this.sizeVar = width
    this.isNoView = false
    this
  }

  /**
   * Self modifying version of viewStrides().
   *
   * @throws IndexOutOfBoundsException
   *             if <tt>stride <= 0</tt>.
   */
  protected def vStrides(stride: Int): AbstractMatrix1D[T] = {
    if (stride <= 0)
      throw new IndexOutOfBoundsException("illegal stride: " + stride)
    this.strideVar *= stride
    if (this.sizeVar != 0)
      this.sizeVar = (this.sizeVar - 1) / stride + 1
    this.isNoView = false
    this
  }
}
