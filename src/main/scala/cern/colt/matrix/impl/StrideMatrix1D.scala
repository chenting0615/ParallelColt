package cern.colt.matrix.impl

import cern.colt.matrix.{Matrix, Matrix1D}

/**
 * Abstract base class for 1-d matrices (aka <i>vectors</i>) holding objects or
 * primitive data types such as <code>int</code>, <code>double</code>, etc.
 * This class employs strides within an element array for tracking cells.
 *
 * First see the <a href="package-summary.html">package summary</a> and javadoc
 * <a href="package-tree.html">tree view</a> to get the broad picture.
 * <p>
 * <b>Note that this implementation is not synchronized.</b>
 *
 * A matrix has a number of cells (its <i>size</i>), which are assigned upon
 * instance construction. Elements are accessed via zero based indexes. Legal
 * indexes are of the form <tt>[0..size()-1]</tt>. Any attempt to access an
 * element at a coordinate <tt>index&lt;0 || index&gt;=size()</tt> will throw an
 * <tt>IndexOutOfBoundsException</tt>.
 *
 * @author wolfgang.hoschek@cern.ch
 * @version 1.0, 09/24/99
 *
 * @author Piotr Wendykier (piotr.wendykier@gmail.com)
 */
@specialized
@SerialVersionUID(1L)
abstract class StrideMatrix1D[T: Manifest] extends AbstractMatrix1D[T] {

  /**
   the index of the first element
   */
  protected var zeroVar: Int = 0

  /**
   * the number of indexes between any two elements, i.e.
   * <tt>index(i+1) - index(i)</tt>.
   */
  protected var strideVar: Int = 1

  private var storageMatrix: Matrix[T] = this

  /**
   * Returns the position of the element with the given relative rank within
   * the (virtual or non-virtual) internal 1-dimensional array. You may want
   * to override this method for performance.
   *
   * @param index
   *            the index of the cell within this matrix.
   */
  protected def toRawIndex(index: Int): Long = zeroVar + index * strideVar

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

  /**
   * Constructs and returns a new <i>flip view</i>. What used to be index
   * <tt>0</tt> is now index <tt>size()-1</tt>, ..., what used to be index
   * <tt>size()-1</tt> is now index <tt>0</tt>. The returned view is backed by
   * this matrix, so changes in the returned view are reflected in this
   * matrix, and vice-versa.
   *
   * @return a new flip view.
   */
  override def viewFlip(): Matrix1D[T] = {
    view().vFlip().asInstanceOf[Matrix1D[T]]
  }

  /**
   * Constructs and returns a new <i>sub-range view</i> that is a
   * <tt>width</tt> sub matrix starting at <tt>index</tt>.
   *
   * Operations on the returned view can only be applied to the restricted
   * range. Any attempt to access coordinates not contained in the view will
   * throw an <tt>IndexOutOfBoundsException</tt>.
   * <p>
   * <b>Note that the view is really just a range restriction:</b> The
   * returned matrix is backed by this matrix, so changes in the returned
   * matrix are reflected in this matrix, and vice-versa.
   * <p>
   * The view contains the cells from <tt>index..index+width-1</tt>. and has
   * <tt>view.size() == width</tt>. A view's legal coordinates are again zero
   * based, as usual. In other words, legal coordinates of the view are
   * <tt>0 .. view.size()-1==width-1</tt>. As usual, any attempt to access a
   * cell at other coordinates will throw an
   * <tt>IndexOutOfBoundsException</tt>.
   *
   * @param index
   *            The index of the first cell.
   * @param width
   *            The width of the range.
   * @throws IndexOutOfBoundsException
   *             if <tt>index<0 || width<0 || index+width>size()</tt>.
   * @return the new view.
   *
   */
  def viewPart(index: Int, width: Int): Matrix1D[T] = {
    view().vPart(index, width).asInstanceOf[Matrix1D[T]]
  }

  /**
   * Constructs and returns a new <i>selection view</i> that is a matrix
   * holding the indicated cells. There holds
   * <tt>view.size() == indexes.length</tt> and
   * <tt>view.get(i) == this.get(indexes[i])</tt>. Indexes can occur multiple
   * times and can be in arbitrary order.
   * <p>
   * <b>Example:</b> <br>
   *
   * <pre>
   * 	 this     = (0,0,8,0,7)
   * 	 indexes  = (0,2,4,2)
   * 	 --&gt;
   * 	 view     = (0,8,7,8)
   *
   * </pre>
   *
   * Note that modifying <tt>indexes</tt> after this call has returned has no
   * effect on the view. The returned view is backed by this matrix, so
   * changes in the returned view are reflected in this matrix, and
   * vice-versa.
   *
   * @param indexes
   *            The indexes of the cells that shall be visible in the new
   *            view. To indicate that <i>all</i> cells shall be visible,
   *            simply set this parameter to <tt>null</tt>.
   * @return the new view.
   * @throws IndexOutOfBoundsException
   *             if <tt>!(0 <= indexes[i] < size())</tt> for any
   *             <tt>i=0..indexes.length()-1</tt>.
   */
  def viewSelection(indexes: Array[Int]): Matrix1D[T] = {
    if (indexes == null)
      return this
    checkIndexes(indexes)
    new WrapperMatrix1D[T](this) {
      override def remapIndex(index: Int) = indexes(index)
    }
  }

  /**
   * Constructs and returns a new <i>stride view</i> which is a sub matrix
   * consisting of every i-th cell. More specifically, the view has size
   * <tt>this.size()/stride</tt> holding cells <tt>this.get(i*stride)</tt> for
   * all <tt>i = 0..size()/stride - 1</tt>.
   *
   * @param stride
   *            the step factor.
   * @throws IndexOutOfBoundsException
   *             if <tt>stride <= 0</tt>.
   * @return the new view.
   *
   */
  def viewStrides(stride: Int): StrideMatrix1D[T] = {
    view().vStrides(stride).asInstanceOf[StrideMatrix1D[T]]
  }

  override def getStorageMatrix = storageMatrix

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
  protected def view(): StrideMatrix1D[T] = {
    val v = clone().asInstanceOf[StrideMatrix1D[T]]
    v.storageMatrix = this.getStorageMatrix
    v.isNoView = false
    v
  }
}
