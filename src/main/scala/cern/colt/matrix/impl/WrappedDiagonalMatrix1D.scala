package cern.colt.matrix.impl

import cern.colt.matrix.{Matrix2D, Matrix1D}

/**
 * 1-d matrix holding typed elements; a view wrapping the diagonal of another 2-d
 * matrix and therefore delegating calls to it.
 *
 * @author wolfgang.hoschek@cern.ch
 * @version 1.0, 09/24/99
 * @author Piotr Wendykier (piotr.wendykier@gmail.com)
 */
@specialized
@SerialVersionUID(1L)
class WrappedDiagonalMatrix1D[T: Manifest: Numeric](content2D: Matrix2D[T]) extends AbstractMatrix1D[T] {

  this.sizeVar = Math.min(content2D.rows, content2D.columns)
  this.isNoView = false

  def getQuick(index: Int): T = content2D.getQuick(index, index)

  def setQuick(index: Int, value: T) {
    content2D.setQuick(index, index, value)
  }

  /**
   * Sets all cells to the state specified by <tt>value</tt>.
   *
   * @param value
   * the value to be filled into the cells.
   * @return <tt>this</tt> (for convenience only).
   */
  override def assignConstant(value: T) = {
    for(idx <- 0 until sizeVar)
      content2D.setQuick(idx, idx, value)
    this
  }

  override def forEachNonZero(f: Function2[Int, T, T]) = {
    var i: Int = 0
    while (i < content2D.rows && i < content2D.columns) {
      val oldValue: T = content2D.getQuick(i, i)
      if (oldValue != zero) {
        val newValue: T = f.apply(i, oldValue)
        if (newValue != oldValue) {
          content2D.setQuick(i, i, newValue)
        }
      }
      i += 1
    }
    this
  }

  /**
   * @return Returns true if this matrix uses a sparse representation for storing cell values
   */
  override def isSparse: Boolean = content2D.isSparse

  override def like1D(size: Int) = content2D.like1D(size)

  override def like2D(rows: Int, columns: Int) = content2D.like2D(rows, columns)

  def reshape(rows: Int, columns: Int): Matrix2D[T] = {
    throw new IllegalArgumentException("This method is not supported.")
  }

  /**
   * Constructs and returns a new <i>stride view</i> which is a sub matrix
   * consisting of every i-th cell. More specifically, the view has size
   * <tt>this.size()/stride</tt> holding cells <tt>this.get(i*stride)</tt> for
   * all <tt>i = 0..size()/stride - 1</tt>.
   *
   * @param stride
   * the step factor.
   * @throws IndexOutOfBoundsException
   *         if <tt>stride <= 0</tt>.
   * @return the new view.
   *
   */
  def viewStrides(stride: Int): Matrix1D[T] = {
    new WrappedDiagonalMatrix1D[T](content2D.viewStrides(stride, stride))
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
   * this     = (0,0,8,0,7)
   * indexes  = (0,2,4,2)
   * --&gt;
   * view     = (0,8,7,8)
   *
   * </pre>
   *
   * Note that modifying <tt>indexes</tt> after this call has returned has no
   * effect on the view. The returned view is backed by this matrix, so
   * changes in the returned view are reflected in this matrix, and
   * vice-versa.
   *
   * @param indexes
   * The indexes of the cells that shall be visible in the new
   * view. To indicate that <i>all</i> cells shall be visible,
   * simply set this parameter to <tt>null</tt>.
   * @return the new view.
   * @throws IndexOutOfBoundsException
   *         if <tt>!(0 <= indexes[i] < size())</tt> for any
   *         <tt>i=0..indexes.length()-1</tt>.
   */
  def viewSelection(indexes: Array[Int]): Matrix1D[T] = {
    new WrappedDiagonalMatrix1D[T](content2D.viewSelection(indexes, indexes))
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
   * The index of the first cell.
   * @param width
   * The width of the range.
   * @throws IndexOutOfBoundsException
   *         if <tt>index<0 || width<0 || index+width>size()</tt>.
   * @return the new view.
   *
   */
  def viewPart(index: Int, width: Int) = {
    new WrappedDiagonalMatrix1D[T](content2D.viewPart(0, index, content2D.rows, width))
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
  def viewFlip() = {
    new WrappedDiagonalMatrix1D[T](content2D.viewColumnFlip())
  }
}
