package cern.colt.matrix.impl

import cern.colt.matrix._

/**
 * 1-d matrix wrapping another 1-d matrix; either a view wrapping another
 * matrix or a matrix whose views are wrappers.
 *
 * @author wolfgang.hoschek@cern.ch
 * @version 1.0, 09/24/99
 *
 * @author Piotr Wendykier (piotr.wendykier@gmail.com)
 */
@SerialVersionUID(1L)
class WrapperMatrix1D[@specialized T: Manifest: Numeric](protected val content1D: Matrix1D[T]) extends AbstractMatrix1D[T] {

  sizeVar = content1D.size.toInt

  def this(content: Matrix1D[T], size: Int) {
    this(content)
    this.sizeVar = size
  }

  override def allCellsAreSettable = content1D.allCellsAreSettable

  override def canSetCellAt(index: Int): Boolean = content1D.canSetCellAt(remapIndex(index))

  override def getStorageMatrix = this.content1D.getStorageMatrix

  protected def remapIndex(index: Int): Int = index

  def getQuick(index: Int): T = {
    content1D.getQuick(remapIndex(index))
  }

  def setQuick(index: Int, value: T) {
    content1D.setQuick(remapIndex(index), value)
  }

  def reshape(rows: Int, columns: Int) = {
    throw new IllegalArgumentException("This method is not supported.")
  }

  override def viewFlip(): WrapperMatrix1D[T] = {
    new WrapperMatrix1D[T](this) {
      override protected def remapIndex(index: Int): Int = size.toInt - 1 - index
    }
  }

  override def viewPart(indexOffset: Int, width: Int): WrapperMatrix1D[T] = {
    checkRange(indexOffset, width)
    val view = new WrapperMatrix1D[T](this, width) {
      override protected def remapIndex(i: Int): Int = indexOffset + i
    }
    view
  }

  override def viewSelection(indexes: Array[Int]): WrapperMatrix1D[T] = {
    if (indexes == null || indexes.length == 0)
      return this
    checkIndexes(indexes)
    val view = new WrapperMatrix1D[T](this, indexes.length) {
      override protected def remapIndex(index: Int): Int = indexes(index)
    }
    view
  }

  override def viewStrides(stride: Int): WrapperMatrix1D[T] = {
    if (stride <= 0)
      throw new IllegalArgumentException("illegal stride: " + stride)
    val viewSize = if (sizeVar != 0) (sizeVar - 1) / stride + 1 else sizeVar
    val view = new WrapperMatrix1D(this, viewSize) {
      override protected def remapIndex(index: Int): Int = index * stride
    }
    view
  }

  /**
   * @return Returns true if this matrix uses a sparse representation for storing cell values
   */
  override def isSparse: Boolean = content1D.isSparse

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
   * the number of cell the matrix shall have.
   * @return a new empty matrix of the same dynamic type.
   */
  def like1D(size: Int): Matrix1D[T] = content1D.like1D(size)

  def like2D(rows: Int, columns: Int): Matrix2D[T] = content1D.like2D(rows, columns)
}
