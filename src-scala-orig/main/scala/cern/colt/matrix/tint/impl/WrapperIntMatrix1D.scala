package cern.colt.matrix.tint.impl

import cern.colt.matrix.tint.IntMatrix1D
import cern.colt.matrix.tint.IntMatrix2D
import cern.colt.matrix.tint.IntMatrix3D
//remove if not needed
import scala.collection.JavaConversions._

/**
 * 1-d matrix holding <tt>int</tt> elements; either a view wrapping another
 * matrix or a matrix whose views are wrappers.
 *
 * @author wolfgang.hoschek@cern.ch
 * @version 1.0, 09/24/99
 *
 * @author Piotr Wendykier (piotr.wendykier@gmail.com)
 * @version 1.1, 08/22/2007
 */
@SerialVersionUID(1L)
class WrapperIntMatrix1D(newContent: IntMatrix1D) extends IntMatrix1D {

  protected var content: IntMatrix1D = newContent

  if (newContent != null) setUp(newContent.size.toInt)

  /**
   * Returns the content of this matrix if it is a wrapper; or <tt>this</tt>
   * otherwise. Override this method in wrappers.
   */
  protected def getStorageMatrix(): IntMatrix1D = this.content

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
    synchronized {
      content.getQuick(index)
    }
  }

  def elements(): AnyRef = content.elements()

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
  def like(size: Int): IntMatrix1D = content.like(size)

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
  def like2D(rows: Int, columns: Int): IntMatrix2D = content.like2D(rows, columns)

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
    synchronized {
      content.setQuick(index, value)
    }
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
  def viewFlip(): IntMatrix1D = {
    val view = new WrapperIntMatrix1D(this) {

      private val serialVersionUID = 1L

      def getQuick(index: Int): Int = {
        synchronized {
          return content.getQuick(size - 1 - index)
        }
      }

      def setQuick(index: Int, value: Int) {
        synchronized {
          content.setQuick(size - 1 - index, value)
        }
      }

      def get(index: Int): Int = {
        synchronized {
          return content.get(size - 1 - index)
        }
      }

      def set(index: Int, value: Int) {
        synchronized {
          content.set(size - 1 - index, value)
        }
      }
    }
    view
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
  def viewPart(index: Int, width: Int): IntMatrix1D = {
    checkRange(index, width)
    val view = new WrapperIntMatrix1D(this) {

      private val serialVersionUID = 1L

      def getQuick(i: Int): Int = {
        synchronized {
          return content.getQuick(index + i)
        }
      }

      def setQuick(i: Int, value: Int) {
        synchronized {
          content.setQuick(index + i, value)
        }
      }

      def get(i: Int): Int = {
        synchronized {
          return content.get(index + i)
        }
      }

      def set(i: Int, value: Int) {
        synchronized {
          content.set(index + i, value)
        }
      }
    }
    view.setSize(width)
    view
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
  def viewSelection(indexes: Array[Int]): IntMatrix1D = {
    if (indexes == null) {
      indexes = Array.ofDim[Int](size)
      var i = size
      while (i >= 0) indexes(i) = i
    }
    checkIndexes(indexes)
    val idx = indexes
    val view = new WrapperIntMatrix1D(this) {

      private val serialVersionUID = 1L

      def getQuick(i: Int): Int = {
        synchronized {
          return content.getQuick(idx(i))
        }
      }

      def setQuick(i: Int, value: Int) {
        synchronized {
          content.setQuick(idx(i), value)
        }
      }

      def get(i: Int): Int = {
        synchronized {
          return content.get(idx(i))
        }
      }

      def set(i: Int, value: Int) {
        synchronized {
          content.set(idx(i), value)
        }
      }
    }
    view.setSize(indexes.length)
    view
  }

  /**
   * Construct and returns a new selection view.
   *
   * @param offsets
   *            the offsets of the visible elements.
   * @return a new view.
   */
  protected def viewSelectionLike(offsets: Array[Int]): IntMatrix1D = throw new InternalError()

  /**
   * Constructs and returns a new <i>stride view</i> which is a sub matrix
   * consisting of every i-th cell. More specifically, the view has size
   * <tt>this.size()/stride</tt> holding cells <tt>this.get(i*stride)</tt> for
   * all <tt>i = 0..size()/stride - 1</tt>.
   *
   * @param _stride
   *            the step factor.
   * @throws IndexOutOfBoundsException
   *             if <tt>stride <= 0</tt>.
   * @return the new view.
   *
   */
  def viewStrides(_stride: Int): IntMatrix1D = {
    if (stride <= 0) throw new IndexOutOfBoundsException("illegal stride: " + stride)
    val view = new WrapperIntMatrix1D(this) {

      private val serialVersionUID = 1L

      def getQuick(index: Int): Int = {
        synchronized {
          return content.getQuick(index * _stride)
        }
      }

      def setQuick(index: Int, value: Int) {
        synchronized {
          content.setQuick(index * _stride, value)
        }
      }

      def get(index: Int): Int = {
        synchronized {
          return content.get(index * _stride)
        }
      }

      def set(index: Int, value: Int) {
        synchronized {
          content.set(index * _stride, value)
        }
      }
    }
    view.setSize(size)
    if (size != 0) view.setSize((size - 1) / _stride + 1)
    view
  }
}
