package cern.colt.matrix.tint.impl

import cern.colt.matrix.tint.IntMatrix1D
import cern.colt.matrix.tint.IntMatrix2D
//remove if not needed
import scala.collection.JavaConversions._

/**
 * 1-d matrix holding <tt>int</tt> elements; either a view wrapping another 2-d
 * matrix and therefore delegating calls to it.
 *
 * @author wolfgang.hoschek@cern.ch
 * @version 1.0, 09/24/99
 */
@SerialVersionUID(1L)
class DelegateIntMatrix1D(newContent: IntMatrix2D, protected var row: Int) extends WrapperIntMatrix1D(null) {

  protected var content: IntMatrix2D = newContent

  if (row < 0 || row >= newContent.rows()) throw new IllegalArgumentException()

  setUp(newContent.columns())

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
      content.getQuick(row, index)
    }
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
  def like(size: Int): IntMatrix1D = content.like1D(size)

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
  def like2D(rows: Int, columns: Int): IntMatrix2D = content.like(rows, columns)

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
      content.setQuick(row, index, value)
    }
  }
}
