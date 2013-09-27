package cern.colt.matrix.tdouble.impl

import cern.colt.matrix.tdouble.StrideMatrix1D
import cern.colt.matrix.tdouble.StrideMatrix2D
import cern.colt.matrix.tdouble.DoubleMatrix3D
//remove if not needed
import scala.collection.JavaConversions._

/**
 * 2-d matrix holding <tt>double</tt> elements; a view wrapping another 3-d
 * matrix and therefore delegating calls to it.
 *
 * @author Piotr Wendykier (piotr.wendykier@gmail.com)
 */
@SerialVersionUID(1L)
class DelegateDoubleMatrix2D(newContent: DoubleMatrix3D, protected var axis: Int, protected var index: Int)
    extends StrideMatrix2D {

  protected var content: DoubleMatrix3D = newContent

  axis match {
    case 0 =>
      if (index < 0 || index >= newContent.slices()) throw new IllegalArgumentException()
      setUp(newContent.rows(), newContent.columns())

    case 1 =>
      if (index < 0 || index >= newContent.rows()) throw new IllegalArgumentException()
      setUp(newContent.slices(), newContent.columns())

    case 2 =>
      if (index < 0 || index >= newContent.columns()) throw new IllegalArgumentException()
      setUp(newContent.slices(), newContent.rows())

    case _ => throw new IllegalArgumentException()
  }

  def getQuick(row: Int, column: Int): Double = {
    synchronized axis match {
      case 0 => content.getQuick(index, row, column)
      case 1 => content.getQuick(row, index, column)
      case 2 => content.getQuick(row, column, index)
      case _ => throw new IllegalArgumentException()
    }
  }

  def like(rows: Int, columns: Int): StrideMatrix2D = content.like2D(rows, columns)

  def setQuick(row: Int, column: Int, value: Double) {
    synchronized axis match {
      case 0 => content.setQuick(index, row, column, value)
      case 1 => content.setQuick(row, index, column, value)
      case 2 => content.setQuick(row, column, index, value)
      case _ => throw new IllegalArgumentException()
    }
  }

  def viewColumn(column: Int): StrideMatrix1D = {
    checkColumn(column)
    new WrapperMatrix2D(this).viewColumn(column)
  }

  def elements(): AnyRef = content.elements()

  protected def viewSelectionLike(rowOffsets: Array[Int], columnOffsets: Array[Int]): StrideMatrix2D = {
    throw new InternalError()
  }

  def like1D(size: Int): StrideMatrix1D = throw new InternalError()

  protected def like1D(size: Int, zero: Int, stride: Int): StrideMatrix1D = throw new InternalError()

  def vectorize(): StrideMatrix1D = {
    val v = new DenseMatrix1D(rows * columns)
    var idx = 0
    for (c <- 0 until columns; r <- 0 until rows) {
      v.setQuick(idx += 1, getQuick(r, c))
    }
    v
  }
}
