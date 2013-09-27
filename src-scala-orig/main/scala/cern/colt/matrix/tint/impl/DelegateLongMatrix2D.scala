package cern.colt.matrix.tint.impl

import cern.colt.matrix.tint.IntMatrix1D
import cern.colt.matrix.tint.IntMatrix2D
import cern.colt.matrix.tint.IntMatrix3D
//remove if not needed
import scala.collection.JavaConversions._

/**
 * 2-d matrix holding <tt>int</tt> elements; a view wrapping another 3-d matrix
 * and therefore delegating calls to it.
 *
 * @author Piotr Wendykier (piotr.wendykier@gmail.com)
 */
@SerialVersionUID(1L)
class DelegateIntMatrix2D(newContent: IntMatrix3D, protected var axis: Int, protected var index: Int)
    extends IntMatrix2D {

  protected var content: IntMatrix3D = newContent

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

  def getQuick(row: Int, column: Int): Int = {
    synchronized axis match {
      case 0 => content.getQuick(index, row, column)
      case 1 => content.getQuick(row, index, column)
      case 2 => content.getQuick(row, column, index)
      case _ => throw new IllegalArgumentException()
    }
  }

  def like(rows: Int, columns: Int): IntMatrix2D = content.like2D(rows, columns)

  def setQuick(row: Int, column: Int, value: Int) {
    synchronized axis match {
      case 0 => content.setQuick(index, row, column, value)
      case 1 => content.setQuick(row, index, column, value)
      case 2 => content.setQuick(row, column, index, value)
      case _ => throw new IllegalArgumentException()
    }
  }

  def viewColumn(column: Int): IntMatrix1D = {
    checkColumn(column)
    new WrapperIntMatrix2D(this).viewColumn(column)
  }

  def elements(): AnyRef = content.elements()

  protected def viewSelectionLike(rowOffsets: Array[Int], columnOffsets: Array[Int]): IntMatrix2D = {
    throw new InternalError()
  }

  def like1D(size: Int): IntMatrix1D = throw new InternalError()

  protected def like1D(size: Int, zero: Int, stride: Int): IntMatrix1D = throw new InternalError()

  def vectorize(): IntMatrix1D = {
    val v = new DenseIntMatrix1D(rows * columns)
    var idx = 0
    for (c <- 0 until columns; r <- 0 until rows) {
      v.setQuick(idx += 1, getQuick(r, c))
    }
    v
  }
}
