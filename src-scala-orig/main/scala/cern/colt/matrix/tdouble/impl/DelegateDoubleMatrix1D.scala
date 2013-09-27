package cern.colt.matrix.tdouble.impl

import cern.colt.function.tdouble.Function2
import cern.colt.function.tdouble.Function3
import cern.colt.matrix.tdouble.StrideMatrix1D
import cern.colt.matrix.tdouble.StrideMatrix2D
import cern.colt.matrix.tdouble.DoubleMatrix3D
//remove if not needed
import scala.collection.JavaConversions._

/**
 * 1-d matrix holding <tt>double</tt> elements; a view wrapping another 2-d
 * matrix and therefore delegating calls to it.
 *
 * @author wolfgang.hoschek@cern.ch
 * @version 1.0, 09/24/99
 * @author Piotr Wendykier (piotr.wendykier@gmail.com)
 */
@SerialVersionUID(1L)
class DelegateDoubleMatrix1D(newContent: StrideMatrix2D, protected var row: Int)
    extends StrideMatrix1D {

  protected var content: StrideMatrix2D = newContent

  if (row < 0 || row >= newContent.rows()) throw new IllegalArgumentException()

  setUp(newContent.columns())

  def getQuick(index: Int): Double = {
    synchronized {
      content.getQuick(row, index)
    }
  }

  def like(size: Int): StrideMatrix1D = content.like1D(size)

  def like2D(rows: Int, columns: Int): StrideMatrix2D = content.like(rows, columns)

  def setQuick(index: Int, value: Double) {
    synchronized {
      content.setQuick(row, index, value)
    }
  }

  def elements(): AnyRef = content.elements()

  override def forEachNonZero(f: Function2): StrideMatrix1D = {
    content.forEachNonZeroInRow(row, new Function3() {

      override def apply(rowIdx: Int, colIdx: Int, value: Double): Double = return f.apply(colIdx, value)
    })
    this
  }

  override def forEachNegativeValue(f: Function2): StrideMatrix1D = {
    content.forEachNonZeroInRow(row, new Function3() {

      override def apply(rowIdx: Int, colIdx: Int, value: Double): Double = {
        if (value < 0.0) return f.apply(colIdx, value) else return value
      }
    })
    this
  }

  override def forEachPositiveValue(f: Function2): StrideMatrix1D = {
    content.forEachNonZeroInRow(row, new Function3() {

      override def apply(rowIdx: Int, colIdx: Int, value: Double): Double = {
        if (value > 0.0) return f.apply(colIdx, value) else return value
      }
    })
    this
  }

  def reshape(rows: Int, columns: Int): StrideMatrix2D = {
    throw new IllegalArgumentException("This method is not supported.")
  }

  def reshape(slices: Int, rows: Int, columns: Int): DoubleMatrix3D = {
    throw new IllegalArgumentException("This method is not supported.")
  }

  protected def viewSelectionLike(offsets: Array[Int]): StrideMatrix1D = throw new InternalError()
}
