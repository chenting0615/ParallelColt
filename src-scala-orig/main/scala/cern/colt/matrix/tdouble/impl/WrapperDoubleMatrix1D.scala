package cern.colt.matrix.tdouble.impl

import cern.colt.function.tdouble.Function2
import cern.colt.matrix.tdouble.StrideMatrix1D
import cern.colt.matrix.tdouble.StrideMatrix2D
import cern.colt.matrix.tdouble.DoubleMatrix3D
//remove if not needed
import scala.collection.JavaConversions._

/**
 * 1-d matrix holding <tt>double</tt> elements; either a view wrapping another
 * matrix or a matrix whose views are wrappers.
 *
 * @author wolfgang.hoschek@cern.ch
 * @version 1.0, 09/24/99
 *
 * @author Piotr Wendykier (piotr.wendykier@gmail.com)
 */
@SerialVersionUID(1L)
class WrapperDoubleMatrix1D(newContent: StrideMatrix1D) extends StrideMatrix1D {

  protected var content: StrideMatrix1D = newContent

  if (newContent != null) setUp(newContent.size.toInt)

  protected def getStorageMatrix(): StrideMatrix1D = this.content

  def getQuick(index: Int): Double = {
    synchronized {
      content.getQuick(index)
    }
  }

  def elements(): AnyRef = content.elements()

  def like(size: Int): StrideMatrix1D = content.like(size)

  def like2D(rows: Int, columns: Int): StrideMatrix2D = content.like2D(rows, columns)

  def reshape(rows: Int, columns: Int): StrideMatrix2D = {
    throw new IllegalArgumentException("This method is not supported.")
  }

  def reshape(slices: Int, rows: Int, columns: Int): DoubleMatrix3D = {
    throw new IllegalArgumentException("This method is not supported.")
  }

  def setQuick(index: Int, value: Double) {
    synchronized {
      content.setQuick(index, value)
    }
  }

  def viewFlip(): StrideMatrix1D = {
    new RemappedDoubleMatrix1D(this) {

      private val serialVersionUID = 1L

      protected def remapIndex(index: Int): Int = size - 1 - index
    }
  }

  def viewPart(index: Int, width: Int): StrideMatrix1D = {
    checkRange(index, width)
    val view = new RemappedDoubleMatrix1D(this) {

      private val serialVersionUID = 1L

      protected def remapIndex(i: Int): Int = return index + i
    }
    view.size = width
    view
  }

  def viewSelection(indexes: Array[Int]): StrideMatrix1D = {
    if (indexes == null) {
      indexes = Array.ofDim[Int](size)
      var i = size
      while (i >= 0) indexes(i) = i
    }
    checkIndexes(indexes)
    val idx = indexes
    val view = new RemappedDoubleMatrix1D(this) {

      private val serialVersionUID = 1L

      protected def remapIndex(index: Int): Int = return idx(index)
    }
    view.size = indexes.length
    view
  }

  protected def viewSelectionLike(offsets: Array[Int]): StrideMatrix1D = throw new InternalError()

  @SerialVersionUID(1L)
  private abstract class RemappedDoubleMatrix1D(m: WrapperMatrix1D) extends WrapperMatrix1D(m) {

    protected def remapIndex(index: Int): Int

    def getQuick(index: Int): Double = content.getQuick(remapIndex(index))

    def setQuick(index: Int, value: Double) {
      content.setQuick(remapIndex(index), value)
    }

    def get(index: Int): Double = content.get(remapIndex(index))

    def set(index: Int, value: Double) {
      content.set(remapIndex(index), value)
    }

    override def forEachNonZero(f: Function2): StrideMatrix1D = {
      content.forEachNonZero(new Function2() {

        override def apply(index: Int, value: Double): Double = f.apply(remapIndex(index), value)
      })
    }

    override def forEachNegativeValue(f: Function2): StrideMatrix1D = {
      content.forEachNegativeValue(new Function2() {

        override def apply(index: Int, value: Double): Double = f.apply(remapIndex(index), value)
      })
    }

    override def forEachPositiveValue(f: Function2): StrideMatrix1D = {
      content.forEachPositiveValue(new Function2() {

        override def apply(index: Int, value: Double): Double = f.apply(remapIndex(index), value)
      })
    }
  }

  def viewStrides(_stride: Int): StrideMatrix1D = {
    if (stride <= 0) throw new IndexOutOfBoundsException("illegal stride: " + stride)
    val view = new RemappedDoubleMatrix1D(this) {

      private val serialVersionUID = 1L

      protected def remapIndex(index: Int): Int = return index * _stride
    }
    if (size != 0) view.size = (size - 1) / _stride + 1
    view
  }

  override def forEachNonZero(f: Function2): StrideMatrix1D = content.forEachNonZero(f)

  override def forEachNegativeValue(f: Function2): StrideMatrix1D = content.forEachNegativeValue(f)

  override def forEachPositiveValue(f: Function2): StrideMatrix1D = content.forEachPositiveValue(f)
}
