package cern.colt.matrix.impl

import cern.colt.matrix._
import scala.Tuple2

/**
 * 2-d matrix holding elements of a certain type; either a view wrapping another
 * matrix or a matrix whose views are wrappers.
 *<p>
 * IMPORTANT:
 * If you derive from this matrix, you MUST override the following methods:
 * <pre>
 *    getQuick()
 *    setQuick()
 *    like1D()
 *    like2D()
 *    vectorize()
 * </pre>
 *
 * @author wolfgang.hoschek@cern.ch
 * @version 1.0, 04/14/2000
 *
 * @author Piotr Wendykier (piotr.wendykier@gmail.com)
 */
@SerialVersionUID(1L)
class WrapperMatrix2D[@specialized T: Manifest: Numeric](protected val content: Matrix2D[T], rows: Int, columns: Int) extends RemappedMatrix2D[T] {

  isNoView = false
  if (content == null)
    throw new NullPointerException("Wrapped content matrix is null")
  try {
    setUp(rows, columns)
  }
  catch {
    case exc: IllegalArgumentException => if ("matrix too large" != exc.getMessage) throw exc
  }

  def this(content: Matrix2D[T]) {
    this(content, content.rows, content.columns)
  }

  override def allCellsAreSettable = content.allCellsAreSettable

  override def canSetCellAt(row: Int, column: Int): Boolean = { val t = remapIndexes(row, column); content.canSetCellAt(t._1, t._2) }

  protected def remapIndexes(row: Int, column: Int): Tuple2[Int, Int] = (row, column)

  def getQuick(row: Int, column: Int): T = {
    val t = remapIndexes(row, column)
    content.getQuick(t._1, t._2)
  }

  def setQuick(row: Int, column: Int, value: T) {
    val t = remapIndexes(row, column)
    content.setQuick(t._1, t._2, value)
  }

  def like2D(rows: Int, columns: Int): Matrix2D[T] = content.like2D(rows, columns)

  def like1D(size: Int): Matrix1D[T] = content.like1D(size)

  override def vectorize() = {
    val v = content.like1D(size.toInt)
    var idx = 0
    for (c <- 0 until columnsVar; r <- 0 until rowsVar) {
      v.setQuick(idx, getQuick(r, c))
      idx += 1
    }
    v
  }

  override def getStorageMatrix = content.getStorageMatrix

  /**
   * @return Returns true if this matrix uses a sparse representation for storing cell values
   */
  override def isSparse: Boolean = content.isSparse

  override def isRowMajor: Boolean = content.isRowMajor
}
