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
@specialized
@SerialVersionUID(1L)
class WrapperMatrix2D[T: Manifest: Numeric](protected val content: Matrix2D[T], rows: Int, columns: Int) extends RemappedMatrix2D[T] {

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
   * @return Return the ParallelStrategy object used by this matrix.
   *         The ParallelStrategy manages the division of matrix operations into
   *         rows/columns.
   *         TODO: Should this be available as a type class instead?
   *         TODO: Is there a way to make the implicit type class lookup resolve to this
   *         method?
   */
  override def getParallelStrategy = content.getParallelStrategy

  override def setParallelStrategy(s: ParallelStrategy) {}

  /**
   * @return Returns true if this matrix uses a sparse representation for storing cell values
   */
  override def isSparse: Boolean = content.isSparse

  override def isRowMajor: Boolean = content.isRowMajor

  /**
   * @return Return the MatrixFactory which can produce more matrices like this one.
   *         TODO: Should this be available as a type class instead? Since it is based on the
   *         creation context, instead of operation-calling context, probably not.
   */
  override def getFactory: MatrixFactory = content.getFactory

  override protected def setFactory(f: MatrixFactory) {}
}
