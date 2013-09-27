package cern.colt.matrix.tint.impl

import java.util.concurrent.Future
import cern.colt.list.tint.IntArrayList
import cern.colt.matrix.tint.IntMatrix1D
import cern.colt.matrix.tint.IntMatrix2D
import edu.emory.mathcs.utils.ConcurrencyUtils
//remove if not needed
import scala.collection.JavaConversions._

/**
 * 2-d matrix holding <tt>int</tt> elements; either a view wrapping another
 * matrix or a matrix whose views are wrappers.
 *
 * @author wolfgang.hoschek@cern.ch
 * @version 1.0, 04/14/2000
 *
 * @author Piotr Wendykier (piotr.wendykier@gmail.com)
 */
@SerialVersionUID(1L)
class WrapperIntMatrix2D(newContent: IntMatrix2D) extends IntMatrix2D {

  protected var content: IntMatrix2D = newContent

  if (newContent != null) try {
    setUp(newContent.rows(), newContent.columns())
  } catch {
    case exc: IllegalArgumentException => if ("matrix too large" != exc.getMessage) throw exc
  }

  def assign(y: IntMatrix2D, function: cern.colt.function.tint.IntIntFunction): IntMatrix2D = {
    checkShape(y)
    if (y.isInstanceOf[WrapperIntMatrix2D]) {
      val rowList = new IntArrayList()
      val columnList = new IntArrayList()
      val valueList = new IntArrayList()
      y.getNonZeros(rowList, columnList, valueList)
      assign(y, function, rowList, columnList)
    } else {
      super.assign(y, function)
    }
    this
  }

  def assign(values: Array[Int]): IntMatrix2D = {
    if (content.isInstanceOf[DiagonalIntMatrix2D]) {
      val dlength = content.asInstanceOf[DiagonalIntMatrix2D].dlength
      val elems = content.asInstanceOf[DiagonalIntMatrix2D].elements
      if (values.length != dlength) throw new IllegalArgumentException("Must have same length: length=" + values.length + " dlength=" +
        dlength)
      var nthreads = ConcurrencyUtils.getNumberOfThreads
      if ((nthreads > 1) && (dlength >= ConcurrencyUtils.getThreadsBeginN_2D)) {
        nthreads = Math.min(nthreads, dlength)
        val futures = Array.ofDim[Future](nthreads)
        val k = dlength / nthreads
        for (j <- 0 until nthreads) {
          val firstIdx = j * k
          val lastIdx = if ((j == nthreads - 1)) dlength else firstIdx + k
          futures(j) = ConcurrencyUtils.submit(new Runnable() {

            def run() {
              for (i <- firstIdx until lastIdx) {
                elems(i) = values(i)
              }
            }
          })
        }
        ConcurrencyUtils.waitForCompletion(futures)
      } else {
        for (i <- 0 until dlength) {
          elems(i) = values(i)
        }
      }
      this
    } else {
      super.assign(values)
    }
  }

  def elements(): AnyRef = content.elements()

  def getQuick(row: Int, column: Int): Int = {
    synchronized {
      content.getQuick(row, column)
    }
  }

  override def equals(value: Int): Boolean = {
    if (content.isInstanceOf[DiagonalIntMatrix2D]) {
      val elements = content.elements().asInstanceOf[Array[Int]]
      for (r <- 0 until elements.length) {
        val x = elements(r)
        val diff = value - x
        if (diff != 0) {
          return false
        }
      }
      true
    } else {
      super == value
    }
  }

  override def equals(obj: Any): Boolean = {
    if (content.isInstanceOf[DiagonalIntMatrix2D] && obj.isInstanceOf[DiagonalIntMatrix2D]) {
      if (this == obj) return true
      if (!(this != null && obj != null)) return false
      val A = content.asInstanceOf[DiagonalIntMatrix2D]
      val B = obj.asInstanceOf[DiagonalIntMatrix2D]
      if (A.columns() != B.columns() || A.rows() != B.rows() || A.diagonalIndex() != B.diagonalIndex() ||
        A.diagonalLength() != B.diagonalLength()) return false
      val AElements = A.elements()
      val BElements = B.elements()
      for (r <- 0 until AElements.length) {
        val x = AElements(r)
        val value = BElements(r)
        val diff = value - x
        if (diff != 0) {
          return false
        }
      }
      true
    } else {
      super == obj
    }
  }

  def like(rows: Int, columns: Int): IntMatrix2D = content.like(rows, columns)

  def like1D(size: Int): IntMatrix1D = content.like1D(size)

  def setQuick(row: Int, column: Int, value: Int) {
    synchronized {
      content.setQuick(row, column, value)
    }
  }

  def vectorize(): IntMatrix1D = {
    val v = new DenseIntMatrix1D(size.toInt)
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_2D)) {
      nthreads = Math.min(nthreads, columns)
      val futures = Array.ofDim[Future](nthreads)
      val k = columns / nthreads
      for (j <- 0 until nthreads) {
        val firstCol = j * k
        val lastCol = if ((j == nthreads - 1)) columns else firstCol + k
        val firstidx = j * k * rows
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            var idx = firstidx
            for (c <- firstCol until lastCol; r <- 0 until rows) {
              v.setQuick(idx += 1, getQuick(r, c))
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
    } else {
      var idx = 0
      for (c <- 0 until columns; r <- 0 until rows) {
        v.setQuick(idx += 1, getQuick(r, c))
      }
    }
    v
  }

  def viewColumn(column: Int): IntMatrix1D = viewDice().viewRow(column)

  def viewColumnFlip(): IntMatrix2D = {
    if (columns == 0) return this
    val view = new WrapperIntMatrix2D(this) {

      private val serialVersionUID = 1L

      def getQuick(row: Int, column: Int): Int = {
        synchronized {
          return content.getQuick(row, columns - 1 - column)
        }
      }

      def setQuick(row: Int, column: Int, value: Int) {
        synchronized {
          content.setQuick(row, columns - 1 - column, value)
        }
      }

      def get(row: Int, column: Int): Int = {
        synchronized {
          return content.get(row, columns - 1 - column)
        }
      }

      def set(row: Int, column: Int, value: Int) {
        synchronized {
          content.set(row, columns - 1 - column, value)
        }
      }
    }
    view.isNoView = false
    view
  }

  def viewDice(): IntMatrix2D = {
    val view = new WrapperIntMatrix2D(this) {

      private val serialVersionUID = 1L

      def getQuick(row: Int, column: Int): Int = {
        synchronized {
          return content.getQuick(column, row)
        }
      }

      def setQuick(row: Int, column: Int, value: Int) {
        synchronized {
          content.setQuick(column, row, value)
        }
      }

      def get(row: Int, column: Int): Int = {
        synchronized {
          return content.get(column, row)
        }
      }

      def set(row: Int, column: Int, value: Int) {
        synchronized {
          content.set(column, row, value)
        }
      }
    }
    view.rows = columns
    view.columns = rows
    view.isNoView = false
    view
  }

  def viewPart(row: Int,
      column: Int,
      height: Int,
      width: Int): IntMatrix2D = {
    checkBox(row, column, height, width)
    val view = new WrapperIntMatrix2D(this) {

      private val serialVersionUID = 1L

      def getQuick(i: Int, j: Int): Int = {
        synchronized {
          return content.getQuick(row + i, column + j)
        }
      }

      def setQuick(i: Int, j: Int, value: Int) {
        synchronized {
          content.setQuick(row + i, column + j, value)
        }
      }

      def get(i: Int, j: Int): Int = {
        synchronized {
          return content.get(row + i, column + j)
        }
      }

      def set(i: Int, j: Int, value: Int) {
        synchronized {
          content.set(row + i, column + j, value)
        }
      }
    }
    view.rows = height
    view.columns = width
    view.isNoView = false
    view
  }

  def viewRow(row: Int): IntMatrix1D = {
    checkRow(row)
    new DelegateIntMatrix1D(this, row)
  }

  def viewRowFlip(): IntMatrix2D = {
    if (rows == 0) return this
    val view = new WrapperIntMatrix2D(this) {

      private val serialVersionUID = 1L

      def getQuick(row: Int, column: Int): Int = {
        synchronized {
          return content.getQuick(rows - 1 - row, column)
        }
      }

      def setQuick(row: Int, column: Int, value: Int) {
        synchronized {
          content.setQuick(rows - 1 - row, column, value)
        }
      }

      def get(row: Int, column: Int): Int = {
        synchronized {
          return content.get(rows - 1 - row, column)
        }
      }

      def set(row: Int, column: Int, value: Int) {
        synchronized {
          content.set(rows - 1 - row, column, value)
        }
      }
    }
    view.isNoView = false
    view
  }

  def viewSelection(rowIndexes: Array[Int], columnIndexes: Array[Int]): IntMatrix2D = {
    if (rowIndexes == null) {
      rowIndexes = Array.ofDim[Int](rows)
      var i = rows
      while (i >= 0) rowIndexes(i) = i
    }
    if (columnIndexes == null) {
      columnIndexes = Array.ofDim[Int](columns)
      var i = columns
      while (i >= 0) columnIndexes(i) = i
    }
    checkRowIndexes(rowIndexes)
    checkColumnIndexes(columnIndexes)
    val rix = rowIndexes
    val cix = columnIndexes
    val view = new WrapperIntMatrix2D(this) {

      private val serialVersionUID = 1L

      def getQuick(i: Int, j: Int): Int = {
        synchronized {
          return content.getQuick(rix(i), cix(j))
        }
      }

      def setQuick(i: Int, j: Int, value: Int) {
        synchronized {
          content.setQuick(rix(i), cix(j), value)
        }
      }

      def get(i: Int, j: Int): Int = {
        synchronized {
          return content.get(rix(i), cix(j))
        }
      }

      def set(i: Int, j: Int, value: Int) {
        synchronized {
          content.set(rix(i), cix(j), value)
        }
      }
    }
    view.rows = rowIndexes.length
    view.columns = columnIndexes.length
    view.isNoView = false
    view
  }

  def viewStrides(_rowStride: Int, _columnStride: Int): IntMatrix2D = {
    if (_rowStride <= 0 || _columnStride <= 0) throw new IndexOutOfBoundsException("illegal stride")
    val view = new WrapperIntMatrix2D(this) {

      private val serialVersionUID = 1L

      def getQuick(row: Int, column: Int): Int = {
        synchronized {
          return content.getQuick(_rowStride * row, _columnStride * column)
        }
      }

      def setQuick(row: Int, column: Int, value: Int) {
        synchronized {
          content.setQuick(_rowStride * row, _columnStride * column, value)
        }
      }

      def get(row: Int, column: Int): Int = {
        synchronized {
          return content.get(_rowStride * row, _columnStride * column)
        }
      }

      def set(row: Int, column: Int, value: Int) {
        synchronized {
          content.set(_rowStride * row, _columnStride * column, value)
        }
      }
    }
    if (rows != 0) view.rows = (rows - 1) / _rowStride + 1
    if (columns != 0) view.columns = (columns - 1) / _columnStride + 1
    view.isNoView = false
    view
  }

  protected def getStorageMatrix(): IntMatrix2D = content

  protected def like1D(size: Int, offset: Int, stride: Int): IntMatrix1D = throw new InternalError()

  protected def viewSelectionLike(rowOffsets: Array[Int], columnOffsets: Array[Int]): IntMatrix2D = {
    throw new InternalError()
  }
}
