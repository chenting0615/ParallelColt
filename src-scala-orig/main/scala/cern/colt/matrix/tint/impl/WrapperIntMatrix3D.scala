package cern.colt.matrix.tint.impl

import cern.colt.matrix.tint.IntMatrix1D
import cern.colt.matrix.tint.IntMatrix2D
import cern.colt.matrix.tint.IntMatrix3D
//remove if not needed
import scala.collection.JavaConversions._

/**
 * 3-d matrix holding <tt>int</tt> elements; either a view wrapping another
 * matrix or a matrix whose views are wrappers.
 *
 *
 * @author Piotr Wendykier (piotr.wendykier@gmail.com)
 */
@SerialVersionUID(1L)
class WrapperIntMatrix3D(newContent: IntMatrix3D) extends IntMatrix3D {

  protected var content: IntMatrix3D = newContent

  if (newContent != null) try {
    setUp(newContent.slices(), newContent.rows(), newContent.columns())
  } catch {
    case exc: IllegalArgumentException => if ("matrix too large" != exc.getMessage) throw exc
  }

  def elements(): AnyRef = content.elements()

  def getQuick(slice: Int, row: Int, column: Int): Int = {
    synchronized {
      content.getQuick(slice, row, column)
    }
  }

  def like(slices: Int, rows: Int, columns: Int): IntMatrix3D = content.like(slices, rows, columns)

  def setQuick(slice: Int,
      row: Int,
      column: Int,
      value: Int) {
    synchronized {
      content.setQuick(slice, row, column, value)
    }
  }

  def vectorize(): IntMatrix1D = {
    val v = new DenseIntMatrix1D(size.toInt)
    val length = rows * columns
    for (s <- 0 until slices) {
      v.viewPart(s * length, length).assign(viewSlice(s).vectorize())
    }
    v
  }

  def viewColumn(column: Int): IntMatrix2D = {
    checkColumn(column)
    new DelegateIntMatrix2D(this, 2, column)
  }

  def viewColumnFlip(): IntMatrix3D = {
    if (columns == 0) return this
    val view = new WrapperIntMatrix3D(this) {

      private val serialVersionUID = 1L

      def getQuick(slice: Int, row: Int, column: Int): Int = {
        synchronized {
          return content.getQuick(slice, row, columns - 1 - column)
        }
      }

      def setQuick(slice: Int,
          row: Int,
          column: Int,
          value: Int) {
        synchronized {
          content.setQuick(slice, row, columns - 1 - column, value)
        }
      }

      def get(slice: Int, row: Int, column: Int): Int = {
        synchronized {
          return content.get(slice, row, columns - 1 - column)
        }
      }

      def set(slice: Int,
          row: Int,
          column: Int,
          value: Int) {
        synchronized {
          content.set(slice, row, columns - 1 - column, value)
        }
      }
    }
    view.isNoView = false
    view
  }

  def viewSlice(slice: Int): IntMatrix2D = {
    checkSlice(slice)
    new DelegateIntMatrix2D(this, 0, slice)
  }

  def viewSliceFlip(): IntMatrix3D = {
    if (slices == 0) return this
    val view = new WrapperIntMatrix3D(this) {

      private val serialVersionUID = 1L

      def getQuick(slice: Int, row: Int, column: Int): Int = {
        synchronized {
          return content.getQuick(slices - 1 - slice, row, column)
        }
      }

      def setQuick(slice: Int,
          row: Int,
          column: Int,
          value: Int) {
        synchronized {
          content.setQuick(slices - 1 - slice, row, column, value)
        }
      }

      def get(slice: Int, row: Int, column: Int): Int = {
        synchronized {
          return content.get(slices - 1 - slice, row, column)
        }
      }

      def set(slice: Int,
          row: Int,
          column: Int,
          value: Int) {
        synchronized {
          content.set(slices - 1 - slice, row, column, value)
        }
      }
    }
    view.isNoView = false
    view
  }

  def viewDice(axis0: Int, axis1: Int, axis2: Int): IntMatrix3D = {
    val d = 3
    if (axis0 < 0 || axis0 >= d || axis1 < 0 || axis1 >= d || axis2 < 0 ||
      axis2 >= d ||
      axis0 == axis1 ||
      axis0 == axis2 ||
      axis1 == axis2) {
      throw new IllegalArgumentException("Illegal Axes: " + axis0 + ", " + axis1 + ", " + axis2)
    }
    var view: WrapperIntMatrix3D = null
    if (axis0 == 0 && axis1 == 1 && axis2 == 2) {
      view = new WrapperIntMatrix3D(this)
    } else if (axis0 == 1 && axis1 == 0 && axis2 == 2) {
      view = new WrapperIntMatrix3D(this) {

        /**
         *
         */
        private val serialVersionUID = 1L

        def getQuick(slice: Int, row: Int, column: Int): Int = {
          synchronized {
            return content.getQuick(row, slice, column)
          }
        }

        def setQuick(slice: Int,
            row: Int,
            column: Int,
            value: Int) {
          synchronized {
            content.setQuick(row, slice, column, value)
          }
        }

        def get(slice: Int, row: Int, column: Int): Int = {
          synchronized {
            return content.get(row, slice, column)
          }
        }

        def set(slice: Int,
            row: Int,
            column: Int,
            value: Int) {
          synchronized {
            content.set(row, slice, column, value)
          }
        }
      }
    } else if (axis0 == 1 && axis1 == 2 && axis2 == 0) {
      view = new WrapperIntMatrix3D(this) {

        /**
         *
         */
        private val serialVersionUID = 1L

        def getQuick(slice: Int, row: Int, column: Int): Int = {
          synchronized {
            return content.getQuick(row, column, slice)
          }
        }

        def setQuick(slice: Int,
            row: Int,
            column: Int,
            value: Int) {
          synchronized {
            content.setQuick(row, column, slice, value)
          }
        }

        def get(slice: Int, row: Int, column: Int): Int = {
          synchronized {
            return content.get(row, column, slice)
          }
        }

        def set(slice: Int,
            row: Int,
            column: Int,
            value: Int) {
          synchronized {
            content.set(row, column, slice, value)
          }
        }
      }
    } else if (axis0 == 2 && axis1 == 1 && axis2 == 0) {
      view = new WrapperIntMatrix3D(this) {

        /**
         *
         */
        private val serialVersionUID = 1L

        def getQuick(slice: Int, row: Int, column: Int): Int = {
          synchronized {
            return content.getQuick(column, row, slice)
          }
        }

        def setQuick(slice: Int,
            row: Int,
            column: Int,
            value: Int) {
          synchronized {
            content.setQuick(column, row, slice, value)
          }
        }

        def get(slice: Int, row: Int, column: Int): Int = {
          synchronized {
            return content.get(column, row, slice)
          }
        }

        def set(slice: Int,
            row: Int,
            column: Int,
            value: Int) {
          synchronized {
            content.set(column, row, slice, value)
          }
        }
      }
    } else if (axis0 == 2 && axis1 == 0 && axis2 == 1) {
      view = new WrapperIntMatrix3D(this) {

        /**
         *
         */
        private val serialVersionUID = 1L

        def getQuick(slice: Int, row: Int, column: Int): Int = {
          synchronized {
            return content.getQuick(column, slice, row)
          }
        }

        def setQuick(slice: Int,
            row: Int,
            column: Int,
            value: Int) {
          synchronized {
            content.setQuick(column, slice, row, value)
          }
        }

        def get(slice: Int, row: Int, column: Int): Int = {
          synchronized {
            return content.get(column, slice, row)
          }
        }

        def set(slice: Int,
            row: Int,
            column: Int,
            value: Int) {
          synchronized {
            content.set(column, slice, row, value)
          }
        }
      }
    }
    val shape = shape()
    view.slices = shape(axis0)
    view.rows = shape(axis1)
    view.columns = shape(axis2)
    view.isNoView = false
    view
  }

  def viewPart(slice: Int,
      row: Int,
      column: Int,
      depth: Int,
      height: Int,
      width: Int): IntMatrix3D = {
    checkBox(slice, row, column, depth, height, width)
    val view = new WrapperIntMatrix3D(this) {

      private val serialVersionUID = 1L

      def getQuick(i: Int, j: Int, k: Int): Int = {
        synchronized {
          return content.getQuick(slice + i, row + j, column + k)
        }
      }

      def setQuick(i: Int,
          j: Int,
          k: Int,
          value: Int) {
        synchronized {
          content.setQuick(slice + i, row + j, column + k, value)
        }
      }

      def get(i: Int, j: Int, k: Int): Int = {
        synchronized {
          return content.get(slice + i, row + j, column + k)
        }
      }

      def set(i: Int,
          j: Int,
          k: Int,
          value: Int) {
        synchronized {
          content.set(slice + i, row + j, column + k, value)
        }
      }
    }
    view.slices = depth
    view.rows = height
    view.columns = width
    view.isNoView = false
    view
  }

  def viewRow(row: Int): IntMatrix2D = {
    checkRow(row)
    new DelegateIntMatrix2D(this, 1, row)
  }

  def viewRowFlip(): IntMatrix3D = {
    if (rows == 0) return this
    val view = new WrapperIntMatrix3D(this) {

      private val serialVersionUID = 1L

      def getQuick(slice: Int, row: Int, column: Int): Int = {
        synchronized {
          return content.getQuick(slice, rows - 1 - row, column)
        }
      }

      def setQuick(slice: Int,
          row: Int,
          column: Int,
          value: Int) {
        synchronized {
          content.setQuick(slice, rows - 1 - row, column, value)
        }
      }

      def get(slice: Int, row: Int, column: Int): Int = {
        synchronized {
          return content.get(slice, rows - 1 - row, column)
        }
      }

      def set(slice: Int,
          row: Int,
          column: Int,
          value: Int) {
        synchronized {
          content.set(slice, rows - 1 - row, column, value)
        }
      }
    }
    view.isNoView = false
    view
  }

  def viewSelection(sliceIndexes: Array[Int], rowIndexes: Array[Int], columnIndexes: Array[Int]): IntMatrix3D = {
    if (sliceIndexes == null) {
      sliceIndexes = Array.ofDim[Int](slices)
      var i = slices
      while (i >= 0) sliceIndexes(i) = i
    }
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
    checkSliceIndexes(sliceIndexes)
    checkRowIndexes(rowIndexes)
    checkColumnIndexes(columnIndexes)
    val six = sliceIndexes
    val rix = rowIndexes
    val cix = columnIndexes
    val view = new WrapperIntMatrix3D(this) {

      private val serialVersionUID = 1L

      def getQuick(i: Int, j: Int, k: Int): Int = {
        synchronized {
          return content.getQuick(six(i), rix(j), cix(k))
        }
      }

      def setQuick(i: Int,
          j: Int,
          k: Int,
          value: Int) {
        synchronized {
          content.setQuick(six(i), rix(j), cix(k), value)
        }
      }

      def get(i: Int, j: Int, k: Int): Int = {
        synchronized {
          return content.get(six(i), rix(j), cix(k))
        }
      }

      def set(i: Int,
          j: Int,
          k: Int,
          value: Int) {
        synchronized {
          content.set(six(i), rix(j), cix(k), value)
        }
      }
    }
    view.slices = sliceIndexes.length
    view.rows = rowIndexes.length
    view.columns = columnIndexes.length
    view.isNoView = false
    view
  }

  def viewStrides(_sliceStride: Int, _rowStride: Int, _columnStride: Int): IntMatrix3D = {
    if (_sliceStride <= 0 || _rowStride <= 0 || _columnStride <= 0) throw new IndexOutOfBoundsException("illegal stride")
    val view = new WrapperIntMatrix3D(this) {

      private val serialVersionUID = 1L

      def getQuick(slice: Int, row: Int, column: Int): Int = {
        synchronized {
          return content.getQuick(_sliceStride * slice, _rowStride * row, _columnStride * column)
        }
      }

      def setQuick(slice: Int,
          row: Int,
          column: Int,
          value: Int) {
        synchronized {
          content.setQuick(_sliceStride * slice, _rowStride * row, _columnStride * column, value)
        }
      }

      def get(slice: Int, row: Int, column: Int): Int = {
        synchronized {
          return content.get(_sliceStride * slice, _rowStride * row, _columnStride * column)
        }
      }

      def set(slice: Int,
          row: Int,
          column: Int,
          value: Int) {
        synchronized {
          content.set(_sliceStride * slice, _rowStride * row, _columnStride * column, value)
        }
      }
    }
    if (slices != 0) view.slices = (slices - 1) / _sliceStride + 1
    if (rows != 0) view.rows = (rows - 1) / _rowStride + 1
    if (columns != 0) view.columns = (columns - 1) / _columnStride + 1
    view.isNoView = false
    view
  }

  protected def getStorageMatrix(): IntMatrix3D = content

  def like2D(rows: Int, columns: Int): IntMatrix2D = throw new InternalError()

  protected def like2D(rows: Int,
      columns: Int,
      rowZero: Int,
      columnZero: Int,
      rowStride: Int,
      columnStride: Int): IntMatrix2D = throw new InternalError()

  protected def viewSelectionLike(sliceOffsets: Array[Int], rowOffsets: Array[Int], columnOffsets: Array[Int]): IntMatrix3D = {
    throw new InternalError()
  }
}
