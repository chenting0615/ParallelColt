package cern.colt.matrix.tint.impl

import java.io.IOException
import java.util.concurrent.Callable
import java.util.concurrent.ExecutionException
import java.util.concurrent.Future
import cern.colt.function.tint.IntFunction
import cern.colt.function.tint.IntIntFunction
import cern.colt.list.tint.IntArrayList
import cern.colt.matrix.io.MatrixInfo
import cern.colt.matrix.io.MatrixSize
import cern.colt.matrix.io.MatrixVectorReader
import cern.colt.matrix.tint.IntMatrix1D
import cern.colt.matrix.tint.IntMatrix2D
import edu.emory.mathcs.utils.ConcurrencyUtils
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Dense 2-d matrix holding <tt>int</tt> elements. First see the <a
 * href="package-summary.html">package summary</a> and javadoc <a
 * href="package-tree.html">tree view</a> to get the broad picture.
 * <p>
 * <b>Implementation:</b>
 * <p>
 * Internally holds one single contigous one-dimensional array, addressed in
 * column major. Note that this implementation is not synchronized.
 * <p>
 * <b>Time complexity:</b>
 * <p>
 * <tt>O(1)</tt> (i.e. constant time) for the basic operations <tt>get</tt>,
 * <tt>getQuick</tt>, <tt>set</tt>, <tt>setQuick</tt> and <tt>size</tt>,
 * <p>
 * Cells are internally addressed in column-major. Applications demanding utmost
 * speed can exploit this fact. Setting/getting values in a loop
 * column-by-column is quicker than row-by-row. Thus
 *
 * <pre>
 * for (int column = 0; column &lt; columns; column++) {
 *     for (int row = 0; row &lt; rows; row++) {
 *         matrix.setQuick(row, column, someValue);
 *     }
 * }
 *
 * </pre>
 *
 * is quicker than
 *
 * <pre>
 * for (int row = 0; row &lt; rows; row++) {
 *     for (int column = 0; column &lt; columns; column++) {
 *         matrix.setQuick(row, column, someValue);
 *     }
 * }
 *
 * </pre>
 *
 * @author Piotr Wendykier (piotr.wendykier@gmail.com)
 *
 */
@SerialVersionUID(1L)
class DenseColumnIntMatrix2D(rows: Int, columns: Int) extends IntMatrix2D {

  protected var elements: Array[Int] = new Array[Int](rows * columns)

  setUp(rows, columns, 0, 0, 1, rows)

  /**
   * Constructs a matrix with a copy of the given values. <tt>values</tt> is
   * required to have the form <tt>values[row][column]</tt> and have exactly
   * the same number of columns in every row.
   * <p>
   * The values are copied. So subsequent changes in <tt>values</tt> are not
   * reflected in the matrix, and vice-versa.
   *
   * @param values
   *            The values to be filled into the new matrix.
   * @throws IllegalArgumentException
   *             if
   *             <tt>for any 1 &lt;= row &lt; values.length: values[row].length != values[row-1].length</tt>
   *             .
   */
  def this(values: Array[Array[Int]]) {
    this(values.length, if (values.length == 0) 0 else values(0).length)
    assign(values)
  }

  /**
   * Constructs a matrix with the given parameters.
   *
   * @param rows
   *            the number of rows the matrix shall have.
   * @param columns
   *            the number of columns the matrix shall have.
   * @param elements
   *            the cells.
   * @param rowZero
   *            the position of the first element.
   * @param columnZero
   *            the position of the first element.
   * @param rowStride
   *            the number of elements between two rows, i.e.
   *            <tt>index(i+1,j)-index(i,j)</tt>.
   * @param columnStride
   *            the number of elements between two columns, i.e.
   *            <tt>index(i,j+1)-index(i,j)</tt>.
   * @param isView
   *            if true then a matrix view is constructed
   * @throws IllegalArgumentException
   *             if
   *             <tt>rows<0 || columns<0 || (double)columns*rows > Integer.MAX_VALUE</tt>
   *             or flip's are illegal.
   */
  def this(rows: Int,
      columns: Int,
      elements: Array[Int],
      rowZero: Int,
      columnZero: Int,
      rowStride: Int,
      columnStride: Int,
      isView: Boolean) {
    this()
    setUp(rows, columns, rowZero, columnZero, rowStride, columnStride)
    this.elements = elements
    this.isNoView = !isView
  }

  /**
   * Constructs a matrix from MatrixVectorReader.
   *
   * @param reader
   *            matrix reader
   * @throws IOException
   */
  def this(reader: MatrixVectorReader) {
    this()
    var info: MatrixInfo = null
    info = if (reader.hasInfo()) reader.readMatrixInfo() else new MatrixInfo(true, MatrixInfo.MatrixField.Real,
      MatrixInfo.MatrixSymmetry.General)
    if (info.isPattern) throw new UnsupportedOperationException("Pattern matrices are not supported")
    if (info.isDense) throw new UnsupportedOperationException("Dense matrices are not supported")
    if (info.isComplex) throw new UnsupportedOperationException("Complex matrices are not supported")
    val size = reader.readMatrixSize(info)
    setUp(size.numRows(), size.numColumns())
    this.elements = Array.ofDim[Int](rows * columns)
    val numEntries = size.numEntries()
    val columnIndexes = Array.ofDim[Int](numEntries)
    val rowIndexes = Array.ofDim[Int](numEntries)
    val values = Array.ofDim[Int](numEntries)
    reader.readCoordinate(rowIndexes, columnIndexes, values)
    for (i <- 0 until numEntries) {
      setQuick(rowIndexes(i), columnIndexes(i), values(i))
    }
    if (info.isSymmetric) {
      for (i <- 0 until numEntries if rowIndexes(i) != columnIndexes(i)) {
        setQuick(columnIndexes(i), rowIndexes(i), values(i))
      }
    } else if (info.isSkewSymmetric) {
      for (i <- 0 until numEntries if rowIndexes(i) != columnIndexes(i)) {
        setQuick(columnIndexes(i), rowIndexes(i), -values(i))
      }
    }
  }

  def aggregate(aggr: IntIntFunction, f: IntFunction): Int = {
    if (size == 0) throw new IllegalArgumentException("size() = 0")
    val zero = index(0, 0).toInt
    var a = 0
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_2D)) {
      nthreads = Math.min(nthreads, columns)
      val futures = Array.ofDim[Future](nthreads)
      val k = columns / nthreads
      for (j <- 0 until nthreads) {
        val firstColumn = columns - j * k
        val lastColumn = if ((j == (nthreads - 1))) 0 else firstColumn - k
        futures(j) = ConcurrencyUtils.submit(new Callable[Integer]() {

          def call(): java.lang.Integer = {
            var a = f.apply(elements(zero + (rows - 1) * rowStride + (firstColumn - 1) * columnStride))
            var d = 1
            var c = firstColumn
            while (c >= lastColumn) {
              var cidx = zero + c * columnStride
              var r = rows - d
              while (r >= 0) {
                a = aggr.apply(a, f.apply(elements(r * rowStride + cidx)))
              }
              d = 0
            }
            return a
          }
        })
      }
      a = ConcurrencyUtils.waitForCompletion(futures, aggr)
    } else {
      a = f.apply(elements(zero + (rows - 1) * rowStride + (columns - 1) * columnStride))
      var d = 1
      var c = columns
      while (c >= 0) {
        val cidx = zero + c * columnStride
        var r = rows - d
        while (r >= 0) {
          a = aggr.apply(a, f.apply(elements(r * rowStride + cidx)))
        }
        d = 0
      }
    }
    a
  }

  def aggregate(aggr: IntIntFunction, f: IntFunction, cond: IntProcedure): Int = {
    if (size == 0) throw new IllegalArgumentException("size() = 0")
    val zero = index(0, 0).toInt
    var a = 0
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_2D)) {
      nthreads = Math.min(nthreads, columns)
      val futures = Array.ofDim[Future](nthreads)
      val k = columns / nthreads
      for (j <- 0 until nthreads) {
        val firstColumn = columns - j * k
        val lastColumn = if ((j == (nthreads - 1))) 0 else firstColumn - k
        futures(j) = ConcurrencyUtils.submit(new Callable[Integer]() {

          def call(): java.lang.Integer = {
            var elem = elements(zero + (rows - 1) * rowStride + (firstColumn - 1) * columnStride)
            var a = 0
            if (cond.apply(elem) == true) {
              a = f.apply(elem)
            }
            var d = 1
            var c = firstColumn
            while (c >= lastColumn) {
              var cidx = zero + c * columnStride
              var r = rows - d
              while (r >= 0) {
                elem = elements(r * rowStride + cidx)
                if (cond.apply(elem) == true) {
                  a = aggr.apply(a, f.apply(elem))
                }
              }
              d = 0
            }
            return a
          }
        })
      }
      a = ConcurrencyUtils.waitForCompletion(futures, aggr)
    } else {
      var elem = elements(zero + (rows - 1) * rowStride + (columns - 1) * columnStride)
      if (cond.apply(elem) == true) {
        a = f.apply(elem)
      }
      var d = 1
      var c = columns
      while (c >= 0) {
        val cidx = zero + c * columnStride
        var r = rows - d
        while (r >= 0) {
          elem = elements(r * rowStride + cidx)
          if (cond.apply(elem) == true) {
            a = aggr.apply(a, f.apply(elem))
          }
        }
        d = 0
      }
    }
    a
  }

  def aggregate(aggr: IntIntFunction,
      f: IntFunction,
      rowList: IntArrayList,
      columnList: IntArrayList): Int = {
    if (size == 0) throw new IllegalArgumentException("size() = 0")
    val zero = index(0, 0).toInt
    val size = rowList.size
    val rowElements = rowList.elements()
    val columnElements = columnList.elements()
    var a = 0
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_2D)) {
      nthreads = Math.min(nthreads, columns)
      val futures = Array.ofDim[Future](nthreads)
      val k = size / nthreads
      for (j <- 0 until nthreads) {
        val firstIdx = size - j * k
        val lastIdx = if ((j == (nthreads - 1))) 0 else firstIdx - k
        futures(j) = ConcurrencyUtils.submit(new Callable[Integer]() {

          def call(): java.lang.Integer = {
            var a = f.apply(elements(zero + rowElements(firstIdx - 1) * rowStride + columnElements(firstIdx - 1) * columnStride))
            var i = firstIdx - 1
            while (i >= lastIdx) {
              a = aggr.apply(a, f.apply(elements(zero + rowElements(i) * rowStride + columnElements(i) * columnStride)))
            }
            return a
          }
        })
      }
      a = ConcurrencyUtils.waitForCompletion(futures, aggr)
    } else {
      a = f.apply(elements(zero + rowElements(size - 1) * rowStride + columnElements(size - 1) * columnStride))
      var i = size - 1
      while (i >= 0) {
        a = aggr.apply(a, f.apply(elements(zero + rowElements(i) * rowStride + columnElements(i) * columnStride)))
      }
    }
    a
  }

  def aggregate(other: IntMatrix2D, aggr: IntIntFunction, f: IntIntFunction): Int = {
    if (!(other.isInstanceOf[DenseColumnIntMatrix2D])) {
      return super.aggregate(other, aggr, f)
    }
    checkShape(other)
    if (size == 0) throw new IllegalArgumentException("size() = 0")
    val zero = index(0, 0).toInt
    val zeroOther = other.index(0, 0).toInt
    val rowStrideOther = other.rowStride()
    val columnStrideOther = other.columnStride()
    val otherElements = other.elements().asInstanceOf[Array[Int]]
    var a = 0
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_2D)) {
      nthreads = Math.min(nthreads, columns)
      val futures = Array.ofDim[Future](nthreads)
      val k = columns / nthreads
      for (j <- 0 until nthreads) {
        val firstColumn = columns - j * k
        val lastColumn = if ((j == (nthreads - 1))) 0 else firstColumn - k
        futures(j) = ConcurrencyUtils.submit(new Callable[Integer]() {

          def call(): java.lang.Integer = {
            var a = f.apply(elements(zero + (rows - 1) * rowStride + (firstColumn - 1) * columnStride),
              otherElements(zeroOther + (rows - 1) * rowStrideOther + (firstColumn - 1) * columnStrideOther))
            var d = 1
            var c = firstColumn
            while (c >= lastColumn) {
              var cidx = zero + c * columnStride
              var cidxOther = zeroOther + c * columnStrideOther
              var r = rows - d
              while (r >= 0) {
                a = aggr.apply(a, f.apply(elements(r * rowStride + cidx), otherElements(r * rowStrideOther + cidxOther)))
              }
              d = 0
            }
            return a
          }
        })
      }
      a = ConcurrencyUtils.waitForCompletion(futures, aggr)
    } else {
      var d = 1
      a = f.apply(elements(zero + (rows - 1) * rowStride + (columns - 1) * columnStride), otherElements(zeroOther + (rows - 1) * rowStrideOther + (columns - 1) * columnStrideOther))
      var c = columns
      while (c >= 0) {
        val cidx = zero + c * columnStride
        val cidxOther = zeroOther + c * columnStrideOther
        var r = rows - d
        while (r >= 0) {
          a = aggr.apply(a, f.apply(elements(r * rowStride + cidx), otherElements(r * rowStrideOther + cidxOther)))
        }
        d = 0
      }
    }
    a
  }

  def assign(function: IntFunction): IntMatrix2D = {
    if (function.isInstanceOf[cern.jet.math.tint.IntMult]) {
      val multiplicator = function.asInstanceOf[cern.jet.math.tint.IntMult].multiplicator
      if (multiplicator == 1) return this
      if (multiplicator == 0) return assign(0)
    }
    val zero = index(0, 0).toInt
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_2D)) {
      nthreads = Math.min(nthreads, columns)
      val futures = Array.ofDim[Future](nthreads)
      val k = columns / nthreads
      for (j <- 0 until nthreads) {
        val firstColumn = columns - j * k
        val lastColumn = if ((j == (nthreads - 1))) 0 else firstColumn - k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            var idx = zero + (rows - 1) * rowStride + (firstColumn - 1) * columnStride
            if (function.isInstanceOf[cern.jet.math.tint.IntMult]) {
              var multiplicator = function.asInstanceOf[cern.jet.math.tint.IntMult].multiplicator
              var c = firstColumn
              while (c >= lastColumn) {
                var i = idx
                var r = rows
                while (r >= 0) {
                  elements(i) *= multiplicator
                  i -= rowStride
                }
                idx -= columnStride
              }
            } else {
              var c = firstColumn
              while (c >= lastColumn) {
                var i = idx
                var r = rows
                while (r >= 0) {
                  elements(i) = function.apply(elements(i))
                  i -= rowStride
                }
                idx -= columnStride
              }
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
    } else {
      var idx = zero + (rows - 1) * rowStride + (columns - 1) * columnStride
      if (function.isInstanceOf[cern.jet.math.tint.IntMult]) {
        val multiplicator = function.asInstanceOf[cern.jet.math.tint.IntMult].multiplicator
        var c = columns
        while (c >= 0) {
          var i = idx
          var r = rows
          while (r >= 0) {
            elements(i) *= multiplicator
            i -= rowStride
          }
          idx -= columnStride
        }
      } else {
        var c = columns
        while (c >= 0) {
          var i = idx
          var r = rows
          while (r >= 0) {
            elements(i) = function.apply(elements(i))
            i -= rowStride
          }
          idx -= columnStride
        }
      }
    }
    this
  }

  def assign(cond: IntProcedure, function: IntFunction): IntMatrix2D = {
    val zero = index(0, 0).toInt
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_2D)) {
      nthreads = Math.min(nthreads, columns)
      val futures = Array.ofDim[Future](nthreads)
      val k = columns / nthreads
      for (j <- 0 until nthreads) {
        val firstColumn = columns - j * k
        val lastColumn = if ((j == (nthreads - 1))) 0 else firstColumn - k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            var elem: Int = 0
            var idx = zero + (rows - 1) * rowStride + (firstColumn - 1) * columnStride
            var c = firstColumn
            while (c >= lastColumn) {
              var i = idx
              var r = rows
              while (r >= 0) {
                elem = elements(i)
                if (cond.apply(elem) == true) {
                  elements(i) = function.apply(elem)
                }
                i -= rowStride
              }
              idx -= columnStride
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
    } else {
      var elem: Int = 0
      var idx = zero + (rows - 1) * rowStride + (columns - 1) * columnStride
      var c = columns
      while (c >= 0) {
        var i = idx
        var r = rows
        while (r >= 0) {
          elem = elements(i)
          if (cond.apply(elem) == true) {
            elements(i) = function.apply(elem)
          }
          i -= rowStride
        }
        idx -= columnStride
      }
    }
    this
  }

  def assign(cond: IntProcedure, value: Int): IntMatrix2D = {
    val zero = index(0, 0).toInt
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_2D)) {
      nthreads = Math.min(nthreads, columns)
      val futures = Array.ofDim[Future](nthreads)
      val k = columns / nthreads
      for (j <- 0 until nthreads) {
        val firstColumn = columns - j * k
        val lastColumn = if ((j == (nthreads - 1))) 0 else firstColumn - k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            var elem: Int = 0
            var idx = zero + (rows - 1) * rowStride + (firstColumn - 1) * columnStride
            var c = firstColumn
            while (c >= lastColumn) {
              var i = idx
              var r = rows
              while (r >= 0) {
                elem = elements(i)
                if (cond.apply(elem) == true) {
                  elements(i) = value
                }
                i -= rowStride
              }
              idx -= columnStride
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
    } else {
      var elem: Int = 0
      var idx = zero + (rows - 1) * rowStride + (columns - 1) * columnStride
      var c = columns
      while (c >= 0) {
        var i = idx
        var r = rows
        while (r >= 0) {
          elem = elements(i)
          if (cond.apply(elem) == true) {
            elements(i) = value
          }
          i -= rowStride
        }
        idx -= columnStride
      }
    }
    this
  }

  def assign(value: Int): IntMatrix2D = {
    val zero = index(0, 0).toInt
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_2D)) {
      nthreads = Math.min(nthreads, columns)
      val futures = Array.ofDim[Future](nthreads)
      val k = columns / nthreads
      for (j <- 0 until nthreads) {
        val firstColumn = columns - j * k
        val lastColumn = if ((j == (nthreads - 1))) 0 else firstColumn - k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            var idx = zero + (rows - 1) * rowStride + (firstColumn - 1) * columnStride
            var c = firstColumn
            while (c >= lastColumn) {
              var i = idx
              var r = rows
              while (r >= 0) {
                elements(i) = value
                i -= rowStride
              }
              idx -= columnStride
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
    } else {
      var idx = zero + (rows - 1) * rowStride + (columns - 1) * columnStride
      var c = columns
      while (c >= 0) {
        var i = idx
        var r = rows
        while (r >= 0) {
          elements(i) = value
          i -= rowStride
        }
        idx -= columnStride
      }
    }
    this
  }

  def assign(values: Array[Int]): IntMatrix2D = {
    if (values.length != size) throw new IllegalArgumentException("Must have same length: length=" + values.length + " rows()*columns()=" +
      rows() * columns())
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if (this.isNoView) {
      System.arraycopy(values, 0, this.elements, 0, values.length)
    } else {
      val zero = index(0, 0).toInt
      if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_2D)) {
        nthreads = Math.min(nthreads, columns)
        val futures = Array.ofDim[Future](nthreads)
        val k = columns / nthreads
        for (j <- 0 until nthreads) {
          val firstColumn = columns - j * k
          val lastColumn = if ((j == (nthreads - 1))) 0 else firstColumn - k
          futures(j) = ConcurrencyUtils.submit(new Runnable() {

            def run() {
              var idx = zero + (rows - 1) * rowStride + (firstColumn - 1) * columnStride
              var idxOther = (rows - 1) + (firstColumn - 1) * rows
              var c = firstColumn
              while (c >= lastColumn) {
                var i = idx
                var r = rows
                while (r >= 0) {
                  elements(i) = values(idxOther -= 1)
                  i -= rowStride
                }
                idx -= columnStride
              }
            }
          })
        }
        ConcurrencyUtils.waitForCompletion(futures)
      } else {
        var idx = zero + (rows - 1) * rowStride + (columns - 1) * columnStride
        val idxOther = values.length - 1
        var c = columns
        while (c >= 0) {
          var i = idx
          var r = rows
          while (r >= 0) {
            elements(i) = values(idxOther -= 1)
            i -= rowStride
          }
          idx -= columnStride
        }
      }
    }
    this
  }

  def assign(values: Array[Array[Int]]): IntMatrix2D = {
    if (values.length != rows) throw new IllegalArgumentException("Must have same number of rows: rows=" + values.length +
      "columns()=" +
      rows())
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    val zero = index(0, 0).toInt
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_2D)) {
      nthreads = Math.min(nthreads, columns)
      val futures = Array.ofDim[Future](nthreads)
      val k = rows / nthreads
      for (j <- 0 until nthreads) {
        val firstRow = rows - j * k
        val lastRow = if ((j == (nthreads - 1))) 0 else firstRow - k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            var idx = zero + (firstRow - 1) * rowStride + (columns - 1) * columnStride
            var r = firstRow
            while (r >= lastRow) {
              var currentRow = values(r)
              if (currentRow.length != columns) throw new IllegalArgumentException("Must have same number of columns in every row: column=" +
                currentRow.length +
                "columns()=" +
                columns())
              var i = idx
              var c = columns
              while (c >= 0) {
                elements(i) = currentRow(c)
                i -= columnStride
              }
              idx -= rowStride
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
    } else {
      var idx = zero + (rows - 1) * rowStride + (columns - 1) * columnStride
      var r = rows
      while (r >= 0) {
        val currentRow = values(r)
        if (currentRow.length != columns) throw new IllegalArgumentException("Must have same number of columns in every row: column=" +
          currentRow.length +
          "columns()=" +
          columns())
        var i = idx
        var c = columns
        while (c >= 0) {
          elements(i) = currentRow(c)
          i -= columnStride
        }
        idx -= rowStride
      }
    }
    this
  }

  def assign(source: IntMatrix2D): IntMatrix2D = {
    if (!(source.isInstanceOf[DenseColumnIntMatrix2D])) {
      super.assign(source)
      return this
    }
    var other = source.asInstanceOf[DenseColumnIntMatrix2D]
    if (other == this) return this
    checkShape(other)
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if (this.isNoView && other.isNoView) {
      System.arraycopy(other.elements, 0, elements, 0, elements.length)
      return this
    }
    if (haveSharedCells(other)) {
      val c = other.copy()
      if (!(c.isInstanceOf[DenseColumnIntMatrix2D])) {
        super.assign(other)
        return this
      }
      other = c.asInstanceOf[DenseColumnIntMatrix2D]
    }
    val zeroOther = other.index(0, 0).toInt
    val zero = index(0, 0).toInt
    val columnStrideOther = other.columnStride
    val rowStrideOther = other.rowStride
    val otherElements = other.elements
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_2D)) {
      nthreads = Math.min(nthreads, columns)
      val futures = Array.ofDim[Future](nthreads)
      val k = columns / nthreads
      for (j <- 0 until nthreads) {
        val firstColumn = columns - j * k
        val lastColumn = if ((j == (nthreads - 1))) 0 else firstColumn - k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            var idx = zero + (rows - 1) * rowStride + (firstColumn - 1) * columnStride
            var idxOther = zeroOther + (rows - 1) * rowStrideOther + (firstColumn - 1) * columnStrideOther
            var c = firstColumn
            while (c >= lastColumn) {
              var i = idx
              var j = idxOther
              var r = rows
              while (r >= 0) {
                elements(i) = otherElements(j)
                i -= rowStride
                j -= rowStrideOther
              }
              idx -= columnStride
              idxOther -= columnStrideOther
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
    } else {
      var idx = zero + (rows - 1) * rowStride + (columns - 1) * columnStride
      var idxOther = zeroOther + (rows - 1) * rowStrideOther + (columns - 1) * columnStrideOther
      var c = columns
      while (c >= 0) {
        var i = idx
        var j = idxOther
        var r = rows
        while (r >= 0) {
          elements(i) = otherElements(j)
          i -= rowStride
          j -= rowStrideOther
        }
        idx -= columnStride
        idxOther -= columnStrideOther
      }
    }
    this
  }

  def assign(y: IntMatrix2D, function: IntIntFunction): IntMatrix2D = {
    if (function.isInstanceOf[cern.jet.math.tint.IntPlusMultSecond]) {
      val multiplicator = function.asInstanceOf[cern.jet.math.tint.IntPlusMultSecond].multiplicator
      if (multiplicator == 0) {
        return this
      }
    }
    if (function.isInstanceOf[cern.jet.math.tint.IntPlusMultFirst]) {
      val multiplicator = function.asInstanceOf[cern.jet.math.tint.IntPlusMultFirst].multiplicator
      if (multiplicator == 0) {
        return assign(y)
      }
    }
    if (!(y.isInstanceOf[DenseColumnIntMatrix2D])) {
      super.assign(y, function)
      return this
    }
    val other = y.asInstanceOf[DenseColumnIntMatrix2D]
    checkShape(y)
    val otherElements = other.elements
    val zeroOther = other.index(0, 0).toInt
    val zero = index(0, 0).toInt
    val columnStrideOther = other.columnStride
    val rowStrideOther = other.rowStride
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_2D)) {
      nthreads = Math.min(nthreads, columns)
      val futures = Array.ofDim[Future](nthreads)
      val k = columns / nthreads
      for (j <- 0 until nthreads) {
        val firstColumn = columns - j * k
        val lastColumn = if ((j == (nthreads - 1))) 0 else firstColumn - k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            var idx = zero + (rows - 1) * rowStride + (firstColumn - 1) * columnStride
            var idxOther = zeroOther + (rows - 1) * rowStrideOther + (firstColumn - 1) * columnStrideOther
            if (function == cern.jet.math.tint.IntFunctions.mult) {
              var c = firstColumn
              while (c >= lastColumn) {
                var i = idx
                var j = idxOther
                var r = rows
                while (r >= 0) {
                  elements(i) *= otherElements(j)
                  i -= rowStride
                  j -= rowStrideOther
                }
                idx -= columnStride
                idxOther -= columnStrideOther
              }
            } else if (function == cern.jet.math.tint.IntFunctions.div) {
              var c = firstColumn
              while (c >= lastColumn) {
                var i = idx
                var j = idxOther
                var r = rows
                while (r >= 0) {
                  elements(i) /= otherElements(j)
                  i -= rowStride
                  j -= rowStrideOther
                }
                idx -= columnStride
                idxOther -= columnStrideOther
              }
            } else if (function.isInstanceOf[cern.jet.math.tint.IntPlusMultSecond]) {
              var multiplicator = function.asInstanceOf[cern.jet.math.tint.IntPlusMultSecond].multiplicator
              if (multiplicator == 1) {
                var c = firstColumn
                while (c >= lastColumn) {
                  var i = idx
                  var j = idxOther
                  var r = rows
                  while (r >= 0) {
                    elements(i) += otherElements(j)
                    i -= rowStride
                    j -= rowStrideOther
                  }
                  idx -= columnStride
                  idxOther -= columnStrideOther
                }
              } else if (multiplicator == -1) {
                var c = firstColumn
                while (c >= lastColumn) {
                  var i = idx
                  var j = idxOther
                  var r = rows
                  while (r >= 0) {
                    elements(i) -= otherElements(j)
                    i -= rowStride
                    j -= rowStrideOther
                  }
                  idx -= columnStride
                  idxOther -= columnStrideOther
                }
              } else {
                var c = firstColumn
                while (c >= lastColumn) {
                  var i = idx
                  var j = idxOther
                  var r = rows
                  while (r >= 0) {
                    elements(i) += multiplicator * otherElements(j)
                    i -= rowStride
                    j -= rowStrideOther
                  }
                  idx -= columnStride
                  idxOther -= columnStrideOther
                }
              }
            } else if (function.isInstanceOf[cern.jet.math.tint.IntPlusMultFirst]) {
              var multiplicator = function.asInstanceOf[cern.jet.math.tint.IntPlusMultFirst].multiplicator
              if (multiplicator == 1) {
                var c = firstColumn
                while (c >= lastColumn) {
                  var i = idx
                  var j = idxOther
                  var r = rows
                  while (r >= 0) {
                    elements(i) += otherElements(j)
                    i -= rowStride
                    j -= rowStrideOther
                  }
                  idx -= columnStride
                  idxOther -= columnStrideOther
                }
              } else if (multiplicator == -1) {
                var c = firstColumn
                while (c >= lastColumn) {
                  var i = idx
                  var j = idxOther
                  var r = rows
                  while (r >= 0) {
                    elements(i) = otherElements(j) - elements(i)
                    i -= rowStride
                    j -= rowStrideOther
                  }
                  idx -= columnStride
                  idxOther -= columnStrideOther
                }
              } else {
                var c = firstColumn
                while (c >= lastColumn) {
                  var i = idx
                  var j = idxOther
                  var r = rows
                  while (r >= 0) {
                    elements(i) = multiplicator * elements(i) + otherElements(j)
                    i -= rowStride
                    j -= rowStrideOther
                  }
                  idx -= columnStride
                  idxOther -= columnStrideOther
                }
              }
            } else {
              var c = firstColumn
              while (c >= lastColumn) {
                var i = idx
                var j = idxOther
                var r = rows
                while (r >= 0) {
                  elements(i) = function.apply(elements(i), otherElements(j))
                  i -= rowStride
                  j -= rowStrideOther
                }
                idx -= columnStride
                idxOther -= columnStrideOther
              }
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
    } else {
      var idx = zero + (rows - 1) * rowStride + (columns - 1) * columnStride
      var idxOther = zeroOther + (rows - 1) * rowStrideOther + (columns - 1) * columnStrideOther
      if (function == cern.jet.math.tint.IntFunctions.mult) {
        var c = columns
        while (c >= 0) {
          var i = idx
          var j = idxOther
          var r = rows
          while (r >= 0) {
            elements(i) *= otherElements(j)
            i -= rowStride
            j -= rowStrideOther
          }
          idx -= columnStride
          idxOther -= columnStrideOther
        }
      } else if (function == cern.jet.math.tint.IntFunctions.div) {
        var c = columns
        while (c >= 0) {
          var i = idx
          var j = idxOther
          var r = rows
          while (r >= 0) {
            elements(i) /= otherElements(j)
            i -= rowStride
            j -= rowStrideOther
          }
          idx -= columnStride
          idxOther -= columnStrideOther
        }
      } else if (function.isInstanceOf[cern.jet.math.tint.IntPlusMultSecond]) {
        val multiplicator = function.asInstanceOf[cern.jet.math.tint.IntPlusMultSecond].multiplicator
        if (multiplicator == 1) {
          var c = columns
          while (c >= 0) {
            var i = idx
            var j = idxOther
            var r = rows
            while (r >= 0) {
              elements(i) += otherElements(j)
              i -= rowStride
              j -= rowStrideOther
            }
            idx -= columnStride
            idxOther -= columnStrideOther
          }
        } else if (multiplicator == -1) {
          var c = columns
          while (c >= 0) {
            var i = idx
            var j = idxOther
            var r = rows
            while (r >= 0) {
              elements(i) -= otherElements(j)
              i -= rowStride
              j -= rowStrideOther
            }
            idx -= columnStride
            idxOther -= columnStrideOther
          }
        } else {
          var c = columns
          while (c >= 0) {
            var i = idx
            var j = idxOther
            var r = rows
            while (r >= 0) {
              elements(i) += multiplicator * otherElements(j)
              i -= rowStride
              j -= rowStrideOther
            }
            idx -= columnStride
            idxOther -= columnStrideOther
          }
        }
      } else if (function.isInstanceOf[cern.jet.math.tint.IntPlusMultFirst]) {
        val multiplicator = function.asInstanceOf[cern.jet.math.tint.IntPlusMultFirst].multiplicator
        if (multiplicator == 1) {
          var c = columns
          while (c >= 0) {
            var i = idx
            var j = idxOther
            var r = rows
            while (r >= 0) {
              elements(i) += otherElements(j)
              i -= rowStride
              j -= rowStrideOther
            }
            idx -= columnStride
            idxOther -= columnStrideOther
          }
        } else if (multiplicator == -1) {
          var c = columns
          while (c >= 0) {
            var i = idx
            var j = idxOther
            var r = rows
            while (r >= 0) {
              elements(i) = otherElements(j) - elements(i)
              i -= rowStride
              j -= rowStrideOther
            }
            idx -= columnStride
            idxOther -= columnStrideOther
          }
        } else {
          var c = columns
          while (c >= 0) {
            var i = idx
            var j = idxOther
            var r = rows
            while (r >= 0) {
              elements(i) = multiplicator * elements(i) + otherElements(j)
              i -= rowStride
              j -= rowStrideOther
            }
            idx -= columnStride
            idxOther -= columnStrideOther
          }
        }
      } else {
        var c = columns
        while (c >= 0) {
          var i = idx
          var j = idxOther
          var r = rows
          while (r >= 0) {
            elements(i) = function.apply(elements(i), otherElements(j))
            i -= rowStride
            j -= rowStrideOther
          }
          idx -= columnStride
          idxOther -= columnStrideOther
        }
      }
    }
    this
  }

  def assign(y: IntMatrix2D,
      function: IntIntFunction,
      rowList: IntArrayList,
      columnList: IntArrayList): IntMatrix2D = {
    checkShape(y)
    if (!(y.isInstanceOf[DenseColumnIntMatrix2D])) {
      super.assign(y, function)
      return this
    }
    val other = y.asInstanceOf[DenseColumnIntMatrix2D]
    val size = rowList.size
    val rowElements = rowList.elements()
    val columnElements = columnList.elements()
    val otherElements = other.elements()
    val zeroOther = other.index(0, 0).toInt
    val zero = index(0, 0).toInt
    val columnStrideOther = other.columnStride()
    val rowStrideOther = other.rowStride()
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_2D)) {
      nthreads = Math.min(nthreads, columns)
      val futures = Array.ofDim[Future](nthreads)
      val k = size / nthreads
      for (j <- 0 until nthreads) {
        val firstIdx = size - j * k
        val lastIdx = if ((j == (nthreads - 1))) 0 else firstIdx - k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            var idx: Int = 0
            var idxOther: Int = 0
            var i = firstIdx
            while (i >= lastIdx) {
              idx = zero + rowElements(i) * rowStride + columnElements(i) * columnStride
              idxOther = zeroOther + rowElements(i) * rowStrideOther + columnElements(i) * columnStrideOther
              elements(idx) = function.apply(elements(idx), otherElements(idxOther))
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
    } else {
      var idx: Int = 0
      var idxOther: Int = 0
      var i = size
      while (i >= 0) {
        idx = zero + rowElements(i) * rowStride + columnElements(i) * columnStride
        idxOther = zeroOther + rowElements(i) * rowStrideOther + columnElements(i) * columnStrideOther
        elements(idx) = function.apply(elements(idx), otherElements(idxOther))
      }
    }
    this
  }

  def cardinality(): Int = {
    var cardinality = 0
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    val zero = index(0, 0).toInt
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_2D)) {
      nthreads = Math.min(nthreads, columns)
      val futures = Array.ofDim[Future](nthreads)
      val results = Array.ofDim[Integer](nthreads)
      val k = columns / nthreads
      for (j <- 0 until nthreads) {
        val firstColumn = columns - j * k
        val lastColumn = if ((j == (nthreads - 1))) 0 else firstColumn - k
        futures(j) = ConcurrencyUtils.submit(new Callable[Integer]() {

          def call(): java.lang.Integer = {
            var cardinality = 0
            var idx = zero + (rows - 1) * rowStride + (firstColumn - 1) * columnStride
            var c = firstColumn
            while (c >= lastColumn) {
              var i = idx
              var r = rows
              while (r >= 0) {
                if (elements(i) != 0) cardinality += 1
                i -= rowStride
              }
              idx -= columnStride
            }
            return cardinality
          }
        })
      }
      try {
        for (j <- 0 until nthreads) {
          results(j) = futures(j).get.asInstanceOf[java.lang.Integer]
        }
        cardinality = results(0)
        for (j <- 1 until nthreads) {
          cardinality += results(j)
        }
      } catch {
        case ex: ExecutionException => ex.printStackTrace()
        case e: InterruptedException => e.printStackTrace()
      }
    } else {
      var idx = zero + (rows - 1) * rowStride + (columns - 1) * columnStride
      var c = columns
      while (c >= 0) {
        var i = idx
        var r = rows
        while (r >= 0) {
          if (elements(i) != 0) cardinality += 1
          i -= rowStride
        }
        idx -= columnStride
      }
    }
    cardinality
  }

  def elements(): Array[Int] = elements

  def forEachNonZero(function: cern.colt.function.tint.IntIntIntFunction): IntMatrix2D = {
    val zero = index(0, 0).toInt
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_2D)) {
      nthreads = Math.min(nthreads, columns)
      val futures = Array.ofDim[Future](nthreads)
      val k = columns / nthreads
      for (j <- 0 until nthreads) {
        val firstColumn = columns - j * k
        val lastColumn = if ((j == (nthreads - 1))) 0 else firstColumn - k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            var idx = zero + (rows - 1) * rowStride + (firstColumn - 1) * columnStride
            var c = firstColumn
            while (c >= lastColumn) {
              var i = idx
              var r = rows
              while (r >= 0) {
                var value = elements(i)
                if (value != 0) {
                  elements(i) = function.apply(r, c, value)
                }
                i -= rowStride
              }
              idx -= columnStride
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
    } else {
      var idx = zero + (rows - 1) * rowStride + (columns - 1) * columnStride
      var c = columns
      while (c >= 0) {
        var i = idx
        var r = rows
        while (r >= 0) {
          val value = elements(i)
          if (value != 0) {
            elements(i) = function.apply(r, c, value)
          }
          i -= rowStride
        }
        idx -= columnStride
      }
    }
    this
  }

  /**
   * Returns a new matrix that has the same elements as this matrix, but they
   * are addressed internally in row major. This method creates a new object
   * (not a view), so changes in the returned matrix are NOT reflected in this
   * matrix.
   *
   * @return this matrix with elements addressed internally in row major
   */
  def getRowMajor(): DenseIntMatrix2D = {
    val R = new DenseIntMatrix2D(rows, columns)
    val zeroR = R.index(0, 0).toInt
    val rowStrideR = R.rowStride()
    val columnStrideR = R.columnStride()
    val elementsR = R.elements()
    val zero = index(0, 0).toInt
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_2D)) {
      nthreads = Math.min(nthreads, columns)
      val futures = Array.ofDim[Future](nthreads)
      val k = columns / nthreads
      for (j <- 0 until nthreads) {
        val firstColumn = columns - j * k
        val lastColumn = if ((j == (nthreads - 1))) 0 else firstColumn - k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            var idx = zero + (rows - 1) * rowStride + (firstColumn - 1) * columnStride
            var idxR = zeroR + (rows - 1) * rowStrideR + (firstColumn - 1) * columnStrideR
            var c = firstColumn
            while (c >= lastColumn) {
              var i = idx
              var j = idxR
              var r = rows
              while (r >= 0) {
                elementsR(j) = elements(i)
                i -= rowStride
                j -= rowStrideR
              }
              idx -= columnStride
              idxR -= columnStrideR
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
    } else {
      var idx = zero + (rows - 1) * rowStride + (columns - 1) * columnStride
      var idxR = zeroR + (rows - 1) * rowStrideR + (columns - 1) * columnStrideR
      var c = columns
      while (c >= 0) {
        var i = idx
        var j = idxR
        var r = rows
        while (r >= 0) {
          elementsR(j) = elements(i)
          i -= rowStride
          j -= rowStrideR
        }
        idx -= columnStride
        idxR -= columnStrideR
      }
    }
    R
  }

  def getNegativeValues(rowList: IntArrayList, columnList: IntArrayList, valueList: IntArrayList) {
    rowList.clear()
    columnList.clear()
    valueList.clear()
    var idx = index(0, 0).toInt
    for (c <- 0 until columns) {
      for (i <- idx until rows) {
        val value = elements(i)
        if (value < 0) {
          rowList.add(r)
          columnList.add(c)
          valueList.add(value)
        }
        i += rowStride
      }
      idx += columnStride
    }
  }

  def getNonZeros(rowList: IntArrayList, columnList: IntArrayList, valueList: IntArrayList) {
    rowList.clear()
    columnList.clear()
    valueList.clear()
    var idx = index(0, 0).toInt
    for (c <- 0 until columns) {
      for (i <- idx until rows) {
        val value = elements(i)
        if (value != 0) {
          rowList.add(r)
          columnList.add(c)
          valueList.add(value)
        }
        i += rowStride
      }
      idx += columnStride
    }
  }

  def getPositiveValues(rowList: IntArrayList, columnList: IntArrayList, valueList: IntArrayList) {
    rowList.clear()
    columnList.clear()
    valueList.clear()
    var idx = index(0, 0).toInt
    for (c <- 0 until columns) {
      for (i <- idx until rows) {
        val value = elements(i)
        if (value > 0) {
          rowList.add(r)
          columnList.add(c)
          valueList.add(value)
        }
        i += rowStride
      }
      idx += columnStride
    }
  }

  def getQuick(row: Int, column: Int): Int = {
    elements(rowZero + row * rowStride + columnZero + column * columnStride)
  }

  def index(row: Int, column: Int): Long = {
    rowZero + row * rowStride + columnZero + column * columnStride
  }

  def like(rows: Int, columns: Int): IntMatrix2D = {
    new DenseColumnIntMatrix2D(rows, columns)
  }

  def like1D(size: Int): IntMatrix1D = new DenseIntMatrix1D(size)

  def getMaxLocation(): Array[Int] = {
    var rowLocation = 0
    var columnLocation = 0
    val zero = index(0, 0).toInt
    var maxValue = 0
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_2D)) {
      nthreads = Math.min(nthreads, columns)
      val futures = Array.ofDim[Future](nthreads)
      val results = Array.ofDim[Int](nthreads, 3)
      val k = columns / nthreads
      for (j <- 0 until nthreads) {
        val firstColumn = columns - j * k
        val lastColumn = if ((j == (nthreads - 1))) 0 else firstColumn - k
        futures(j) = ConcurrencyUtils.submit(new Callable[Array[Int]]() {

          def call(): Array[Int] = {
            var maxValue = elements(zero + (rows - 1) * rowStride + (firstColumn - 1) * columnStride)
            var rowLocation = rows - 1
            var columnLocation = firstColumn - 1
            var elem: Int = 0
            var d = 1
            var c = firstColumn
            while (c >= lastColumn) {
              var cidx = zero + c * columnStride
              var r = rows - d
              while (r >= 0) {
                elem = elements(r * rowStride + cidx)
                if (maxValue < elem) {
                  maxValue = elem
                  rowLocation = r
                  columnLocation = c
                }
              }
              d = 0
            }
            return Array(maxValue, rowLocation, columnLocation)
          }
        })
      }
      try {
        for (j <- 0 until nthreads) {
          results(j) = futures(j).get.asInstanceOf[Array[Int]]
        }
        maxValue = results(0)(0)
        rowLocation = results(0)(1).toInt
        columnLocation = results(0)(2).toInt
        for (j <- 1 until nthreads if maxValue < results(j)(0)) {
          maxValue = results(j)(0)
          rowLocation = results(j)(1).toInt
          columnLocation = results(j)(2).toInt
        }
      } catch {
        case ex: ExecutionException => ex.printStackTrace()
        case e: InterruptedException => e.printStackTrace()
      }
    } else {
      maxValue = elements(zero + (rows - 1) * rowStride + (columns - 1) * columnStride)
      rowLocation = rows - 1
      columnLocation = columns - 1
      var elem: Int = 0
      var d = 1
      var c = columns
      while (c >= 0) {
        val cidx = zero + c * columnStride
        var r = rows - d
        while (r >= 0) {
          elem = elements(r * rowStride + cidx)
          if (maxValue < elem) {
            maxValue = elem
            rowLocation = r
            columnLocation = c
          }
        }
        d = 0
      }
    }
    Array(maxValue, rowLocation, columnLocation)
  }

  def getMinLocation(): Array[Int] = {
    var rowLocation = 0
    var columnLocation = 0
    val zero = index(0, 0).toInt
    var minValue = 0
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_2D)) {
      nthreads = Math.min(nthreads, columns)
      val futures = Array.ofDim[Future](nthreads)
      val results = Array.ofDim[Int](nthreads, 3)
      val k = columns / nthreads
      for (j <- 0 until nthreads) {
        val firstColumn = columns - j * k
        val lastColumn = if ((j == (nthreads - 1))) 0 else firstColumn - k
        futures(j) = ConcurrencyUtils.submit(new Callable[Array[Int]]() {

          def call(): Array[Int] = {
            var minValue = elements(zero + (rows - 1) * rowStride + (firstColumn - 1) * columnStride)
            var rowLocation = rows - 1
            var columnLocation = firstColumn - 1
            var elem: Int = 0
            var d = 1
            var c = firstColumn
            while (c >= lastColumn) {
              var cidx = zero + c * columnStride
              var r = rows - d
              while (r >= 0) {
                elem = elements(r * rowStride + cidx)
                if (minValue > elem) {
                  minValue = elem
                  rowLocation = r
                  columnLocation = c
                }
              }
              d = 0
            }
            return Array(minValue, rowLocation, columnLocation)
          }
        })
      }
      try {
        for (j <- 0 until nthreads) {
          results(j) = futures(j).get.asInstanceOf[Array[Int]]
        }
        minValue = results(0)(0)
        rowLocation = results(0)(1).toInt
        columnLocation = results(0)(2).toInt
        for (j <- 1 until nthreads if minValue > results(j)(0)) {
          minValue = results(j)(0)
          rowLocation = results(j)(1).toInt
          columnLocation = results(j)(2).toInt
        }
      } catch {
        case ex: ExecutionException => ex.printStackTrace()
        case e: InterruptedException => e.printStackTrace()
      }
    } else {
      minValue = elements(zero + (rows - 1) * rowStride + (columns - 1) * columnStride)
      rowLocation = rows - 1
      columnLocation = columns - 1
      var elem: Int = 0
      var d = 1
      var c = columns
      while (c >= 0) {
        val cidx = zero + c * columnStride
        var r = rows - d
        while (r >= 0) {
          elem = elements(r * rowStride + cidx)
          if (minValue > elem) {
            minValue = elem
            rowLocation = r
            columnLocation = c
          }
        }
        d = 0
      }
    }
    Array(minValue, rowLocation, columnLocation)
  }

  def setQuick(row: Int, column: Int, value: Int) {
    elements(rowZero + row * rowStride + columnZero + column * columnStride) = value
  }

  def toArray(): Array[Array[Int]] = {
    val values = Array.ofDim[Int](rows, columns)
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    val zero = index(0, 0).toInt
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_2D)) {
      nthreads = Math.min(nthreads, columns)
      val futures = Array.ofDim[Future](nthreads)
      val k = columns / nthreads
      for (j <- 0 until nthreads) {
        val firstColumn = columns - j * k
        val lastColumn = if ((j == (nthreads - 1))) 0 else firstColumn - k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            var idx = zero + (rows - 1) * rowStride + (firstColumn - 1) * columnStride
            var c = firstColumn
            while (c >= lastColumn) {
              var i = idx
              var r = rows
              while (r >= 0) {
                values(r)(c) = elements(i)
                i -= rowStride
              }
              idx -= columnStride
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
    } else {
      var idx = zero + (rows - 1) * rowStride + (columns - 1) * columnStride
      var c = columns
      while (c >= 0) {
        var i = idx
        var r = rows
        while (r >= 0) {
          values(r)(c) = elements(i)
          i -= rowStride
        }
        idx -= columnStride
      }
    }
    values
  }

  def vectorize(): IntMatrix1D = {
    val size = size.toInt
    val v = new DenseIntMatrix1D(size)
    if (isNoView == true) {
      System.arraycopy(elements, 0, v.elements(), 0, size)
    } else {
      val zero = index(0, 0).toInt
      val zeroOther = v.index(0).toInt
      val strideOther = v.stride()
      val elementsOther = v.elements().asInstanceOf[Array[Int]]
      var nthreads = ConcurrencyUtils.getNumberOfThreads
      if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_2D)) {
        nthreads = Math.min(nthreads, columns)
        val futures = Array.ofDim[Future](nthreads)
        val k = columns / nthreads
        for (j <- 0 until nthreads) {
          val firstColumn = columns - j * k
          val lastColumn = if ((j == (nthreads - 1))) 0 else firstColumn - k
          val firstIdxOther = size - j * k * rows
          futures(j) = ConcurrencyUtils.submit(new Runnable() {

            def run() {
              var idx = zero + (rows - 1) * rowStride + (firstColumn - 1) * columnStride
              var idxOther = zeroOther + (firstIdxOther - 1) * strideOther
              var c = firstColumn
              while (c >= lastColumn) {
                var i = idx
                var r = rows
                while (r >= 0) {
                  elementsOther(idxOther) = elements(i)
                  i -= rowStride
                  idxOther -= strideOther
                }
                idx -= columnStride
              }
            }
          })
        }
        ConcurrencyUtils.waitForCompletion(futures)
      } else {
        var idx = zero + (rows - 1) * rowStride + (columns - 1) * columnStride
        var idxOther = zeroOther + size - 1
        var c = columns
        while (c >= 0) {
          var i = idx
          var r = rows
          while (r >= 0) {
            elementsOther(idxOther) = elements(i)
            i -= rowStride
            idxOther -= 1
          }
          idx -= columnStride
        }
      }
    }
    v
  }

  def zSum(): Int = {
    var sum = 0
    if (elements == null) throw new InternalError()
    val zero = index(0, 0).toInt
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_2D)) {
      nthreads = Math.min(nthreads, columns)
      val futures = Array.ofDim[Future](nthreads)
      val k = columns / nthreads
      for (j <- 0 until nthreads) {
        val firstColumn = columns - j * k
        val lastColumn = if ((j == (nthreads - 1))) 0 else firstColumn - k
        futures(j) = ConcurrencyUtils.submit(new Callable[Integer]() {

          def call(): java.lang.Integer = {
            var sum = 0
            var idx = zero + (rows - 1) * rowStride + (firstColumn - 1) * columnStride
            var c = firstColumn
            while (c >= lastColumn) {
              var i = idx
              var r = rows
              while (r >= 0) {
                sum += elements(i)
                i -= rowStride
              }
              idx -= columnStride
            }
            return sum
          }
        })
      }
      try {
        for (j <- 0 until nthreads) {
          sum += futures(j).get.asInstanceOf[java.lang.Integer]
        }
      } catch {
        case ex: ExecutionException => ex.printStackTrace()
        case e: InterruptedException => e.printStackTrace()
      }
    } else {
      var idx = zero + (rows - 1) * rowStride + (columns - 1) * columnStride
      var c = columns
      while (c >= 0) {
        var i = idx
        var r = rows
        while (r >= 0) {
          sum += elements(i)
          i -= rowStride
        }
        idx -= columnStride
      }
    }
    sum
  }

  protected def haveSharedCellsRaw(other: IntMatrix2D): Boolean = {
    if (other.isInstanceOf[SelectedDenseColumnIntMatrix2D]) {
      val otherMatrix = other.asInstanceOf[SelectedDenseColumnIntMatrix2D]
      return this.elements == otherMatrix.elements
    } else if (other.isInstanceOf[DenseColumnIntMatrix2D]) {
      val otherMatrix = other.asInstanceOf[DenseColumnIntMatrix2D]
      return this.elements == otherMatrix.elements
    }
    false
  }

  protected def like1D(size: Int, zero: Int, stride: Int): IntMatrix1D = {
    new DenseIntMatrix1D(size, this.elements, zero, stride, true)
  }

  protected def viewSelectionLike(rowOffsets: Array[Int], columnOffsets: Array[Int]): IntMatrix2D = {
    new SelectedDenseColumnIntMatrix2D(this.elements, rowOffsets, columnOffsets, 0)
  }
}
