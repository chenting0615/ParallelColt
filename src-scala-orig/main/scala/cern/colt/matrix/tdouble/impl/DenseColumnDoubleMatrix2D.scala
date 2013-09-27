package cern.colt.matrix.tdouble.impl

import cern.colt.function.tdouble.Function1
import cern.colt.function.tdouble.Procedure1
import cern.colt.list.tdouble.DoubleArrayList
import cern.colt.list.tint.IntArrayList
import cern.colt.matrix.Transpose
import cern.colt.matrix.io.MatrixInfo
import cern.colt.matrix.io.MatrixSize
import cern.colt.matrix.io.MatrixVectorReader
import cern.colt.matrix.tdcomplex.DComplexMatrix2D
import cern.colt.matrix.tdcomplex.impl.DenseDComplexMatrix2D
import cern.colt.matrix.tdouble.StrideMatrix1D
import cern.colt.matrix.tdouble.StrideMatrix2D
import edu.emory.mathcs.jtransforms.dct.DoubleDCT_2D
import edu.emory.mathcs.jtransforms.dht.DoubleDHT_2D
import edu.emory.mathcs.jtransforms.dst.DoubleDST_2D
import edu.emory.mathcs.jtransforms.fft.DoubleFFT_2D
import edu.emory.mathcs.utils.ConcurrencyUtils
import org.netlib.blas.BLAS
import java.io.IOException
import java.util.concurrent.Callable
import java.util.concurrent.ExecutionException
import java.util.concurrent.Future
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Dense 2-d matrix holding <tt>double</tt> elements. First see the <a
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
@SerialVersionUID(1020177651L)
class DenseColumnDoubleMatrix2D(rows: Int, columns: Int) extends StrideMatrix2D {

  private var fft2: DoubleFFT_2D = _

  private var dct2: DoubleDCT_2D = _

  private var dst2: DoubleDST_2D = _

  private var dht2: DoubleDHT_2D = _

  protected var elements: Array[Double] = new Array[Double](rows * columns)

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
  def this(values: Array[Array[Double]]) {
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
      elements: Array[Double],
      rowZero: Int,
      columnZero: Int,
      rowStride: Int,
      columnStride: Int,
      isView: Boolean) {
    this(rows, columns)
    setUp(rows, columns, rowZero, columnZero, rowStride, columnStride)
    this.elementsVar = elements
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
    this.elements = Array.ofDim[Double](rows * columns)
    val numEntries = size.numEntries()
    val columnIndexes = Array.ofDim[Int](numEntries)
    val rowIndexes = Array.ofDim[Int](numEntries)
    val values = Array.ofDim[Double](numEntries)
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

  def aggregate(aggr: DoubleDoubleFunction, f: Function1): Double = {
    if (size == 0) return Double.NaN
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
        futures(j) = ConcurrencyUtils.submit(new Callable[Double]() {

          def call(): java.lang.Double = {
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

  def aggregate(aggr: DoubleDoubleFunction, f: Function1, cond: Procedure1): Double = {
    if (size == 0) return Double.NaN
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
        futures(j) = ConcurrencyUtils.submit(new Callable[Double]() {

          def call(): java.lang.Double = {
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

  def aggregate(aggr: DoubleDoubleFunction,
      f: Function1,
      rowList: IntArrayList,
      columnList: IntArrayList): Double = {
    if (size == 0) return Double.NaN
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
        futures(j) = ConcurrencyUtils.submit(new Callable[Double]() {

          def call(): java.lang.Double = {
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

  def aggregate(other: StrideMatrix2D, aggr: DoubleDoubleFunction, f: DoubleDoubleFunction): Double = {
    if (!(other.isInstanceOf[DenseColumnDoubleMatrix2D])) {
      return super.aggregate(other, aggr, f)
    }
    checkShape(other)
    if (size == 0) return Double.NaN
    val zero = index(0, 0).toInt
    val zeroOther = other.index(0, 0).toInt
    val rowStrideOther = other.rowStride()
    val columnStrideOther = other.columnStride()
    val otherElements = other.elements().asInstanceOf[Array[Double]]
    var a = 0
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_2D)) {
      nthreads = Math.min(nthreads, columns)
      val futures = Array.ofDim[Future](nthreads)
      val k = columns / nthreads
      for (j <- 0 until nthreads) {
        val firstColumn = columns - j * k
        val lastColumn = if ((j == (nthreads - 1))) 0 else firstColumn - k
        futures(j) = ConcurrencyUtils.submit(new Callable[Double]() {

          def call(): java.lang.Double = {
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

  def assign(function: Function1): StrideMatrix2D = {
    if (function.isInstanceOf[cern.jet.math.tdouble.DoubleMult]) {
      val multiplicator = function.asInstanceOf[cern.jet.math.tdouble.DoubleMult].multiplicator
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
            if (function.isInstanceOf[cern.jet.math.tdouble.DoubleMult]) {
              var multiplicator = function.asInstanceOf[cern.jet.math.tdouble.DoubleMult].multiplicator
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
      if (function.isInstanceOf[cern.jet.math.tdouble.DoubleMult]) {
        val multiplicator = function.asInstanceOf[cern.jet.math.tdouble.DoubleMult].multiplicator
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

  def assign(cond: Procedure1, function: Function1): StrideMatrix2D = {
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
            var elem: Double = 0.0
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
      var elem: Double = 0.0
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

  def assign(cond: Procedure1, value: Double): StrideMatrix2D = {
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
            var elem: Double = 0.0
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
      var elem: Double = 0.0
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

  def assign(value: Double): StrideMatrix2D = {
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

  def assign(values: Array[Double]): StrideMatrix2D = {
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

  def assign(values: Array[Array[Double]]): StrideMatrix2D = {
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

  def assign(source: StrideMatrix2D): StrideMatrix2D = {
    if (!(source.isInstanceOf[DenseColumnDoubleMatrix2D])) {
      super.assign(source)
      return this
    }
    var other = source.asInstanceOf[DenseColumnDoubleMatrix2D]
    if (other == this) return this
    checkShape(other)
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if (this.isNoView && other.isNoView) {
      System.arraycopy(other.elements, 0, elements, 0, elements.length)
      return this
    }
    if (haveSharedCells(other)) {
      val c = other.copy()
      if (!(c.isInstanceOf[DenseColumnDoubleMatrix2D])) {
        super.assign(other)
        return this
      }
      other = c.asInstanceOf[DenseColumnDoubleMatrix2D]
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

  def assign(y: StrideMatrix2D, function: DoubleDoubleFunction): StrideMatrix2D = {
    if (function.isInstanceOf[cern.jet.math.tdouble.DoublePlusMultSecond]) {
      val multiplicator = function.asInstanceOf[cern.jet.math.tdouble.DoublePlusMultSecond].multiplicator
      if (multiplicator == 0) {
        return this
      }
    }
    if (function.isInstanceOf[cern.jet.math.tdouble.DoublePlusMultFirst]) {
      val multiplicator = function.asInstanceOf[cern.jet.math.tdouble.DoublePlusMultFirst].multiplicator
      if (multiplicator == 0) {
        return assign(y)
      }
    }
    if (!(y.isInstanceOf[DenseColumnDoubleMatrix2D])) {
      super.assign(y, function)
      return this
    }
    val other = y.asInstanceOf[DenseColumnDoubleMatrix2D]
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
            if (function == cern.jet.math.tdouble.DoubleFunctions.mult) {
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
            } else if (function == cern.jet.math.tdouble.DoubleFunctions.div) {
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
            } else if (function.isInstanceOf[cern.jet.math.tdouble.DoublePlusMultSecond]) {
              var multiplicator = function.asInstanceOf[cern.jet.math.tdouble.DoublePlusMultSecond].multiplicator
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
            } else if (function.isInstanceOf[cern.jet.math.tdouble.DoublePlusMultFirst]) {
              var multiplicator = function.asInstanceOf[cern.jet.math.tdouble.DoublePlusMultFirst].multiplicator
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
      if (function == cern.jet.math.tdouble.DoubleFunctions.mult) {
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
      } else if (function == cern.jet.math.tdouble.DoubleFunctions.div) {
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
      } else if (function.isInstanceOf[cern.jet.math.tdouble.DoublePlusMultSecond]) {
        val multiplicator = function.asInstanceOf[cern.jet.math.tdouble.DoublePlusMultSecond].multiplicator
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
      } else if (function.isInstanceOf[cern.jet.math.tdouble.DoublePlusMultFirst]) {
        val multiplicator = function.asInstanceOf[cern.jet.math.tdouble.DoublePlusMultFirst].multiplicator
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

  def assign(y: StrideMatrix2D,
      function: DoubleDoubleFunction,
      rowList: IntArrayList,
      columnList: IntArrayList): StrideMatrix2D = {
    checkShape(y)
    if (!(y.isInstanceOf[DenseColumnDoubleMatrix2D])) {
      super.assign(y, function)
      return this
    }
    val other = y.asInstanceOf[DenseColumnDoubleMatrix2D]
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

  def assign(values: Array[Float]): StrideMatrix2D = {
    if (values.length != size) throw new IllegalArgumentException("Must have same length: length=" + values.length + "rows()*columns()=" +
      rows() * columns())
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

  /**
   * Computes the 2D discrete cosine transform (DCT-II) of this matrix.
   *
   * @param scale
   *            if true then scaling is performed
   *
   */
  def dct2(scale: Boolean) {
    val transpose = viewDice().copy()
    val oldNthreads = ConcurrencyUtils.getNumberOfThreads
    ConcurrencyUtils.setNumberOfThreads(ConcurrencyUtils.nextPow2(oldNthreads))
    if (dct2 == null) {
      dct2 = new DoubleDCT_2D(rows, columns)
    }
    dct2.forward(transpose.elements().asInstanceOf[Array[Double]], scale)
    this.assign(transpose.viewDice().copy())
    ConcurrencyUtils.setNumberOfThreads(oldNthreads)
  }

  /**
   * Computes the discrete cosine transform (DCT-II) of each column of this
   * matrix.
   *
   * @param scale
   *            if true then scaling is performed
   *
   */
  def dctColumns(scale: Boolean) {
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_2D)) {
      ConcurrencyUtils.setThreadsBeginN_1D_FFT_2Threads(Integer.MAX_VALUE)
      ConcurrencyUtils.setThreadsBeginN_1D_FFT_4Threads(Integer.MAX_VALUE)
      nthreads = Math.min(nthreads, columns)
      val futures = Array.ofDim[Future](nthreads)
      val k = columns / nthreads
      for (j <- 0 until nthreads) {
        val firstColumn = columns - j * k
        val lastColumn = if ((j == (nthreads - 1))) 0 else firstColumn - k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            var c = firstColumn
            while (c >= lastColumn) {
              viewColumn(c).asInstanceOf[DenseMatrix1D].dct(scale)
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
      ConcurrencyUtils.resetThreadsBeginN_FFT()
    } else {
      var c = columns
      while (c >= 0) {
        viewColumn(c).asInstanceOf[DenseMatrix1D].dct(scale)
      }
    }
  }

  /**
   * Computes the discrete cosine transform (DCT-II) of each row of this
   * matrix.
   *
   * @param scale
   *            if true then scaling is performed
   *
   */
  def dctRows(scale: Boolean) {
    val oldNthreads = ConcurrencyUtils.getNumberOfThreads
    ConcurrencyUtils.setNumberOfThreads(ConcurrencyUtils.nextPow2(oldNthreads))
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_2D)) {
      ConcurrencyUtils.setThreadsBeginN_1D_FFT_2Threads(Integer.MAX_VALUE)
      ConcurrencyUtils.setThreadsBeginN_1D_FFT_4Threads(Integer.MAX_VALUE)
      nthreads = Math.min(nthreads, rows)
      val futures = Array.ofDim[Future](nthreads)
      val k = rows / nthreads
      for (j <- 0 until nthreads) {
        val firstRow = rows - j * k
        val lastRow = if ((j == (nthreads - 1))) 0 else firstRow - k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            var r = firstRow
            while (r >= lastRow) {
              viewRow(r).asInstanceOf[DenseMatrix1D].dct(scale)
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
      ConcurrencyUtils.resetThreadsBeginN_FFT()
    } else {
      for (r <- 0 until rows) {
        viewRow(r).asInstanceOf[DenseMatrix1D].dct(scale)
      }
    }
    ConcurrencyUtils.setNumberOfThreads(oldNthreads)
  }

  /**
   * Computes the 2D discrete Hartley transform (DHT) of this matrix.
   *
   */
  def dht2() {
    val transpose = viewDice().copy()
    val oldNthreads = ConcurrencyUtils.getNumberOfThreads
    ConcurrencyUtils.setNumberOfThreads(ConcurrencyUtils.nextPow2(oldNthreads))
    if (dht2 == null) {
      dht2 = new DoubleDHT_2D(rows, columns)
    }
    dht2.forward(transpose.elements().asInstanceOf[Array[Double]])
    this.assign(transpose.viewDice().copy())
    ConcurrencyUtils.setNumberOfThreads(oldNthreads)
  }

  /**
   * Computes the discrete Hartley transform (DHT) of each column of this
   * matrix.
   *
   */
  def dhtColumns() {
    val oldNthreads = ConcurrencyUtils.getNumberOfThreads
    ConcurrencyUtils.setNumberOfThreads(ConcurrencyUtils.nextPow2(oldNthreads))
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_2D)) {
      ConcurrencyUtils.setThreadsBeginN_1D_FFT_2Threads(Integer.MAX_VALUE)
      ConcurrencyUtils.setThreadsBeginN_1D_FFT_4Threads(Integer.MAX_VALUE)
      nthreads = Math.min(nthreads, columns)
      val futures = Array.ofDim[Future](nthreads)
      val k = columns / nthreads
      for (j <- 0 until nthreads) {
        val firstColumn = columns - j * k
        val lastColumn = if ((j == (nthreads - 1))) 0 else firstColumn - k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            var c = firstColumn
            while (c >= lastColumn) {
              viewColumn(c).asInstanceOf[DenseMatrix1D].dht()
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
      ConcurrencyUtils.resetThreadsBeginN_FFT()
    } else {
      var c = columns
      while (c >= 0) {
        viewColumn(c).asInstanceOf[DenseMatrix1D].dht()
      }
    }
    ConcurrencyUtils.setNumberOfThreads(oldNthreads)
  }

  /**
   * Computes the discrete Hartley transform (DHT) of each row of this matrix.
   *
   */
  def dhtRows() {
    val oldNthreads = ConcurrencyUtils.getNumberOfThreads
    ConcurrencyUtils.setNumberOfThreads(ConcurrencyUtils.nextPow2(oldNthreads))
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_2D)) {
      ConcurrencyUtils.setThreadsBeginN_1D_FFT_2Threads(Integer.MAX_VALUE)
      ConcurrencyUtils.setThreadsBeginN_1D_FFT_4Threads(Integer.MAX_VALUE)
      nthreads = Math.min(nthreads, rows)
      val futures = Array.ofDim[Future](nthreads)
      val k = rows / nthreads
      for (j <- 0 until nthreads) {
        val firstRow = rows - j * k
        val lastRow = if ((j == (nthreads - 1))) 0 else firstRow - k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            var r = firstRow
            while (r >= lastRow) {
              viewRow(r).asInstanceOf[DenseMatrix1D].dht()
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
      ConcurrencyUtils.resetThreadsBeginN_FFT()
    } else {
      for (r <- 0 until rows) {
        viewRow(r).asInstanceOf[DenseMatrix1D].dht()
      }
    }
    ConcurrencyUtils.setNumberOfThreads(oldNthreads)
  }

  /**
   * Computes the 2D discrete sine transform (DST-II) of this matrix.
   *
   * @param scale
   *            if true then scaling is performed
   *
   */
  def dst2(scale: Boolean) {
    val transpose = viewDice().copy()
    val oldNthreads = ConcurrencyUtils.getNumberOfThreads
    ConcurrencyUtils.setNumberOfThreads(ConcurrencyUtils.nextPow2(oldNthreads))
    if (dst2 == null) {
      dst2 = new DoubleDST_2D(rows, columns)
    }
    dst2.forward(transpose.elements().asInstanceOf[Array[Double]], scale)
    this.assign(transpose.viewDice().copy())
    ConcurrencyUtils.setNumberOfThreads(oldNthreads)
  }

  /**
   * Computes the discrete sine transform (DST-II) of each column of this
   * matrix.
   *
   * @param scale
   *            if true then scaling is performed
   *
   */
  def dstColumns(scale: Boolean) {
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_2D)) {
      ConcurrencyUtils.setThreadsBeginN_1D_FFT_2Threads(Integer.MAX_VALUE)
      ConcurrencyUtils.setThreadsBeginN_1D_FFT_4Threads(Integer.MAX_VALUE)
      nthreads = Math.min(nthreads, columns)
      val futures = Array.ofDim[Future](nthreads)
      val k = columns / nthreads
      for (j <- 0 until nthreads) {
        val firstColumn = columns - j * k
        val lastColumn = if ((j == (nthreads - 1))) 0 else firstColumn - k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            var c = firstColumn
            while (c >= lastColumn) {
              viewColumn(c).asInstanceOf[DenseMatrix1D].dst(scale)
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
      ConcurrencyUtils.resetThreadsBeginN_FFT()
    } else {
      var c = columns
      while (c >= 0) {
        viewColumn(c).asInstanceOf[DenseMatrix1D].dst(scale)
      }
    }
  }

  /**
   * Computes the discrete sine transform (DST-II) of each row of this matrix.
   *
   * @param scale
   *            if true then scaling is performed
   *
   */
  def dstRows(scale: Boolean) {
    val oldNthreads = ConcurrencyUtils.getNumberOfThreads
    ConcurrencyUtils.setNumberOfThreads(ConcurrencyUtils.nextPow2(oldNthreads))
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_2D)) {
      ConcurrencyUtils.setThreadsBeginN_1D_FFT_2Threads(Integer.MAX_VALUE)
      ConcurrencyUtils.setThreadsBeginN_1D_FFT_4Threads(Integer.MAX_VALUE)
      nthreads = Math.min(nthreads, rows)
      val futures = Array.ofDim[Future](nthreads)
      val k = rows / nthreads
      for (j <- 0 until nthreads) {
        val firstRow = rows - j * k
        val lastRow = if ((j == (nthreads - 1))) 0 else firstRow - k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            var r = firstRow
            while (r >= lastRow) {
              viewRow(r).asInstanceOf[DenseMatrix1D].dst(scale)
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
      ConcurrencyUtils.resetThreadsBeginN_FFT()
    } else {
      for (r <- 0 until rows) {
        viewRow(r).asInstanceOf[DenseMatrix1D].dst(scale)
      }
    }
    ConcurrencyUtils.setNumberOfThreads(oldNthreads)
  }

  def elements(): Array[Double] = elements

  /**
   * Computes the 2D discrete Fourier transform (DFT) of this matrix. The
   * physical layout of the output data is as follows:
   *
   * <pre>
   * this[k1][2*k2] = Re[k1][k2] = Re[rows-k1][columns-k2],
   * this[k1][2*k2+1] = Im[k1][k2] = -Im[rows-k1][columns-k2],
   *       0&lt;k1&lt;rows, 0&lt;k2&lt;columns/2,
   * this[0][2*k2] = Re[0][k2] = Re[0][columns-k2],
   * this[0][2*k2+1] = Im[0][k2] = -Im[0][columns-k2],
   *       0&lt;k2&lt;columns/2,
   * this[k1][0] = Re[k1][0] = Re[rows-k1][0],
   * this[k1][1] = Im[k1][0] = -Im[rows-k1][0],
   * this[rows-k1][1] = Re[k1][columns/2] = Re[rows-k1][columns/2],
   * this[rows-k1][0] = -Im[k1][columns/2] = Im[rows-k1][columns/2],
   *       0&lt;k1&lt;rows/2,
   * this[0][0] = Re[0][0],
   * this[0][1] = Re[0][columns/2],
   * this[rows/2][0] = Re[rows/2][0],
   * this[rows/2][1] = Re[rows/2][columns/2]
   * </pre>
   *
   * This method computes only half of the elements of the real transform. The
   * other half satisfies the symmetry condition. If you want the full real
   * forward transform, use <code>getFft2</code>. To get back the original
   * data, use <code>ifft2</code>.
   *
   * @throws IllegalArgumentException
   *             if the row size or the column size of this matrix is not a
   *             power of 2 number.
   *
   */
  def fft2() {
    val transpose = viewDice().copy()
    val oldNthreads = ConcurrencyUtils.getNumberOfThreads
    ConcurrencyUtils.setNumberOfThreads(ConcurrencyUtils.nextPow2(oldNthreads))
    if (fft2 == null) {
      fft2 = new DoubleFFT_2D(rows, columns)
    }
    fft2.realForward(transpose.elements().asInstanceOf[Array[Double]])
    this.assign(transpose.viewDice().copy())
    ConcurrencyUtils.setNumberOfThreads(oldNthreads)
  }

  def forEachNonZero(function: cern.colt.function.tdouble.Function3): StrideMatrix2D = {
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
  def getRowMajor(): DenseMatrix2D = {
    val R = new DenseMatrix2D(rows, columns)
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

  /**
   * Returns new complex matrix which is the 2D discrete Fourier transform
   * (DFT) of this matrix.
   *
   * @return the 2D discrete Fourier transform (DFT) of this matrix.
   *
   */
  def getFft2(): DenseDComplexMatrix2D = {
    val oldNthreads = ConcurrencyUtils.getNumberOfThreads
    ConcurrencyUtils.setNumberOfThreads(ConcurrencyUtils.nextPow2(oldNthreads))
    if (fft2 == null) {
      fft2 = new DoubleFFT_2D(rows, columns)
    }
    val C = new DenseDComplexMatrix2D(rows, columns)
    val elementsC = (C).elements()
    val zero = index(0, 0).toInt
    val zeroC = C.index(0, 0).toInt
    val rowStrideC = C.rowStride() / 2
    val columnStrideC = 1
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
            var idxOther = zeroC + (rows - 1) * rowStrideC + (firstColumn - 1) * columnStrideC
            var c = firstColumn
            while (c >= lastColumn) {
              var i = idx
              var j = idxOther
              var r = rows
              while (r >= 0) {
                elementsC(j) = elements(i)
                i -= rowStride
                j -= rowStrideC
              }
              idx -= columnStride
              idxOther -= columnStrideC
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
    } else {
      var idx = zero + (rows - 1) * rowStride + (columns - 1) * columnStride
      var idxOther = zeroC + (rows - 1) * rowStrideC + (columns - 1) * columnStrideC
      var c = columns
      while (c >= 0) {
        var i = idx
        var j = idxOther
        var r = rows
        while (r >= 0) {
          elementsC(j) = elements(i)
          i -= rowStride
          j -= rowStrideC
        }
        idx -= columnStride
        idxOther -= columnStrideC
      }
    }
    fft2.realForwardFull(elementsC)
    ConcurrencyUtils.setNumberOfThreads(oldNthreads)
    C
  }

  /**
   * Returns new complex matrix which is the discrete Fourier transform (DFT)
   * of each column of this matrix.
   *
   * @return the discrete Fourier transform (DFT) of each column of this
   *         matrix.
   */
  def getFftColumns(): DenseDComplexMatrix2D = {
    val oldNthreads = ConcurrencyUtils.getNumberOfThreads
    ConcurrencyUtils.setNumberOfThreads(ConcurrencyUtils.nextPow2(oldNthreads))
    val C = new DenseDComplexMatrix2D(rows, columns)
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_2D)) {
      ConcurrencyUtils.setThreadsBeginN_1D_FFT_2Threads(Integer.MAX_VALUE)
      ConcurrencyUtils.setThreadsBeginN_1D_FFT_4Threads(Integer.MAX_VALUE)
      nthreads = Math.min(nthreads, columns)
      val futures = Array.ofDim[Future](nthreads)
      val k = columns / nthreads
      for (j <- 0 until nthreads) {
        val firstColumn = columns - j * k
        val lastColumn = if ((j == (nthreads - 1))) 0 else firstColumn - k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            var c = firstColumn
            while (c >= lastColumn) {
              C.viewColumn(c).assign(viewColumn(c).asInstanceOf[DenseMatrix1D].getFft)
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
      ConcurrencyUtils.resetThreadsBeginN_FFT()
    } else {
      var c = columns
      while (c >= 0) {
        C.viewColumn(c).assign(viewColumn(c).asInstanceOf[DenseMatrix1D].getFft)
      }
    }
    ConcurrencyUtils.setNumberOfThreads(oldNthreads)
    C
  }

  /**
   * Returns new complex matrix which is the discrete Fourier transform (DFT)
   * of each row of this matrix.
   *
   * @return the discrete Fourier transform (DFT) of each row of this matrix.
   */
  def getFftRows(): DenseDComplexMatrix2D = {
    val oldNthreads = ConcurrencyUtils.getNumberOfThreads
    ConcurrencyUtils.setNumberOfThreads(ConcurrencyUtils.nextPow2(oldNthreads))
    val C = new DenseDComplexMatrix2D(rows, columns)
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_2D)) {
      ConcurrencyUtils.setThreadsBeginN_1D_FFT_2Threads(Integer.MAX_VALUE)
      ConcurrencyUtils.setThreadsBeginN_1D_FFT_4Threads(Integer.MAX_VALUE)
      nthreads = Math.min(nthreads, rows)
      val futures = Array.ofDim[Future](nthreads)
      val k = rows / nthreads
      for (j <- 0 until nthreads) {
        val firstRow = rows - j * k
        val lastRow = if ((j == (nthreads - 1))) 0 else firstRow - k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            var r = firstRow
            while (r >= lastRow) {
              C.viewRow(r).assign(viewRow(r).asInstanceOf[DenseMatrix1D].getFft)
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
      ConcurrencyUtils.resetThreadsBeginN_FFT()
    } else {
      var r = rows
      while (r >= 0) {
        C.viewRow(r).assign(viewRow(r).asInstanceOf[DenseMatrix1D].getFft)
      }
    }
    ConcurrencyUtils.setNumberOfThreads(oldNthreads)
    C
  }

  /**
   * Returns new complex matrix which is the 2D inverse of the discrete
   * Fourier transform (IDFT) of this matrix.
   *
   * @return the 2D inverse of the discrete Fourier transform (IDFT) of this
   *         matrix.
   */
  def getIfft2(scale: Boolean): DComplexMatrix2D = {
    val oldNthreads = ConcurrencyUtils.getNumberOfThreads
    ConcurrencyUtils.setNumberOfThreads(ConcurrencyUtils.nextPow2(oldNthreads))
    if (fft2 == null) {
      fft2 = new DoubleFFT_2D(rows, columns)
    }
    val C = new DenseDComplexMatrix2D(rows, columns)
    val elementsC = (C).elements()
    val zero = index(0, 0).toInt
    val zeroC = C.index(0, 0).toInt
    val rowStrideC = C.rowStride() / 2
    val columnStrideC = 1
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
            var idxOther = zeroC + (rows - 1) * rowStrideC + (firstColumn - 1) * columnStrideC
            var c = firstColumn
            while (c >= lastColumn) {
              var i = idx
              var j = idxOther
              var r = rows
              while (r >= 0) {
                elementsC(j) = elements(i)
                i -= rowStride
                j -= rowStrideC
              }
              idx -= columnStride
              idxOther -= columnStrideC
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
    } else {
      var idx = zero + (rows - 1) * rowStride + (columns - 1) * columnStride
      var idxOther = zeroC + (rows - 1) * rowStrideC + (columns - 1) * columnStrideC
      var c = columns
      while (c >= 0) {
        var i = idx
        var j = idxOther
        var r = rows
        while (r >= 0) {
          elementsC(j) = elements(i)
          i -= rowStride
          j -= rowStrideC
        }
        idx -= columnStride
        idxOther -= columnStrideC
      }
    }
    fft2.realInverseFull(elementsC, scale)
    ConcurrencyUtils.setNumberOfThreads(oldNthreads)
    C
  }

  /**
   * Returns new complex matrix which is the inverse of the discrete Fourier
   * transform (IDFT) of each column of this matrix.
   *
   * @return the inverse of the discrete Fourier transform (IDFT) of each
   *         column of this matrix.
   */
  def getIfftColumns(scale: Boolean): DComplexMatrix2D = {
    val oldNthreads = ConcurrencyUtils.getNumberOfThreads
    ConcurrencyUtils.setNumberOfThreads(ConcurrencyUtils.nextPow2(oldNthreads))
    val C = new DenseDComplexMatrix2D(rows, columns)
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_2D)) {
      ConcurrencyUtils.setThreadsBeginN_1D_FFT_2Threads(Integer.MAX_VALUE)
      ConcurrencyUtils.setThreadsBeginN_1D_FFT_4Threads(Integer.MAX_VALUE)
      nthreads = Math.min(nthreads, columns)
      val futures = Array.ofDim[Future](nthreads)
      val k = columns / nthreads
      for (j <- 0 until nthreads) {
        val firstColumn = columns - j * k
        val lastColumn = if ((j == (nthreads - 1))) 0 else firstColumn - k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            var c = firstColumn
            while (c >= lastColumn) {
              C.viewColumn(c).assign(viewColumn(c).asInstanceOf[DenseMatrix1D].getIfft(scale))
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
      ConcurrencyUtils.resetThreadsBeginN_FFT()
    } else {
      var c = columns
      while (c >= 0) {
        C.viewColumn(c).assign(viewColumn(c).asInstanceOf[DenseMatrix1D].getIfft(scale))
      }
    }
    ConcurrencyUtils.setNumberOfThreads(oldNthreads)
    C
  }

  /**
   * Returns new complex matrix which is the inverse of the discrete Fourier
   * transform (IDFT) of each row of this matrix.
   *
   * @return the inverse of the discrete Fourier transform (IDFT) of each row
   *         of this matrix.
   */
  def getIfftRows(scale: Boolean): DComplexMatrix2D = {
    val oldNthreads = ConcurrencyUtils.getNumberOfThreads
    ConcurrencyUtils.setNumberOfThreads(ConcurrencyUtils.nextPow2(oldNthreads))
    val C = new DenseDComplexMatrix2D(rows, columns)
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_2D)) {
      ConcurrencyUtils.setThreadsBeginN_1D_FFT_2Threads(Integer.MAX_VALUE)
      ConcurrencyUtils.setThreadsBeginN_1D_FFT_4Threads(Integer.MAX_VALUE)
      nthreads = Math.min(nthreads, rows)
      val futures = Array.ofDim[Future](nthreads)
      val k = rows / nthreads
      for (j <- 0 until nthreads) {
        val firstRow = rows - j * k
        val lastRow = if ((j == (nthreads - 1))) 0 else firstRow - k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            var r = firstRow
            while (r >= lastRow) {
              C.viewRow(r).assign(viewRow(r).asInstanceOf[DenseMatrix1D].getIfft(scale))
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
      ConcurrencyUtils.resetThreadsBeginN_FFT()
    } else {
      var r = rows
      while (r >= 0) {
        C.viewRow(r).assign(viewRow(r).asInstanceOf[DenseMatrix1D].getIfft(scale))
      }
    }
    ConcurrencyUtils.setNumberOfThreads(oldNthreads)
    C
  }

  def getNegativeValues(rowList: IntArrayList, columnList: IntArrayList, valueList: DoubleArrayList) {
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

  def getNonZeros(rowList: IntArrayList, columnList: IntArrayList, valueList: DoubleArrayList) {
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

  def getPositiveValues(rowList: IntArrayList, columnList: IntArrayList, valueList: DoubleArrayList) {
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

  def getQuick(row: Int, column: Int): Double = {
    elements(rowZero + row * rowStride + columnZero + column * columnStride)
  }

  /**
   * Computes the 2D inverse of the discrete cosine transform (DCT-III) of
   * this matrix.
   *
   * @param scale
   *            if true then scaling is performed
   *
   */
  def idct2(scale: Boolean) {
    val transpose = viewDice().copy()
    val oldNthreads = ConcurrencyUtils.getNumberOfThreads
    ConcurrencyUtils.setNumberOfThreads(ConcurrencyUtils.nextPow2(oldNthreads))
    if (dct2 == null) {
      dct2 = new DoubleDCT_2D(rows, columns)
    }
    dct2.inverse(transpose.elements().asInstanceOf[Array[Double]], scale)
    this.assign(transpose.viewDice().copy())
    ConcurrencyUtils.setNumberOfThreads(oldNthreads)
  }

  /**
   * Computes the inverse of the discrete cosine transform (DCT-III) of each
   * column of this matrix.
   *
   * @param scale
   *            if true then scaling is performed
   *
   */
  def idctColumns(scale: Boolean) {
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_2D)) {
      ConcurrencyUtils.setThreadsBeginN_1D_FFT_2Threads(Integer.MAX_VALUE)
      ConcurrencyUtils.setThreadsBeginN_1D_FFT_4Threads(Integer.MAX_VALUE)
      nthreads = Math.min(nthreads, columns)
      val futures = Array.ofDim[Future](nthreads)
      val k = columns / nthreads
      for (j <- 0 until nthreads) {
        val firstColumn = columns - j * k
        val lastColumn = if ((j == (nthreads - 1))) 0 else firstColumn - k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            var c = firstColumn
            while (c >= lastColumn) {
              viewColumn(c).asInstanceOf[DenseMatrix1D].idct(scale)
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
      ConcurrencyUtils.resetThreadsBeginN_FFT()
    } else {
      var c = columns
      while (c >= 0) {
        viewColumn(c).asInstanceOf[DenseMatrix1D].idct(scale)
      }
    }
  }

  /**
   * Computes the inverse of the discrete cosine transform (DCT-III) of each
   * row of this matrix.
   *
   * @param scale
   *            if true then scaling is performed
   *
   */
  def idctRows(scale: Boolean) {
    val oldNthreads = ConcurrencyUtils.getNumberOfThreads
    ConcurrencyUtils.setNumberOfThreads(ConcurrencyUtils.nextPow2(oldNthreads))
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_2D)) {
      ConcurrencyUtils.setThreadsBeginN_1D_FFT_2Threads(Integer.MAX_VALUE)
      ConcurrencyUtils.setThreadsBeginN_1D_FFT_4Threads(Integer.MAX_VALUE)
      nthreads = Math.min(nthreads, rows)
      val futures = Array.ofDim[Future](nthreads)
      val k = rows / nthreads
      for (j <- 0 until nthreads) {
        val firstRow = rows - j * k
        val lastRow = if ((j == (nthreads - 1))) 0 else firstRow - k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            var r = firstRow
            while (r >= lastRow) {
              viewRow(r).asInstanceOf[DenseMatrix1D].idct(scale)
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
      ConcurrencyUtils.resetThreadsBeginN_FFT()
    } else {
      for (r <- 0 until rows) {
        viewRow(r).asInstanceOf[DenseMatrix1D].idct(scale)
      }
    }
    ConcurrencyUtils.setNumberOfThreads(oldNthreads)
  }

  /**
   * Computes the 2D inverse of the discrete Hartley transform (IDHT) of this
   * matrix.
   *
   * @param scale
   *            if true then scaling is performed
   *
   */
  def idht2(scale: Boolean) {
    val transpose = viewDice().copy()
    val oldNthreads = ConcurrencyUtils.getNumberOfThreads
    ConcurrencyUtils.setNumberOfThreads(ConcurrencyUtils.nextPow2(oldNthreads))
    if (dht2 == null) {
      dht2 = new DoubleDHT_2D(rows, columns)
    }
    dht2.inverse(transpose.elements().asInstanceOf[Array[Double]], scale)
    this.assign(transpose.viewDice().copy())
    ConcurrencyUtils.setNumberOfThreads(oldNthreads)
  }

  /**
   * Computes the inverse of the discrete Hartley transform (IDHT) of each
   * column of this matrix.
   *
   * @param scale
   *            if true then scaling is performed
   *
   */
  def idhtColumns(scale: Boolean) {
    val oldNthreads = ConcurrencyUtils.getNumberOfThreads
    ConcurrencyUtils.setNumberOfThreads(ConcurrencyUtils.nextPow2(oldNthreads))
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_2D)) {
      ConcurrencyUtils.setThreadsBeginN_1D_FFT_2Threads(Integer.MAX_VALUE)
      ConcurrencyUtils.setThreadsBeginN_1D_FFT_4Threads(Integer.MAX_VALUE)
      nthreads = Math.min(nthreads, columns)
      val futures = Array.ofDim[Future](nthreads)
      val k = columns / nthreads
      for (j <- 0 until nthreads) {
        val firstColumn = columns - j * k
        val lastColumn = if ((j == (nthreads - 1))) 0 else firstColumn - k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            var c = firstColumn
            while (c >= lastColumn) {
              viewColumn(c).asInstanceOf[DenseMatrix1D].idht(scale)
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
      ConcurrencyUtils.resetThreadsBeginN_FFT()
    } else {
      var c = columns
      while (c >= 0) {
        viewColumn(c).asInstanceOf[DenseMatrix1D].idht(scale)
      }
    }
    ConcurrencyUtils.setNumberOfThreads(oldNthreads)
  }

  /**
   * Computes the inverse of the discrete Hartley transform (IDHT) of each row
   * of this matrix.
   *
   * @param scale
   *            if true then scaling is performed
   *
   */
  def idhtRows(scale: Boolean) {
    val oldNthreads = ConcurrencyUtils.getNumberOfThreads
    ConcurrencyUtils.setNumberOfThreads(ConcurrencyUtils.nextPow2(oldNthreads))
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_2D)) {
      ConcurrencyUtils.setThreadsBeginN_1D_FFT_2Threads(Integer.MAX_VALUE)
      ConcurrencyUtils.setThreadsBeginN_1D_FFT_4Threads(Integer.MAX_VALUE)
      nthreads = Math.min(nthreads, rows)
      val futures = Array.ofDim[Future](nthreads)
      val k = rows / nthreads
      for (j <- 0 until nthreads) {
        val firstRow = rows - j * k
        val lastRow = if ((j == (nthreads - 1))) 0 else firstRow - k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            var r = firstRow
            while (r >= lastRow) {
              viewRow(r).asInstanceOf[DenseMatrix1D].idht(scale)
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
      ConcurrencyUtils.resetThreadsBeginN_FFT()
    } else {
      for (r <- 0 until rows) {
        viewRow(r).asInstanceOf[DenseMatrix1D].idht(scale)
      }
    }
    ConcurrencyUtils.setNumberOfThreads(oldNthreads)
  }

  /**
   * Computes the 2D inverse of the discrete sine transform (DST-III) of this
   * matrix.
   *
   * @param scale
   *            if true then scaling is performed
   *
   */
  def idst2(scale: Boolean) {
    val transpose = viewDice().copy()
    val oldNthreads = ConcurrencyUtils.getNumberOfThreads
    ConcurrencyUtils.setNumberOfThreads(ConcurrencyUtils.nextPow2(oldNthreads))
    if (dst2 == null) {
      dst2 = new DoubleDST_2D(rows, columns)
    }
    dst2.inverse(transpose.elements().asInstanceOf[Array[Double]], scale)
    this.assign(transpose.viewDice().copy())
    ConcurrencyUtils.setNumberOfThreads(oldNthreads)
  }

  /**
   * Computes the inverse of the discrete sine transform (DST-III) of each
   * column of this matrix.
   *
   * @param scale
   *            if true then scaling is performed
   *
   */
  def idstColumns(scale: Boolean) {
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_2D)) {
      ConcurrencyUtils.setThreadsBeginN_1D_FFT_2Threads(Integer.MAX_VALUE)
      ConcurrencyUtils.setThreadsBeginN_1D_FFT_4Threads(Integer.MAX_VALUE)
      nthreads = Math.min(nthreads, columns)
      val futures = Array.ofDim[Future](nthreads)
      val k = columns / nthreads
      for (j <- 0 until nthreads) {
        val firstColumn = columns - j * k
        val lastColumn = if ((j == (nthreads - 1))) 0 else firstColumn - k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            var c = firstColumn
            while (c >= lastColumn) {
              viewColumn(c).asInstanceOf[DenseMatrix1D].idst(scale)
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
      ConcurrencyUtils.resetThreadsBeginN_FFT()
    } else {
      var c = columns
      while (c >= 0) {
        viewColumn(c).asInstanceOf[DenseMatrix1D].idst(scale)
      }
    }
  }

  /**
   * Computes the inverse of the discrete sine transform (DST-III) of each row
   * of this matrix.
   *
   * @param scale
   *            if true then scaling is performed
   *
   */
  def idstRows(scale: Boolean) {
    val oldNthreads = ConcurrencyUtils.getNumberOfThreads
    ConcurrencyUtils.setNumberOfThreads(ConcurrencyUtils.nextPow2(oldNthreads))
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_2D)) {
      ConcurrencyUtils.setThreadsBeginN_1D_FFT_2Threads(Integer.MAX_VALUE)
      ConcurrencyUtils.setThreadsBeginN_1D_FFT_4Threads(Integer.MAX_VALUE)
      nthreads = Math.min(nthreads, rows)
      val futures = Array.ofDim[Future](nthreads)
      val k = rows / nthreads
      for (j <- 0 until nthreads) {
        val firstRow = rows - j * k
        val lastRow = if ((j == (nthreads - 1))) 0 else firstRow - k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            var r = firstRow
            while (r >= lastRow) {
              viewRow(r).asInstanceOf[DenseMatrix1D].idst(scale)
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
      ConcurrencyUtils.resetThreadsBeginN_FFT()
    } else {
      for (r <- 0 until rows) {
        viewRow(r).asInstanceOf[DenseMatrix1D].idst(scale)
      }
    }
    ConcurrencyUtils.setNumberOfThreads(oldNthreads)
  }

  /**
   * Computes the 2D inverse of the discrete Fourier transform (IDFT) of this
   * matrix. The physical layout of the input data has to be as follows:
   *
   * <pre>
   * this[k1][2*k2] = Re[k1][k2] = Re[rows-k1][columns-k2],
   * this[k1][2*k2+1] = Im[k1][k2] = -Im[rows-k1][columns-k2],
   *       0&lt;k1&lt;rows, 0&lt;k2&lt;columns/2,
   * this[0][2*k2] = Re[0][k2] = Re[0][columns-k2],
   * this[0][2*k2+1] = Im[0][k2] = -Im[0][columns-k2],
   *       0&lt;k2&lt;columns/2,
   * this[k1][0] = Re[k1][0] = Re[rows-k1][0],
   * this[k1][1] = Im[k1][0] = -Im[rows-k1][0],
   * this[rows-k1][1] = Re[k1][columns/2] = Re[rows-k1][columns/2],
   * this[rows-k1][0] = -Im[k1][columns/2] = Im[rows-k1][columns/2],
   *       0&lt;k1&lt;rows/2,
   * this[0][0] = Re[0][0],
   * this[0][1] = Re[0][columns/2],
   * this[rows/2][0] = Re[rows/2][0],
   * this[rows/2][1] = Re[rows/2][columns/2]
   * </pre>
   *
   * This method computes only half of the elements of the real transform. The
   * other half satisfies the symmetry condition. If you want the full real
   * inverse transform, use <code>getIfft2</code>.
   *
   * @throws IllegalArgumentException
   *             if the row size or the column size of this matrix is not a
   *             power of 2 number.
   *
   * @param scale
   *            if true then scaling is performed
   *
   */
  def ifft2(scale: Boolean) {
    val transpose = viewDice().copy()
    val oldNthreads = ConcurrencyUtils.getNumberOfThreads
    ConcurrencyUtils.setNumberOfThreads(ConcurrencyUtils.nextPow2(oldNthreads))
    if (fft2 == null) {
      fft2 = new DoubleFFT_2D(rows, columns)
    }
    fft2.realInverse(transpose.elements().asInstanceOf[Array[Double]], scale)
    this.assign(transpose.viewDice().copy())
    ConcurrencyUtils.setNumberOfThreads(oldNthreads)
  }

  def index(row: Int, column: Int): Long = {
    rowZero + row * rowStride + columnZero + column * columnStride
  }

  def like(rows: Int, columns: Int): StrideMatrix2D = {
    new DenseColumnDoubleMatrix2D(rows, columns)
  }

  def like1D(size: Int): StrideMatrix1D = new DenseMatrix1D(size)

  def getMaxLocation(): Array[Double] = {
    var rowLocation = 0
    var columnLocation = 0
    val zero = index(0, 0).toInt
    var maxValue = 0
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_2D)) {
      nthreads = Math.min(nthreads, columns)
      val futures = Array.ofDim[Future](nthreads)
      val results = Array.ofDim[Double](nthreads, 3)
      val k = columns / nthreads
      for (j <- 0 until nthreads) {
        val firstColumn = columns - j * k
        val lastColumn = if ((j == (nthreads - 1))) 0 else firstColumn - k
        futures(j) = ConcurrencyUtils.submit(new Callable[Array[Double]]() {

          def call(): Array[Double] = {
            var maxValue = elements(zero + (rows - 1) * rowStride + (firstColumn - 1) * columnStride)
            var rowLocation = rows - 1
            var columnLocation = firstColumn - 1
            var elem: Double = 0.0
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
          results(j) = futures(j).get.asInstanceOf[Array[Double]]
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
      var elem: Double = 0.0
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

  def getMinLocation(): Array[Double] = {
    var rowLocation = 0
    var columnLocation = 0
    val zero = index(0, 0).toInt
    var minValue = 0
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_2D)) {
      nthreads = Math.min(nthreads, columns)
      val futures = Array.ofDim[Future](nthreads)
      val results = Array.ofDim[Double](nthreads, 3)
      val k = columns / nthreads
      for (j <- 0 until nthreads) {
        val firstColumn = columns - j * k
        val lastColumn = if ((j == (nthreads - 1))) 0 else firstColumn - k
        futures(j) = ConcurrencyUtils.submit(new Callable[Array[Double]]() {

          def call(): Array[Double] = {
            var minValue = elements(zero + (rows - 1) * rowStride + (firstColumn - 1) * columnStride)
            var rowLocation = rows - 1
            var columnLocation = firstColumn - 1
            var elem: Double = 0.0
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
          results(j) = futures(j).get.asInstanceOf[Array[Double]]
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
      var elem: Double = 0.0
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

  def setQuick(row: Int, column: Int, value: Double) {
    elements(rowZero + row * rowStride + columnZero + column * columnStride) = value
  }

  def toArray(): Array[Array[Double]] = {
    val values = Array.ofDim[Double](rows, columns)
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

  def vectorize(): StrideMatrix1D = {
    val size = size.toInt
    val v = new DenseMatrix1D(size)
    if (isNoView == true) {
      System.arraycopy(elements, 0, v.elements(), 0, size)
    } else {
      val zero = index(0, 0).toInt
      val zeroOther = v.index(0).toInt
      val strideOther = v.stride()
      val elementsOther = v.elements().asInstanceOf[Array[Double]]
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

  def zMult(y: StrideMatrix1D,
      z: StrideMatrix1D,
      alpha: Double,
      beta: Double,
      transposeA: Boolean): StrideMatrix1D = {
    if (z == null) {
      z = new DenseMatrix1D(if (transposeA) columns else rows)
    }
    if ((if (transposeA) rows else columns) != y.size || (if (transposeA) columns else rows) > z.size) throw new IllegalArgumentException("Incompatible args: " + toShapeString() + ", " + y.toShapeString() +
      ", " +
      z.toShapeString())
    if (!(y.isInstanceOf[DenseMatrix1D]) || !(z.isInstanceOf[DenseMatrix1D]) ||
      this.isView ||
      y.isView ||
      z.isView) return super.zMult(y, z, alpha, beta, transposeA)
    val yElements = y.elements().asInstanceOf[Array[Double]]
    val zElements = z.elements().asInstanceOf[Array[Double]]
    val transA = if (transposeA) Transpose.Transpose else Transpose.NoTranspose
    BLAS.getInstance.dgemv(transA.netlib(), rows, columns, alpha, elements, Math.max(rows, 1), yElements,
      1, beta, zElements, 1)
    z
  }

  def zMult(B: StrideMatrix2D,
      C: StrideMatrix2D,
      alpha: Double,
      beta: Double,
      transposeA: Boolean,
      transposeB: Boolean): StrideMatrix2D = {
    val rowsA = if (transposeA) columns else rows
    val columnsA = if (transposeA) rows else columns
    val rowsB = if (transposeB) B.columns() else B.rows()
    val columnsB = if (transposeB) B.rows() else B.columns()
    val rowsC = rowsA
    val columnsC = columnsB
    if (columnsA != rowsB) {
      throw new IllegalArgumentException("Matrix2D inner dimensions must agree:" + this.toShapeString() +
        ", " +
        B.toShapeString())
    }
    if (C == null) {
      C = new DenseColumnDoubleMatrix2D(rowsC, columnsC)
    } else {
      if (rowsA != C.rows() || columnsB != C.columns()) {
        throw new IllegalArgumentException("Incompatibe result matrix: " + this.toShapeString() +
          ", " +
          B.toShapeString() +
          ", " +
          C.toShapeString())
      }
    }
    if (this == C || B == C) throw new IllegalArgumentException("Matrices must not be identical")
    if (!(B.isInstanceOf[DenseColumnDoubleMatrix2D]) || !(C.isInstanceOf[DenseColumnDoubleMatrix2D]) ||
      this.isView ||
      B.isView ||
      C.isView) return super.zMult(B, C, alpha, beta, transposeA, transposeB)
    val transA = if (transposeA) Transpose.Transpose else Transpose.NoTranspose
    val transB = if (transposeB) Transpose.Transpose else Transpose.NoTranspose
    val elementsA = elements
    val elementsB = B.elements().asInstanceOf[Array[Double]]
    val elementsC = C.elements().asInstanceOf[Array[Double]]
    val lda = if (transposeA) Math.max(1, columnsA) else Math.max(1, rowsA)
    val ldb = if (transposeB) Math.max(1, columnsB) else Math.max(1, rowsB)
    val ldc = Math.max(1, rowsA)
    BLAS.getInstance.dgemm(transA.netlib(), transB.netlib(), rowsA, columnsB, columnsA, alpha, elementsA,
      lda, elementsB, ldb, beta, elementsC, ldc)
    C
  }

  def zSum(): Double = {
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
        futures(j) = ConcurrencyUtils.submit(new Callable[Double]() {

          def call(): java.lang.Double = {
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
          sum += futures(j).get.asInstanceOf[java.lang.Double]
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

  protected def haveSharedCellsRaw(other: StrideMatrix2D): Boolean = {
    if (other.isInstanceOf[SelectedDenseColumnDoubleMatrix2D]) {
      val otherMatrix = other.asInstanceOf[SelectedDenseColumnDoubleMatrix2D]
      return this.elements == otherMatrix.elements
    } else if (other.isInstanceOf[DenseColumnDoubleMatrix2D]) {
      val otherMatrix = other.asInstanceOf[DenseColumnDoubleMatrix2D]
      return this.elements == otherMatrix.elements
    }
    false
  }

  protected def like1D(size: Int, zero: Int, stride: Int): StrideMatrix1D = {
    new DenseMatrix1D(size, this.elements, zero, stride, true)
  }

  protected def viewSelectionLike(rowOffsets: Array[Int], columnOffsets: Array[Int]): StrideMatrix2D = {
    new SelectedDenseColumnDoubleMatrix2D(this.elements, rowOffsets, columnOffsets, 0)
  }
}
