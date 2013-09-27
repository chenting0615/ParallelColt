package cern.colt.matrix.tdouble.impl

import java.util.concurrent.Callable
import java.util.concurrent.ExecutionException
import java.util.concurrent.Future
import cern.colt.list.tdouble.DoubleArrayList
import cern.colt.list.tint.IntArrayList
import cern.colt.matrix.tdcomplex.impl.DenseDComplexMatrix3D
import cern.colt.matrix.tdouble.StrideMatrix1D
import cern.colt.matrix.tdouble.StrideMatrix2D
import cern.colt.matrix.tdouble.DoubleMatrix3D
import edu.emory.mathcs.jtransforms.dct.DoubleDCT_3D
import edu.emory.mathcs.jtransforms.dht.DoubleDHT_3D
import edu.emory.mathcs.jtransforms.dst.DoubleDST_3D
import edu.emory.mathcs.jtransforms.fft.DoubleFFT_3D
import edu.emory.mathcs.utils.ConcurrencyUtils
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Dense 3-d matrix holding <tt>double</tt> elements. First see the <a
 * href="package-summary.html">package summary</a> and javadoc <a
 * href="package-tree.html">tree view</a> to get the broad picture.
 * <p>
 * <b>Implementation:</b>
 * <p>
 * Internally holds one single contiguous one-dimensional array, addressed in
 * (in decreasing order of significance): slice major, row major, column major.
 * Note that this implementation is not synchronized.
 * <p>
 * <b>Time complexity:</b>
 * <p>
 * <tt>O(1)</tt> (i.e. constant time) for the basic operations <tt>get</tt>,
 * <tt>getQuick</tt>, <tt>set</tt>, <tt>setQuick</tt> and <tt>size</tt>,
 * <p>
 * Applications demanding utmost speed can exploit knowledge about the internal
 * addressing. Setting/getting values in a loop slice-by-slice, row-by-row,
 * column-by-column is quicker than, for example, column-by-column, row-by-row,
 * slice-by-slice. Thus
 *
 * <pre>
 * for (int slice = 0; slice &lt; slices; slice++) {
 *     for (int row = 0; row &lt; rows; row++) {
 *         for (int column = 0; column &lt; columns; column++) {
 *             matrix.setQuick(slice, row, column, someValue);
 *         }
 *     }
 * }
 * </pre>
 *
 * is quicker than
 *
 * <pre>
 * for (int column = 0; column &lt; columns; column++) {
 *     for (int row = 0; row &lt; rows; row++) {
 *         for (int slice = 0; slice &lt; slices; slice++) {
 *             matrix.setQuick(slice, row, column, someValue);
 *         }
 *     }
 * }
 * </pre>
 *
 * @author wolfgang.hoschek@cern.ch
 * @version 1.0, 09/24/99
 *
 * @author Piotr Wendykier (piotr.wendykier@gmail.com)
 */
@SerialVersionUID(1L)
class DenseDoubleMatrix3D(slices: Int, rows: Int, columns: Int) extends DoubleMatrix3D {

  private var fft3: DoubleFFT_3D = _

  private var dct3: DoubleDCT_3D = _

  private var dst3: DoubleDST_3D = _

  private var dht3: DoubleDHT_3D = _

  protected var elements: Array[Double] = new Array[Double](slices * rows * columns)

  setUp(slices, rows, columns)

  /**
   * Constructs a matrix with a copy of the given values. <tt>values</tt> is
   * required to have the form <tt>values[slice][row][column]</tt> and have
   * exactly the same number of rows in in every slice and exactly the same
   * number of columns in in every row.
   * <p>
   * The values are copied. So subsequent changes in <tt>values</tt> are not
   * reflected in the matrix, and vice-versa.
   *
   * @param values
   *            The values to be filled into the new matrix.
   * @throws IllegalArgumentException
   *             if
   *             <tt>for any 1 &lt;= slice &lt; values.length: values[slice].length != values[slice-1].length</tt>
   *             .
   * @throws IllegalArgumentException
   *             if
   *             <tt>for any 1 &lt;= row &lt; values[0].length: values[slice][row].length != values[slice][row-1].length</tt>
   *             .
   */
  def this(values: Array[Array[Array[Double]]]) {
    this(values.length, (if (values.length == 0) 0 else values(0).length), (if (values.length == 0) 0 else if (values(0).length == 0) 0 else values(0)(0).length))
    assign(values)
  }

  /**
   * Constructs a matrix with the given parameters.
   *
   * @param slices
   *            the number of slices the matrix shall have.
   * @param rows
   *            the number of rows the matrix shall have.
   * @param columns
   *            the number of columns the matrix shall have.
   * @param elements
   *            the cells.
   * @param sliceZero
   *            the position of the first element.
   * @param rowZero
   *            the position of the first element.
   * @param columnZero
   *            the position of the first element.
   * @param sliceStride
   *            the number of elements between two slices, i.e.
   *            <tt>index(k+1,i,j)-index(k,i,j)</tt>.
   * @param rowStride
   *            the number of elements between two rows, i.e.
   *            <tt>index(k,i+1,j)-index(k,i,j)</tt>.
   * @param columnStride
   *            the number of elements between two columns, i.e.
   *            <tt>index(k,i,j+1)-index(k,i,j)</tt>.
   * @param isView
   *            if true then a matrix view is constructed
   * @throws IllegalArgumentException
   *             if <tt>(double)slices*columns*rows > Integer.MAX_VALUE</tt>.
   * @throws IllegalArgumentException
   *             if <tt>slices<0 || rows<0 || columns<0</tt>.
   */
  def this(slices: Int,
      rows: Int,
      columns: Int,
      elements: Array[Double],
      sliceZero: Int,
      rowZero: Int,
      columnZero: Int,
      sliceStride: Int,
      rowStride: Int,
      columnStride: Int,
      isView: Boolean) {
    this()
    setUp(slices, rows, columns, sliceZero, rowZero, columnZero, sliceStride, rowStride, columnStride)
    this.elements = elements
    this.isNoView = !isView
  }

  def aggregate(aggr: cern.colt.function.tdouble.DoubleDoubleFunction, f: cern.colt.function.tdouble.Function1): Double = {
    if (size == 0) return Double.NaN
    var a = 0
    val zero = index(0, 0, 0).toInt
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_3D)) {
      nthreads = Math.min(nthreads, slices)
      val futures = Array.ofDim[Future](nthreads)
      val k = slices / nthreads
      for (j <- 0 until nthreads) {
        val firstSlice = j * k
        val lastSlice = if ((j == nthreads - 1)) slices else firstSlice + k
        futures(j) = ConcurrencyUtils.submit(new Callable[Double]() {

          def call(): java.lang.Double = {
            var a = f.apply(elements(zero + firstSlice * sliceStride))
            var d = 1
            for (s <- firstSlice until lastSlice; r <- 0 until rows) {
              for (c <- d until columns) {
                a = aggr.apply(a, f.apply(elements(zero + s * sliceStride + r * rowStride + c * columnStride)))
              }
              d = 0
            }
            return a
          }
        })
      }
      a = ConcurrencyUtils.waitForCompletion(futures, aggr)
    } else {
      a = f.apply(elements(zero))
      var d = 1
      for (s <- 0 until slices; r <- 0 until rows) {
        for (c <- d until columns) {
          a = aggr.apply(a, f.apply(elements(zero + s * sliceStride + r * rowStride + c * columnStride)))
        }
        d = 0
      }
    }
    a
  }

  def aggregate(aggr: cern.colt.function.tdouble.DoubleDoubleFunction, f: cern.colt.function.tdouble.Function1, cond: cern.colt.function.tdouble.Procedure1): Double = {
    if (size == 0) return Double.NaN
    var a = 0
    val zero = index(0, 0, 0).toInt
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) &&
      (slices * rows * columns >= ConcurrencyUtils.getThreadsBeginN_3D)) {
      nthreads = Math.min(nthreads, slices)
      val futures = Array.ofDim[Future](nthreads)
      val k = slices / nthreads
      for (j <- 0 until nthreads) {
        val firstSlice = j * k
        val lastSlice = if ((j == nthreads - 1)) slices else firstSlice + k
        futures(j) = ConcurrencyUtils.submit(new Callable[Double]() {

          def call(): java.lang.Double = {
            var elem = elements(zero + firstSlice * sliceStride)
            var a = 0
            if (cond.apply(elem) == true) {
              a = aggr.apply(a, f.apply(elem))
            }
            var d = 1
            for (s <- firstSlice until lastSlice; r <- 0 until rows; c <- d until columns) {
              elem = elements(zero + s * sliceStride + r * rowStride + c * columnStride)
              if (cond.apply(elem) == true) {
                a = aggr.apply(a, f.apply(elem))
              }
              d = 0
            }
            return a
          }
        })
      }
      a = ConcurrencyUtils.waitForCompletion(futures, aggr)
    } else {
      var elem = elements(zero)
      if (cond.apply(elem) == true) {
        a = aggr.apply(a, f.apply(elem))
      }
      var d = 1
      for (s <- 0 until slices; r <- 0 until rows; c <- d until columns) {
        elem = elements(zero + s * sliceStride + r * rowStride + c * columnStride)
        if (cond.apply(elem) == true) {
          a = aggr.apply(a, f.apply(elem))
        }
        d = 0
      }
    }
    a
  }

  def aggregate(aggr: cern.colt.function.tdouble.DoubleDoubleFunction,
      f: cern.colt.function.tdouble.Function1,
      sliceList: IntArrayList,
      rowList: IntArrayList,
      columnList: IntArrayList): Double = {
    if (size == 0) return Double.NaN
    if (sliceList.size == 0 || rowList.size == 0 || columnList.size == 0) return Double.NaN
    val size = sliceList.size
    val sliceElements = sliceList.elements()
    val rowElements = rowList.elements()
    val columnElements = columnList.elements()
    val zero = index(0, 0, 0).toInt
    var a = 0
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_3D)) {
      nthreads = Math.min(nthreads, slices)
      val futures = Array.ofDim[Future](nthreads)
      val k = size / nthreads
      for (j <- 0 until nthreads) {
        val firstIdx = j * k
        val lastIdx = if ((j == nthreads - 1)) size else firstIdx + k
        futures(j) = ConcurrencyUtils.submit(new Callable[Double]() {

          def call(): java.lang.Double = {
            var a = f.apply(elements(zero + sliceElements(firstIdx) * sliceStride + rowElements(firstIdx) * rowStride +
              columnElements(firstIdx) * columnStride))
            var elem: Double = 0.0
            for (i <- firstIdx + 1 until lastIdx) {
              elem = elements(zero + sliceElements(i) * sliceStride + rowElements(i) * rowStride +
                columnElements(i) * columnStride)
              a = aggr.apply(a, f.apply(elem))
            }
            return a
          }
        })
      }
      a = ConcurrencyUtils.waitForCompletion(futures, aggr)
    } else {
      a = f.apply(elements(zero + sliceElements(0) * sliceStride + rowElements(0) * rowStride +
        columnElements(0) * columnStride))
      var elem: Double = 0.0
      for (i <- 1 until size) {
        elem = elements(zero + sliceElements(i) * sliceStride + rowElements(i) * rowStride +
          columnElements(i) * columnStride)
        a = aggr.apply(a, f.apply(elem))
      }
    }
    a
  }

  def aggregate(other: DoubleMatrix3D, aggr: cern.colt.function.tdouble.DoubleDoubleFunction, f: cern.colt.function.tdouble.DoubleDoubleFunction): Double = {
    if (!(other.isInstanceOf[DenseDoubleMatrix3D])) {
      return super.aggregate(other, aggr, f)
    }
    checkShape(other)
    if (size == 0) return Double.NaN
    var a = 0
    val zero = index(0, 0, 0).toInt
    val zeroOther = other.index(0, 0, 0).toInt
    val sliceStrideOther = other.sliceStride()
    val rowStrideOther = other.rowStride()
    val columnStrideOther = other.columnStride()
    val elementsOther = other.elements().asInstanceOf[Array[Double]]
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_3D)) {
      nthreads = Math.min(nthreads, slices)
      val futures = Array.ofDim[Future](nthreads)
      val k = slices / nthreads
      for (j <- 0 until nthreads) {
        val firstSlice = j * k
        val lastSlice = if ((j == nthreads - 1)) slices else firstSlice + k
        futures(j) = ConcurrencyUtils.submit(new Callable[Double]() {

          def call(): java.lang.Double = {
            var idx = zero + firstSlice * sliceStride
            var idxOther = zeroOther + firstSlice * sliceStrideOther
            var a = f.apply(elements(idx), elementsOther(idxOther))
            var d = 1
            for (s <- firstSlice until lastSlice; r <- 0 until rows) {
              for (c <- d until columns) {
                idx = zero + s * sliceStride + r * rowStride + c * columnStride
                idxOther = zeroOther + s * sliceStrideOther + r * rowStrideOther +
                  c * columnStrideOther
                a = aggr.apply(a, f.apply(elements(idx), elementsOther(idxOther)))
              }
              d = 0
            }
            return a
          }
        })
      }
      a = ConcurrencyUtils.waitForCompletion(futures, aggr)
    } else {
      a = f.apply(getQuick(0, 0, 0), other.getQuick(0, 0, 0))
      var d = 1
      var idx: Int = 0
      var idxOther: Int = 0
      for (s <- 0 until slices; r <- 0 until rows) {
        for (c <- d until columns) {
          idx = zero + s * sliceStride + r * rowStride + c * columnStride
          idxOther = zeroOther + s * sliceStrideOther + r * rowStrideOther +
            c * columnStrideOther
          a = aggr.apply(a, f.apply(elements(idx), elementsOther(idxOther)))
        }
        d = 0
      }
    }
    a
  }

  def assign(function: cern.colt.function.tdouble.Function1): DoubleMatrix3D = {
    val zero = index(0, 0, 0).toInt
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_3D)) {
      nthreads = Math.min(nthreads, slices)
      val futures = Array.ofDim[Future](nthreads)
      val k = slices / nthreads
      for (j <- 0 until nthreads) {
        val firstSlice = j * k
        val lastSlice = if ((j == nthreads - 1)) slices else firstSlice + k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            var idx: Int = 0
            for (s <- firstSlice until lastSlice; r <- 0 until rows) {
              idx = zero + s * sliceStride + r * rowStride
              for (c <- 0 until columns) {
                elements(idx) = function.apply(elements(idx))
                idx += columnStride
              }
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
    } else {
      var idx: Int = 0
      for (s <- 0 until slices; r <- 0 until rows) {
        idx = zero + s * sliceStride + r * rowStride
        for (c <- 0 until columns) {
          elements(idx) = function.apply(elements(idx))
          idx += columnStride
        }
      }
    }
    this
  }

  def assign(cond: cern.colt.function.tdouble.Procedure1, f: cern.colt.function.tdouble.Function1): DoubleMatrix3D = {
    val zero = index(0, 0, 0).toInt
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) &&
      (slices * rows * columns >= ConcurrencyUtils.getThreadsBeginN_3D)) {
      nthreads = Math.min(nthreads, slices)
      val futures = Array.ofDim[Future](nthreads)
      val k = slices / nthreads
      for (j <- 0 until nthreads) {
        val firstSlice = j * k
        val lastSlice = if ((j == nthreads - 1)) slices else firstSlice + k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            var elem: Double = 0.0
            var idx: Int = 0
            for (s <- firstSlice until lastSlice; r <- 0 until rows) {
              idx = zero + s * sliceStride + r * rowStride
              for (c <- 0 until columns) {
                elem = elements(idx)
                if (cond.apply(elem) == true) {
                  elements(idx) = f.apply(elem)
                }
                idx += columnStride
              }
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
    } else {
      var elem: Double = 0.0
      var idx: Int = 0
      for (s <- 0 until slices; r <- 0 until rows) {
        idx = zero + s * sliceStride + r * rowStride
        for (c <- 0 until columns) {
          elem = elements(idx)
          if (cond.apply(elem) == true) {
            elements(idx) = f.apply(elem)
          }
          idx += columnStride
        }
      }
    }
    this
  }

  def assign(cond: cern.colt.function.tdouble.Procedure1, value: Double): DoubleMatrix3D = {
    val zero = index(0, 0, 0).toInt
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) &&
      (slices * rows * columns >= ConcurrencyUtils.getThreadsBeginN_3D)) {
      nthreads = Math.min(nthreads, slices)
      val futures = Array.ofDim[Future](nthreads)
      val k = slices / nthreads
      for (j <- 0 until nthreads) {
        val firstSlice = j * k
        val lastSlice = if ((j == nthreads - 1)) slices else firstSlice + k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            var elem: Double = 0.0
            var idx: Int = 0
            for (s <- firstSlice until lastSlice; r <- 0 until rows) {
              idx = zero + s * sliceStride + r * rowStride
              for (c <- 0 until columns) {
                elem = elements(idx)
                if (cond.apply(elem) == true) {
                  elements(idx) = value
                }
                idx += columnStride
              }
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
    } else {
      var elem: Double = 0.0
      var idx: Int = 0
      for (s <- 0 until slices; r <- 0 until rows) {
        idx = zero + s * sliceStride + r * rowStride
        for (c <- 0 until columns) {
          elem = elements(idx)
          if (cond.apply(elem) == true) {
            elements(idx) = value
          }
          idx += columnStride
        }
      }
    }
    this
  }

  def assign(value: Double): DoubleMatrix3D = {
    val zero = index(0, 0, 0).toInt
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_3D)) {
      nthreads = Math.min(nthreads, slices)
      val futures = Array.ofDim[Future](nthreads)
      val k = slices / nthreads
      for (j <- 0 until nthreads) {
        val firstSlice = j * k
        val lastSlice = if ((j == nthreads - 1)) slices else firstSlice + k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            var idx: Int = 0
            for (s <- firstSlice until lastSlice; r <- 0 until rows) {
              idx = zero + s * sliceStride + r * rowStride
              for (c <- 0 until columns) {
                elements(idx) = value
                idx += columnStride
              }
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
    } else {
      var idx: Int = 0
      for (s <- 0 until slices; r <- 0 until rows) {
        idx = zero + s * sliceStride + r * rowStride
        for (c <- 0 until columns) {
          elements(idx) = value
          idx += columnStride
        }
      }
    }
    this
  }

  def assign(values: Array[Double]): DoubleMatrix3D = {
    if (values.length != size) throw new IllegalArgumentException("Must have same length: length=" + values.length + "slices()*rows()*columns()=" +
      slices() * rows() * columns())
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if (this.isNoView) {
      System.arraycopy(values, 0, this.elements, 0, values.length)
    } else {
      val zero = index(0, 0, 0).toInt
      if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_3D)) {
        nthreads = Math.min(nthreads, slices)
        val futures = Array.ofDim[Future](nthreads)
        val k = slices / nthreads
        for (j <- 0 until nthreads) {
          val firstSlice = j * k
          val lastSlice = if ((j == nthreads - 1)) slices else firstSlice + k
          futures(j) = ConcurrencyUtils.submit(new Runnable() {

            def run() {
              var idxOther = firstSlice * rows * columns
              var idx: Int = 0
              for (s <- firstSlice until lastSlice; r <- 0 until rows) {
                idx = zero + s * sliceStride + r * rowStride
                for (c <- 0 until columns) {
                  elements(idx) = values(idxOther += 1)
                  idx += columnStride
                }
              }
            }
          })
        }
        try {
          for (j <- 0 until nthreads) {
            futures(j).get
          }
        } catch {
          case ex: ExecutionException => ex.printStackTrace()
          case e: InterruptedException => e.printStackTrace()
        }
      } else {
        val idxOther = 0
        var idx: Int = 0
        for (s <- 0 until slices; r <- 0 until rows) {
          idx = zero + s * sliceStride + r * rowStride
          for (c <- 0 until columns) {
            elements(idx) = values(idxOther += 1)
            idx += columnStride
          }
        }
      }
    }
    this
  }

  def assign(values: Array[Array[Array[Double]]]): DoubleMatrix3D = {
    if (values.length != slices) throw new IllegalArgumentException("Must have same number of slices: slices=" + values.length +
      "slices()=" +
      slices())
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if (this.isNoView) {
      if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_3D)) {
        nthreads = Math.min(nthreads, slices)
        val futures = Array.ofDim[Future](nthreads)
        val k = slices / nthreads
        for (j <- 0 until nthreads) {
          val firstSlice = j * k
          val lastSlice = if ((j == nthreads - 1)) slices else firstSlice + k
          futures(j) = ConcurrencyUtils.submit(new Runnable() {

            def run() {
              var i = firstSlice * sliceStride
              for (s <- firstSlice until lastSlice) {
                var currentSlice = values(s)
                if (currentSlice.length != rows) throw new IllegalArgumentException("Must have same number of rows in every slice: rows=" +
                  currentSlice.length +
                  "rows()=" +
                  rows())
                for (r <- 0 until rows) {
                  var currentRow = currentSlice(r)
                  if (currentRow.length != columns) throw new IllegalArgumentException("Must have same number of columns in every row: columns=" +
                    currentRow.length +
                    "columns()=" +
                    columns())
                  System.arraycopy(currentRow, 0, elements, i, columns)
                  i += columns
                }
              }
            }
          })
        }
        try {
          for (j <- 0 until nthreads) {
            futures(j).get
          }
        } catch {
          case ex: ExecutionException => ex.printStackTrace()
          case e: InterruptedException => e.printStackTrace()
        }
      } else {
        var i = 0
        for (s <- 0 until slices) {
          val currentSlice = values(s)
          if (currentSlice.length != rows) throw new IllegalArgumentException("Must have same number of rows in every slice: rows=" +
            currentSlice.length +
            "rows()=" +
            rows())
          for (r <- 0 until rows) {
            val currentRow = currentSlice(r)
            if (currentRow.length != columns) throw new IllegalArgumentException("Must have same number of columns in every row: columns=" +
              currentRow.length +
              "columns()=" +
              columns())
            System.arraycopy(currentRow, 0, this.elements, i, columns)
            i += columns
          }
        }
      }
    } else {
      val zero = index(0, 0, 0).toInt
      if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_3D)) {
        nthreads = Math.min(nthreads, slices)
        val futures = Array.ofDim[Future](nthreads)
        val k = slices / nthreads
        for (j <- 0 until nthreads) {
          val firstSlice = j * k
          val lastSlice = if ((j == nthreads - 1)) slices else firstSlice + k
          futures(j) = ConcurrencyUtils.submit(new Runnable() {

            def run() {
              var idx: Int = 0
              for (s <- firstSlice until lastSlice) {
                var currentSlice = values(s)
                if (currentSlice.length != rows) throw new IllegalArgumentException("Must have same number of rows in every slice: rows=" +
                  currentSlice.length +
                  "rows()=" +
                  rows())
                for (r <- 0 until rows) {
                  idx = zero + s * sliceStride + r * rowStride
                  var currentRow = currentSlice(r)
                  if (currentRow.length != columns) throw new IllegalArgumentException("Must have same number of columns in every row: columns=" +
                    currentRow.length +
                    "columns()=" +
                    columns())
                  for (c <- 0 until columns) {
                    elements(idx) = currentRow(c)
                    idx += columnStride
                  }
                }
              }
            }
          })
        }
        try {
          for (j <- 0 until nthreads) {
            futures(j).get
          }
        } catch {
          case ex: ExecutionException => ex.printStackTrace()
          case e: InterruptedException => e.printStackTrace()
        }
      } else {
        var idx: Int = 0
        for (s <- 0 until slices) {
          val currentSlice = values(s)
          if (currentSlice.length != rows) throw new IllegalArgumentException("Must have same number of rows in every slice: rows=" +
            currentSlice.length +
            "rows()=" +
            rows())
          for (r <- 0 until rows) {
            idx = zero + s * sliceStride + r * rowStride
            val currentRow = currentSlice(r)
            if (currentRow.length != columns) throw new IllegalArgumentException("Must have same number of columns in every row: columns=" +
              currentRow.length +
              "columns()=" +
              columns())
            for (c <- 0 until columns) {
              elements(idx) = currentRow(c)
              idx += columnStride
            }
          }
        }
      }
    }
    this
  }

  def assign(source: DoubleMatrix3D): DoubleMatrix3D = {
    if (!(source.isInstanceOf[DenseDoubleMatrix3D])) {
      super.assign(source)
      return this
    }
    var other = source.asInstanceOf[DenseDoubleMatrix3D]
    if (other == this) return this
    checkShape(other)
    if (haveSharedCells(other)) {
      val c = other.copy()
      if (!(c.isInstanceOf[DenseDoubleMatrix3D])) {
        super.assign(source)
        return this
      }
      other = c.asInstanceOf[DenseDoubleMatrix3D]
    }
    val other_final = other
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if (this.isNoView && other.isNoView) {
      System.arraycopy(other_final.elements, 0, this.elements, 0, this.elements.length)
      this
    } else {
      val zero = index(0, 0, 0).toInt
      val zeroOther = other_final.index(0, 0, 0).toInt
      val sliceStrideOther = other_final.sliceStride
      val rowStrideOther = other_final.rowStride
      val columnStrideOther = other_final.columnStride
      val elementsOther = other_final.elements
      if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_3D)) {
        nthreads = Math.min(nthreads, slices)
        val futures = Array.ofDim[Future](nthreads)
        val k = slices / nthreads
        for (j <- 0 until nthreads) {
          val firstSlice = j * k
          val lastSlice = if ((j == nthreads - 1)) slices else firstSlice + k
          futures(j) = ConcurrencyUtils.submit(new Runnable() {

            def run() {
              var idx: Int = 0
              var idxOther: Int = 0
              for (s <- firstSlice until lastSlice; r <- 0 until rows) {
                idx = zero + s * sliceStride + r * rowStride
                idxOther = zeroOther + s * sliceStrideOther + r * rowStrideOther
                for (c <- 0 until columns) {
                  elements(idx) = elementsOther(idxOther)
                  idx += columnStride
                  idxOther += columnStrideOther
                }
              }
            }
          })
        }
        try {
          for (j <- 0 until nthreads) {
            futures(j).get
          }
        } catch {
          case ex: ExecutionException => ex.printStackTrace()
          case e: InterruptedException => e.printStackTrace()
        }
      } else {
        var idx: Int = 0
        var idxOther: Int = 0
        for (s <- 0 until slices; r <- 0 until rows) {
          idx = zero + s * sliceStride + r * rowStride
          idxOther = zeroOther + s * sliceStrideOther + r * rowStrideOther
          for (c <- 0 until columns) {
            elements(idx) = elementsOther(idxOther)
            idx += columnStride
            idxOther += columnStrideOther
          }
        }
      }
      this
    }
  }

  def assign(y: DoubleMatrix3D, function: cern.colt.function.tdouble.DoubleDoubleFunction): DoubleMatrix3D = {
    if (!(y.isInstanceOf[DenseDoubleMatrix3D])) {
      super.assign(y, function)
      return this
    }
    checkShape(y)
    val zero = index(0, 0, 0).toInt
    val zeroOther = y.index(0, 0, 0).toInt
    val sliceStrideOther = y.sliceStride()
    val rowStrideOther = y.rowStride()
    val columnStrideOther = y.columnStride()
    val elementsOther = y.elements().asInstanceOf[Array[Double]]
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_3D)) {
      nthreads = Math.min(nthreads, slices)
      val futures = Array.ofDim[Future](nthreads)
      val k = slices / nthreads
      for (j <- 0 until nthreads) {
        val firstSlice = j * k
        val lastSlice = if ((j == nthreads - 1)) slices else firstSlice + k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            var idx: Int = 0
            var idxOther: Int = 0
            for (s <- firstSlice until lastSlice; r <- 0 until rows) {
              idx = zero + s * sliceStride + r * rowStride
              idxOther = zeroOther + s * sliceStrideOther + r * rowStrideOther
              for (c <- 0 until columns) {
                elements(idx) = function.apply(elements(idx), elementsOther(idxOther))
                idx += columnStride
                idxOther += columnStrideOther
              }
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
    } else {
      var idx: Int = 0
      var idxOther: Int = 0
      for (s <- 0 until slices; r <- 0 until rows) {
        idx = zero + s * sliceStride + r * rowStride
        idxOther = zeroOther + s * sliceStrideOther + r * rowStrideOther
        for (c <- 0 until columns) {
          elements(idx) = function.apply(elements(idx), elementsOther(idxOther))
          idx += columnStride
          idxOther += columnStrideOther
        }
      }
    }
    this
  }

  def assign(y: DoubleMatrix3D,
      function: cern.colt.function.tdouble.DoubleDoubleFunction,
      sliceList: IntArrayList,
      rowList: IntArrayList,
      columnList: IntArrayList): DoubleMatrix3D = {
    if (!(y.isInstanceOf[DenseDoubleMatrix3D])) {
      super.assign(y, function)
      return this
    }
    checkShape(y)
    val zero = index(0, 0, 0).toInt
    val zeroOther = y.index(0, 0, 0).toInt
    val sliceStrideOther = y.sliceStride()
    val rowStrideOther = y.rowStride()
    val columnStrideOther = y.columnStride()
    val elementsOther = y.elements().asInstanceOf[Array[Double]]
    val size = sliceList.size
    val sliceElements = sliceList.elements()
    val rowElements = rowList.elements()
    val columnElements = columnList.elements()
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_3D)) {
      nthreads = Math.min(nthreads, slices)
      val futures = Array.ofDim[Future](nthreads)
      val k = size / nthreads
      for (j <- 0 until nthreads) {
        val firstIdx = j * k
        val lastIdx = if ((j == nthreads - 1)) size else firstIdx + k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            for (i <- firstIdx until lastIdx) {
              var idx = zero + sliceElements(i) * sliceStride + rowElements(i) * rowStride +
                columnElements(i) * columnStride
              var idxOther = zeroOther + sliceElements(i) * sliceStrideOther + rowElements(i) * rowStrideOther +
                columnElements(i) * columnStrideOther
              elements(idx) = function.apply(elements(idx), elementsOther(idxOther))
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
    } else {
      for (i <- 0 until size) {
        val idx = zero + sliceElements(i) * sliceStride + rowElements(i) * rowStride +
          columnElements(i) * columnStride
        val idxOther = zeroOther + sliceElements(i) * sliceStrideOther + rowElements(i) * rowStrideOther +
          columnElements(i) * columnStrideOther
        elements(idx) = function.apply(elements(idx), elementsOther(idxOther))
      }
    }
    this
  }

  def cardinality(): Int = {
    var cardinality = 0
    val zero = index(0, 0, 0).toInt
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_3D)) {
      nthreads = Math.min(nthreads, slices)
      val futures = Array.ofDim[Future](nthreads)
      val results = Array.ofDim[Integer](nthreads)
      val k = slices / nthreads
      for (j <- 0 until nthreads) {
        val firstSlice = j * k
        val lastSlice = if ((j == nthreads - 1)) slices else firstSlice + k
        futures(j) = ConcurrencyUtils.submit(new Callable[Integer]() {

          def call(): java.lang.Integer = {
            var cardinality = 0
            var idx: Int = 0
            for (s <- firstSlice until lastSlice; r <- 0 until rows) {
              idx = zero + s * sliceStride + r * rowStride
              for (c <- 0 until columns) {
                if (elements(idx) != 0) {
                  cardinality += 1
                }
                idx += columnStride
              }
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
      var idx: Int = 0
      for (s <- 0 until slices; r <- 0 until rows) {
        idx = zero + s * sliceStride + r * rowStride
        for (c <- 0 until columns) {
          if (elements(idx) != 0) {
            cardinality += 1
          }
          idx += columnStride
        }
      }
    }
    cardinality
  }

  /**
   * Computes the 2D discrete cosine transform (DCT-II) of each slice of this
   * matrix.
   *
   * @param scale
   *            if true then scaling is performed
   *
   */
  def dct2Slices(scale: Boolean) {
    val oldNthreads = ConcurrencyUtils.getNumberOfThreads
    ConcurrencyUtils.setNumberOfThreads(ConcurrencyUtils.nextPow2(oldNthreads))
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_3D)) {
      nthreads = Math.min(nthreads, slices)
      val futures = Array.ofDim[Future](nthreads)
      val k = slices / nthreads
      for (j <- 0 until nthreads) {
        val firstSlice = j * k
        val lastSlice = if ((j == nthreads - 1)) slices else firstSlice + k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            for (s <- firstSlice until lastSlice) {
              viewSlice(s).asInstanceOf[DenseMatrix2D].dct2(scale)
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
    } else {
      for (s <- 0 until slices) {
        viewSlice(s).asInstanceOf[DenseMatrix2D].dct2(scale)
      }
    }
    ConcurrencyUtils.setNumberOfThreads(oldNthreads)
  }

  /**
   * Computes the 3D discrete cosine transform (DCT-II) of this matrix.
   *
   * @param scale
   *            if true then scaling is performed
   *
   */
  def dct3(scale: Boolean) {
    val oldNthreads = ConcurrencyUtils.getNumberOfThreads
    ConcurrencyUtils.setNumberOfThreads(ConcurrencyUtils.nextPow2(oldNthreads))
    if (dct3 == null) {
      dct3 = new DoubleDCT_3D(slices, rows, columns)
    }
    if (isNoView == true) {
      dct3.forward(elements, scale)
    } else {
      val copy = this.copy()
      dct3.forward(copy.elements().asInstanceOf[Array[Double]], scale)
      this.assign(copy.elements().asInstanceOf[Array[Double]])
    }
    ConcurrencyUtils.setNumberOfThreads(oldNthreads)
  }

  /**
   * Computes the 2D discrete Hartley transform (DHT) of each slice of this
   * matrix.
   *
   */
  def dht2Slices() {
    val oldNthreads = ConcurrencyUtils.getNumberOfThreads
    ConcurrencyUtils.setNumberOfThreads(ConcurrencyUtils.nextPow2(oldNthreads))
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_3D)) {
      nthreads = Math.min(nthreads, slices)
      val futures = Array.ofDim[Future](nthreads)
      val k = slices / nthreads
      for (j <- 0 until nthreads) {
        val firstSlice = j * k
        val lastSlice = if ((j == nthreads - 1)) slices else firstSlice + k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            for (s <- firstSlice until lastSlice) {
              viewSlice(s).asInstanceOf[DenseMatrix2D].dht2()
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
    } else {
      for (s <- 0 until slices) {
        viewSlice(s).asInstanceOf[DenseMatrix2D].dht2()
      }
    }
    ConcurrencyUtils.setNumberOfThreads(oldNthreads)
  }

  /**
   * Computes the 3D discrete Hartley transform (DHT) of this matrix.
   *
   */
  def dht3() {
    val oldNthreads = ConcurrencyUtils.getNumberOfThreads
    ConcurrencyUtils.setNumberOfThreads(ConcurrencyUtils.nextPow2(oldNthreads))
    if (dht3 == null) {
      dht3 = new DoubleDHT_3D(slices, rows, columns)
    }
    if (isNoView == true) {
      dht3.forward(elements)
    } else {
      val copy = this.copy()
      dht3.forward(copy.elements().asInstanceOf[Array[Double]])
      this.assign(copy.elements().asInstanceOf[Array[Double]])
    }
    ConcurrencyUtils.setNumberOfThreads(oldNthreads)
  }

  /**
   * Computes the 2D discrete sine transform (DST-II) of each slice of this
   * matrix.
   *
   * @param scale
   *            if true then scaling is performed
   *
   */
  def dst2Slices(scale: Boolean) {
    val oldNthreads = ConcurrencyUtils.getNumberOfThreads
    ConcurrencyUtils.setNumberOfThreads(ConcurrencyUtils.nextPow2(oldNthreads))
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_3D)) {
      nthreads = Math.min(nthreads, slices)
      val futures = Array.ofDim[Future](nthreads)
      val k = slices / nthreads
      for (j <- 0 until nthreads) {
        val firstSlice = j * k
        val lastSlice = if ((j == nthreads - 1)) slices else firstSlice + k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            for (s <- firstSlice until lastSlice) {
              viewSlice(s).asInstanceOf[DenseMatrix2D].dst2(scale)
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
    } else {
      for (s <- 0 until slices) {
        viewSlice(s).asInstanceOf[DenseMatrix2D].dst2(scale)
      }
    }
    ConcurrencyUtils.setNumberOfThreads(oldNthreads)
  }

  /**
   * Computes the 3D discrete sine transform (DST-II) of this matrix.
   *
   * @param scale
   *            if true then scaling is performed
   *
   */
  def dst3(scale: Boolean) {
    val oldNthreads = ConcurrencyUtils.getNumberOfThreads
    ConcurrencyUtils.setNumberOfThreads(ConcurrencyUtils.nextPow2(oldNthreads))
    if (dst3 == null) {
      dst3 = new DoubleDST_3D(slices, rows, columns)
    }
    if (isNoView == true) {
      dst3.forward(elements, scale)
    } else {
      val copy = this.copy()
      dst3.forward(copy.elements().asInstanceOf[Array[Double]], scale)
      this.assign(copy.elements().asInstanceOf[Array[Double]])
    }
    ConcurrencyUtils.setNumberOfThreads(oldNthreads)
  }

  def elements(): Array[Double] = elements

  /**
   * Computes the 3D discrete Fourier transform (DFT) of this matrix. The
   * physical layout of the output data is as follows:
   *
   * <pre>
   * this[k1][k2][2*k3] = Re[k1][k2][k3]
   *                 = Re[(n1-k1)%n1][(n2-k2)%n2][n3-k3],
   * this[k1][k2][2*k3+1] = Im[k1][k2][k3]
   *                   = -Im[(n1-k1)%n1][(n2-k2)%n2][n3-k3],
   *     0&lt;=k1&lt;n1, 0&lt;=k2&lt;n2, 0&lt;k3&lt;n3/2,
   * this[k1][k2][0] = Re[k1][k2][0]
   *              = Re[(n1-k1)%n1][n2-k2][0],
   * this[k1][k2][1] = Im[k1][k2][0]
   *              = -Im[(n1-k1)%n1][n2-k2][0],
   * this[k1][n2-k2][1] = Re[(n1-k1)%n1][k2][n3/2]
   *                 = Re[k1][n2-k2][n3/2],
   * this[k1][n2-k2][0] = -Im[(n1-k1)%n1][k2][n3/2]
   *                 = Im[k1][n2-k2][n3/2],
   *     0&lt;=k1&lt;n1, 0&lt;k2&lt;n2/2,
   * this[k1][0][0] = Re[k1][0][0]
   *             = Re[n1-k1][0][0],
   * this[k1][0][1] = Im[k1][0][0]
   *             = -Im[n1-k1][0][0],
   * this[k1][n2/2][0] = Re[k1][n2/2][0]
   *                = Re[n1-k1][n2/2][0],
   * this[k1][n2/2][1] = Im[k1][n2/2][0]
   *                = -Im[n1-k1][n2/2][0],
   * this[n1-k1][0][1] = Re[k1][0][n3/2]
   *                = Re[n1-k1][0][n3/2],
   * this[n1-k1][0][0] = -Im[k1][0][n3/2]
   *                = Im[n1-k1][0][n3/2],
   * this[n1-k1][n2/2][1] = Re[k1][n2/2][n3/2]
   *                   = Re[n1-k1][n2/2][n3/2],
   * this[n1-k1][n2/2][0] = -Im[k1][n2/2][n3/2]
   *                   = Im[n1-k1][n2/2][n3/2],
   *     0&lt;k1&lt;n1/2,
   * this[0][0][0] = Re[0][0][0],
   * this[0][0][1] = Re[0][0][n3/2],
   * this[0][n2/2][0] = Re[0][n2/2][0],
   * this[0][n2/2][1] = Re[0][n2/2][n3/2],
   * this[n1/2][0][0] = Re[n1/2][0][0],
   * this[n1/2][0][1] = Re[n1/2][0][n3/2],
   * this[n1/2][n2/2][0] = Re[n1/2][n2/2][0],
   * this[n1/2][n2/2][1] = Re[n1/2][n2/2][n3/2]
   * </pre>
   *
   *
   * This method computes only half of the elements of the real transform. The
   * other half satisfies the symmetry condition. If you want the full real
   * forward transform, use <code>getFft3</code>. To get back the original
   * data, use <code>ifft3</code>.
   *
   * @throws IllegalArgumentException
   *             if the slice size or the row size or the column size of this
   *             matrix is not a power of 2 number.
   */
  def fft3() {
    val oldNthreads = ConcurrencyUtils.getNumberOfThreads
    ConcurrencyUtils.setNumberOfThreads(ConcurrencyUtils.nextPow2(oldNthreads))
    if (fft3 == null) {
      fft3 = new DoubleFFT_3D(slices, rows, columns)
    }
    if (isNoView == true) {
      fft3.realForward(elements)
    } else {
      val copy = this.copy()
      fft3.realForward(copy.elements().asInstanceOf[Array[Double]])
      this.assign(copy.elements().asInstanceOf[Array[Double]])
    }
    ConcurrencyUtils.setNumberOfThreads(oldNthreads)
  }

  /**
   * Returns new complex matrix which is the 2D discrete Fourier transform
   * (DFT) of each slice of this matrix.
   *
   * @return the 2D discrete Fourier transform (DFT) of each slice of this
   *         matrix.
   *
   */
  def getFft2Slices(): DenseDComplexMatrix3D = {
    val oldNthreads = ConcurrencyUtils.getNumberOfThreads
    ConcurrencyUtils.setNumberOfThreads(ConcurrencyUtils.nextPow2(oldNthreads))
    val C = new DenseDComplexMatrix3D(slices, rows, columns)
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_3D)) {
      nthreads = Math.min(nthreads, slices)
      val futures = Array.ofDim[Future](nthreads)
      val k = slices / nthreads
      for (j <- 0 until nthreads) {
        val firstSlice = j * k
        val lastSlice = if ((j == nthreads - 1)) slices else firstSlice + k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            for (s <- firstSlice until lastSlice) {
              C.viewSlice(s).assign(viewSlice(s).asInstanceOf[DenseMatrix2D].getFft2)
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
    } else {
      for (s <- 0 until slices) {
        C.viewSlice(s).assign(viewSlice(s).asInstanceOf[DenseMatrix2D].getFft2)
      }
    }
    ConcurrencyUtils.setNumberOfThreads(oldNthreads)
    C
  }

  /**
   * Returns new complex matrix which is the 3D discrete Fourier transform
   * (DFT) of this matrix.
   *
   * @return the 3D discrete Fourier transform (DFT) of this matrix.
   */
  def getFft3(): DenseDComplexMatrix3D = {
    val oldNthreads = ConcurrencyUtils.getNumberOfThreads
    ConcurrencyUtils.setNumberOfThreads(ConcurrencyUtils.nextPow2(oldNthreads))
    val C = new DenseDComplexMatrix3D(slices, rows, columns)
    val sliceStride = rows * columns
    val rowStride = columns
    var elems: Array[Double] = null
    elems = if (isNoView == true) elements else this.copy().elements().asInstanceOf[Array[Double]]
    val cElems = (C).elements()
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_3D)) {
      nthreads = Math.min(nthreads, slices)
      val futures = Array.ofDim[Future](nthreads)
      val k = slices / nthreads
      for (j <- 0 until nthreads) {
        val firstSlice = j * k
        val lastSlice = if ((j == nthreads - 1)) slices else firstSlice + k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            var idx: Int = 0
            for (s <- firstSlice until lastSlice; r <- 0 until rows) {
              idx = s * sliceStride + r * rowStride
              System.arraycopy(elems, idx, cElems, idx, columns)
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
    } else {
      var idx: Int = 0
      for (s <- 0 until slices; r <- 0 until rows) {
        idx = s * sliceStride + r * rowStride
        System.arraycopy(elems, idx, cElems, idx, columns)
      }
    }
    if (fft3 == null) {
      fft3 = new DoubleFFT_3D(slices, rows, columns)
    }
    fft3.realForwardFull(cElems)
    ConcurrencyUtils.setNumberOfThreads(oldNthreads)
    C
  }

  /**
   * Returns new complex matrix which is the 2D inverse of the discrete
   * Fourier transform (IDFT) of each slice of this matrix.
   *
   * @param scale
   *            if true then scaling is performed
   *
   * @return the 2D inverse of the discrete Fourier transform (IDFT) of each
   *         slice of this matrix.
   */
  def getIfft2Slices(scale: Boolean): DenseDComplexMatrix3D = {
    val oldNthreads = ConcurrencyUtils.getNumberOfThreads
    ConcurrencyUtils.setNumberOfThreads(ConcurrencyUtils.nextPow2(oldNthreads))
    val C = new DenseDComplexMatrix3D(slices, rows, columns)
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_3D)) {
      nthreads = Math.min(nthreads, slices)
      val futures = Array.ofDim[Future](nthreads)
      val k = slices / nthreads
      for (j <- 0 until nthreads) {
        val firstSlice = j * k
        val lastSlice = if ((j == nthreads - 1)) slices else firstSlice + k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            for (s <- firstSlice until lastSlice) {
              C.viewSlice(s).assign(viewSlice(s).asInstanceOf[DenseMatrix2D].getIfft2(scale))
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
    } else {
      for (s <- 0 until slices) {
        C.viewSlice(s).assign(viewSlice(s).asInstanceOf[DenseMatrix2D].getIfft2(scale))
      }
    }
    ConcurrencyUtils.setNumberOfThreads(oldNthreads)
    C
  }

  /**
   * Returns new complex matrix which is the 3D inverse of the discrete
   * Fourier transform (IDFT) of this matrix.
   *
   * @param scale
   *            if true then scaling is performed
   *
   * @return the 3D inverse of the discrete Fourier transform (IDFT) of this
   *         matrix.
   *
   */
  def getIfft3(scale: Boolean): DenseDComplexMatrix3D = {
    val oldNthreads = ConcurrencyUtils.getNumberOfThreads
    ConcurrencyUtils.setNumberOfThreads(ConcurrencyUtils.nextPow2(oldNthreads))
    val C = new DenseDComplexMatrix3D(slices, rows, columns)
    val sliceStride = rows * columns
    val rowStride = columns
    val cElems = (C).elements()
    var elems: Array[Double] = null
    elems = if (isNoView == true) elements else this.copy().elements().asInstanceOf[Array[Double]]
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_3D)) {
      nthreads = Math.min(nthreads, slices)
      val futures = Array.ofDim[Future](nthreads)
      val k = slices / nthreads
      for (j <- 0 until nthreads) {
        val firstSlice = j * k
        val lastSlice = if ((j == nthreads - 1)) slices else firstSlice + k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            var idx: Int = 0
            for (s <- firstSlice until lastSlice; r <- 0 until rows) {
              idx = s * sliceStride + r * rowStride
              System.arraycopy(elems, idx, cElems, idx, columns)
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
    } else {
      var idx: Int = 0
      for (s <- 0 until slices; r <- 0 until rows) {
        idx = s * sliceStride + r * rowStride
        System.arraycopy(elems, idx, cElems, idx, columns)
      }
    }
    if (fft3 == null) {
      fft3 = new DoubleFFT_3D(slices, rows, columns)
    }
    fft3.realInverseFull(cElems, scale)
    ConcurrencyUtils.setNumberOfThreads(oldNthreads)
    C
  }

  def getMaxLocation(): Array[Double] = {
    val zero = index(0, 0, 0).toInt
    var slice_loc = 0
    var row_loc = 0
    var col_loc = 0
    var maxValue = 0
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_3D)) {
      nthreads = Math.min(nthreads, slices)
      val futures = Array.ofDim[Future](nthreads)
      val results = Array.ofDim[Double](nthreads, 2)
      val k = slices / nthreads
      for (j <- 0 until nthreads) {
        val firstSlice = j * k
        val lastSlice = if ((j == nthreads - 1)) slices else firstSlice + k
        futures(j) = ConcurrencyUtils.submit(new Callable[Array[Double]]() {

          def call(): Array[Double] = {
            var slice_loc = firstSlice
            var row_loc = 0
            var col_loc = 0
            var maxValue = elements(zero + firstSlice * sliceStride)
            var d = 1
            var elem: Double = 0.0
            for (s <- firstSlice until lastSlice; r <- 0 until rows) {
              for (c <- d until columns) {
                elem = elements(zero + s * sliceStride + r * rowStride + c * columnStride)
                if (maxValue < elem) {
                  maxValue = elem
                  slice_loc = s
                  row_loc = r
                  col_loc = c
                }
              }
              d = 0
            }
            return Array(maxValue, slice_loc, row_loc, col_loc)
          }
        })
      }
      try {
        for (j <- 0 until nthreads) {
          results(j) = futures(j).get.asInstanceOf[Array[Double]]
        }
        maxValue = results(0)(0)
        slice_loc = results(0)(1).toInt
        row_loc = results(0)(2).toInt
        col_loc = results(0)(3).toInt
        for (j <- 1 until nthreads if maxValue < results(j)(0)) {
          maxValue = results(j)(0)
          slice_loc = results(j)(1).toInt
          row_loc = results(j)(2).toInt
          col_loc = results(j)(3).toInt
        }
      } catch {
        case ex: ExecutionException => ex.printStackTrace()
        case e: InterruptedException => e.printStackTrace()
      }
    } else {
      maxValue = elements(zero)
      var elem: Double = 0.0
      var d = 1
      for (s <- 0 until slices; r <- 0 until rows) {
        for (c <- d until columns) {
          elem = elements(zero + s * sliceStride + r * rowStride + c * columnStride)
          if (maxValue < elem) {
            maxValue = elem
            slice_loc = s
            row_loc = r
            col_loc = c
          }
        }
        d = 0
      }
    }
    Array(maxValue, slice_loc, row_loc, col_loc)
  }

  def getMinLocation(): Array[Double] = {
    val zero = index(0, 0, 0).toInt
    var slice_loc = 0
    var row_loc = 0
    var col_loc = 0
    var minValue = 0
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_3D)) {
      nthreads = Math.min(nthreads, slices)
      val futures = Array.ofDim[Future](nthreads)
      val results = Array.ofDim[Double](nthreads, 2)
      val k = slices / nthreads
      for (j <- 0 until nthreads) {
        val firstSlice = j * k
        val lastSlice = if ((j == nthreads - 1)) slices else firstSlice + k
        futures(j) = ConcurrencyUtils.submit(new Callable[Array[Double]]() {

          def call(): Array[Double] = {
            var slice_loc = firstSlice
            var row_loc = 0
            var col_loc = 0
            var minValue = elements(zero + slice_loc * sliceStride)
            var d = 1
            var elem: Double = 0.0
            for (s <- firstSlice until lastSlice; r <- 0 until rows) {
              for (c <- d until columns) {
                elem = elements(zero + s * sliceStride + r * rowStride + c * columnStride)
                if (minValue > elem) {
                  minValue = elem
                  slice_loc = s
                  row_loc = r
                  col_loc = c
                }
              }
              d = 0
            }
            return Array(minValue, slice_loc, row_loc, col_loc)
          }
        })
      }
      try {
        for (j <- 0 until nthreads) {
          results(j) = futures(j).get.asInstanceOf[Array[Double]]
        }
        minValue = results(0)(0)
        slice_loc = results(0)(1).toInt
        row_loc = results(0)(2).toInt
        col_loc = results(0)(3).toInt
        for (j <- 1 until nthreads if minValue > results(j)(0)) {
          minValue = results(j)(0)
          slice_loc = results(j)(1).toInt
          row_loc = results(j)(2).toInt
          col_loc = results(j)(3).toInt
        }
      } catch {
        case ex: ExecutionException => ex.printStackTrace()
        case e: InterruptedException => e.printStackTrace()
      }
    } else {
      minValue = elements(zero)
      var elem: Double = 0.0
      var d = 1
      for (s <- 0 until slices; r <- 0 until rows) {
        for (c <- d until columns) {
          elem = elements(zero + s * sliceStride + r * rowStride + c * columnStride)
          if (minValue > elem) {
            minValue = elem
            slice_loc = s
            row_loc = r
            col_loc = c
          }
        }
        d = 0
      }
    }
    Array(minValue, slice_loc, row_loc, col_loc)
  }

  def getNegativeValues(sliceList: IntArrayList,
      rowList: IntArrayList,
      columnList: IntArrayList,
      valueList: DoubleArrayList) {
    sliceList.clear()
    rowList.clear()
    columnList.clear()
    valueList.clear()
    val zero = index(0, 0, 0).toInt
    var idx: Int = 0
    for (s <- 0 until slices; r <- 0 until rows) {
      idx = zero + s * sliceStride + r * rowStride
      for (c <- 0 until columns) {
        val value = elements(idx)
        if (value < 0) {
          sliceList.add(s)
          rowList.add(r)
          columnList.add(c)
          valueList.add(value)
        }
        idx += columnStride
      }
    }
  }

  def getNonZeros(sliceList: IntArrayList,
      rowList: IntArrayList,
      columnList: IntArrayList,
      valueList: DoubleArrayList) {
    sliceList.clear()
    rowList.clear()
    columnList.clear()
    valueList.clear()
    val zero = index(0, 0, 0).toInt
    var idx: Int = 0
    for (s <- 0 until slices; r <- 0 until rows) {
      idx = zero + s * sliceStride + r * rowStride
      for (c <- 0 until columns) {
        val value = elements(idx)
        if (value != 0) {
          sliceList.add(s)
          rowList.add(r)
          columnList.add(c)
          valueList.add(value)
        }
        idx += columnStride
      }
    }
  }

  def getPositiveValues(sliceList: IntArrayList,
      rowList: IntArrayList,
      columnList: IntArrayList,
      valueList: DoubleArrayList) {
    sliceList.clear()
    rowList.clear()
    columnList.clear()
    valueList.clear()
    val zero = index(0, 0, 0).toInt
    var idx: Int = 0
    for (s <- 0 until slices; r <- 0 until rows) {
      idx = zero + s * sliceStride + r * rowStride
      for (c <- 0 until columns) {
        val value = elements(idx)
        if (value > 0) {
          sliceList.add(s)
          rowList.add(r)
          columnList.add(c)
          valueList.add(value)
        }
        idx += columnStride
      }
    }
  }

  def getQuick(slice: Int, row: Int, column: Int): Double = {
    elements(sliceZero + slice * sliceStride + rowZero + row * rowStride +
      columnZero +
      column * columnStride)
  }

  /**
   * Computes the 2D inverse of the discrete cosine transform (DCT-III) of
   * each slice of this matrix.
   *
   * @param scale
   *            if true then scaling is performed
   */
  def idct2Slices(scale: Boolean) {
    val oldNthreads = ConcurrencyUtils.getNumberOfThreads
    ConcurrencyUtils.setNumberOfThreads(ConcurrencyUtils.nextPow2(oldNthreads))
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_3D)) {
      nthreads = Math.min(nthreads, slices)
      val futures = Array.ofDim[Future](nthreads)
      val k = slices / nthreads
      for (j <- 0 until nthreads) {
        val firstSlice = j * k
        val lastSlice = if ((j == nthreads - 1)) slices else firstSlice + k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            for (s <- firstSlice until lastSlice) {
              viewSlice(s).asInstanceOf[DenseMatrix2D].idct2(scale)
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
    } else {
      for (s <- 0 until slices) {
        viewSlice(s).asInstanceOf[DenseMatrix2D].idct2(scale)
      }
    }
    ConcurrencyUtils.setNumberOfThreads(oldNthreads)
  }

  /**
   * Computes the 3D inverse of the discrete cosine transform (DCT-III) of
   * this matrix.
   *
   * @param scale
   *            if true then scaling is performed
   */
  def idct3(scale: Boolean) {
    val oldNthreads = ConcurrencyUtils.getNumberOfThreads
    ConcurrencyUtils.setNumberOfThreads(ConcurrencyUtils.nextPow2(oldNthreads))
    if (dct3 == null) {
      dct3 = new DoubleDCT_3D(slices, rows, columns)
    }
    if (isNoView == true) {
      dct3.inverse(elements, scale)
    } else {
      val copy = this.copy()
      dct3.inverse(copy.elements().asInstanceOf[Array[Double]], scale)
      this.assign(copy.elements().asInstanceOf[Array[Double]])
    }
    ConcurrencyUtils.setNumberOfThreads(oldNthreads)
  }

  /**
   * Computes the 2D inverse of the discrete Hartley transform (IDHT) of each
   * slice of this matrix.
   *
   * @param scale
   *            if true then scaling is performed
   *
   */
  def idht2Slices(scale: Boolean) {
    val oldNthreads = ConcurrencyUtils.getNumberOfThreads
    ConcurrencyUtils.setNumberOfThreads(ConcurrencyUtils.nextPow2(oldNthreads))
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_3D)) {
      nthreads = Math.min(nthreads, slices)
      val futures = Array.ofDim[Future](nthreads)
      val k = slices / nthreads
      for (j <- 0 until nthreads) {
        val firstSlice = j * k
        val lastSlice = if ((j == nthreads - 1)) slices else firstSlice + k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            for (s <- firstSlice until lastSlice) {
              viewSlice(s).asInstanceOf[DenseMatrix2D].idht2(scale)
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
    } else {
      for (s <- 0 until slices) {
        viewSlice(s).asInstanceOf[DenseMatrix2D].idht2(scale)
      }
    }
    ConcurrencyUtils.setNumberOfThreads(oldNthreads)
  }

  /**
   * Computes the 3D inverse of the discrete Hartley transform (IDHT) of this
   * matrix.
   *
   * @param scale
   *            if true then scaling is performed
   */
  def idht3(scale: Boolean) {
    val oldNthreads = ConcurrencyUtils.getNumberOfThreads
    ConcurrencyUtils.setNumberOfThreads(ConcurrencyUtils.nextPow2(oldNthreads))
    if (dht3 == null) {
      dht3 = new DoubleDHT_3D(slices, rows, columns)
    }
    if (isNoView == true) {
      dht3.inverse(elements, scale)
    } else {
      val copy = this.copy()
      dht3.inverse(copy.elements().asInstanceOf[Array[Double]], scale)
      this.assign(copy.elements().asInstanceOf[Array[Double]])
    }
    ConcurrencyUtils.setNumberOfThreads(oldNthreads)
  }

  /**
   * Computes the 2D inverse of the discrete sine transform (DST-III) of each
   * slice of this matrix.
   *
   * @param scale
   *            if true then scaling is performed
   */
  def idst2Slices(scale: Boolean) {
    val oldNthreads = ConcurrencyUtils.getNumberOfThreads
    ConcurrencyUtils.setNumberOfThreads(ConcurrencyUtils.nextPow2(oldNthreads))
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_3D)) {
      nthreads = Math.min(nthreads, slices)
      val futures = Array.ofDim[Future](nthreads)
      val k = slices / nthreads
      for (j <- 0 until nthreads) {
        val firstSlice = j * k
        val lastSlice = if ((j == nthreads - 1)) slices else firstSlice + k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            for (s <- firstSlice until lastSlice) {
              viewSlice(s).asInstanceOf[DenseMatrix2D].idst2(scale)
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
    } else {
      for (s <- 0 until slices) {
        viewSlice(s).asInstanceOf[DenseMatrix2D].idst2(scale)
      }
    }
    ConcurrencyUtils.setNumberOfThreads(oldNthreads)
  }

  /**
   * Computes the 3D inverse of the discrete sine transform (DST-III) of this
   * matrix.
   *
   * @param scale
   *            if true then scaling is performed
   *
   */
  def idst3(scale: Boolean) {
    val oldNthreads = ConcurrencyUtils.getNumberOfThreads
    ConcurrencyUtils.setNumberOfThreads(ConcurrencyUtils.nextPow2(oldNthreads))
    if (dst3 == null) {
      dst3 = new DoubleDST_3D(slices, rows, columns)
    }
    if (isNoView == true) {
      dst3.inverse(elements, scale)
    } else {
      val copy = this.copy()
      dst3.inverse(copy.elements().asInstanceOf[Array[Double]], scale)
      this.assign(copy.elements().asInstanceOf[Array[Double]])
    }
    ConcurrencyUtils.setNumberOfThreads(oldNthreads)
  }

  /**
   * Computes the 3D inverse of the discrete Fourier transform (IDFT) of this
   * matrix. The physical layout of the input data has to be as follows:
   *
   * <pre>
   * this[k1][k2][2*k3] = Re[k1][k2][k3]
   *                 = Re[(n1-k1)%n1][(n2-k2)%n2][n3-k3],
   * this[k1][k2][2*k3+1] = Im[k1][k2][k3]
   *                   = -Im[(n1-k1)%n1][(n2-k2)%n2][n3-k3],
   *     0&lt;=k1&lt;n1, 0&lt;=k2&lt;n2, 0&lt;k3&lt;n3/2,
   * this[k1][k2][0] = Re[k1][k2][0]
   *              = Re[(n1-k1)%n1][n2-k2][0],
   * this[k1][k2][1] = Im[k1][k2][0]
   *              = -Im[(n1-k1)%n1][n2-k2][0],
   * this[k1][n2-k2][1] = Re[(n1-k1)%n1][k2][n3/2]
   *                 = Re[k1][n2-k2][n3/2],
   * this[k1][n2-k2][0] = -Im[(n1-k1)%n1][k2][n3/2]
   *                 = Im[k1][n2-k2][n3/2],
   *     0&lt;=k1&lt;n1, 0&lt;k2&lt;n2/2,
   * this[k1][0][0] = Re[k1][0][0]
   *             = Re[n1-k1][0][0],
   * this[k1][0][1] = Im[k1][0][0]
   *             = -Im[n1-k1][0][0],
   * this[k1][n2/2][0] = Re[k1][n2/2][0]
   *                = Re[n1-k1][n2/2][0],
   * this[k1][n2/2][1] = Im[k1][n2/2][0]
   *                = -Im[n1-k1][n2/2][0],
   * this[n1-k1][0][1] = Re[k1][0][n3/2]
   *                = Re[n1-k1][0][n3/2],
   * this[n1-k1][0][0] = -Im[k1][0][n3/2]
   *                = Im[n1-k1][0][n3/2],
   * this[n1-k1][n2/2][1] = Re[k1][n2/2][n3/2]
   *                   = Re[n1-k1][n2/2][n3/2],
   * this[n1-k1][n2/2][0] = -Im[k1][n2/2][n3/2]
   *                   = Im[n1-k1][n2/2][n3/2],
   *     0&lt;k1&lt;n1/2,
   * this[0][0][0] = Re[0][0][0],
   * this[0][0][1] = Re[0][0][n3/2],
   * this[0][n2/2][0] = Re[0][n2/2][0],
   * this[0][n2/2][1] = Re[0][n2/2][n3/2],
   * this[n1/2][0][0] = Re[n1/2][0][0],
   * this[n1/2][0][1] = Re[n1/2][0][n3/2],
   * this[n1/2][n2/2][0] = Re[n1/2][n2/2][0],
   * this[n1/2][n2/2][1] = Re[n1/2][n2/2][n3/2]
   * </pre>
   *
   * This method computes only half of the elements of the real transform. The
   * other half satisfies the symmetry condition. If you want the full real
   * inverse transform, use <code>getIfft3</code>.
   *
   * @param scale
   *            if true then scaling is performed
   *
   * @throws IllegalArgumentException
   *             if the slice size or the row size or the column size of this
   *             matrix is not a power of 2 number.
   */
  def ifft3(scale: Boolean) {
    val oldNthreads = ConcurrencyUtils.getNumberOfThreads
    ConcurrencyUtils.setNumberOfThreads(ConcurrencyUtils.nextPow2(oldNthreads))
    if (fft3 == null) {
      fft3 = new DoubleFFT_3D(slices, rows, columns)
    }
    if (isNoView == true) {
      fft3.realInverse(elements, scale)
    } else {
      val copy = this.copy()
      fft3.realInverse(copy.elements().asInstanceOf[Array[Double]], scale)
      this.assign(copy.elements().asInstanceOf[Array[Double]])
    }
    ConcurrencyUtils.setNumberOfThreads(oldNthreads)
  }

  def index(slice: Int, row: Int, column: Int): Long = {
    sliceZero + slice * sliceStride + rowZero + row * rowStride +
      columnZero +
      column * columnStride
  }

  def like(slices: Int, rows: Int, columns: Int): DoubleMatrix3D = {
    new DenseDoubleMatrix3D(slices, rows, columns)
  }

  def like2D(rows: Int, columns: Int): StrideMatrix2D = new DenseMatrix2D(rows, columns)

  def setQuick(slice: Int,
      row: Int,
      column: Int,
      value: Double) {
    elements(sliceZero + slice * sliceStride + rowZero + row * rowStride +
      columnZero +
      column * columnStride) = value
  }

  def toArray(): Array[Array[Array[Double]]] = {
    val values = Array.ofDim[Double](slices, rows, columns)
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    val zero = index(0, 0, 0).toInt
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_3D)) {
      nthreads = Math.min(nthreads, slices)
      val futures = Array.ofDim[Future](nthreads)
      val k = slices / nthreads
      for (j <- 0 until nthreads) {
        val firstSlice = j * k
        val lastSlice = if ((j == nthreads - 1)) slices else firstSlice + k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            var idx: Int = 0
            for (s <- firstSlice until lastSlice) {
              var currentSlice = values(s)
              for (r <- 0 until rows) {
                idx = zero + s * sliceStride + r * rowStride
                var currentRow = currentSlice(r)
                for (c <- 0 until columns) {
                  currentRow(c) = elements(idx)
                  idx += columnStride
                }
              }
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
    } else {
      var idx: Int = 0
      for (s <- 0 until slices) {
        val currentSlice = values(s)
        for (r <- 0 until rows) {
          idx = zero + s * sliceStride + r * rowStride
          val currentRow = currentSlice(r)
          for (c <- 0 until columns) {
            currentRow(c) = elements(idx)
            idx += columnStride
          }
        }
      }
    }
    values
  }

  def vectorize(): StrideMatrix1D = {
    val v = new DenseMatrix1D(size.toInt)
    val length = rows * columns
    for (s <- 0 until slices) {
      v.viewPart(s * length, length).assign(viewSlice(s).vectorize())
    }
    v
  }

  def zAssign27Neighbors(B: DoubleMatrix3D, function: cern.colt.function.tdouble.Double27Function) {
    if (!(B.isInstanceOf[DenseDoubleMatrix3D])) {
      super.zAssign27Neighbors(B, function)
      return
    }
    if (function == null) throw new NullPointerException("function must not be null.")
    checkShape(B)
    val r = rows - 1
    val c = columns - 1
    if (rows < 3 || columns < 3 || slices < 3) return
    val BB = B.asInstanceOf[DenseDoubleMatrix3D]
    val A_ss = sliceStride
    val A_rs = rowStride
    val B_rs = BB.rowStride
    val A_cs = columnStride
    val B_cs = BB.columnStride
    val elems = this.elements
    val B_elems = BB.elements
    if (elems == null || B_elems == null) throw new InternalError()
    for (k <- 1 until slices - 1) {
      var A_index = index(k, 1, 1).toInt
      var B_index = BB.index(k, 1, 1).toInt
      for (i <- 1 until r) {
        var A002 = A_index - A_ss - A_rs - A_cs
        var A012 = A002 + A_rs
        var A022 = A012 + A_rs
        var A102 = A002 + A_ss
        var A112 = A102 + A_rs
        var A122 = A112 + A_rs
        var A202 = A102 + A_ss
        var A212 = A202 + A_rs
        var A222 = A212 + A_rs
        var a000: Double = 0.0
        var a001: Double = 0.0
        var a002: Double = 0.0
        var a010: Double = 0.0
        var a011: Double = 0.0
        var a012: Double = 0.0
        var a020: Double = 0.0
        var a021: Double = 0.0
        var a022: Double = 0.0
        var a100: Double = 0.0
        var a101: Double = 0.0
        var a102: Double = 0.0
        var a110: Double = 0.0
        var a111: Double = 0.0
        var a112: Double = 0.0
        var a120: Double = 0.0
        var a121: Double = 0.0
        var a122: Double = 0.0
        var a200: Double = 0.0
        var a201: Double = 0.0
        var a202: Double = 0.0
        var a210: Double = 0.0
        var a211: Double = 0.0
        var a212: Double = 0.0
        var a220: Double = 0.0
        var a221: Double = 0.0
        var a222: Double = 0.0
        a000 = elems(A002)
        A002 += A_cs
        a001 = elems(A002)
        a010 = elems(A012)
        A012 += A_cs
        a011 = elems(A012)
        a020 = elems(A022)
        A022 += A_cs
        a021 = elems(A022)
        a100 = elems(A102)
        A102 += A_cs
        a101 = elems(A102)
        a110 = elems(A112)
        A112 += A_cs
        a111 = elems(A112)
        a120 = elems(A122)
        A122 += A_cs
        a121 = elems(A122)
        a200 = elems(A202)
        A202 += A_cs
        a201 = elems(A202)
        a210 = elems(A212)
        A212 += A_cs
        a211 = elems(A212)
        a220 = elems(A222)
        A222 += A_cs
        a221 = elems(A222)
        var B11 = B_index
        for (j <- 1 until c) {
          a002 = elems(A002 += A_cs)
          a012 = elems(A012 += A_cs)
          a022 = elems(A022 += A_cs)
          a102 = elems(A102 += A_cs)
          a112 = elems(A112 += A_cs)
          a122 = elems(A122 += A_cs)
          a202 = elems(A202 += A_cs)
          a212 = elems(A212 += A_cs)
          a222 = elems(A222 += A_cs)
          B_elems(B11) = function.apply(a000, a001, a002, a010, a011, a012, a020, a021, a022, a100, a101,
            a102, a110, a111, a112, a120, a121, a122, a200, a201, a202, a210, a211, a212, a220, a221,
            a222)
          B11 += B_cs
          a000 = a001
          a001 = a002
          a010 = a011
          a011 = a012
          a020 = a021
          a021 = a022
          a100 = a101
          a101 = a102
          a110 = a111
          a111 = a112
          a120 = a121
          a121 = a122
          a200 = a201
          a201 = a202
          a210 = a211
          a211 = a212
          a220 = a221
          a221 = a222
        }
        A_index += A_rs
        B_index += B_rs
      }
    }
  }

  def zSum(): Double = {
    var sum = 0
    val zero = index(0, 0, 0).toInt
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_3D)) {
      nthreads = Math.min(nthreads, slices)
      val futures = Array.ofDim[Future](nthreads)
      val k = slices / nthreads
      for (j <- 0 until nthreads) {
        val firstSlice = j * k
        val lastSlice = if ((j == nthreads - 1)) slices else firstSlice + k
        futures(j) = ConcurrencyUtils.submit(new Callable[Double]() {

          def call(): java.lang.Double = {
            var sum = 0
            var idx: Int = 0
            for (s <- firstSlice until lastSlice; r <- 0 until rows) {
              idx = zero + s * sliceStride + r * rowStride
              for (c <- 0 until columns) {
                sum += elements(idx)
                idx += columnStride
              }
            }
            return sum
          }
        })
      }
      try {
        for (j <- 0 until nthreads) {
          sum = sum + futures(j).get.asInstanceOf[java.lang.Double]
        }
      } catch {
        case ex: ExecutionException => ex.printStackTrace()
        case e: InterruptedException => e.printStackTrace()
      }
    } else {
      var idx: Int = 0
      for (s <- 0 until slices; r <- 0 until rows) {
        idx = zero + s * sliceStride + r * rowStride
        for (c <- 0 until columns) {
          sum += elements(idx)
          idx += columnStride
        }
      }
    }
    sum
  }

  protected def haveSharedCellsRaw(other: DoubleMatrix3D): Boolean = {
    if (other.isInstanceOf[SelectedDenseDoubleMatrix3D]) {
      val otherMatrix = other.asInstanceOf[SelectedDenseDoubleMatrix3D]
      return this.elements == otherMatrix.elements
    } else if (other.isInstanceOf[DenseDoubleMatrix3D]) {
      val otherMatrix = other.asInstanceOf[DenseDoubleMatrix3D]
      return this.elements == otherMatrix.elements
    }
    false
  }

  protected def like2D(rows: Int,
      columns: Int,
      rowZero: Int,
      columnZero: Int,
      rowStride: Int,
      columnStride: Int): StrideMatrix2D = {
    new DenseMatrix2D(rows, columns, this.elements, rowZero, columnZero, rowStride, columnStride,
      true)
  }

  protected def viewSelectionLike(sliceOffsets: Array[Int], rowOffsets: Array[Int], columnOffsets: Array[Int]): DoubleMatrix3D = {
    new SelectedDenseDoubleMatrix3D(this.elements, sliceOffsets, rowOffsets, columnOffsets, 0)
  }
}
