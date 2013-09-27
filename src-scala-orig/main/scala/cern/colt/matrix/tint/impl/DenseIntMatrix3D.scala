package cern.colt.matrix.tint.impl

import java.util.concurrent.Callable
import java.util.concurrent.ExecutionException
import java.util.concurrent.Future
import cern.colt.list.tint.IntArrayList
import cern.colt.matrix.tint.IntMatrix1D
import cern.colt.matrix.tint.IntMatrix2D
import cern.colt.matrix.tint.IntMatrix3D
import edu.emory.mathcs.utils.ConcurrencyUtils
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Dense 3-d matrix holding <tt>int</tt> elements. First see the <a
 * href="package-summary.html">package summary</a> and javadoc <a
 * href="package-tree.html">tree view</a> to get the broad picture.
 * <p>
 * <b>Implementation:</b>
 * <p>
 * Internally holds one single contiguous one-dimensional array, addressed in
 * (in decreasing order of significance): slice major, row major, column major.
 * Note that this implementation is not synchronized.
 * <p>
 * <b>Memory requirements:</b>
 * <p>
 * <tt>memory [bytes] = 8*slices()*rows()*columns()</tt>. Thus, a 100*100*100
 * matrix uses 8 MB.
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
class DenseIntMatrix3D(slices: Int, rows: Int, columns: Int) extends IntMatrix3D {

  /**
   * The elements of this matrix. elements are stored in slice major, then row
   * major, then column major, in order of significance, i.e.
   * index==slice*sliceStride+ row*rowStride + column*columnStride i.e.
   * {slice0 row0..m}, {slice1 row0..m}, ..., {sliceN row0..m} with each row
   * storead as {row0 column0..m}, {row1 column0..m}, ..., {rown column0..m}
   */
  protected var elements: Array[Int] = new Array[Int](slices * rows * columns)

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
  def this(values: Array[Array[Array[Int]]]) {
    this(values.length, (if (values.length == 0) 0 else values(0).length), (if (values.length == 0) 0 else if (values(0).length == 0) 0 else values(0)(0).length))
    assign(values)
  }

  /**
   * Constructs a view with the given parameters.
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
   *             if <tt>(int)slices*columns*rows > Int.MAX_VALUE</tt>.
   * @throws IllegalArgumentException
   *             if <tt>slices<0 || rows<0 || columns<0</tt>.
   */
  def this(slices: Int, 
      rows: Int, 
      columns: Int, 
      elements: Array[Int], 
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

  def aggregate(aggr: cern.colt.function.tint.IntIntFunction, f: cern.colt.function.tint.IntFunction): Int = {
    if (size == 0) throw new IllegalArgumentException("size == 0")
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
        futures(j) = ConcurrencyUtils.submit(new Callable[Integer]() {

          def call(): java.lang.Integer = {
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

  def aggregate(aggr: cern.colt.function.tint.IntIntFunction, f: cern.colt.function.tint.IntFunction, cond: cern.colt.function.tint.IntProcedure): Int = {
    if (size == 0) throw new IllegalArgumentException("size == 0")
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
        futures(j) = ConcurrencyUtils.submit(new Callable[Integer]() {

          def call(): java.lang.Integer = {
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

  def aggregate(aggr: cern.colt.function.tint.IntIntFunction, 
      f: cern.colt.function.tint.IntFunction, 
      sliceList: IntArrayList, 
      rowList: IntArrayList, 
      columnList: IntArrayList): Int = {
    if (size == 0) throw new IllegalArgumentException("size == 0")
    if (sliceList.size == 0 || rowList.size == 0 || columnList.size == 0) throw new IllegalArgumentException("size == 0")
    val size = sliceList.size
    val sliceElements = sliceList.elements()
    val rowElements = rowList.elements()
    val columnElements = columnList.elements()
    val zero = index(0, 0, 0).toInt
    var a = 0
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_3D)) {
      nthreads = Math.min(nthreads, size)
      val futures = Array.ofDim[Future](nthreads)
      val k = size / nthreads
      for (j <- 0 until nthreads) {
        val firstIdx = j * k
        val lastIdx = if ((j == nthreads - 1)) size else firstIdx + k
        futures(j) = ConcurrencyUtils.submit(new Callable[Integer]() {

          def call(): java.lang.Integer = {
            var a = f.apply(elements(zero + sliceElements(firstIdx) * sliceStride + rowElements(firstIdx) * rowStride + 
              columnElements(firstIdx) * columnStride))
            var elem: Int = 0
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
      var elem: Int = 0
      for (i <- 1 until size) {
        elem = elements(zero + sliceElements(i) * sliceStride + rowElements(i) * rowStride + 
          columnElements(i) * columnStride)
        a = aggr.apply(a, f.apply(elem))
      }
    }
    a
  }

  def aggregate(other: IntMatrix3D, aggr: cern.colt.function.tint.IntIntFunction, f: cern.colt.function.tint.IntIntFunction): Int = {
    if (!(other.isInstanceOf[DenseIntMatrix3D])) {
      return super.aggregate(other, aggr, f)
    }
    checkShape(other)
    if (size == 0) throw new IllegalArgumentException("size == 0")
    var a = 0
    val zero = index(0, 0, 0).toInt
    val zeroOther = other.index(0, 0, 0).toInt
    val sliceStrideOther = other.sliceStride()
    val rowStrideOther = other.rowStride()
    val colStrideOther = other.columnStride()
    val elemsOther = other.elements().asInstanceOf[Array[Int]]
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_3D)) {
      nthreads = Math.min(nthreads, slices)
      val futures = Array.ofDim[Future](nthreads)
      val k = slices / nthreads
      for (j <- 0 until nthreads) {
        val firstSlice = j * k
        val lastSlice = if ((j == nthreads - 1)) slices else firstSlice + k
        futures(j) = ConcurrencyUtils.submit(new Callable[Integer]() {

          def call(): java.lang.Integer = {
            var idx = zero + firstSlice * sliceStride
            var idxOther = zeroOther + firstSlice * sliceStrideOther
            var a = f.apply(elements(idx), elemsOther(idxOther))
            var d = 1
            for (s <- firstSlice until lastSlice; r <- 0 until rows) {
              for (c <- d until columns) {
                idx = zero + s * sliceStride + r * rowStride + c * columnStride
                idxOther = zeroOther + s * sliceStrideOther + r * rowStrideOther + 
                  c * colStrideOther
                a = aggr.apply(a, f.apply(elements(idx), elemsOther(idxOther)))
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
            c * colStrideOther
          a = aggr.apply(a, f.apply(elements(idx), elemsOther(idxOther)))
        }
        d = 0
      }
    }
    a
  }

  def assign(function: cern.colt.function.tint.IntFunction): IntMatrix3D = {
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

  def assign(value: Int): IntMatrix3D = {
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

  def assign(values: Array[Int]): IntMatrix3D = {
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
        ConcurrencyUtils.waitForCompletion(futures)
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

  def assign(values: Array[Array[Array[Int]]]): IntMatrix3D = {
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
        ConcurrencyUtils.waitForCompletion(futures)
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
        ConcurrencyUtils.waitForCompletion(futures)
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

  def assign(cond: cern.colt.function.tint.IntProcedure, f: cern.colt.function.tint.IntFunction): IntMatrix3D = {
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
            var elem: Int = 0
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
      var elem: Int = 0
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

  def assign(cond: cern.colt.function.tint.IntProcedure, value: Int): IntMatrix3D = {
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
            var elem: Int = 0
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
      var elem: Int = 0
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

  def assign(source: IntMatrix3D): IntMatrix3D = {
    if (!(source.isInstanceOf[DenseIntMatrix3D])) {
      super.assign(source)
      return this
    }
    var other = source.asInstanceOf[DenseIntMatrix3D]
    if (other == this) return this
    checkShape(other)
    if (haveSharedCells(other)) {
      val c = other.copy()
      if (!(c.isInstanceOf[DenseIntMatrix3D])) {
        super.assign(source)
        return this
      }
      other = c.asInstanceOf[DenseIntMatrix3D]
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
      val elemsOther = other_final.elements
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
                  elements(idx) = elemsOther(idxOther)
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
            elements(idx) = elemsOther(idxOther)
            idx += columnStride
            idxOther += columnStrideOther
          }
        }
      }
      this
    }
  }

  def assign(y: IntMatrix3D, function: cern.colt.function.tint.IntIntFunction): IntMatrix3D = {
    if (!(y.isInstanceOf[DenseIntMatrix3D])) {
      super.assign(y, function)
      return this
    }
    checkShape(y)
    val zero = index(0, 0, 0).toInt
    val zeroOther = y.index(0, 0, 0).toInt
    val sliceStrideOther = y.sliceStride()
    val rowStrideOther = y.rowStride()
    val columnStrideOther = y.columnStride()
    val elemsOther = y.elements().asInstanceOf[Array[Int]]
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
                elements(idx) = function.apply(elements(idx), elemsOther(idxOther))
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
          elements(idx) = function.apply(elements(idx), elemsOther(idxOther))
          idx += columnStride
          idxOther += columnStrideOther
        }
      }
    }
    this
  }

  def assign(y: IntMatrix3D, 
      function: cern.colt.function.tint.IntIntFunction, 
      sliceList: IntArrayList, 
      rowList: IntArrayList, 
      columnList: IntArrayList): IntMatrix3D = {
    if (!(y.isInstanceOf[DenseIntMatrix3D])) {
      super.assign(y, function)
      return this
    }
    checkShape(y)
    val zero = index(0, 0, 0).toInt
    val zeroOther = y.index(0, 0, 0).toInt
    val sliceStrideOther = y.sliceStride()
    val rowStrideOther = y.rowStride()
    val columnStrideOther = y.columnStride()
    val elemsOther = y.elements().asInstanceOf[Array[Int]]
    val size = sliceList.size
    val sliceElements = sliceList.elements()
    val rowElements = rowList.elements()
    val columnElements = columnList.elements()
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_3D)) {
      nthreads = Math.min(nthreads, size)
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
              elements(idx) = function.apply(elements(idx), elemsOther(idxOther))
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
        elements(idx) = function.apply(elements(idx), elemsOther(idxOther))
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

  def elements(): Array[Int] = elements

  def getNegativeValues(sliceList: IntArrayList, 
      rowList: IntArrayList, 
      columnList: IntArrayList, 
      valueList: IntArrayList) {
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
      valueList: IntArrayList) {
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
      valueList: IntArrayList) {
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

  def getQuick(slice: Int, row: Int, column: Int): Int = {
    elements(sliceZero + slice * sliceStride + rowZero + row * rowStride + 
      columnZero + 
      column * columnStride)
  }

  def index(slice: Int, row: Int, column: Int): Long = {
    sliceZero + slice * sliceStride + rowZero + row * rowStride + 
      columnZero + 
      column * columnStride
  }

  def like(slices: Int, rows: Int, columns: Int): IntMatrix3D = {
    new DenseIntMatrix3D(slices, rows, columns)
  }

  def like2D(rows: Int, columns: Int): IntMatrix2D = new DenseIntMatrix2D(rows, columns)

  def getMaxLocation(): Array[Int] = {
    val zero = index(0, 0, 0).toInt
    var slice_loc = 0
    var row_loc = 0
    var col_loc = 0
    var maxValue = 0
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_3D)) {
      nthreads = Math.min(nthreads, slices)
      val futures = Array.ofDim[Future](nthreads)
      val results = Array.ofDim[Int](nthreads, 2)
      val k = slices / nthreads
      for (j <- 0 until nthreads) {
        val firstSlice = j * k
        val lastSlice = if ((j == nthreads - 1)) slices else firstSlice + k
        futures(j) = ConcurrencyUtils.submit(new Callable[Array[Int]]() {

          def call(): Array[Int] = {
            var slice_loc = firstSlice
            var row_loc = 0
            var col_loc = 0
            var maxValue = elements(zero + firstSlice * sliceStride)
            var d = 1
            var elem: Int = 0
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
          results(j) = futures(j).get.asInstanceOf[Array[Int]]
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
      var elem: Int = 0
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

  def getMinLocation(): Array[Int] = {
    val zero = index(0, 0, 0).toInt
    var slice_loc = 0
    var row_loc = 0
    var col_loc = 0
    var minValue = 0
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_3D)) {
      nthreads = Math.min(nthreads, slices)
      val futures = Array.ofDim[Future](nthreads)
      val results = Array.ofDim[Int](nthreads, 2)
      val k = slices / nthreads
      for (j <- 0 until nthreads) {
        val firstSlice = j * k
        val lastSlice = if ((j == nthreads - 1)) slices else firstSlice + k
        futures(j) = ConcurrencyUtils.submit(new Callable[Array[Int]]() {

          def call(): Array[Int] = {
            var slice_loc = firstSlice
            var row_loc = 0
            var col_loc = 0
            var minValue = elements(zero + slice_loc * sliceStride)
            var d = 1
            var elem: Int = 0
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
          results(j) = futures(j).get.asInstanceOf[Array[Int]]
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
      var elem: Int = 0
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

  def setQuick(slice: Int, 
      row: Int, 
      column: Int, 
      value: Int) {
    elements(sliceZero + slice * sliceStride + rowZero + row * rowStride + 
      columnZero + 
      column * columnStride) = value
  }

  def toArray(): Array[Array[Array[Int]]] = {
    val values = Array.ofDim[Int](slices, rows, columns)
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

  def vectorize(): IntMatrix1D = {
    val v = new DenseIntMatrix1D(size.toInt)
    val length = rows * columns
    for (s <- 0 until slices) {
      v.viewPart(s * length, length).assign(viewSlice(s).vectorize())
    }
    v
  }

  def zSum(): Int = {
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
        futures(j) = ConcurrencyUtils.submit(new Callable[Integer]() {

          def call(): java.lang.Integer = {
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
          sum = sum + futures(j).get.asInstanceOf[java.lang.Integer]
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

  protected def haveSharedCellsRaw(other: IntMatrix3D): Boolean = {
    if (other.isInstanceOf[SelectedDenseIntMatrix3D]) {
      val otherMatrix = other.asInstanceOf[SelectedDenseIntMatrix3D]
      return this.elements == otherMatrix.elements
    } else if (other.isInstanceOf[DenseIntMatrix3D]) {
      val otherMatrix = other.asInstanceOf[DenseIntMatrix3D]
      return this.elements == otherMatrix.elements
    }
    false
  }

  protected def like2D(rows: Int, 
      columns: Int, 
      rowZero: Int, 
      columnZero: Int, 
      rowStride: Int, 
      columnStride: Int): IntMatrix2D = {
    new DenseIntMatrix2D(rows, columns, this.elements, rowZero, columnZero, rowStride, columnStride, 
      true)
  }

  protected def viewSelectionLike(sliceOffsets: Array[Int], rowOffsets: Array[Int], columnOffsets: Array[Int]): IntMatrix3D = {
    new SelectedDenseIntMatrix3D(this.elements, sliceOffsets, rowOffsets, columnOffsets, 0)
  }
}
