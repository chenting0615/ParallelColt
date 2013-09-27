package cern.colt.matrix.tint.impl

import java.util.concurrent.Callable
import java.util.concurrent.ExecutionException
import java.util.concurrent.Future
import cern.colt.list.tint.IntArrayList
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
 * Internally holds one single contigous one-dimensional array, addressed in row
 * major. Note that this implementation is not synchronized.
 * <p>
 * <b>Memory requirements:</b>
 * <p>
 * <tt>memory [bytes] = 8*rows()*columns()</tt>. Thus, a 1000*1000 matrix uses 8
 * MB.
 * <p>
 * <b>Time complexity:</b>
 * <p>
 * <tt>O(1)</tt> (i.e. constant time) for the basic operations <tt>get</tt>,
 * <tt>getQuick</tt>, <tt>set</tt>, <tt>setQuick</tt> and <tt>size</tt>,
 * <p>
 * Cells are internally addressed in row-major. Applications demanding utmost
 * speed can exploit this fact. Setting/getting values in a loop row-by-row is
 * quicker than column-by-column. Thus
 *
 * <pre>
 * for (int row = 0; row &lt; rows; row++) {
 *     for (int column = 0; column &lt; columns; column++) {
 *         matrix.setQuick(row, column, someValue);
 *     }
 * }
 * </pre>
 *
 * is quicker than
 *
 * <pre>
 * for (int column = 0; column &lt; columns; column++) {
 *     for (int row = 0; row &lt; rows; row++) {
 *         matrix.setQuick(row, column, someValue);
 *     }
 * }
 * </pre>
 *
 * @author wolfgang.hoschek@cern.ch
 * @version 1.0, 09/24/99
 *
 * @author Piotr Wendykier (piotr.wendykier@gmail.com)
 *
 */
@SerialVersionUID(1L)
class DenseIntMatrix2D(rows: Int, columns: Int) extends IntMatrix2D {

  /**
   * The elements of this matrix. elements are stored in row major, i.e.
   * index==row*columns + column columnOf(index)==index%columns
   * rowOf(index)==index/columns i.e. {row0 column0..m}, {row1 column0..m},
   * ..., {rown column0..m}
   */
  protected var elements: Array[Int] = new Array[Int](rows * columns)

  setUp(rows, columns)

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
   * Constructs a view with the given parameters.
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
   *             <tt>rows<0 || columns<0 || (int)columns*rows > Int.MAX_VALUE</tt>
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

  def aggregate(aggr: cern.colt.function.tint.IntIntFunction, f: cern.colt.function.tint.IntFunction): Int = {
    if (size == 0) throw new IllegalArgumentException("size == 0")
    val zero = index(0, 0).toInt
    var a = 0
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_2D)) {
      nthreads = Math.min(nthreads, rows)
      val futures = Array.ofDim[Future](nthreads)
      val k = rows / nthreads
      for (j <- 0 until nthreads) {
        val firstRow = j * k
        val lastRow = if ((j == nthreads - 1)) rows else firstRow + k
        futures(j) = ConcurrencyUtils.submit(new Callable[Integer]() {

          def call(): java.lang.Integer = {
            var a = f.apply(elements(zero + firstRow * rowStride))
            var d = 1
            for (r <- firstRow until lastRow) {
              for (c <- d until columns) {
                a = aggr.apply(a, f.apply(elements(zero + r * rowStride + c * columnStride)))
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
      for (r <- 0 until rows) {
        for (c <- d until columns) {
          a = aggr.apply(a, f.apply(elements(zero + r * rowStride + c * columnStride)))
        }
        d = 0
      }
    }
    a
  }

  def aggregate(aggr: cern.colt.function.tint.IntIntFunction, f: cern.colt.function.tint.IntFunction, cond: cern.colt.function.tint.IntProcedure): Int = {
    if (size == 0) throw new IllegalArgumentException("size == 0")
    val zero = index(0, 0).toInt
    var a = 0
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_2D)) {
      nthreads = Math.min(nthreads, rows)
      val futures = Array.ofDim[Future](nthreads)
      val k = rows / nthreads
      for (j <- 0 until nthreads) {
        val firstRow = j * k
        val lastRow = if ((j == nthreads - 1)) rows else firstRow + k
        futures(j) = ConcurrencyUtils.submit(new Callable[Integer]() {

          def call(): java.lang.Integer = {
            var elem = elements(zero + firstRow * rowStride)
            var a = 0
            if (cond.apply(elem) == true) {
              a = f.apply(elem)
            }
            var d = 1
            for (r <- firstRow until lastRow) {
              for (c <- d until columns) {
                elem = elements(zero + r * rowStride + c * columnStride)
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
      var elem = elements(zero)
      if (cond.apply(elem) == true) {
        a = f.apply(elements(zero))
      }
      var d = 1
      for (r <- 0 until rows) {
        for (c <- d until columns) {
          elem = elements(zero + r * rowStride + c * columnStride)
          if (cond.apply(elem) == true) {
            a = aggr.apply(a, f.apply(elem))
          }
        }
        d = 0
      }
    }
    a
  }

  def aggregate(aggr: cern.colt.function.tint.IntIntFunction,
      f: cern.colt.function.tint.IntFunction,
      rowList: IntArrayList,
      columnList: IntArrayList): Int = {
    if (size == 0) throw new IllegalArgumentException("size == 0")
    val zero = index(0, 0).toInt
    val size = rowList.size
    val rowElements = rowList.elements()
    val columnElements = columnList.elements()
    var a = 0
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_2D)) {
      nthreads = Math.min(nthreads, size)
      val futures = Array.ofDim[Future](nthreads)
      val k = size / nthreads
      for (j <- 0 until nthreads) {
        val firstIdx = j * k
        val lastIdx = if ((j == nthreads - 1)) size else firstIdx + k
        futures(j) = ConcurrencyUtils.submit(new Callable[Integer]() {

          def call(): java.lang.Integer = {
            var a = f.apply(elements(zero + rowElements(firstIdx) * rowStride + columnElements(firstIdx) * columnStride))
            var elem: Int = 0
            for (i <- firstIdx + 1 until lastIdx) {
              elem = elements(zero + rowElements(i) * rowStride + columnElements(i) * columnStride)
              a = aggr.apply(a, f.apply(elem))
            }
            return a
          }
        })
      }
      a = ConcurrencyUtils.waitForCompletion(futures, aggr)
    } else {
      var elem: Int = 0
      a = f.apply(elements(zero + rowElements(0) * rowStride + columnElements(0) * columnStride))
      for (i <- 1 until size) {
        elem = elements(zero + rowElements(i) * rowStride + columnElements(i) * columnStride)
        a = aggr.apply(a, f.apply(elem))
      }
    }
    a
  }

  def aggregate(other: IntMatrix2D, aggr: cern.colt.function.tint.IntIntFunction, f: cern.colt.function.tint.IntIntFunction): Int = {
    if (!(other.isInstanceOf[DenseIntMatrix2D])) {
      return super.aggregate(other, aggr, f)
    }
    checkShape(other)
    if (size == 0) throw new IllegalArgumentException("size == 0")
    val zero = index(0, 0).toInt
    val zeroOther = other.index(0, 0).toInt
    val rowStrideOther = other.rowStride()
    val colStrideOther = other.columnStride()
    val elemsOther = other.elements().asInstanceOf[Array[Int]]
    var a = 0
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_2D)) {
      nthreads = Math.min(nthreads, rows)
      val futures = Array.ofDim[Future](nthreads)
      val k = rows / nthreads
      for (j <- 0 until nthreads) {
        val firstRow = j * k
        val lastRow = if ((j == nthreads - 1)) rows else firstRow + k
        futures(j) = ConcurrencyUtils.submit(new Callable[Integer]() {

          def call(): java.lang.Integer = {
            var a = f.apply(elements(zero + firstRow * rowStride), elemsOther(zeroOther + firstRow * rowStrideOther))
            var d = 1
            for (r <- firstRow until lastRow) {
              for (c <- d until columns) {
                a = aggr.apply(a, f.apply(elements(zero + r * rowStride + c * columnStride), elemsOther(zeroOther + r * rowStrideOther + c * colStrideOther)))
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
      a = f.apply(elements(zero), elemsOther(zeroOther))
      for (r <- 0 until rows) {
        for (c <- d until columns) {
          a = aggr.apply(a, f.apply(elements(zero + r * rowStride + c * columnStride), elemsOther(zeroOther + r * rowStrideOther + c * colStrideOther)))
        }
        d = 0
      }
    }
    a
  }

  def assign(function: cern.colt.function.tint.IntFunction): IntMatrix2D = {
    val elems = this.elements
    if (elems == null) throw new InternalError()
    val zero = index(0, 0).toInt
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_2D)) {
      if (function.isInstanceOf[cern.jet.math.tint.IntMult]) {
        val multiplicator = function.asInstanceOf[cern.jet.math.tint.IntMult].multiplicator
        if (multiplicator == 1) return this
        if (multiplicator == 0) return assign(0)
      }
      nthreads = Math.min(nthreads, rows)
      val futures = Array.ofDim[Future](nthreads)
      val k = rows / nthreads
      for (j <- 0 until nthreads) {
        val firstRow = j * k
        val lastRow = if ((j == nthreads - 1)) rows else firstRow + k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            var idx = zero + firstRow * rowStride
            if (function.isInstanceOf[cern.jet.math.tint.IntMult]) {
              var multiplicator = function.asInstanceOf[cern.jet.math.tint.IntMult].multiplicator
              if (multiplicator == 1) return
              for (r <- firstRow until lastRow) {
                for (i <- idx until columns) {
                  elems(i) *= multiplicator
                  i += columnStride
                }
                idx += rowStride
              }
            } else {
              for (r <- firstRow until lastRow) {
                for (i <- idx until columns) {
                  elems(i) = function.apply(elems(i))
                  i += columnStride
                }
                idx += rowStride
              }
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
    } else {
      var idx = zero
      if (function.isInstanceOf[cern.jet.math.tint.IntMult]) {
        val multiplicator = function.asInstanceOf[cern.jet.math.tint.IntMult].multiplicator
        if (multiplicator == 1) return this
        if (multiplicator == 0) return assign(0)
        for (r <- 0 until rows) {
          for (i <- idx until columns) {
            elems(i) *= multiplicator
            i += columnStride
          }
          idx += rowStride
        }
      } else {
        for (r <- 0 until rows) {
          for (i <- idx until columns) {
            elems(i) = function.apply(elems(i))
            i += columnStride
          }
          idx += rowStride
        }
      }
    }
    this
  }

  def assign(cond: cern.colt.function.tint.IntProcedure, function: cern.colt.function.tint.IntFunction): IntMatrix2D = {
    val zero = index(0, 0).toInt
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_2D)) {
      nthreads = Math.min(nthreads, rows)
      val futures = Array.ofDim[Future](nthreads)
      val k = rows / nthreads
      for (j <- 0 until nthreads) {
        val firstRow = j * k
        val lastRow = if ((j == nthreads - 1)) rows else firstRow + k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            var elem: Int = 0
            var idx = zero + firstRow * rowStride
            for (r <- firstRow until lastRow) {
              for (i <- idx until columns) {
                elem = elements(i)
                if (cond.apply(elem) == true) {
                  elements(i) = function.apply(elem)
                }
                i += columnStride
              }
              idx += rowStride
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
    } else {
      var elem: Int = 0
      var idx = zero
      for (r <- 0 until rows) {
        for (i <- idx until columns) {
          elem = elements(i)
          if (cond.apply(elem) == true) {
            elements(i) = function.apply(elem)
          }
          i += columnStride
        }
        idx += rowStride
      }
    }
    this
  }

  def assign(cond: cern.colt.function.tint.IntProcedure, value: Int): IntMatrix2D = {
    val zero = index(0, 0).toInt
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_2D)) {
      nthreads = Math.min(nthreads, rows)
      val futures = Array.ofDim[Future](nthreads)
      val k = rows / nthreads
      for (j <- 0 until nthreads) {
        val firstRow = j * k
        val lastRow = if ((j == nthreads - 1)) rows else firstRow + k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            var elem: Int = 0
            var idx = zero + firstRow * rowStride
            for (r <- firstRow until lastRow) {
              for (i <- idx until columns) {
                elem = elements(i)
                if (cond.apply(elem) == true) {
                  elements(i) = value
                }
                i += columnStride
              }
              idx += rowStride
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
    } else {
      var elem: Int = 0
      var idx = zero
      for (r <- 0 until rows) {
        for (i <- idx until columns) {
          elem = elements(i)
          if (cond.apply(elem) == true) {
            elements(i) = value
          }
          i += columnStride
        }
        idx += rowStride
      }
    }
    this
  }

  def assign(value: Int): IntMatrix2D = {
    val elems = this.elements
    val zero = index(0, 0).toInt
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_2D)) {
      nthreads = Math.min(nthreads, rows)
      val futures = Array.ofDim[Future](nthreads)
      val k = rows / nthreads
      for (j <- 0 until nthreads) {
        val firstRow = j * k
        val lastRow = if ((j == nthreads - 1)) rows else firstRow + k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            var idx = zero + firstRow * rowStride
            for (r <- firstRow until lastRow) {
              for (i <- idx until columns) {
                elems(i) = value
                i += columnStride
              }
              idx += rowStride
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
    } else {
      var idx = zero
      for (r <- 0 until rows) {
        for (i <- idx until columns) {
          elems(i) = value
          i += columnStride
        }
        idx += rowStride
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
        nthreads = Math.min(nthreads, rows)
        val futures = Array.ofDim[Future](nthreads)
        val k = rows / nthreads
        for (j <- 0 until nthreads) {
          val firstRow = j * k
          val lastRow = if ((j == nthreads - 1)) rows else firstRow + k
          futures(j) = ConcurrencyUtils.submit(new Runnable() {

            def run() {
              var idxOther = firstRow * columns
              var idx = zero + firstRow * rowStride
              for (r <- firstRow until lastRow) {
                for (i <- idx until columns) {
                  elements(i) = values(idxOther += 1)
                  i += columnStride
                }
                idx += rowStride
              }
            }
          })
        }
        ConcurrencyUtils.waitForCompletion(futures)
      } else {
        val idxOther = 0
        var idx = zero
        for (r <- 0 until rows) {
          for (i <- idx until columns) {
            elements(i) = values(idxOther += 1)
            i += columnStride
          }
          idx += rowStride
        }
      }
    }
    this
  }

  def assign(values: Array[Array[Int]]): IntMatrix2D = {
    if (values.length != rows) throw new IllegalArgumentException("Must have same number of rows: rows=" + values.length +
      "rows()=" +
      rows())
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if (this.isNoView) {
      if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_2D)) {
        nthreads = Math.min(nthreads, rows)
        val futures = Array.ofDim[Future](nthreads)
        val k = rows / nthreads
        for (j <- 0 until nthreads) {
          val firstRow = j * k
          val lastRow = if ((j == nthreads - 1)) rows else firstRow + k
          futures(j) = ConcurrencyUtils.submit(new Runnable() {

            def run() {
              var i = firstRow * rowStride
              for (r <- firstRow until lastRow) {
                var currentRow = values(r)
                if (currentRow.length != columns) throw new IllegalArgumentException("Must have same number of columns in every row: columns=" +
                  currentRow.length +
                  "columns()=" +
                  columns())
                System.arraycopy(currentRow, 0, elements, i, columns)
                i += columns
              }
            }
          })
        }
        ConcurrencyUtils.waitForCompletion(futures)
      } else {
        var i = 0
        for (r <- 0 until rows) {
          val currentRow = values(r)
          if (currentRow.length != columns) throw new IllegalArgumentException("Must have same number of columns in every row: columns=" +
            currentRow.length +
            "columns()=" +
            columns())
          System.arraycopy(currentRow, 0, this.elements, i, columns)
          i += columns
        }
      }
    } else {
      val zero = index(0, 0).toInt
      if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_2D)) {
        nthreads = Math.min(nthreads, rows)
        val futures = Array.ofDim[Future](nthreads)
        val k = rows / nthreads
        for (j <- 0 until nthreads) {
          val firstRow = j * k
          val lastRow = if ((j == nthreads - 1)) rows else firstRow + k
          futures(j) = ConcurrencyUtils.submit(new Runnable() {

            def run() {
              var idx = zero + firstRow * rowStride
              for (r <- firstRow until lastRow) {
                var currentRow = values(r)
                if (currentRow.length != columns) throw new IllegalArgumentException("Must have same number of columns in every row: columns=" +
                  currentRow.length +
                  "columns()=" +
                  columns())
                for (i <- idx until columns) {
                  elements(i) = currentRow(c)
                  i += columnStride
                }
                idx += rowStride
              }
            }
          })
        }
        ConcurrencyUtils.waitForCompletion(futures)
      } else {
        var idx = zero
        for (r <- 0 until rows) {
          val currentRow = values(r)
          if (currentRow.length != columns) throw new IllegalArgumentException("Must have same number of columns in every row: columns=" +
            currentRow.length +
            "columns()=" +
            columns())
          for (i <- idx until columns) {
            elements(i) = currentRow(c)
            i += columnStride
          }
          idx += rowStride
        }
      }
      return this
    }
    this
  }

  def assign(source: IntMatrix2D): IntMatrix2D = {
    if (!(source.isInstanceOf[DenseIntMatrix2D])) {
      super.assign(source)
      return this
    }
    val other_final = source.asInstanceOf[DenseIntMatrix2D]
    if (other_final == this) return this
    checkShape(other_final)
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if (this.isNoView && other_final.isNoView) {
      System.arraycopy(other_final.elements, 0, this.elements, 0, this.elements.length)
      return this
    }
    var other = source.asInstanceOf[DenseIntMatrix2D]
    if (haveSharedCells(other)) {
      val c = other.copy()
      if (!(c.isInstanceOf[DenseIntMatrix2D])) {
        super.assign(other)
        return this
      }
      other = c.asInstanceOf[DenseIntMatrix2D]
    }
    val elemsOther = other.elements
    if (elements == null || elemsOther == null) throw new InternalError()
    val zeroOther = other.index(0, 0).toInt
    val zero = index(0, 0).toInt
    val columnStrideOther = other.columnStride
    val rowStrideOther = other.rowStride
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_2D)) {
      nthreads = Math.min(nthreads, rows)
      val futures = Array.ofDim[Future](nthreads)
      val k = rows / nthreads
      for (j <- 0 until nthreads) {
        val firstRow = j * k
        val lastRow = if ((j == nthreads - 1)) rows else firstRow + k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            var idx = zero + firstRow * rowStride
            var idxOther = zeroOther + firstRow * rowStrideOther
            for (r <- firstRow until lastRow) {
              for (i <- idx until columns) {
                elements(i) = elemsOther(j)
                i += columnStride
                j += columnStrideOther
              }
              idx += rowStride
              idxOther += rowStrideOther
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
    } else {
      var idx = zero
      var idxOther = zeroOther
      for (r <- 0 until rows) {
        for (i <- idx until columns) {
          elements(i) = elemsOther(j)
          i += columnStride
          j += columnStrideOther
        }
        idx += rowStride
        idxOther += rowStrideOther
      }
    }
    this
  }

  def assign(y: IntMatrix2D, function: cern.colt.function.tint.IntIntFunction): IntMatrix2D = {
    if (!(y.isInstanceOf[DenseIntMatrix2D])) {
      super.assign(y, function)
      return this
    }
    val other = y.asInstanceOf[DenseIntMatrix2D]
    checkShape(y)
    val elemsOther = other.elements
    if (elements == null || elemsOther == null) throw new InternalError()
    val zeroOther = other.index(0, 0).toInt
    val zero = index(0, 0).toInt
    val columnStrideOther = other.columnStride
    val rowStrideOther = other.rowStride
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_2D)) {
      if (function.isInstanceOf[cern.jet.math.tint.IntPlusMultSecond]) {
        val multiplicator = function.asInstanceOf[cern.jet.math.tint.IntPlusMultSecond].multiplicator
        if (multiplicator == 0) {
          return this
        }
      }
      nthreads = Math.min(nthreads, rows)
      val futures = Array.ofDim[Future](nthreads)
      val k = rows / nthreads
      for (j <- 0 until nthreads) {
        val firstRow = j * k
        val lastRow = if ((j == nthreads - 1)) rows else firstRow + k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            var idx: Int = 0
            var idxOther: Int = 0
            if (function == cern.jet.math.tint.IntFunctions.mult) {
              idx = zero + firstRow * rowStride
              idxOther = zeroOther + firstRow * rowStrideOther
              for (r <- firstRow until lastRow) {
                for (i <- idx until columns) {
                  elements(i) *= elemsOther(j)
                  i += columnStride
                  j += columnStrideOther
                }
                idx += rowStride
                idxOther += rowStrideOther
              }
            } else if (function == cern.jet.math.tint.IntFunctions.div) {
              idx = zero + firstRow * rowStride
              idxOther = zeroOther + firstRow * rowStrideOther
              for (r <- firstRow until lastRow) {
                for (i <- idx until columns) {
                  elements(i) /= elemsOther(j)
                  i += columnStride
                  j += columnStrideOther
                }
                idx += rowStride
                idxOther += rowStrideOther
              }
            } else if (function.isInstanceOf[cern.jet.math.tint.IntPlusMultSecond]) {
              var multiplicator = function.asInstanceOf[cern.jet.math.tint.IntPlusMultSecond].multiplicator
              if (multiplicator == 1) {
                idx = zero + firstRow * rowStride
                idxOther = zeroOther + firstRow * rowStrideOther
                for (r <- firstRow until lastRow) {
                  for (i <- idx until columns) {
                    elements(i) += elemsOther(j)
                    i += columnStride
                    j += columnStrideOther
                  }
                  idx += rowStride
                  idxOther += rowStrideOther
                }
              } else if (multiplicator == -1) {
                idx = zero + firstRow * rowStride
                idxOther = zeroOther + firstRow * rowStrideOther
                for (r <- firstRow until lastRow) {
                  for (i <- idx until columns) {
                    elements(i) -= elemsOther(j)
                    i += columnStride
                    j += columnStrideOther
                  }
                  idx += rowStride
                  idxOther += rowStrideOther
                }
              } else {
                idx = zero + firstRow * rowStride
                idxOther = zeroOther + firstRow * rowStrideOther
                for (r <- firstRow until lastRow) {
                  for (i <- idx until columns) {
                    elements(i) += multiplicator * elemsOther(j)
                    i += columnStride
                    j += columnStrideOther
                  }
                  idx += rowStride
                  idxOther += rowStrideOther
                }
              }
            } else {
              idx = zero + firstRow * rowStride
              idxOther = zeroOther + firstRow * rowStrideOther
              for (r <- firstRow until lastRow) {
                for (i <- idx until columns) {
                  elements(i) = function.apply(elements(i), elemsOther(j))
                  i += columnStride
                  j += columnStrideOther
                }
                idx += rowStride
                idxOther += rowStrideOther
              }
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
    } else {
      var idx: Int = 0
      var idxOther: Int = 0
      if (function == cern.jet.math.tint.IntFunctions.mult) {
        idx = zero
        idxOther = zeroOther
        for (r <- 0 until rows) {
          for (i <- idx until columns) {
            elements(i) *= elemsOther(j)
            i += columnStride
            j += columnStrideOther
          }
          idx += rowStride
          idxOther += rowStrideOther
        }
      } else if (function == cern.jet.math.tint.IntFunctions.div) {
        idx = zero
        idxOther = zeroOther
        for (r <- 0 until rows) {
          for (i <- idx until columns) {
            elements(i) /= elemsOther(j)
            i += columnStride
            j += columnStrideOther
          }
          idx += rowStride
          idxOther += rowStrideOther
        }
      } else if (function.isInstanceOf[cern.jet.math.tint.IntPlusMultSecond]) {
        val multiplicator = function.asInstanceOf[cern.jet.math.tint.IntPlusMultSecond].multiplicator
        if (multiplicator == 0) {
          return this
        } else if (multiplicator == 1) {
          idx = zero
          idxOther = zeroOther
          for (r <- 0 until rows) {
            for (i <- idx until columns) {
              elements(i) += elemsOther(j)
              i += columnStride
              j += columnStrideOther
            }
            idx += rowStride
            idxOther += rowStrideOther
          }
        } else if (multiplicator == -1) {
          idx = zero
          idxOther = zeroOther
          for (r <- 0 until rows) {
            for (i <- idx until columns) {
              elements(i) -= elemsOther(j)
              i += columnStride
              j += columnStrideOther
            }
            idx += rowStride
            idxOther += rowStrideOther
          }
        } else {
          idx = zero
          idxOther = zeroOther
          for (r <- 0 until rows) {
            for (i <- idx until columns) {
              elements(i) += multiplicator * elemsOther(j)
              i += columnStride
              j += columnStrideOther
            }
            idx += rowStride
            idxOther += rowStrideOther
          }
        }
      } else {
        idx = zero
        idxOther = zeroOther
        for (r <- 0 until rows) {
          for (i <- idx until columns) {
            elements(i) = function.apply(elements(i), elemsOther(j))
            i += columnStride
            j += columnStrideOther
          }
          idx += rowStride
          idxOther += rowStrideOther
        }
      }
    }
    this
  }

  def assign(y: IntMatrix2D,
      function: cern.colt.function.tint.IntIntFunction,
      rowList: IntArrayList,
      columnList: IntArrayList): IntMatrix2D = {
    checkShape(y)
    val size = rowList.size
    val rowElements = rowList.elements()
    val columnElements = columnList.elements()
    val elemsOther = y.elements().asInstanceOf[Array[Int]]
    val zeroOther = y.index(0, 0).toInt
    val zero = index(0, 0).toInt
    val columnStrideOther = y.columnStride()
    val rowStrideOther = y.rowStride()
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_2D)) {
      nthreads = Math.min(nthreads, size)
      val futures = Array.ofDim[Future](nthreads)
      val k = size / nthreads
      for (j <- 0 until nthreads) {
        val firstIdx = j * k
        val lastIdx = if ((j == nthreads - 1)) size else firstIdx + k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            var idx: Int = 0
            var idxOther: Int = 0
            for (i <- firstIdx until lastIdx) {
              idx = zero + rowElements(i) * rowStride + columnElements(i) * columnStride
              idxOther = zeroOther + rowElements(i) * rowStrideOther + columnElements(i) * columnStrideOther
              elements(idx) = function.apply(elements(idx), elemsOther(idxOther))
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
    } else {
      var idx: Int = 0
      var idxOther: Int = 0
      for (i <- 0 until size) {
        idx = zero + rowElements(i) * rowStride + columnElements(i) * columnStride
        idxOther = zeroOther + rowElements(i) * rowStrideOther + columnElements(i) * columnStrideOther
        elements(idx) = function.apply(elements(idx), elemsOther(idxOther))
      }
    }
    this
  }

  def cardinality(): Int = {
    var cardinality = 0
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    val zero = index(0, 0).toInt
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_2D)) {
      nthreads = Math.min(nthreads, rows)
      val futures = Array.ofDim[Future](nthreads)
      val results = Array.ofDim[Integer](nthreads)
      val k = rows / nthreads
      for (j <- 0 until nthreads) {
        val firstRow = j * k
        val lastRow = if ((j == nthreads - 1)) rows else firstRow + k
        futures(j) = ConcurrencyUtils.submit(new Callable[Integer]() {

          def call(): java.lang.Integer = {
            var cardinality = 0
            var idx = zero + firstRow * rowStride
            for (r <- firstRow until lastRow) {
              for (i <- idx until columns) {
                if (elements(i) != 0) cardinality += 1
                i += columnStride
              }
              idx += rowStride
            }
            return cardinality
          }
        })
      }
      try {
        for (j <- 0 until nthreads) {
          results(j) = futures(j).get.asInstanceOf[java.lang.Integer]
        }
        cardinality = results(0).intValue()
        for (j <- 1 until nthreads) {
          cardinality += results(j).intValue()
        }
      } catch {
        case ex: ExecutionException => ex.printStackTrace()
        case e: InterruptedException => e.printStackTrace()
      }
    } else {
      var idx = zero
      for (r <- 0 until rows) {
        for (i <- idx until columns) {
          if (elements(i) != 0) cardinality += 1
          i += columnStride
        }
        idx += rowStride
      }
    }
    cardinality
  }

  def elements(): Array[Int] = elements

  def forEachNonZero(function: cern.colt.function.tint.IntIntIntFunction): IntMatrix2D = {
    val zero = index(0, 0).toInt
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_2D)) {
      nthreads = Math.min(nthreads, rows)
      val futures = Array.ofDim[Future](nthreads)
      val k = rows / nthreads
      for (j <- 0 until nthreads) {
        val firstRow = j * k
        val lastRow = if ((j == nthreads - 1)) rows else firstRow + k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            var idx = zero + firstRow * rowStride
            for (r <- firstRow until lastRow) {
              for (i <- idx until columns) {
                var value = elements(i)
                if (value != 0) {
                  elements(i) = function.apply(r, c, value)
                }
                i += columnStride
              }
              idx += rowStride
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
    } else {
      var idx = zero
      for (r <- 0 until rows) {
        for (i <- idx until columns) {
          val value = elements(i)
          if (value != 0) {
            elements(i) = function.apply(r, c, value)
          }
          i += columnStride
        }
        idx += rowStride
      }
    }
    this
  }

  def getNegativeValues(rowList: IntArrayList, columnList: IntArrayList, valueList: IntArrayList) {
    rowList.clear()
    columnList.clear()
    valueList.clear()
    var idx = index(0, 0).toInt
    for (r <- 0 until rows) {
      for (i <- idx until columns) {
        val value = elements(i)
        if (value < 0) {
          rowList.add(r)
          columnList.add(c)
          valueList.add(value)
        }
        i += columnStride
      }
      idx += rowStride
    }
  }

  def getNonZeros(rowList: IntArrayList, columnList: IntArrayList, valueList: IntArrayList) {
    rowList.clear()
    columnList.clear()
    valueList.clear()
    var idx = index(0, 0).toInt
    for (r <- 0 until rows) {
      for (i <- idx until columns) {
        val value = elements(i)
        if (value != 0) {
          rowList.add(r)
          columnList.add(c)
          valueList.add(value)
        }
        i += columnStride
      }
      idx += rowStride
    }
  }

  def getPositiveValues(rowList: IntArrayList, columnList: IntArrayList, valueList: IntArrayList) {
    rowList.clear()
    columnList.clear()
    valueList.clear()
    var idx = index(0, 0).toInt
    for (r <- 0 until rows) {
      for (i <- idx until columns) {
        val value = elements(i)
        if (value > 0) {
          rowList.add(r)
          columnList.add(c)
          valueList.add(value)
        }
        i += columnStride
      }
      idx += rowStride
    }
  }

  def getQuick(row: Int, column: Int): Int = {
    elements(rowZero + row * rowStride + columnZero + column * columnStride)
  }

  def index(row: Int, column: Int): Long = {
    rowZero + row * rowStride + columnZero + column * columnStride
  }

  def like(rows: Int, columns: Int): IntMatrix2D = new DenseIntMatrix2D(rows, columns)

  def like1D(size: Int): IntMatrix1D = new DenseIntMatrix1D(size)

  def getMaxLocation(): Array[Int] = {
    var rowLocation = 0
    var columnLocation = 0
    val zero = index(0, 0).toInt
    var maxValue = 0
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_2D)) {
      nthreads = Math.min(nthreads, rows)
      val futures = Array.ofDim[Future](nthreads)
      val results = Array.ofDim[Int](nthreads, 2)
      val k = rows / nthreads
      for (j <- 0 until nthreads) {
        val firstRow = j * k
        val lastRow = if ((j == nthreads - 1)) rows else firstRow + k
        futures(j) = ConcurrencyUtils.submit(new Callable[Array[Int]]() {

          def call(): Array[Int] = {
            var maxValue = elements(zero + firstRow * rowStride)
            var rowLocation = firstRow
            var colLocation = 0
            var elem: Int = 0
            var d = 1
            for (r <- firstRow until lastRow) {
              for (c <- d until columns) {
                elem = elements(zero + r * rowStride + c * columnStride)
                if (maxValue < elem) {
                  maxValue = elem
                  rowLocation = r
                  colLocation = c
                }
              }
              d = 0
            }
            return Array(maxValue, rowLocation, colLocation)
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
      maxValue = elements(zero)
      var d = 1
      var elem: Int = 0
      for (r <- 0 until rows) {
        for (c <- d until columns) {
          elem = elements(zero + r * rowStride + c * columnStride)
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
      nthreads = Math.min(nthreads, rows)
      val futures = Array.ofDim[Future](nthreads)
      val results = Array.ofDim[Int](nthreads, 2)
      val k = rows / nthreads
      for (j <- 0 until nthreads) {
        val firstRow = j * k
        val lastRow = if ((j == nthreads - 1)) rows else firstRow + k
        futures(j) = ConcurrencyUtils.submit(new Callable[Array[Int]]() {

          def call(): Array[Int] = {
            var rowLocation = firstRow
            var columnLocation = 0
            var minValue = elements(zero + firstRow * rowStride)
            var elem: Int = 0
            var d = 1
            for (r <- firstRow until lastRow) {
              for (c <- d until columns) {
                elem = elements(zero + r * rowStride + c * columnStride)
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
      minValue = elements(zero)
      var d = 1
      var elem: Int = 0
      for (r <- 0 until rows) {
        for (c <- d until columns) {
          elem = elements(zero + r * rowStride + c * columnStride)
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
      nthreads = Math.min(nthreads, rows)
      val futures = Array.ofDim[Future](nthreads)
      val k = rows / nthreads
      for (j <- 0 until nthreads) {
        val firstRow = j * k
        val lastRow = if ((j == nthreads - 1)) rows else firstRow + k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            var idx = zero + firstRow * rowStride
            for (r <- firstRow until lastRow) {
              var currentRow = values(r)
              for (i <- idx until columns) {
                currentRow(c) = elements(i)
                i += columnStride
              }
              idx += rowStride
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
    } else {
      var idx = zero
      for (r <- 0 until rows) {
        val currentRow = values(r)
        for (i <- idx until columns) {
          currentRow(c) = elements(i)
          i += columnStride
        }
        idx += rowStride
      }
    }
    values
  }

  def vectorize(): IntMatrix1D = {
    val v = new DenseIntMatrix1D(size.toInt)
    val zero = index(0, 0).toInt
    val zeroOther = v.index(0).toInt
    val strideOther = v.stride()
    val elemsOther = v.elements()
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_2D)) {
      nthreads = Math.min(nthreads, columns)
      val futures = Array.ofDim[Future](nthreads)
      val k = columns / nthreads
      for (j <- 0 until nthreads) {
        val firstColumn = j * k
        val lastColumn = if ((j == nthreads - 1)) columns else firstColumn + k
        val startidx = j * k * rows
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            var idx = 0
            var idxOther = zeroOther + startidx * strideOther
            for (c <- firstColumn until lastColumn) {
              idx = zero + c * columnStride
              for (r <- 0 until rows) {
                elemsOther(idxOther) = elements(idx)
                idx += rowStride
                idxOther += strideOther
              }
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
    } else {
      var idx = zero
      var idxOther = zeroOther
      for (c <- 0 until columns) {
        idx = zero + c * columnStride
        for (r <- 0 until rows) {
          elemsOther(idxOther) = elements(idx)
          idx += rowStride
          idxOther += strideOther
        }
      }
    }
    v
  }

  def zMult(y: IntMatrix1D,
      z: IntMatrix1D,
      alpha: Int,
      beta: Int,
      transposeA: Boolean): IntMatrix1D = {
    if (transposeA) return viewDice().zMult(y, z, alpha, beta, false)
    if (z == null) {
      z = new DenseIntMatrix1D(rows)
    }
    if (!(y.isInstanceOf[DenseIntMatrix1D] && z.isInstanceOf[DenseIntMatrix1D])) return super.zMult(y,
      z, alpha, beta, transposeA)
    if (columns != y.size || rows > z.size) throw new IllegalArgumentException("Incompatible args: " + toShapeString() + ", " + y.toShapeString() +
      ", " +
      z.toShapeString())
    val elemsY = y.elements().asInstanceOf[Array[Int]]
    val elemsZ = z.elements().asInstanceOf[Array[Int]]
    if (elements == null || elemsY == null || elemsZ == null) throw new InternalError()
    val strideY = y.stride()
    val strideZ = z.stride()
    val zero = index(0, 0).toInt
    val zeroY = y.index(0).toInt
    val zeroZ = z.index(0).toInt
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_2D)) {
      nthreads = Math.min(nthreads, rows)
      val futures = Array.ofDim[Future](nthreads)
      val k = rows / nthreads
      for (j <- 0 until nthreads) {
        val firstRow = j * k
        val lastRow = if ((j == nthreads - 1)) rows else firstRow + k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            var idxZero = zero + firstRow * rowStride
            var idxZeroZ = zeroZ + firstRow * strideZ
            for (r <- firstRow until lastRow) {
              var sum = 0
              var idx = idxZero
              var idxY = zeroY
              for (c <- 0 until columns) {
                sum += elements(idx) * elemsY(idxY)
                idx += columnStride
                idxY += strideY
              }
              elemsZ(idxZeroZ) = alpha * sum + beta * elemsZ(idxZeroZ)
              idxZero += rowStride
              idxZeroZ += strideZ
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
    } else {
      var idxZero = zero
      var idxZeroZ = zeroZ
      for (r <- 0 until rows) {
        var sum = 0
        var idx = idxZero
        var idxY = zeroY
        for (c <- 0 until columns) {
          sum += elements(idx) * elemsY(idxY)
          idx += columnStride
          idxY += strideY
        }
        elemsZ(idxZeroZ) = alpha * sum + beta * elemsZ(idxZeroZ)
        idxZero += rowStride
        idxZeroZ += strideZ
      }
    }
    z
  }

  def zMult(B: IntMatrix2D,
      C: IntMatrix2D,
      alpha: Int,
      beta: Int,
      transposeA: Boolean,
      transposeB: Boolean): IntMatrix2D = {
    val rowsA = rows
    val columnsA = columns
    val rowsB = B.rows()
    val columnsB = B.columns()
    val rowsC = if (transposeA) columnsA else rowsA
    val columnsC = if (transposeB) rowsB else columnsB
    if (C == null) {
      C = new DenseIntMatrix2D(rowsC, columnsC)
    }
    if (transposeA) return viewDice().zMult(B, C, alpha, beta, false, transposeB)
    if (B.isInstanceOf[SparseIntMatrix2D] || B.isInstanceOf[SparseRCIntMatrix2D]) {
      if (C == null) {
        return B.zMult(this, null, alpha, beta, !transposeB, true)
          .viewDice()
      } else {
        B.zMult(this, C.viewDice(), alpha, beta, !transposeB, true)
        return C
      }
    }
    if (transposeB) return this.zMult(B.viewDice(), C, alpha, beta, transposeA, false)
    if (!(C.isInstanceOf[DenseIntMatrix2D])) return super.zMult(B, C, alpha, beta, transposeA, transposeB)
    if (B.rows() != columnsA) throw new IllegalArgumentException("Matrix2D inner dimensions must agree:" + this.toShapeString() +
      ", " +
      B.toShapeString())
    if (C.rows() != rowsA || C.columns() != columnsB) throw new IllegalArgumentException("Incompatibe result matrix: " + this.toShapeString() +
      ", " +
      B.toShapeString() +
      ", " +
      C.toShapeString())
    if (this == C || B == C) throw new IllegalArgumentException("Matrices must not be identical")
    val flops = 2 * rowsA * columnsA * columnsB
    var noOfTasks = Math.min(flops / 30000, ConcurrencyUtils.getNumberOfThreads).toInt
    val splitB = (columnsB >= noOfTasks)
    val width = if (splitB) columnsB else rowsA
    noOfTasks = Math.min(width, noOfTasks)
    if (noOfTasks < 2) {
      return this.zMultSequential(B, C, alpha, beta, transposeA, transposeB)
    }
    var span = width / noOfTasks
    val subTasks = Array.ofDim[Future](noOfTasks)
    for (i <- 0 until noOfTasks) {
      val offset = i * span
      if (i == noOfTasks - 1) span = width - span * i
      var AA: IntMatrix2D = null
      var BB: IntMatrix2D = null
      var CC: IntMatrix2D = null
      if (splitB) {
        AA = this
        BB = B.viewPart(0, offset, columnsA, span)
        CC = C.viewPart(0, offset, rowsA, span)
      } else {
        AA = this.viewPart(offset, 0, span, columnsA)
        BB = B
        CC = C.viewPart(offset, 0, span, columnsB)
      }
      subTasks(i) = ConcurrencyUtils.submit(new Runnable() {

        def run() {
          AA.asInstanceOf[DenseIntMatrix2D].zMultSequential(BB, CC, alpha, beta, transposeA, transposeB)
        }
      })
    }
    ConcurrencyUtils.waitForCompletion(subTasks)
    C
  }

  def zSum(): Int = {
    var sum = 0
    if (elements == null) throw new InternalError()
    val zero = index(0, 0).toInt
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_2D)) {
      nthreads = Math.min(nthreads, rows)
      val futures = Array.ofDim[Future](nthreads)
      val k = rows / nthreads
      for (j <- 0 until nthreads) {
        val firstRow = j * k
        val lastRow = if ((j == nthreads - 1)) rows else firstRow + k
        futures(j) = ConcurrencyUtils.submit(new Callable[Integer]() {

          def call(): java.lang.Integer = {
            var sum = 0
            var idx = zero + firstRow * rowStride
            for (r <- firstRow until lastRow) {
              for (i <- idx until columns) {
                sum += elements(i)
                i += columnStride
              }
              idx += rowStride
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
      var idx = zero
      for (r <- 0 until rows) {
        for (i <- idx until columns) {
          sum += elements(i)
          i += columnStride
        }
        idx += rowStride
      }
    }
    sum
  }

  private def zMultSequential(B: IntMatrix2D,
      C: IntMatrix2D,
      alpha: Int,
      beta: Int,
      transposeA: Boolean,
      transposeB: Boolean): IntMatrix2D = {
    if (transposeA) return viewDice().zMult(B, C, alpha, beta, false, transposeB)
    if (B.isInstanceOf[SparseIntMatrix2D] || B.isInstanceOf[SparseRCIntMatrix2D] ||
      B.isInstanceOf[SparseCCIntMatrix2D]) {
      if (C == null) {
        return B.zMult(this, null, alpha, beta, !transposeB, true)
          .viewDice()
      } else {
        B.zMult(this, C.viewDice(), alpha, beta, !transposeB, true)
        return C
      }
    }
    if (transposeB) return this.zMult(B.viewDice(), C, alpha, beta, transposeA, false)
    val rowsA = rows
    val columnsA = columns
    val p = B.columns()
    if (C == null) {
      C = new DenseIntMatrix2D(rowsA, p)
    }
    if (!(B.isInstanceOf[DenseIntMatrix2D]) || !(C.isInstanceOf[DenseIntMatrix2D])) return super.zMult(B,
      C, alpha, beta, transposeA, transposeB)
    if (B.rows() != columnsA) throw new IllegalArgumentException("Matrix2D inner dimensions must agree:" + toShapeString() +
      ", " +
      B.toShapeString())
    if (C.rows() != rowsA || C.columns() != p) throw new IllegalArgumentException("Incompatibel result matrix: " + toShapeString() + ", " +
      B.toShapeString() +
      ", " +
      C.toShapeString())
    if (this == C || B == C) throw new IllegalArgumentException("Matrices must not be identical")
    val BB = B.asInstanceOf[DenseIntMatrix2D]
    val CC = C.asInstanceOf[DenseIntMatrix2D]
    val AElems = this.elements
    val BElems = BB.elements
    val CElems = CC.elements
    if (AElems == null || BElems == null || CElems == null) throw new InternalError()
    val cA = this.columnStride
    val cB = BB.columnStride
    val cC = CC.columnStride
    val rA = this.rowStride
    val rB = BB.rowStride
    val rC = CC.rowStride
    val BLOCK_SIZE = 30000
    var m_optimal = (BLOCK_SIZE - columnsA) / (columnsA + 1)
    if (m_optimal <= 0) m_optimal = 1
    var blocks = rowsA / m_optimal
    var rr = 0
    if (rowsA % m_optimal != 0) blocks += 1
    while (blocks >= 0) {
      var jB = BB.index(0, 0).toInt
      val indexA = index(rr, 0).toInt
      var jC = CC.index(rr, 0).toInt
      rr += m_optimal
      if (blocks == 0) m_optimal += rowsA - rr
      var j = p
      while (j >= 0) {
        var iA = indexA
        var iC = jC
        var i = m_optimal
        while (i >= 0) {
          var kA = iA
          var kB = jB
          var s = 0
          kA -= cA
          kB -= rB
          var k = columnsA % 4
          while (k >= 0) {
            s += AElems(kA += cA) * BElems(kB += rB)
          }
          var k = columnsA / 4
          while (k >= 0) {
            s += AElems(kA += cA) * BElems(kB += rB) + AElems(kA += cA) * BElems(kB += rB) +
              AElems(kA += cA) * BElems(kB += rB) +
              AElems(kA += cA) * BElems(kB += rB)
          }
          CElems(iC) = alpha * s + beta * CElems(iC)
          iA += rA
          iC += rC
        }
        jB += cB
        jC += cC
      }
    }
    C
  }

  protected def haveSharedCellsRaw(other: IntMatrix2D): Boolean = {
    if (other.isInstanceOf[SelectedDenseIntMatrix2D]) {
      val otherMatrix = other.asInstanceOf[SelectedDenseIntMatrix2D]
      return this.elements == otherMatrix.elements
    } else if (other.isInstanceOf[DenseIntMatrix2D]) {
      val otherMatrix = other.asInstanceOf[DenseIntMatrix2D]
      return this.elements == otherMatrix.elements
    }
    false
  }

  protected def like1D(size: Int, zero: Int, stride: Int): IntMatrix1D = {
    new DenseIntMatrix1D(size, this.elements, zero, stride, true)
  }

  protected def viewSelectionLike(rowOffsets: Array[Int], columnOffsets: Array[Int]): IntMatrix2D = {
    new SelectedDenseIntMatrix2D(this.elements, rowOffsets, columnOffsets, 0)
  }
}
