package cern.colt.matrix.tint

import java.util.concurrent.Callable
import java.util.concurrent.ExecutionException
import java.util.concurrent.Future
import cern.colt.list.tint.IntArrayList
import cern.colt.matrix.AbstractMatrix3D
import edu.emory.mathcs.utils.ConcurrencyUtils
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Abstract base class for 3-d matrices holding <tt>int</tt> elements. First see
 * the <a href="package-summary.html">package summary</a> and javadoc <a
 * href="package-tree.html">tree view</a> to get the broad picture.
 * <p>
 * A matrix has a number of slices, rows and columns, which are assigned upon
 * instance construction - The matrix's size is then
 * <tt>slices()*rows()*columns()</tt>. Elements are accessed via
 * <tt>[slice,row,column]</tt> coordinates. Legal coordinates range from
 * <tt>[0,0,0]</tt> to <tt>[slices()-1,rows()-1,columns()-1]</tt>. Any attempt
 * to access an element at a coordinate
 * <tt>slice&lt;0 || slice&gt;=slices() || row&lt;0 || row&gt;=rows() || column&lt;0 || column&gt;=column()</tt>
 * will throw an <tt>IndexOutOfBoundsException</tt>.
 * <p>
 * <b>Note</b> that this implementation is not synchronized.
 *
 * @author wolfgang.hoschek@cern.ch
 * @version 1.0, 09/24/99
 *
 * @author Piotr Wendykier (piotr.wendykier@gmail.com)
 *
 */
@SerialVersionUID(1L)
abstract class IntMatrix3D protected () extends AbstractMatrix3D {

  /**
   * Applies a function to each cell and aggregates the results. Returns a
   * value <tt>v</tt> such that <tt>v==a(size())</tt> where
   * <tt>a(i) == aggr( a(i-1), f(get(slice,row,column)) )</tt> and terminators
   * are <tt>a(1) == f(get(0,0,0)), a(0)==Int.NaN</tt>.
   * <p>
   * <b>Example:</b>
   *
   * <pre>
   *   cern.jet.math.Functions F = cern.jet.math.Functions.functions;
   *   2 x 2 x 2 matrix
   *   0 1
   *   2 3
   *
   *   4 5
   *   6 7
   *
   *   // Sum( x[slice,row,col]*x[slice,row,col] )
   *   matrix.aggregate(F.plus,F.square);
   *   --&gt; 140
   *
   * </pre>
   *
   * For further examples, see the <a
   * href="package-summary.html#FunctionObjects">package doc</a>.
   *
   * @param aggr
   *            an aggregation function taking as first argument the current
   *            aggregation and as second argument the transformed current
   *            cell value.
   * @param f
   *            a function transforming the current cell value.
   * @return the aggregated measure.
   * @see cern.jet.math.tint.IntFunctions
   */
  def aggregate(aggr: cern.colt.function.tint.IntIntFunction, f: cern.colt.function.tint.IntFunction): Int = {
    if (size == 0) throw new IllegalArgumentException("size == 0")
    var a = 0
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
            var a = f.apply(getQuick(firstSlice, 0, 0))
            var d = 1
            for (s <- firstSlice until lastSlice; r <- 0 until rows) {
              for (c <- d until columns) {
                a = aggr.apply(a, f.apply(getQuick(s, r, c)))
              }
              d = 0
            }
            return a
          }
        })
      }
      a = ConcurrencyUtils.waitForCompletion(futures, aggr)
    } else {
      a = f.apply(getQuick(0, 0, 0))
      var d = 1
      for (s <- 0 until slices; r <- 0 until rows) {
        for (c <- d until columns) {
          a = aggr.apply(a, f.apply(getQuick(s, r, c)))
        }
        d = 0
      }
    }
    a
  }

  /**
   * Applies a function to each cell that satisfies a condition and aggregates
   * the results.
   *
   * @param aggr
   *            an aggregation function taking as first argument the current
   *            aggregation and as second argument the transformed current
   *            cell value.
   * @param f
   *            a function transforming the current cell value.
   * @param cond
   *            a condition.
   * @return the aggregated measure.
   * @see cern.jet.math.tint.IntFunctions
   */
  def aggregate(aggr: cern.colt.function.tint.IntIntFunction, f: cern.colt.function.tint.IntFunction, cond: cern.colt.function.tint.IntProcedure): Int = {
    if (size == 0) throw new IllegalArgumentException("size == 0")
    var a = 0
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
            var elem = getQuick(firstSlice, 0, 0)
            var a = 0
            if (cond.apply(elem) == true) {
              a = aggr.apply(a, f.apply(elem))
            }
            var d = 1
            for (s <- firstSlice until lastSlice; r <- 0 until rows; c <- d until columns) {
              elem = getQuick(s, r, c)
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
      var elem = getQuick(0, 0, 0)
      if (cond.apply(elem) == true) {
        a = aggr.apply(a, f.apply(elem))
      }
      var d = 1
      for (s <- 0 until slices; r <- 0 until rows; c <- d until columns) {
        elem = getQuick(s, r, c)
        if (cond.apply(elem) == true) {
          a = aggr.apply(a, f.apply(elem))
        }
        d = 0
      }
    }
    a
  }

  /**
   * Applies a function to all cells with a given indexes and aggregates the
   * results.
   *
   * @param aggr
   *            an aggregation function taking as first argument the current
   *            aggregation and as second argument the transformed current
   *            cell value.
   * @param f
   *            a function transforming the current cell value.
   * @param sliceList
   *            slice indexes.
   * @param rowList
   *            row indexes.
   * @param columnList
   *            column indexes.
   * @return the aggregated measure.
   * @see cern.jet.math.tint.IntFunctions
   */
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
            var a = f.apply(getQuick(sliceElements(firstIdx), rowElements(firstIdx), columnElements(firstIdx)))
            var elem: Int = 0
            for (i <- firstIdx + 1 until lastIdx) {
              elem = getQuick(sliceElements(i), rowElements(i), columnElements(i))
              a = aggr.apply(a, f.apply(elem))
            }
            return a
          }
        })
      }
      a = ConcurrencyUtils.waitForCompletion(futures, aggr)
    } else {
      a = f.apply(getQuick(sliceElements(0), rowElements(0), columnElements(0)))
      var elem: Int = 0
      for (i <- 1 until size) {
        elem = getQuick(sliceElements(i), rowElements(i), columnElements(i))
        a = aggr.apply(a, f.apply(elem))
      }
    }
    a
  }

  /**
   * Applies a function to each corresponding cell of two matrices and
   * aggregates the results. Returns a value <tt>v</tt> such that
   * <tt>v==a(size())</tt> where
   * <tt>a(i) == aggr( a(i-1), f(get(slice,row,column),other.get(slice,row,column)) )</tt>
   * and terminators are
   * <tt>a(1) == f(get(0,0,0),other.get(0,0,0)), a(0)==Int.NaN</tt>.
   * <p>
   * <b>Example:</b>
   *
   * <pre>
   *   cern.jet.math.Functions F = cern.jet.math.Functions.functions;
   *   x = 2 x 2 x 2 matrix
   *   0 1
   *   2 3
   *
   *   4 5
   *   6 7
   *
   *   y = 2 x 2 x 2 matrix
   *   0 1
   *   2 3
   *
   *   4 5
   *   6 7
   *
   *   // Sum( x[slice,row,col] * y[slice,row,col] )
   *   x.aggregate(y, F.plus, F.mult);
   *   --&gt; 140
   *
   *   // Sum( (x[slice,row,col] + y[slice,row,col])&circ;2 )
   *   x.aggregate(y, F.plus, F.chain(F.square,F.plus));
   *   --&gt; 560
   *
   * </pre>
   *
   * For further examples, see the <a
   * href="package-summary.html#FunctionObjects">package doc</a>.
   *
   * @param aggr
   *            an aggregation function taking as first argument the current
   *            aggregation and as second argument the transformed current
   *            cell values.
   * @param f
   *            a function transforming the current cell values.
   * @return the aggregated measure.
   * @throws IllegalArgumentException
   *             if
   *             <tt>slices() != other.slices() || rows() != other.rows() || columns() != other.columns()</tt>
   * @see cern.jet.math.tint.IntFunctions
   */
  def aggregate(other: IntMatrix3D, aggr: cern.colt.function.tint.IntIntFunction, f: cern.colt.function.tint.IntIntFunction): Int = {
    checkShape(other)
    if (size == 0) throw new IllegalArgumentException("size == 0")
    var a = 0
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
            var a = f.apply(getQuick(firstSlice, 0, 0), other.getQuick(firstSlice, 0, 0))
            var d = 1
            for (s <- firstSlice until lastSlice; r <- 0 until rows) {
              for (c <- d until columns) {
                a = aggr.apply(a, f.apply(getQuick(s, r, c), other.getQuick(s, r, c)))
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
      for (s <- 0 until slices; r <- 0 until rows) {
        for (c <- d until columns) {
          a = aggr.apply(a, f.apply(getQuick(s, r, c), other.getQuick(s, r, c)))
        }
        d = 0
      }
    }
    a
  }

  /**
   * Assigns the result of a function to each cell;
   * <tt>x[slice,row,col] = function(x[slice,row,col])</tt>.
   * <p>
   * <b>Example:</b>
   *
   * <pre>
   *   matrix = 1 x 2 x 2 matrix
   *   0.5 1.5
   *   2.5 3.5
   *
   *   // change each cell to its sine
   *   matrix.assign(cern.jet.math.Functions.sin);
   *   --&gt;
   *   1 x 2 x 2 matrix
   *   0.479426  0.997495
   *   0.598472 -0.350783
   *
   * </pre>
   *
   * For further examples, see the <a
   * href="package-summary.html#FunctionObjects">package doc</a>.
   *
   * @param function
   *            a function object taking as argument the current cell's value.
   * @return <tt>this</tt> (for convenience only).
   * @see cern.jet.math.tint.IntFunctions
   */
  def assign(function: cern.colt.function.tint.IntFunction): IntMatrix3D = {
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
            for (s <- firstSlice until lastSlice; r <- 0 until rows; c <- 0 until columns) {
              setQuick(s, r, c, function.apply(getQuick(s, r, c)))
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
    } else {
      for (s <- 0 until slices; r <- 0 until rows; c <- 0 until columns) {
        setQuick(s, r, c, function.apply(getQuick(s, r, c)))
      }
    }
    this
  }

  /**
   * Sets all cells to the state specified by <tt>value</tt>.
   *
   * @param value
   *            the value to be filled into the cells.
   * @return <tt>this</tt> (for convenience only).
   */
  def assign(value: Int): IntMatrix3D = {
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
            for (s <- firstSlice until lastSlice; r <- 0 until rows; c <- 0 until columns) {
              setQuick(s, r, c, value)
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
    } else {
      for (s <- 0 until slices; r <- 0 until rows; c <- 0 until columns) {
        setQuick(s, r, c, value)
      }
    }
    this
  }

  /**
   * Sets all cells to the state specified by <tt>values</tt>. <tt>values</tt>
   * is required to have the form <tt>values[slice*row*column]</tt>.
   * <p>
   * The values are copied. So subsequent changes in <tt>values</tt> are not
   * reflected in the matrix, and vice-versa.
   *
   * @param values
   *            the values to be filled into the cells.
   * @return <tt>this</tt> (for convenience only).
   * @throws IllegalArgumentException
   *             if <tt>values.length != slices()*rows()*columns()</tt>
   */
  def assign(values: Array[Int]): IntMatrix3D = {
    if (values.length != slices * rows * columns) throw new IllegalArgumentException("Must have same length: length=" + values.length + "slices()*rows()*columns()=" +
      slices() * rows() * columns())
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
            var idx = firstSlice * rows * columns
            for (s <- firstSlice until lastSlice; r <- 0 until rows; c <- 0 until columns) {
              setQuick(s, r, c, values(idx += 1))
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
    } else {
      var idx = 0
      for (s <- 0 until slices; r <- 0 until rows; c <- 0 until columns) {
        setQuick(s, r, c, values(idx += 1))
      }
    }
    this
  }

  /**
   * Sets all cells to the state specified by <tt>values</tt>. <tt>values</tt>
   * is required to have the form <tt>values[slice][row][column]</tt> and have
   * exactly the same number of slices, rows and columns as the receiver.
   * <p>
   * The values are copied. So subsequent changes in <tt>values</tt> are not
   * reflected in the matrix, and vice-versa.
   *
   * @param values
   *            the values to be filled into the cells.
   * @return <tt>this</tt> (for convenience only).
   * @throws IllegalArgumentException
   *             if
   *             <tt>values.length != slices() || for any 0 &lt;= slice &lt; slices(): values[slice].length != rows()</tt>
   *             .
   * @throws IllegalArgumentException
   *             if
   *             <tt>for any 0 &lt;= column &lt; columns(): values[slice][row].length != columns()</tt>
   *             .
   */
  def assign(values: Array[Array[Array[Int]]]): IntMatrix3D = {
    if (values.length != slices) throw new IllegalArgumentException("Must have same number of slices: slices=" + values.length +
      "slices()=" +
      slices())
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
                for (c <- 0 until columns) {
                  setQuick(s, r, c, currentRow(c))
                }
              }
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
    } else {
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
          for (c <- 0 until columns) {
            setQuick(s, r, c, currentRow(c))
          }
        }
      }
    }
    this
  }

  /**
   * Assigns the result of a function to all cells that satisfy a condition.
   *
   * @param cond
   *            a condition.
   *
   * @param f
   *            a function object.
   * @return <tt>this</tt> (for convenience only).
   * @see cern.jet.math.tint.IntFunctions
   */
  def assign(cond: cern.colt.function.tint.IntProcedure, f: cern.colt.function.tint.IntFunction): IntMatrix3D = {
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
            for (s <- firstSlice until lastSlice; r <- 0 until rows; c <- 0 until columns) {
              elem = getQuick(s, r, c)
              if (cond.apply(elem) == true) {
                setQuick(s, r, c, f.apply(elem))
              }
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
    } else {
      var elem: Int = 0
      for (s <- 0 until slices; r <- 0 until rows; c <- 0 until columns) {
        elem = getQuick(s, r, c)
        if (cond.apply(elem) == true) {
          setQuick(s, r, c, f.apply(elem))
        }
      }
    }
    this
  }

  /**
   * Assigns a value to all cells that satisfy a condition.
   *
   * @param cond
   *            a condition.
   *
   * @param value
   *            a value.
   * @return <tt>this</tt> (for convenience only).
   *
   */
  def assign(cond: cern.colt.function.tint.IntProcedure, value: Int): IntMatrix3D = {
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
            for (s <- firstSlice until lastSlice; r <- 0 until rows; c <- 0 until columns) {
              elem = getQuick(s, r, c)
              if (cond.apply(elem) == true) {
                setQuick(s, r, c, value)
              }
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
    } else {
      var elem: Int = 0
      for (s <- 0 until slices; r <- 0 until rows; c <- 0 until columns) {
        elem = getQuick(s, r, c)
        if (cond.apply(elem) == true) {
          setQuick(s, r, c, value)
        }
      }
    }
    this
  }

  /**
   * Replaces all cell values of the receiver with the values of another
   * matrix. Both matrices must have the same number of slices, rows and
   * columns. If both matrices share the same cells (as is the case if they
   * are views derived from the same matrix) and intersect in an ambiguous
   * way, then replaces <i>as if</i> using an intermediate auxiliary deep copy
   * of <tt>other</tt>.
   *
   * @param other
   *            the source matrix to copy from (may be identical to the
   *            receiver).
   * @return <tt>this</tt> (for convenience only).
   * @throws IllegalArgumentException
   *             if
   *             <tt>slices() != other.slices() || rows() != other.rows() || columns() != other.columns()</tt>
   */
  def assign(other: IntMatrix3D): IntMatrix3D = {
    if (other == this) return this
    checkShape(other)
    var otherLoc: IntMatrix3D = null
    otherLoc = if (haveSharedCells(other)) other.copy() else other
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
            for (s <- firstSlice until lastSlice; r <- 0 until rows; c <- 0 until columns) {
              setQuick(s, r, c, otherLoc.getQuick(s, r, c))
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
    } else {
      for (s <- 0 until slices; r <- 0 until rows; c <- 0 until columns) {
        setQuick(s, r, c, otherLoc.getQuick(s, r, c))
      }
    }
    this
  }

  /**
   * Assigns the result of a function to each cell;
   * <tt>x[row,col] = function(x[row,col],y[row,col])</tt>.
   * <p>
   * <b>Example:</b>
   *
   * <pre>
   *   // assign x[row,col] = x[row,col]&lt;sup&gt;y[row,col]&lt;/sup&gt;
   *   m1 = 1 x 2 x 2 matrix
   *   0 1
   *   2 3
   *
   *   m2 = 1 x 2 x 2 matrix
   *   0 2
   *   4 6
   *
   *   m1.assign(m2, cern.jet.math.Functions.pow);
   *   --&gt;
   *   m1 == 1 x 2 x 2 matrix
   *   1   1
   *   16 729
   *
   * </pre>
   *
   * For further examples, see the <a
   * href="package-summary.html#FunctionObjects">package doc</a>.
   *
   * @param y
   *            the secondary matrix to operate on.
   * @param function
   *            a function object taking as first argument the current cell's
   *            value of <tt>this</tt>, and as second argument the current
   *            cell's value of <tt>y</tt>,
   * @return <tt>this</tt> (for convenience only).
   * @throws IllegalArgumentException
   *             if
   *             <tt>slices() != other.slices() || rows() != other.rows() || columns() != other.columns()</tt>
   * @see cern.jet.math.tint.IntFunctions
   */
  def assign(y: IntMatrix3D, function: cern.colt.function.tint.IntIntFunction): IntMatrix3D = {
    checkShape(y)
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
            for (s <- firstSlice until lastSlice; r <- 0 until rows; c <- 0 until columns) {
              setQuick(s, r, c, function.apply(getQuick(s, r, c), y.getQuick(s, r, c)))
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
    } else {
      for (s <- 0 until slices; r <- 0 until rows; c <- 0 until columns) {
        setQuick(s, r, c, function.apply(getQuick(s, r, c), y.getQuick(s, r, c)))
      }
    }
    this
  }

  /**
   * Assigns the result of a function to all cells with a given indexes
   *
   * @param y
   *            the secondary matrix to operate on.
   * @param function
   *            a function object taking as first argument the current cell's
   *            value of <tt>this</tt>, and as second argument the current
   *            cell's value of <tt>y</tt>, *
   * @param sliceList
   *            slice indexes.
   * @param rowList
   *            row indexes.
   * @param columnList
   *            column indexes.
   * @return <tt>this</tt> (for convenience only).
   * @throws IllegalArgumentException
   *             if
   *             <tt>slices() != other.slices() || rows() != other.rows() || columns() != other.columns()</tt>
   * @see cern.jet.math.tint.IntFunctions
   */
  def assign(y: IntMatrix3D,
      function: cern.colt.function.tint.IntIntFunction,
      sliceList: IntArrayList,
      rowList: IntArrayList,
      columnList: IntArrayList): IntMatrix3D = {
    checkShape(y)
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
              setQuick(sliceElements(i), rowElements(i), columnElements(i), function.apply(getQuick(sliceElements(i),
                rowElements(i), columnElements(i)), y.getQuick(sliceElements(i), rowElements(i), columnElements(i))))
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
    } else {
      for (i <- 0 until size) {
        setQuick(sliceElements(i), rowElements(i), columnElements(i), function.apply(getQuick(sliceElements(i),
          rowElements(i), columnElements(i)), y.getQuick(sliceElements(i), rowElements(i), columnElements(i))))
      }
    }
    this
  }

  /**
   * Returns the number of cells having non-zero values; ignores tolerance.
   *
   * @return the number of cells having non-zero values.
   */
  def cardinality(): Int = {
    var cardinality = 0
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) &&
      (slices * rows * columns >= ConcurrencyUtils.getThreadsBeginN_3D)) {
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
            for (s <- firstSlice until lastSlice; r <- 0 until rows; c <- 0 until columns if getQuick(s,
              r, c) != 0) cardinality += 1
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
      for (s <- 0 until slices; r <- 0 until rows; c <- 0 until columns if getQuick(s, r, c) != 0) cardinality += 1
    }
    cardinality
  }

  /**
   * Constructs and returns a deep copy of the receiver.
   * <p>
   * <b>Note that the returned matrix is an independent deep copy.</b> The
   * returned matrix is not backed by this matrix, so changes in the returned
   * matrix are not reflected in this matrix, and vice-versa.
   *
   * @return a deep copy of the receiver.
   */
  def copy(): IntMatrix3D = like().assign(this)

  /**
   * Returns the elements of this matrix.
   *
   * @return the elements
   */
  def elements(): AnyRef

  /**
   * Returns whether all cells are equal to the given value.
   *
   * @param value
   *            the value to test against.
   * @return <tt>true</tt> if all cells are equal to the given value,
   *         <tt>false</tt> otherwise.
   */
  override def equals(value: Int): Boolean = {
    cern.colt.matrix.tint.algo.IntProperty.DEFAULT.==(this, value)
  }

  /**
   * Compares this object against the specified object. The result is
   * <code>true</code> if and only if the argument is not <code>null</code>
   * and is at least a <code>IntMatrix3D</code> object that has the same
   * number of slices, rows and columns as the receiver and has exactly the
   * same values at the same coordinates.
   *
   * @param obj
   *            the object to compare with.
   * @return <code>true</code> if the objects are the same; <code>false</code>
   *         otherwise.
   */
  override def equals(obj: Any): Boolean = {
    if (this == obj) return true
    if (obj == null) return false
    if (!(obj.isInstanceOf[IntMatrix3D])) return false
    cern.colt.matrix.tint.algo.IntProperty.DEFAULT.==(this, obj.asInstanceOf[IntMatrix3D])
  }

  /**
   * Returns the matrix cell value at coordinate <tt>[slice,row,column]</tt>.
   *
   * @param slice
   *            the index of the slice-coordinate.
   * @param row
   *            the index of the row-coordinate.
   * @param column
   *            the index of the column-coordinate.
   * @return the value of the specified cell.
   * @throws IndexOutOfBoundsException
   *             if
   *             <tt>slice&lt;0 || slice&gt;=slices() || row&lt;0 || row&gt;=rows() || column&lt;0 || column&gt;=column()</tt>
   *             .
   */
  def get(slice: Int, row: Int, column: Int): Int = {
    if (slice < 0 || slice >= slices || row < 0 || row >= rows ||
      column < 0 ||
      column >= columns) throw new IndexOutOfBoundsException("slice:" + slice + ", row:" + row + ", column:" + column)
    getQuick(slice, row, column)
  }

  /**
   * Returns the content of this matrix if it is a wrapper; or <tt>this</tt>
   * otherwise. Override this method in wrappers.
   */
  protected def getStorageMatrix(): IntMatrix3D = this

  /**
   * Fills the coordinates and values of cells having negative values into the
   * specified lists. Fills into the lists, starting at index 0. After this
   * call returns the specified lists all have a new size, the number of
   * non-zero values.
   *
   * @param sliceList
   *            the list to be filled with slice indexes, can have any size.
   * @param rowList
   *            the list to be filled with row indexes, can have any size.
   * @param columnList
   *            the list to be filled with column indexes, can have any size.
   * @param valueList
   *            the list to be filled with values, can have any size.
   */
  def getNegativeValues(sliceList: IntArrayList,
      rowList: IntArrayList,
      columnList: IntArrayList,
      valueList: IntArrayList) {
    sliceList.clear()
    rowList.clear()
    columnList.clear()
    valueList.clear()
    for (s <- 0 until slices; r <- 0 until rows; c <- 0 until columns) {
      val value = getQuick(s, r, c)
      if (value < 0) {
        sliceList.add(s)
        rowList.add(r)
        columnList.add(c)
        valueList.add(value)
      }
    }
  }

  /**
   * Fills the coordinates and values of cells having non-zero values into the
   * specified lists. Fills into the lists, starting at index 0. After this
   * call returns the specified lists all have a new size, the number of
   * non-zero values.
   * <p>
   * In general, fill order is <i>unspecified</i>. This implementation fill
   * like:
   * <tt>for (slice = 0..slices-1) for (row = 0..rows-1) for (column = 0..colums-1) do ... </tt>
   * . However, subclasses are free to us any other order, even an order that
   * may change over time as cell values are changed. (Of course, result lists
   * indexes are guaranteed to correspond to the same cell). For an example,
   * see
   * {@link IntMatrix3D#getNonZeros(IntArrayList,IntArrayList,IntArrayList,IntArrayList)}.
   *
   * @param sliceList
   *            the list to be filled with slice indexes, can have any size.
   * @param rowList
   *            the list to be filled with row indexes, can have any size.
   * @param columnList
   *            the list to be filled with column indexes, can have any size.
   * @param valueList
   *            the list to be filled with values, can have any size.
   */
  def getNonZeros(sliceList: IntArrayList,
      rowList: IntArrayList,
      columnList: IntArrayList,
      valueList: IntArrayList) {
    sliceList.clear()
    rowList.clear()
    columnList.clear()
    valueList.clear()
    for (s <- 0 until slices; r <- 0 until rows; c <- 0 until columns) {
      val value = getQuick(s, r, c)
      if (value != 0) {
        sliceList.add(s)
        rowList.add(r)
        columnList.add(c)
        valueList.add(value)
      }
    }
  }

  /**
   * Fills the coordinates and values of cells having positive values into the
   * specified lists. Fills into the lists, starting at index 0. After this
   * call returns the specified lists all have a new size, the number of
   * non-zero values.
   *
   * @param sliceList
   *            the list to be filled with slice indexes, can have any size.
   * @param rowList
   *            the list to be filled with row indexes, can have any size.
   * @param columnList
   *            the list to be filled with column indexes, can have any size.
   * @param valueList
   *            the list to be filled with values, can have any size.
   */
  def getPositiveValues(sliceList: IntArrayList,
      rowList: IntArrayList,
      columnList: IntArrayList,
      valueList: IntArrayList) {
    sliceList.clear()
    rowList.clear()
    columnList.clear()
    valueList.clear()
    for (s <- 0 until slices; r <- 0 until rows; c <- 0 until columns) {
      val value = getQuick(s, r, c)
      if (value > 0) {
        sliceList.add(s)
        rowList.add(r)
        columnList.add(c)
        valueList.add(value)
      }
    }
  }

  /**
   * Returns the matrix cell value at coordinate <tt>[slice,row,column]</tt>.
   *
   * <p>
   * Provided with invalid parameters this method may return invalid objects
   * without throwing any exception. <b>You should only use this method when
   * you are absolutely sure that the coordinate is within bounds.</b>
   * Precondition (unchecked):
   * <tt>slice&lt;0 || slice&gt;=slices() || row&lt;0 || row&gt;=rows() || column&lt;0 || column&gt;=column()</tt>.
   *
   * @param slice
   *            the index of the slice-coordinate.
   * @param row
   *            the index of the row-coordinate.
   * @param column
   *            the index of the column-coordinate.
   * @return the value at the specified coordinate.
   */
  def getQuick(slice: Int, row: Int, column: Int): Int

  /**
   * Returns <tt>true</tt> if both matrices share at least one identical cell.
   */
  protected def haveSharedCells(other: IntMatrix3D): Boolean = {
    if (other == null) return false
    if (this == other) return true
    getStorageMatrix.haveSharedCellsRaw(other.getStorageMatrix)
  }

  /**
   * Returns <tt>true</tt> if both matrices share at least one identical cell.
   */
  protected def haveSharedCellsRaw(other: IntMatrix3D): Boolean = false

  /**
   * Construct and returns a new empty matrix <i>of the same dynamic type</i>
   * as the receiver, having the same number of slices, rows and columns. For
   * example, if the receiver is an instance of type <tt>DenseIntMatrix3D</tt>
   * the new matrix must also be of type <tt>DenseIntMatrix3D</tt>, if the
   * receiver is an instance of type <tt>SparseIntMatrix3D</tt> the new matrix
   * must also be of type <tt>SparseIntMatrix3D</tt>, etc. In general, the new
   * matrix should have internal parametrization as similar as possible.
   *
   * @return a new empty matrix of the same dynamic type.
   */
  def like(): IntMatrix3D = like(slices, rows, columns)

  /**
   * Construct and returns a new empty matrix <i>of the same dynamic type</i>
   * as the receiver, having the specified number of slices, rows and columns.
   * For example, if the receiver is an instance of type
   * <tt>DenseIntMatrix3D</tt> the new matrix must also be of type
   * <tt>DenseIntMatrix3D</tt>, if the receiver is an instance of type
   * <tt>SparseIntMatrix3D</tt> the new matrix must also be of type
   * <tt>SparseIntMatrix3D</tt>, etc. In general, the new matrix should have
   * internal parametrization as similar as possible.
   *
   * @param slices
   *            the number of slices the matrix shall have.
   * @param rows
   *            the number of rows the matrix shall have.
   * @param columns
   *            the number of columns the matrix shall have.
   * @return a new empty matrix of the same dynamic type.
   */
  def like(slices: Int, rows: Int, columns: Int): IntMatrix3D

  /**
   * Construct and returns a new 2-d matrix <i>of the corresponding dynamic
   * type</i>, sharing the same cells. For example, if the receiver is an
   * instance of type <tt>DenseDoubleMatrix3D</tt> the new matrix must also be
   * of type <tt>DenseDoubleMatrix2D</tt>, if the receiver is an instance of
   * type <tt>SparseDoubleMatrix3D</tt> the new matrix must also be of type
   * <tt>SparseDoubleMatrix2D</tt>, etc.
   *
   * @param rows
   *            the number of rows the matrix shall have.
   * @param columns
   *            the number of columns the matrix shall have.
   * @return a new matrix of the corresponding dynamic type.
   */
  def like2D(rows: Int, columns: Int): IntMatrix2D

  /**
   * Construct and returns a new 2-d matrix <i>of the corresponding dynamic
   * type</i>, sharing the same cells. For example, if the receiver is an
   * instance of type <tt>DenseIntMatrix3D</tt> the new matrix must also be of
   * type <tt>DenseIntMatrix2D</tt>, if the receiver is an instance of type
   * <tt>SparseIntMatrix3D</tt> the new matrix must also be of type
   * <tt>SparseIntMatrix2D</tt>, etc.
   *
   * @param rows
   *            the number of rows the matrix shall have.
   * @param columns
   *            the number of columns the matrix shall have.
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
   * @return a new matrix of the corresponding dynamic type.
   */
  protected def like2D(rows: Int,
      columns: Int,
      rowZero: Int,
      columnZero: Int,
      rowStride: Int,
      columnStride: Int): IntMatrix2D

  /**
   * Return maximum value of this matrix together with its location
   *
   * @return { maximum_value, slice_location, row_location, column_location };
   */
  def getMaxLocation(): Array[Int] = {
    var sliceLocation = 0
    var rowLocation = 0
    var columnLocation = 0
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
            var sliceLocation = firstSlice
            var rowLocation = 0
            var columnLocation = 0
            var maxValue = getQuick(sliceLocation, 0, 0)
            var d = 1
            var elem: Int = 0
            for (s <- firstSlice until lastSlice; r <- 0 until rows) {
              for (c <- d until columns) {
                elem = getQuick(s, r, c)
                if (maxValue < elem) {
                  maxValue = elem
                  sliceLocation = s
                  rowLocation = r
                  columnLocation = c
                }
              }
              d = 0
            }
            return Array(maxValue, sliceLocation, rowLocation, columnLocation)
          }
        })
      }
      try {
        for (j <- 0 until nthreads) {
          results(j) = futures(j).get.asInstanceOf[Array[Int]]
        }
        maxValue = results(0)(0)
        sliceLocation = results(0)(1).toInt
        rowLocation = results(0)(2).toInt
        columnLocation = results(0)(3).toInt
        for (j <- 1 until nthreads if maxValue < results(j)(0)) {
          maxValue = results(j)(0)
          sliceLocation = results(j)(1).toInt
          rowLocation = results(j)(2).toInt
          columnLocation = results(j)(3).toInt
        }
      } catch {
        case ex: ExecutionException => ex.printStackTrace()
        case e: InterruptedException => e.printStackTrace()
      }
    } else {
      maxValue = getQuick(0, 0, 0)
      var elem: Int = 0
      var d = 1
      for (s <- 0 until slices; r <- 0 until rows) {
        for (c <- d until columns) {
          elem = getQuick(s, r, c)
          if (maxValue < elem) {
            maxValue = elem
            sliceLocation = s
            rowLocation = r
            columnLocation = c
          }
        }
        d = 0
      }
    }
    Array(maxValue, sliceLocation, rowLocation, columnLocation)
  }

  /**
   * Returns minimum value of this matrix together with its location
   *
   * @return { minimum_value, slice_location, row_location, column_location };
   */
  def getMinLocation(): Array[Int] = {
    var sliceLocation = 0
    var rowLocation = 0
    var columnLocation = 0
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
            var sliceLocation = firstSlice
            var rowLocation = 0
            var columnLocation = 0
            var minValue = getQuick(sliceLocation, 0, 0)
            var d = 1
            var elem: Int = 0
            for (s <- firstSlice until lastSlice; r <- 0 until rows) {
              for (c <- d until columns) {
                elem = getQuick(s, r, c)
                if (minValue > elem) {
                  minValue = elem
                  sliceLocation = s
                  rowLocation = r
                  columnLocation = c
                }
              }
              d = 0
            }
            return Array(minValue, sliceLocation, rowLocation, columnLocation)
          }
        })
      }
      try {
        for (j <- 0 until nthreads) {
          results(j) = futures(j).get.asInstanceOf[Array[Int]]
        }
        minValue = results(0)(0)
        sliceLocation = results(0)(1).toInt
        rowLocation = results(0)(2).toInt
        columnLocation = results(0)(3).toInt
        for (j <- 1 until nthreads if minValue > results(j)(0)) {
          minValue = results(j)(0)
          sliceLocation = results(j)(1).toInt
          rowLocation = results(j)(2).toInt
          columnLocation = results(j)(3).toInt
        }
      } catch {
        case ex: ExecutionException => ex.printStackTrace()
        case e: InterruptedException => e.printStackTrace()
      }
    } else {
      minValue = getQuick(0, 0, 0)
      var elem: Int = 0
      var d = 1
      for (s <- 0 until slices; r <- 0 until rows) {
        for (c <- d until columns) {
          elem = getQuick(s, r, c)
          if (minValue > elem) {
            minValue = elem
            sliceLocation = s
            rowLocation = r
            columnLocation = c
          }
        }
        d = 0
      }
    }
    Array(minValue, sliceLocation, rowLocation, columnLocation)
  }

  /**
   * Sets the matrix cell at coordinate <tt>[slice,row,column]</tt> to the
   * specified value.
   *
   * @param slice
   *            the index of the slice-coordinate.
   * @param row
   *            the index of the row-coordinate.
   * @param column
   *            the index of the column-coordinate.
   * @param value
   *            the value to be filled into the specified cell.
   * @throws IndexOutOfBoundsException
   *             if
   *             <tt>row&lt;0 || row&gt;=rows() || slice&lt;0 || slice&gt;=slices() || column&lt;0 || column&gt;=column()</tt>
   *             .
   */
  def set(slice: Int,
      row: Int,
      column: Int,
      value: Int) {
    if (slice < 0 || slice >= slices || row < 0 || row >= rows ||
      column < 0 ||
      column >= columns) throw new IndexOutOfBoundsException("slice:" + slice + ", row:" + row + ", column:" + column)
    setQuick(slice, row, column, value)
  }

  /**
   * Sets the matrix cell at coordinate <tt>[slice,row,column]</tt> to the
   * specified value.
   *
   * <p>
   * Provided with invalid parameters this method may access illegal indexes
   * without throwing any exception. <b>You should only use this method when
   * you are absolutely sure that the coordinate is within bounds.</b>
   * Precondition (unchecked):
   * <tt>slice&lt;0 || slice&gt;=slices() || row&lt;0 || row&gt;=rows() || column&lt;0 || column&gt;=column()</tt>.
   *
   * @param slice
   *            the index of the slice-coordinate.
   * @param row
   *            the index of the row-coordinate.
   * @param column
   *            the index of the column-coordinate.
   * @param value
   *            the value to be filled into the specified cell.
   */
  def setQuick(slice: Int,
      row: Int,
      column: Int,
      value: Int): Unit

  /**
   * Constructs and returns a 2-dimensional array containing the cell values.
   * The returned array <tt>values</tt> has the form
   * <tt>values[slice][row][column]</tt> and has the same number of slices,
   * rows and columns as the receiver.
   * <p>
   * The values are copied. So subsequent changes in <tt>values</tt> are not
   * reflected in the matrix, and vice-versa.
   *
   * @return an array filled with the values of the cells.
   */
  def toArray(): Array[Array[Array[Int]]] = {
    val values = Array.ofDim[Int](slices, rows, columns)
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
              var currentSlice = values(s)
              for (r <- 0 until rows) {
                var currentRow = currentSlice(r)
                for (c <- 0 until columns) {
                  currentRow(c) = getQuick(s, r, c)
                }
              }
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
    } else {
      for (s <- 0 until slices) {
        val currentSlice = values(s)
        for (r <- 0 until rows) {
          val currentRow = currentSlice(r)
          for (c <- 0 until columns) {
            currentRow(c) = getQuick(s, r, c)
          }
        }
      }
    }
    values
  }

  /**
   * Returns a string representation using default formatting.
   *
   * @see cern.colt.matrix.tint.algo.IntFormatter
   */
  override def toString(): String = {
    new cern.colt.matrix.tint.algo.IntFormatter() toString this
  }

  /**
   * Returns a vector obtained by stacking the columns of each slice of the
   * matrix on top of one another.
   *
   * @return a vector obtained by stacking the columns of each slice of the
   *         matrix on top of one another.
   */
  def vectorize(): IntMatrix1D

  /**
   * Constructs and returns a new view equal to the receiver. The view is a
   * shallow clone. Calls <code>clone()</code> and casts the result.
   * <p>
   * <b>Note that the view is not a deep copy.</b> The returned matrix is
   * backed by this matrix, so changes in the returned matrix are reflected in
   * this matrix, and vice-versa.
   * <p>
   * Use {@link #copy()} if you want to construct an independent deep copy
   * rather than a new view.
   *
   * @return a new view of the receiver.
   */
  protected def view(): IntMatrix3D = clone().asInstanceOf[IntMatrix3D]

  /**
   * Constructs and returns a new 2-dimensional <i>slice view</i> representing
   * the slices and rows of the given column. The returned view is backed by
   * this matrix, so changes in the returned view are reflected in this
   * matrix, and vice-versa.
   * <p>
   * To obtain a slice view on subranges, construct a sub-ranging view (
   * <tt>view().part(...)</tt>), then apply this method to the sub-range view.
   * To obtain 1-dimensional views, apply this method, then apply another
   * slice view (methods <tt>viewColumn</tt>, <tt>viewRow</tt>) on the
   * intermediate 2-dimensional view. To obtain 1-dimensional views on
   * subranges, apply both steps.
   *
   * @param column
   *            the index of the column to fix.
   * @return a new 2-dimensional slice view.
   * @throws IndexOutOfBoundsException
   *             if <tt>column < 0 || column >= columns()</tt>.
   * @see #viewSlice(int)
   * @see #viewRow(int)
   */
  def viewColumn(column: Int): IntMatrix2D = {
    checkColumn(column)
    val sliceRows = this.slices
    val sliceColumns = this.rows
    val sliceRowZero = sliceZero
    val sliceColumnZero = rowZero + _columnOffset(_columnRank(column))
    val sliceRowStride = this.sliceStride
    val sliceColumnStride = this.rowStride
    like2D(sliceRows, sliceColumns, sliceRowZero, sliceColumnZero, sliceRowStride, sliceColumnStride)
  }

  /**
   * Constructs and returns a new <i>flip view</i> aint the column axis. What
   * used to be column <tt>0</tt> is now column <tt>columns()-1</tt>, ...,
   * what used to be column <tt>columns()-1</tt> is now column <tt>0</tt>. The
   * returned view is backed by this matrix, so changes in the returned view
   * are reflected in this matrix, and vice-versa.
   *
   * @return a new flip view.
   * @see #viewSliceFlip()
   * @see #viewRowFlip()
   */
  def viewColumnFlip(): IntMatrix3D = {
    (view().vColumnFlip()).asInstanceOf[IntMatrix3D]
  }

  /**
   * Constructs and returns a new <i>dice view</i>; Swaps dimensions (axes);
   * Example: 3 x 4 x 5 matrix --> 4 x 3 x 5 matrix. The view has dimensions
   * exchanged; what used to be one axis is now another, in all desired
   * permutations. The returned view is backed by this matrix, so changes in
   * the returned view are reflected in this matrix, and vice-versa.
   *
   * @param axis0
   *            the axis that shall become axis 0 (legal values 0..2).
   * @param axis1
   *            the axis that shall become axis 1 (legal values 0..2).
   * @param axis2
   *            the axis that shall become axis 2 (legal values 0..2).
   * @return a new dice view.
   * @throws IllegalArgumentException
   *             if some of the parameters are equal or not in range 0..2.
   */
  def viewDice(axis0: Int, axis1: Int, axis2: Int): IntMatrix3D = {
    (view().vDice(axis0, axis1, axis2)).asInstanceOf[IntMatrix3D]
  }

  /**
   * Constructs and returns a new <i>sub-range view</i> that is a
   * <tt>depth x height x width</tt> sub matrix starting at
   * <tt>[slice,row,column]</tt>; Equivalent to
   * <tt>view().part(slice,row,column,depth,height,width)</tt>; Provided for
   * convenience only. The returned view is backed by this matrix, so changes
   * in the returned view are reflected in this matrix, and vice-versa.
   *
   * @param slice
   *            The index of the slice-coordinate.
   * @param row
   *            The index of the row-coordinate.
   * @param column
   *            The index of the column-coordinate.
   * @param depth
   *            The depth of the box.
   * @param height
   *            The height of the box.
   * @param width
   *            The width of the box.
   * @throws IndexOutOfBoundsException
   *             if
   *
   *             <tt>slice<0 || depth<0 || slice+depth>slices() || row<0 || height<0 || row+height>rows() || column<0 || width<0 || column+width>columns()</tt>
   * @return the new view.
   *
   */
  def viewPart(slice: Int,
      row: Int,
      column: Int,
      depth: Int,
      height: Int,
      width: Int): IntMatrix3D = {
    (view().vPart(slice, row, column, depth, height, width)).asInstanceOf[IntMatrix3D]
  }

  /**
   * Constructs and returns a new 2-dimensional <i>slice view</i> representing
   * the slices and columns of the given row. The returned view is backed by
   * this matrix, so changes in the returned view are reflected in this
   * matrix, and vice-versa.
   * <p>
   * To obtain a slice view on subranges, construct a sub-ranging view (
   * <tt>view().part(...)</tt>), then apply this method to the sub-range view.
   * To obtain 1-dimensional views, apply this method, then apply another
   * slice view (methods <tt>viewColumn</tt>, <tt>viewRow</tt>) on the
   * intermediate 2-dimensional view. To obtain 1-dimensional views on
   * subranges, apply both steps.
   *
   * @param row
   *            the index of the row to fix.
   * @return a new 2-dimensional slice view.
   * @throws IndexOutOfBoundsException
   *             if <tt>row < 0 || row >= row()</tt>.
   * @see #viewSlice(int)
   * @see #viewColumn(int)
   */
  def viewRow(row: Int): IntMatrix2D = {
    checkRow(row)
    val sliceRows = this.slices
    val sliceColumns = this.columns
    val sliceRowZero = sliceZero
    val sliceColumnZero = columnZero + _rowOffset(_rowRank(row))
    val sliceRowStride = this.sliceStride
    val sliceColumnStride = this.columnStride
    like2D(sliceRows, sliceColumns, sliceRowZero, sliceColumnZero, sliceRowStride, sliceColumnStride)
  }

  /**
   * Constructs and returns a new <i>flip view</i> aint the row axis. What
   * used to be row <tt>0</tt> is now row <tt>rows()-1</tt>, ..., what used to
   * be row <tt>rows()-1</tt> is now row <tt>0</tt>. The returned view is
   * backed by this matrix, so changes in the returned view are reflected in
   * this matrix, and vice-versa.
   *
   * @return a new flip view.
   * @see #viewSliceFlip()
   * @see #viewColumnFlip()
   */
  def viewRowFlip(): IntMatrix3D = {
    (view().vRowFlip()).asInstanceOf[IntMatrix3D]
  }

  /**
   * Constructs and returns a new <i>selection view</i> that is a matrix
   * holding all <b>slices</b> matching the given condition. Applies the
   * condition to each slice and takes only those where
   * <tt>condition.apply(viewSlice(i))</tt> yields <tt>true</tt>. To match
   * rows or columns, use a dice view.
   * <p>
   * <b>Example:</b> <br>
   *
   * <pre>
   * // extract and view all slices which have an aggregate sum &gt; 1000
   * matrix.viewSelection(new IntMatrix2DProcedure() {
   *     public final boolean apply(IntMatrix2D m) {
   *         return m.zSum &gt; 1000;
   *     }
   * });
   * </pre>
   *
   * For further examples, see the <a
   * href="package-summary.html#FunctionObjects">package doc</a>. The returned
   * view is backed by this matrix, so changes in the returned view are
   * reflected in this matrix, and vice-versa.
   *
   * @param condition
   *            The condition to be matched.
   * @return the new view.
   */
  def viewSelection(condition: IntMatrix2DProcedure): IntMatrix3D = {
    val matches = new IntArrayList()
    for (i <- 0 until slices if condition.apply(viewSlice(i))) matches.add(i)
    matches.trimToSize()
    viewSelection(matches.elements(), null, null)
  }

  /**
   * Constructs and returns a new <i>selection view</i> that is a matrix
   * holding the indicated cells. There holds
   *
   * <tt>view.slices() == sliceIndexes.length, view.rows() == rowIndexes.length, view.columns() == columnIndexes.length</tt>
   * and
   * <tt>view.get(k,i,j) == this.get(sliceIndexes[k],rowIndexes[i],columnIndexes[j])</tt>
   * . Indexes can occur multiple times and can be in arbitrary order. For an
   * example see {@link IntMatrix2D#viewSelection(int[],int[])}.
   * <p>
   * Note that modifying the index arguments after this call has returned has
   * no effect on the view. The returned view is backed by this matrix, so
   * changes in the returned view are reflected in this matrix, and
   * vice-versa.
   *
   * @param sliceIndexes
   *            The slices of the cells that shall be visible in the new view.
   *            To indicate that <i>all</i> slices shall be visible, simply
   *            set this parameter to <tt>null</tt>.
   * @param rowIndexes
   *            The rows of the cells that shall be visible in the new view.
   *            To indicate that <i>all</i> rows shall be visible, simply set
   *            this parameter to <tt>null</tt>.
   * @param columnIndexes
   *            The columns of the cells that shall be visible in the new
   *            view. To indicate that <i>all</i> columns shall be visible,
   *            simply set this parameter to <tt>null</tt>.
   * @return the new view.
   * @throws IndexOutOfBoundsException
   *             if <tt>!(0 <= sliceIndexes[i] < slices())</tt> for any
   *             <tt>i=0..sliceIndexes.length()-1</tt>.
   * @throws IndexOutOfBoundsException
   *             if <tt>!(0 <= rowIndexes[i] < rows())</tt> for any
   *             <tt>i=0..rowIndexes.length()-1</tt>.
   * @throws IndexOutOfBoundsException
   *             if <tt>!(0 <= columnIndexes[i] < columns())</tt> for any
   *             <tt>i=0..columnIndexes.length()-1</tt>.
   */
  def viewSelection(sliceIndexes: Array[Int], rowIndexes: Array[Int], columnIndexes: Array[Int]): IntMatrix3D = {
    if (sliceIndexes == null) {
      sliceIndexes = Array.ofDim[Int](slices)
      for (i <- 0 until slices) sliceIndexes(i) = i
    }
    if (rowIndexes == null) {
      rowIndexes = Array.ofDim[Int](rows)
      for (i <- 0 until rows) rowIndexes(i) = i
    }
    if (columnIndexes == null) {
      columnIndexes = Array.ofDim[Int](columns)
      for (i <- 0 until columns) columnIndexes(i) = i
    }
    checkSliceIndexes(sliceIndexes)
    checkRowIndexes(rowIndexes)
    checkColumnIndexes(columnIndexes)
    val sliceOffsets = Array.ofDim[Int](sliceIndexes.length)
    val rowOffsets = Array.ofDim[Int](rowIndexes.length)
    val columnOffsets = Array.ofDim[Int](columnIndexes.length)
    for (i <- 0 until sliceIndexes.length) {
      sliceOffsets(i) = _sliceOffset(_sliceRank(sliceIndexes(i)))
    }
    for (i <- 0 until rowIndexes.length) {
      rowOffsets(i) = _rowOffset(_rowRank(rowIndexes(i)))
    }
    for (i <- 0 until columnIndexes.length) {
      columnOffsets(i) = _columnOffset(_columnRank(columnIndexes(i)))
    }
    viewSelectionLike(sliceOffsets, rowOffsets, columnOffsets)
  }

  /**
   * Construct and returns a new selection view.
   *
   * @param sliceOffsets
   *            the offsets of the visible elements.
   * @param rowOffsets
   *            the offsets of the visible elements.
   * @param columnOffsets
   *            the offsets of the visible elements.
   * @return a new view.
   */
  protected def viewSelectionLike(sliceOffsets: Array[Int], rowOffsets: Array[Int], columnOffsets: Array[Int]): IntMatrix3D

  /**
   * Constructs and returns a new 2-dimensional <i>slice view</i> representing
   * the rows and columns of the given slice. The returned view is backed by
   * this matrix, so changes in the returned view are reflected in this
   * matrix, and vice-versa.
   * <p>
   * To obtain a slice view on subranges, construct a sub-ranging view (
   * <tt>view().part(...)</tt>), then apply this method to the sub-range view.
   * To obtain 1-dimensional views, apply this method, then apply another
   * slice view (methods <tt>viewColumn</tt>, <tt>viewRow</tt>) on the
   * intermediate 2-dimensional view. To obtain 1-dimensional views on
   * subranges, apply both steps.
   *
   * @param slice
   *            the index of the slice to fix.
   * @return a new 2-dimensional slice view.
   * @throws IndexOutOfBoundsException
   *             if <tt>slice < 0 || slice >= slices()</tt>.
   * @see #viewRow(int)
   * @see #viewColumn(int)
   */
  def viewSlice(slice: Int): IntMatrix2D = {
    checkSlice(slice)
    val sliceRows = this.rows
    val sliceColumns = this.columns
    val sliceRowZero = rowZero
    val sliceColumnZero = columnZero + _sliceOffset(_sliceRank(slice))
    val sliceRowStride = this.rowStride
    val sliceColumnStride = this.columnStride
    like2D(sliceRows, sliceColumns, sliceRowZero, sliceColumnZero, sliceRowStride, sliceColumnStride)
  }

  /**
   * Constructs and returns a new <i>flip view</i> aint the slice axis. What
   * used to be slice <tt>0</tt> is now slice <tt>slices()-1</tt>, ..., what
   * used to be slice <tt>slices()-1</tt> is now slice <tt>0</tt>. The
   * returned view is backed by this matrix, so changes in the returned view
   * are reflected in this matrix, and vice-versa.
   *
   * @return a new flip view.
   * @see #viewRowFlip()
   * @see #viewColumnFlip()
   */
  def viewSliceFlip(): IntMatrix3D = {
    (view().vSliceFlip()).asInstanceOf[IntMatrix3D]
  }

  /**
   * Sorts the matrix slices into ascending order, according to the <i>natural
   * ordering</i> of the matrix values in the given <tt>[row,column]</tt>
   * position. This sort is guaranteed to be <i>stable</i>. For further
   * information, see
   * {@link cern.colt.matrix.tint.algo.IntSorting#sort(IntMatrix3D,int,int)}.
   * For more advanced sorting functionality, see
   * {@link cern.colt.matrix.tint.algo.IntSorting}.
   *
   * @return a new sorted vector (matrix) view.
   * @throws IndexOutOfBoundsException
   *             if
   *             <tt>row < 0 || row >= rows() || column < 0 || column >= columns()</tt>
   *             .
   */
  def viewSorted(row: Int, column: Int): IntMatrix3D = {
    cern.colt.matrix.tint.algo.IntSorting.mergeSort.sort(this, row, column)
  }

  /**
   * Constructs and returns a new <i>stride view</i> which is a sub matrix
   * consisting of every i-th cell. More specifically, the view has
   * <tt>this.slices()/sliceStride</tt> slices and
   * <tt>this.rows()/rowStride</tt> rows and
   * <tt>this.columns()/columnStride</tt> columns holding cells
   * <tt>this.get(k*sliceStride,i*rowStride,j*columnStride)</tt> for all
   *
   * <tt>k = 0..slices()/sliceStride - 1, i = 0..rows()/rowStride - 1, j = 0..columns()/columnStride - 1</tt>
   * . The returned view is backed by this matrix, so changes in the returned
   * view are reflected in this matrix, and vice-versa.
   *
   * @param sliceStride
   *            the slice step factor.
   * @param rowStride
   *            the row step factor.
   * @param columnStride
   *            the column step factor.
   * @return a new view.
   * @throws IndexOutOfBoundsException
   *             if <tt>sliceStride<=0 || rowStride<=0 || columnStride<=0</tt>
   *             .
   */
  def viewStrides(sliceStride: Int, rowStride: Int, columnStride: Int): IntMatrix3D = {
    (view().vStrides(sliceStride, rowStride, columnStride)).asInstanceOf[IntMatrix3D]
  }

  /**
   * Returns the sum of all cells; <tt>Sum( x[i,j,k] )</tt>.
   *
   * @return the sum.
   */
  def zSum(): Int = {
    if (size == 0) return 0
    aggregate(cern.jet.math.tint.IntFunctions.plus, cern.jet.math.tint.IntFunctions.identity)
  }
}
