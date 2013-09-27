package cern.colt.matrix.tint

import java.util.Iterator
import java.util.Set
import java.util.concurrent.Callable
import java.util.concurrent.ExecutionException
import java.util.concurrent.Future
import cern.colt.list.tint.IntArrayList
import cern.colt.matrix.AbstractMatrix2D
import cern.colt.matrix.tint.impl.DenseIntMatrix1D
import cern.colt.matrix.tint.impl.DenseIntMatrix2D
import edu.emory.mathcs.utils.ConcurrencyUtils
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Abstract base class for 2-d matrices holding <tt>int</tt> elements. First see
 * the <a href="package-summary.html">package summary</a> and javadoc <a
 * href="package-tree.html">tree view</a> to get the broad picture.
 * <p>
 * A matrix has a number of rows and columns, which are assigned upon instance
 * construction - The matrix's size is then <tt>rows()*columns()</tt>. Elements
 * are accessed via <tt>[row,column]</tt> coordinates. Legal coordinates range
 * from <tt>[0,0]</tt> to <tt>[rows()-1,columns()-1]</tt>. Any attempt to access
 * an element at a coordinate
 * <tt>column&lt;0 || column&gt;=columns() || row&lt;0 || row&gt;=rows()</tt>
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
abstract class IntMatrix2D protected () extends AbstractMatrix2D {

  /**
   * Applies a function to each cell and aggregates the results. Returns a
   * value <tt>v</tt> such that <tt>v==a(size())</tt> where
   * <tt>a(i) == aggr( a(i-1), f(get(row,column)) )</tt> and terminators are
   * <tt>a(1) == f(get(0,0)), a(0)==Int.NaN</tt>.
   * <p>
   * <b>Example:</b>
   *
   * <pre>
   *   cern.jet.math.Functions F = cern.jet.math.Functions.functions;
   *   2 x 2 matrix
   *   0 1
   *   2 3
   *
   *   // Sum( x[row,col]*x[row,col] )
   *   matrix.aggregate(F.plus,F.square);
   *   --&gt; 14
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
    if ((nthreads > 1) &&
      (rows * columns >= ConcurrencyUtils.getThreadsBeginN_2D)) {
      nthreads = Math.min(nthreads, rows)
      val futures = Array.ofDim[Future](nthreads)
      val k = rows / nthreads
      for (j <- 0 until nthreads) {
        val firstRow = j * k
        val lastRow = if ((j == nthreads - 1)) rows else firstRow + k
        futures(j) = ConcurrencyUtils.submit(new Callable[Integer]() {

          def call(): java.lang.Integer = {
            var a = f.apply(getQuick(firstRow, 0))
            var d = 1
            for (r <- firstRow until lastRow) {
              for (c <- d until columns) {
                a = aggr.apply(a, f.apply(getQuick(r, c)))
              }
              d = 0
            }
            return a
          }
        })
      }
      a = ConcurrencyUtils.waitForCompletion(futures, aggr)
    } else {
      a = f.apply(getQuick(0, 0))
      var d = 1
      for (r <- 0 until rows) {
        for (c <- d until columns) {
          a = aggr.apply(a, f.apply(getQuick(r, c)))
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
      (rows * columns >= ConcurrencyUtils.getThreadsBeginN_2D)) {
      nthreads = Math.min(nthreads, rows)
      val futures = Array.ofDim[Future](nthreads)
      val k = rows / nthreads
      for (j <- 0 until nthreads) {
        val firstRow = j * k
        val lastRow = if ((j == nthreads - 1)) rows else firstRow + k
        futures(j) = ConcurrencyUtils.submit(new Callable[Integer]() {

          def call(): java.lang.Integer = {
            var elem = getQuick(firstRow, 0)
            var a = 0
            if (cond.apply(elem) == true) {
              a = aggr.apply(a, f.apply(elem))
            }
            var d = 1
            for (r <- firstRow until lastRow) {
              for (c <- d until columns) {
                elem = getQuick(r, c)
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
      var elem = getQuick(0, 0)
      if (cond.apply(elem) == true) {
        a = aggr.apply(a, f.apply(elem))
      }
      var d = 1
      for (r <- 0 until rows) {
        for (c <- d until columns) {
          elem = getQuick(r, c)
          if (cond.apply(elem) == true) {
            a = aggr.apply(a, f.apply(elem))
          }
        }
        d = 0
      }
    }
    a
  }

  /**
   *
   * Applies a function to all cells with a given indexes and aggregates the
   * results.
   *
   * @param aggr
   *            an aggregation function taking as first argument the current
   *            aggregation and as second argument the transformed current
   *            cell value.
   * @param f
   *            a function transforming the current cell value.
   * @param rowList
   *            row indexes.
   * @param columnList
   *            column indexes.
   *
   * @return the aggregated measure.
   * @see cern.jet.math.tint.IntFunctions
   */
  def aggregate(aggr: cern.colt.function.tint.IntIntFunction,
      f: cern.colt.function.tint.IntFunction,
      rowList: IntArrayList,
      columnList: IntArrayList): Int = {
    if (size == 0) throw new IllegalArgumentException("size == 0")
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
            var a = f.apply(getQuick(rowElements(firstIdx), columnElements(firstIdx)))
            var elem: Int = 0
            for (i <- firstIdx + 1 until lastIdx) {
              elem = getQuick(rowElements(i), columnElements(i))
              a = aggr.apply(a, f.apply(elem))
            }
            return a
          }
        })
      }
      a = ConcurrencyUtils.waitForCompletion(futures, aggr)
    } else {
      var elem: Int = 0
      a = f.apply(getQuick(rowElements(0), columnElements(0)))
      for (i <- 1 until size) {
        elem = getQuick(rowElements(i), columnElements(i))
        a = aggr.apply(a, f.apply(elem))
      }
    }
    a
  }

  /**
   * Applies a function to each corresponding cell of two matrices and
   * aggregates the results. Returns a value <tt>v</tt> such that
   * <tt>v==a(size())</tt> where
   * <tt>a(i) == aggr( a(i-1), f(get(row,column),other.get(row,column)) )</tt>
   * and terminators are
   * <tt>a(1) == f(get(0,0),other.get(0,0)), a(0)==Int.NaN</tt>.
   * <p>
   * <b>Example:</b>
   *
   * <pre>
   *   cern.jet.math.Functions F = cern.jet.math.Functions.functions;
   *   x == 2 x 2 matrix
   *   0 1
   *   2 3
   *
   *   y == 2 x 2 matrix
   *   0 1
   *   2 3
   *
   *   // Sum( x[row,col] * y[row,col] )
   *   x.aggregate(y, F.plus, F.mult);
   *   --&gt; 14
   *
   *   // Sum( (x[row,col] + y[row,col])&circ;2 )
   *   x.aggregate(y, F.plus, F.chain(F.square,F.plus));
   *   --&gt; 56
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
   *             <tt>columns() != other.columns() || rows() != other.rows()</tt>
   * @see cern.jet.math.tint.IntFunctions
   */
  def aggregate(other: IntMatrix2D, aggr: cern.colt.function.tint.IntIntFunction, f: cern.colt.function.tint.IntIntFunction): Int = {
    checkShape(other)
    if (size == 0) throw new IllegalArgumentException("size == 0")
    var a = 0
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) &&
      (rows * columns >= ConcurrencyUtils.getThreadsBeginN_2D)) {
      nthreads = Math.min(nthreads, rows)
      val futures = Array.ofDim[Future](nthreads)
      val k = rows / nthreads
      for (j <- 0 until nthreads) {
        val firstRow = j * k
        val lastRow = if ((j == nthreads - 1)) rows else firstRow + k
        futures(j) = ConcurrencyUtils.submit(new Callable[Integer]() {

          def call(): java.lang.Integer = {
            var a = f.apply(getQuick(firstRow, 0), other.getQuick(firstRow, 0))
            var d = 1
            for (r <- firstRow until lastRow) {
              for (c <- d until columns) {
                a = aggr.apply(a, f.apply(getQuick(r, c), other.getQuick(r, c)))
              }
              d = 0
            }
            return a
          }
        })
      }
      a = ConcurrencyUtils.waitForCompletion(futures, aggr)
    } else {
      a = f.apply(getQuick(0, 0), other.getQuick(0, 0))
      var d = 1
      for (r <- 0 until rows) {
        for (c <- d until columns) {
          a = aggr.apply(a, f.apply(getQuick(r, c), other.getQuick(r, c)))
        }
        d = 0
      }
    }
    a
  }

  /**
   * Assigns the result of a function to each cell;
   * <tt>x[row,col] = function(x[row,col])</tt>.
   * <p>
   * <b>Example:</b>
   *
   * <pre>
   *   matrix = 2 x 2 matrix
   *   0.5 1.5
   *   2.5 3.5
   *
   *   // change each cell to its sine
   *   matrix.assign(cern.jet.math.Functions.sin);
   *   --&gt;
   *   2 x 2 matrix
   *   0.479426  0.997495
   *   0.598472 -0.350783
   *
   * </pre>
   *
   * For further examples, see the <a
   * href="package-summary.html#FunctionObjects">package doc</a>.
   *
   * @param f
   *            a function object taking as argument the current cell's value.
   * @return <tt>this</tt> (for convenience only).
   * @see cern.jet.math.tint.IntFunctions
   */
  def assign(f: cern.colt.function.tint.IntFunction): IntMatrix2D = {
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) &&
      (rows * columns >= ConcurrencyUtils.getThreadsBeginN_2D)) {
      nthreads = Math.min(nthreads, rows)
      val futures = Array.ofDim[Future](nthreads)
      val k = rows / nthreads
      for (j <- 0 until nthreads) {
        val firstRow = j * k
        val lastRow = if ((j == nthreads - 1)) rows else firstRow + k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            for (r <- firstRow until lastRow; c <- 0 until columns) {
              setQuick(r, c, f.apply(getQuick(r, c)))
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
    } else {
      for (r <- 0 until rows; c <- 0 until columns) {
        setQuick(r, c, f.apply(getQuick(r, c)))
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
  def assign(cond: cern.colt.function.tint.IntProcedure, f: cern.colt.function.tint.IntFunction): IntMatrix2D = {
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) &&
      (rows * columns >= ConcurrencyUtils.getThreadsBeginN_2D)) {
      nthreads = Math.min(nthreads, rows)
      val futures = Array.ofDim[Future](nthreads)
      val k = rows / nthreads
      for (j <- 0 until nthreads) {
        val firstRow = j * k
        val lastRow = if ((j == nthreads - 1)) rows else firstRow + k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            var elem: Int = 0
            for (r <- firstRow until lastRow; c <- 0 until columns) {
              elem = getQuick(r, c)
              if (cond.apply(elem) == true) {
                setQuick(r, c, f.apply(elem))
              }
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
    } else {
      var elem: Int = 0
      for (r <- 0 until rows; c <- 0 until columns) {
        elem = getQuick(r, c)
        if (cond.apply(elem) == true) {
          setQuick(r, c, f.apply(elem))
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
  def assign(cond: cern.colt.function.tint.IntProcedure, value: Int): IntMatrix2D = {
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) &&
      (rows * columns >= ConcurrencyUtils.getThreadsBeginN_2D)) {
      nthreads = Math.min(nthreads, rows)
      val futures = Array.ofDim[Future](nthreads)
      val k = rows / nthreads
      for (j <- 0 until nthreads) {
        val firstRow = j * k
        val lastRow = if ((j == nthreads - 1)) rows else firstRow + k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            var elem: Int = 0
            for (r <- firstRow until lastRow; c <- 0 until columns) {
              elem = getQuick(r, c)
              if (cond.apply(elem) == true) {
                setQuick(r, c, value)
              }
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
    } else {
      var elem: Int = 0
      for (r <- 0 until rows; c <- 0 until columns) {
        elem = getQuick(r, c)
        if (cond.apply(elem) == true) {
          setQuick(r, c, value)
        }
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
  def assign(value: Int): IntMatrix2D = {
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) &&
      (rows * columns >= ConcurrencyUtils.getThreadsBeginN_2D)) {
      nthreads = Math.min(nthreads, rows)
      val futures = Array.ofDim[Future](nthreads)
      val k = rows / nthreads
      for (j <- 0 until nthreads) {
        val firstRow = j * k
        val lastRow = if ((j == nthreads - 1)) rows else firstRow + k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            for (r <- firstRow until lastRow; c <- 0 until columns) {
              setQuick(r, c, value)
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
    } else {
      for (r <- 0 until rows; c <- 0 until columns) {
        setQuick(r, c, value)
      }
    }
    this
  }

  /**
   * Sets all cells to the state specified by <tt>values</tt>. <tt>values</tt>
   * is required to have the form <tt>values[row*column]</tt> and elements
   * have to be stored in a row-wise order.
   * <p>
   * The values are copied. So subsequent changes in <tt>values</tt> are not
   * reflected in the matrix, and vice-versa.
   *
   * @param values
   *            the values to be filled into the cells.
   * @return <tt>this</tt> (for convenience only).
   * @throws IllegalArgumentException
   *             if <tt>values.length != rows()*columns()</tt>.
   */
  def assign(values: Array[Int]): IntMatrix2D = {
    if (values.length != rows * columns) throw new IllegalArgumentException("Must have same length: length=" + values.length + "rows()*columns()=" +
      rows() * columns())
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) &&
      (rows * columns >= ConcurrencyUtils.getThreadsBeginN_2D)) {
      nthreads = Math.min(nthreads, rows)
      val futures = Array.ofDim[Future](nthreads)
      val k = rows / nthreads
      for (j <- 0 until nthreads) {
        val firstRow = j * k
        val lastRow = if ((j == nthreads - 1)) rows else firstRow + k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            var idx = firstRow * columns
            for (r <- firstRow until lastRow; c <- 0 until columns) {
              setQuick(r, c, values(idx += 1))
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
    } else {
      var idx = 0
      for (r <- 0 until rows; c <- 0 until columns) {
        setQuick(r, c, values(idx += 1))
      }
    }
    this
  }

  /**
   * Sets all cells to the state specified by <tt>values</tt>. <tt>values</tt>
   * is required to have the form <tt>values[row][column]</tt> and have
   * exactly the same number of rows and columns as the receiver.
   * <p>
   * The values are copied. So subsequent changes in <tt>values</tt> are not
   * reflected in the matrix, and vice-versa.
   *
   * @param values
   *            the values to be filled into the cells.
   * @return <tt>this</tt> (for convenience only).
   * @throws IllegalArgumentException
   *             if
   *             <tt>values.length != rows() || for any 0 &lt;= row &lt; rows(): values[row].length != columns()</tt>
   *             .
   */
  def assign(values: Array[Array[Int]]): IntMatrix2D = {
    if (values.length != rows) throw new IllegalArgumentException("Must have same number of rows: rows=" + values.length +
      "rows()=" +
      rows())
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) &&
      (rows * columns >= ConcurrencyUtils.getThreadsBeginN_2D)) {
      nthreads = Math.min(nthreads, rows)
      val futures = Array.ofDim[Future](nthreads)
      val k = rows / nthreads
      for (j <- 0 until nthreads) {
        val firstRow = j * k
        val lastRow = if ((j == nthreads - 1)) rows else firstRow + k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            for (r <- firstRow until lastRow) {
              var currentRow = values(r)
              if (currentRow.length != columns) throw new IllegalArgumentException("Must have same number of columns in every row: columns=" +
                currentRow.length +
                "columns()=" +
                columns())
              for (c <- 0 until columns) {
                setQuick(r, c, currentRow(c))
              }
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
    } else {
      for (r <- 0 until rows) {
        val currentRow = values(r)
        if (currentRow.length != columns) throw new IllegalArgumentException("Must have same number of columns in every row: columns=" +
          currentRow.length +
          "columns()=" +
          columns())
        for (c <- 0 until columns) {
          setQuick(r, c, currentRow(c))
        }
      }
    }
    this
  }

  /**
   * Replaces all cell values of the receiver with the values of another
   * matrix. Both matrices must have the same number of rows and columns. If
   * both matrices share the same cells (as is the case if they are views
   * derived from the same matrix) and intersect in an ambiguous way, then
   * replaces <i>as if</i> using an intermediate auxiliary deep copy of
   * <tt>other</tt>.
   *
   * @param other
   *            the source matrix to copy from (may be identical to the
   *            receiver).
   * @return <tt>this</tt> (for convenience only).
   * @throws IllegalArgumentException
   *             if
   *             <tt>columns() != other.columns() || rows() != other.rows()</tt>
   */
  def assign(other: IntMatrix2D): IntMatrix2D = {
    if (other == this) return this
    checkShape(other)
    var other_loc: IntMatrix2D = null
    other_loc = if (haveSharedCells(other)) other.copy() else other
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) &&
      (rows * columns >= ConcurrencyUtils.getThreadsBeginN_2D)) {
      nthreads = Math.min(nthreads, rows)
      val futures = Array.ofDim[Future](nthreads)
      val k = rows / nthreads
      for (j <- 0 until nthreads) {
        val firstRow = j * k
        val lastRow = if ((j == nthreads - 1)) rows else firstRow + k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            for (r <- firstRow until lastRow; c <- 0 until columns) {
              setQuick(r, c, other_loc.getQuick(r, c))
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
    } else {
      for (r <- 0 until rows; c <- 0 until columns) {
        setQuick(r, c, other_loc.getQuick(r, c))
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
   *   m1 = 2 x 2 matrix
   *   0 1
   *   2 3
   *
   *   m2 = 2 x 2 matrix
   *   0 2
   *   4 6
   *
   *   m1.assign(m2, cern.jet.math.Functions.pow);
   *   --&gt;
   *   m1 == 2 x 2 matrix
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
   *             <tt>columns() != other.columns() || rows() != other.rows()</tt>
   * @see cern.jet.math.tint.IntFunctions
   */
  def assign(y: IntMatrix2D, function: cern.colt.function.tint.IntIntFunction): IntMatrix2D = {
    checkShape(y)
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) &&
      (rows * columns >= ConcurrencyUtils.getThreadsBeginN_2D)) {
      nthreads = Math.min(nthreads, rows)
      val futures = Array.ofDim[Future](nthreads)
      val k = rows / nthreads
      for (j <- 0 until nthreads) {
        val firstRow = j * k
        val lastRow = if ((j == nthreads - 1)) rows else firstRow + k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            for (r <- firstRow until lastRow; c <- 0 until columns) {
              setQuick(r, c, function.apply(getQuick(r, c), y.getQuick(r, c)))
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
    } else {
      for (r <- 0 until rows; c <- 0 until columns) {
        setQuick(r, c, function.apply(getQuick(r, c), y.getQuick(r, c)))
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
   *            cell's value of <tt>y</tt>,
   * @param rowList
   *            row indexes.
   * @param columnList
   *            column indexes.
   *
   * @return <tt>this</tt> (for convenience only).
   * @throws IllegalArgumentException
   *             if
   *             <tt>columns() != other.columns() || rows() != other.rows()</tt>
   * @see cern.jet.math.tint.IntFunctions
   */
  def assign(y: IntMatrix2D,
      function: cern.colt.function.tint.IntIntFunction,
      rowList: IntArrayList,
      columnList: IntArrayList): IntMatrix2D = {
    checkShape(y)
    val size = rowList.size
    val rowElements = rowList.elements()
    val columnElements = columnList.elements()
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
            for (i <- firstIdx until lastIdx) {
              setQuick(rowElements(i), columnElements(i), function.apply(getQuick(rowElements(i), columnElements(i)),
                y.getQuick(rowElements(i), columnElements(i))))
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
    } else {
      for (i <- 0 until size) {
        setQuick(rowElements(i), columnElements(i), function.apply(getQuick(rowElements(i), columnElements(i)),
          y.getQuick(rowElements(i), columnElements(i))))
      }
    }
    this
  }

  /**
   * Returns the number of cells having non-zero values; ignores tolerance.
   *
   * @return cardinality
   */
  def cardinality(): Int = {
    var cardinality = 0
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) &&
      (rows * columns >= ConcurrencyUtils.getThreadsBeginN_2D)) {
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
            for (r <- firstRow until lastRow; c <- 0 until columns if getQuick(r, c) != 0) cardinality += 1
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
      for (r <- 0 until rows; c <- 0 until columns if getQuick(r, c) != 0) cardinality += 1
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
  def copy(): IntMatrix2D = like().assign(this)

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
   * and is at least a <code>IntMatrix2D</code> object that has the same
   * number of columns and rows as the receiver and has exactly the same
   * values at the same coordinates.
   *
   * @param obj
   *            the object to compare with.
   * @return <code>true</code> if the objects are the same; <code>false</code>
   *         otherwise.
   */
  override def equals(obj: Any): Boolean = {
    if (this == obj) return true
    if (obj == null) return false
    if (!(obj.isInstanceOf[IntMatrix2D])) return false
    cern.colt.matrix.tint.algo.IntProperty.DEFAULT.==(this, obj.asInstanceOf[IntMatrix2D])
  }

  /**
   * Assigns the result of a function to each <i>non-zero</i> cell;
   * <tt>x[row,col] = function(x[row,col])</tt>. Use this method for fast
   * special-purpose iteration. If you want to modify another matrix instead
   * of <tt>this</tt> (i.e. work in read-only mode), simply return the input
   * value unchanged.
   *
   * Parameters to function are as follows: <tt>first==row</tt>,
   * <tt>second==column</tt>, <tt>third==nonZeroValue</tt>.
   *
   * @param function
   *            a function object taking as argument the current non-zero
   *            cell's row, column and value.
   * @return <tt>this</tt> (for convenience only).
   */
  def forEachNonZero(function: cern.colt.function.tint.IntIntIntFunction): IntMatrix2D = {
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
            for (r <- firstRow until lastRow; c <- 0 until columns) {
              var value = getQuick(r, c)
              if (value != 0) {
                var a = function.apply(r, c, value)
                if (a != value) setQuick(r, c, a)
              }
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
    } else {
      for (r <- 0 until rows; c <- 0 until columns) {
        val value = getQuick(r, c)
        if (value != 0) {
          val a = function.apply(r, c, value)
          if (a != value) setQuick(r, c, a)
        }
      }
    }
    this
  }

  /**
   * Returns the matrix cell value at coordinate <tt>[row,column]</tt>.
   *
   * @param row
   *            the index of the row-coordinate.
   * @param column
   *            the index of the column-coordinate.
   * @return the value of the specified cell.
   * @throws IndexOutOfBoundsException
   *             if
   *             <tt>column&lt;0 || column&gt;=columns() || row&lt;0 || row&gt;=rows()</tt>
   */
  def get(row: Int, column: Int): Int = {
    if (column < 0 || column >= columns || row < 0 || row >= rows) throw new IndexOutOfBoundsException("row:" + row + ", column:" + column)
    getQuick(row, column)
  }

  /**
   * Returns the content of this matrix if it is a wrapper; or <tt>this</tt>
   * otherwise. Override this method in wrappers.
   */
  protected def getStorageMatrix(): IntMatrix2D = this

  /**
   * Fills the coordinates and values of cells having negative values into the
   * specified lists. Fills into the lists, starting at index 0. After this
   * call returns the specified lists all have a new size, the number of
   * non-zero values.
   *
   * @param rowList
   *            the list to be filled with row indexes, can have any size.
   * @param columnList
   *            the list to be filled with column indexes, can have any size.
   * @param valueList
   *            the list to be filled with values, can have any size.
   */
  def getNegativeValues(rowList: IntArrayList, columnList: IntArrayList, valueList: IntArrayList) {
    rowList.clear()
    columnList.clear()
    valueList.clear()
    for (r <- 0 until rows; c <- 0 until columns) {
      val value = getQuick(r, c)
      if (value < 0) {
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
   * In general, fill order is <i>unspecified</i>. This implementation fills
   * like <tt>for (row = 0..rows-1) for (column = 0..columns-1) do ... </tt>.
   * However, subclasses are free to us any other order, even an order that
   * may change over time as cell values are changed. (Of course, result lists
   * indexes are guaranteed to correspond to the same cell).
   * <p>
   * <b>Example:</b> <br>
   *
   * <pre>
   *   2 x 3 matrix:
   *   0, 0, 8
   *   0, 7, 0
   *   --&gt;
   *   rowList    = (0,1)
   *   columnList = (2,1)
   *   valueList  = (8,7)
   *
   * </pre>
   *
   * In other words, <tt>get(0,2)==8, get(1,1)==7</tt>.
   *
   * @param rowList
   *            the list to be filled with row indexes, can have any size.
   * @param columnList
   *            the list to be filled with column indexes, can have any size.
   * @param valueList
   *            the list to be filled with values, can have any size.
   */
  def getNonZeros(rowList: IntArrayList, columnList: IntArrayList, valueList: IntArrayList) {
    rowList.clear()
    columnList.clear()
    valueList.clear()
    for (r <- 0 until rows; c <- 0 until columns) {
      val value = getQuick(r, c)
      if (value != 0) {
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
   * @param rowList
   *            the list to be filled with row indexes, can have any size.
   * @param columnList
   *            the list to be filled with column indexes, can have any size.
   * @param valueList
   *            the list to be filled with values, can have any size.
   */
  def getPositiveValues(rowList: IntArrayList, columnList: IntArrayList, valueList: IntArrayList) {
    rowList.clear()
    columnList.clear()
    valueList.clear()
    for (r <- 0 until rows; c <- 0 until columns) {
      val value = getQuick(r, c)
      if (value > 0) {
        rowList.add(r)
        columnList.add(c)
        valueList.add(value)
      }
    }
  }

  /**
   * Returns the matrix cell value at coordinate <tt>[row,column]</tt>.
   *
   * <p>
   * Provided with invalid parameters this method may return invalid objects
   * without throwing any exception. <b>You should only use this method when
   * you are absolutely sure that the coordinate is within bounds.</b>
   * Precondition (unchecked):
   * <tt>0 &lt;= column &lt; columns() && 0 &lt;= row &lt; rows()</tt>.
   *
   * @param row
   *            the index of the row-coordinate.
   * @param column
   *            the index of the column-coordinate.
   * @return the value at the specified coordinate.
   */
  def getQuick(row: Int, column: Int): Int

  /**
   * Returns <tt>true</tt> if both matrices share at least one identical cell.
   */
  protected def haveSharedCells(other: IntMatrix2D): Boolean = {
    if (other == null) return false
    if (this == other) return true
    getStorageMatrix.haveSharedCellsRaw(other.getStorageMatrix)
  }

  /**
   * Returns <tt>true</tt> if both matrices share at least one identical cell.
   */
  protected def haveSharedCellsRaw(other: IntMatrix2D): Boolean = false

  /**
   * Construct and returns a new empty matrix <i>of the same dynamic type</i>
   * as the receiver, having the same number of rows and columns. For example,
   * if the receiver is an instance of type <tt>DenseIntMatrix2D</tt> the new
   * matrix must also be of type <tt>DenseIntMatrix2D</tt>, if the receiver is
   * an instance of type <tt>SparseIntMatrix2D</tt> the new matrix must also
   * be of type <tt>SparseIntMatrix2D</tt>, etc. In general, the new matrix
   * should have internal parametrization as similar as possible.
   *
   * @return a new empty matrix of the same dynamic type.
   */
  def like(): IntMatrix2D = like(rows, columns)

  /**
   * Construct and returns a new empty matrix <i>of the same dynamic type</i>
   * as the receiver, having the specified number of rows and columns. For
   * example, if the receiver is an instance of type <tt>DenseIntMatrix2D</tt>
   * the new matrix must also be of type <tt>DenseIntMatrix2D</tt>, if the
   * receiver is an instance of type <tt>SparseIntMatrix2D</tt> the new matrix
   * must also be of type <tt>SparseIntMatrix2D</tt>, etc. In general, the new
   * matrix should have internal parametrization as similar as possible.
   *
   * @param rows
   *            the number of rows the matrix shall have.
   * @param columns
   *            the number of columns the matrix shall have.
   * @return a new empty matrix of the same dynamic type.
   */
  def like(rows: Int, columns: Int): IntMatrix2D

  /**
   * Construct and returns a new 1-d matrix <i>of the corresponding dynamic
   * type</i>, entirelly independent of the receiver. For example, if the
   * receiver is an instance of type <tt>DenseIntMatrix2D</tt> the new matrix
   * must be of type <tt>DenseIntMatrix1D</tt>, if the receiver is an instance
   * of type <tt>SparseIntMatrix2D</tt> the new matrix must be of type
   * <tt>SparseIntMatrix1D</tt>, etc.
   *
   * @param size
   *            the number of cells the matrix shall have.
   * @return a new matrix of the corresponding dynamic type.
   */
  def like1D(size: Int): IntMatrix1D

  /**
   * Construct and returns a new 1-d matrix <i>of the corresponding dynamic
   * type</i>, sharing the same cells. For example, if the receiver is an
   * instance of type <tt>DenseIntMatrix2D</tt> the new matrix must be of type
   * <tt>DenseIntMatrix1D</tt>, if the receiver is an instance of type
   * <tt>SparseIntMatrix2D</tt> the new matrix must be of type
   * <tt>SparseIntMatrix1D</tt>, etc.
   *
   * @param size
   *            the number of cells the matrix shall have.
   * @param zero
   *            the index of the first element.
   * @param stride
   *            the number of indexes between any two elements, i.e.
   *            <tt>index(i+1)-index(i)</tt>.
   * @return a new matrix of the corresponding dynamic type.
   */
  protected def like1D(size: Int, zero: Int, stride: Int): IntMatrix1D

  /**
   * Return the maximum value of this matrix together with its location
   *
   * @return maximum_value, row_location, column_location };
   */
  def getMaxLocation(): Array[Int] = {
    var rowLocation = 0
    var columnLocation = 0
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
            var rowLocation = firstRow
            var columnLocation = 0
            var maxValue = getQuick(rowLocation, 0)
            var d = 1
            var elem: Int = 0
            for (r <- firstRow until lastRow) {
              for (c <- d until columns) {
                elem = getQuick(r, c)
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
      maxValue = getQuick(0, 0)
      var elem: Int = 0
      var d = 1
      for (r <- 0 until rows) {
        for (c <- d until columns) {
          elem = getQuick(r, c)
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

  /**
   * Return the minimum value of this matrix together with its location
   *
   * @return minimum_value, row_location, column_location};
   */
  def getMinLocation(): Array[Int] = {
    var rowLocation = 0
    var columnLocation = 0
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
            var minValue = getQuick(rowLocation, 0)
            var d = 1
            var elem: Int = 0
            for (r <- firstRow until lastRow) {
              for (c <- d until columns) {
                elem = getQuick(r, c)
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
      minValue = getQuick(0, 0)
      var elem: Int = 0
      var d = 1
      for (r <- 0 until rows) {
        for (c <- d until columns) {
          elem = getQuick(r, c)
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

  /**
   * Sets the matrix cell at coordinate <tt>[row,column]</tt> to the specified
   * value.
   *
   * @param row
   *            the index of the row-coordinate.
   * @param column
   *            the index of the column-coordinate.
   * @param value
   *            the value to be filled into the specified cell.
   * @throws IndexOutOfBoundsException
   *             if
   *             <tt>column&lt;0 || column&gt;=columns() || row&lt;0 || row&gt;=rows()</tt>
   */
  def set(row: Int, column: Int, value: Int) {
    if (column < 0 || column >= columns || row < 0 || row >= rows) throw new IndexOutOfBoundsException("row:" + row + ", column:" + column)
    setQuick(row, column, value)
  }

  /**
   * Sets the matrix cell at coordinate <tt>[row,column]</tt> to the specified
   * value.
   *
   * <p>
   * Provided with invalid parameters this method may access illegal indexes
   * without throwing any exception. <b>You should only use this method when
   * you are absolutely sure that the coordinate is within bounds.</b>
   * Precondition (unchecked):
   * <tt>0 &lt;= column &lt; columns() && 0 &lt;= row &lt; rows()</tt>.
   *
   * @param row
   *            the index of the row-coordinate.
   * @param column
   *            the index of the column-coordinate.
   * @param value
   *            the value to be filled into the specified cell.
   */
  def setQuick(row: Int, column: Int, value: Int): Unit

  /**
   * Constructs and returns a 2-dimensional array containing the cell values.
   * The returned array <tt>values</tt> has the form
   * <tt>values[row][column]</tt> and has the same number of rows and columns
   * as the receiver.
   * <p>
   * The values are copied. So subsequent changes in <tt>values</tt> are not
   * reflected in the matrix, and vice-versa.
   *
   * @return an array filled with the values of the cells.
   */
  def toArray(): Array[Array[Int]] = {
    val values = Array.ofDim[Int](rows, columns)
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
            for (r <- firstRow until lastRow) {
              var currentRow = values(r)
              for (c <- 0 until columns) {
                currentRow(c) = getQuick(r, c)
              }
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
    } else {
      for (r <- 0 until rows) {
        val currentRow = values(r)
        for (c <- 0 until columns) {
          currentRow(c) = getQuick(r, c)
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
   * Returns a vector obtained by stacking the columns of the matrix on top of
   * one another.
   *
   * @return a vector of columns of this matrix.
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
   * Use {@link #copy()} to construct an independent deep copy rather than a
   * new view.
   *
   * @return a new view of the receiver.
   */
  protected def view(): IntMatrix2D = clone().asInstanceOf[IntMatrix2D]

  /**
   * Constructs and returns a new <i>slice view</i> representing the rows of
   * the given column. The returned view is backed by this matrix, so changes
   * in the returned view are reflected in this matrix, and vice-versa. To
   * obtain a slice view on subranges, construct a sub-ranging view (
   * <tt>viewPart(...)</tt>), then apply this method to the sub-range view.
   * <p>
   * <b>Example:</b>
   * <table border="0">
   * <tr nowrap>
   * <td valign="top">2 x 3 matrix: <br>
   * 1, 2, 3<br>
   * 4, 5, 6</td>
   * <td>viewColumn(0) ==></td>
   * <td valign="top">Matrix1D of size 2:<br>
   * 1, 4</td>
   * </tr>
   * </table>
   *
   * @param column
   *            the column to fix.
   * @return a new slice view.
   * @throws IndexOutOfBoundsException
   *             if <tt>column < 0 || column >= columns()</tt>.
   * @see #viewRow(int)
   */
  def viewColumn(column: Int): IntMatrix1D = {
    checkColumn(column)
    val viewSize = this.rows
    val viewZero = index(0, column).toInt
    val viewStride = this.rowStride
    like1D(viewSize, viewZero, viewStride)
  }

  /**
   * Constructs and returns a new <i>flip view</i> aint the column axis. What
   * used to be column <tt>0</tt> is now column <tt>columns()-1</tt>, ...,
   * what used to be column <tt>columns()-1</tt> is now column <tt>0</tt>. The
   * returned view is backed by this matrix, so changes in the returned view
   * are reflected in this matrix, and vice-versa.
   * <p>
   * <b>Example:</b>
   * <table border="0">
   * <tr nowrap>
   * <td valign="top">2 x 3 matrix: <br>
   * 1, 2, 3<br>
   * 4, 5, 6</td>
   * <td>columnFlip ==></td>
   * <td valign="top">2 x 3 matrix:<br>
   * 3, 2, 1 <br>
   * 6, 5, 4</td>
   * <td>columnFlip ==></td>
   * <td valign="top">2 x 3 matrix: <br>
   * 1, 2, 3<br>
   * 4, 5, 6</td>
   * </tr>
   * </table>
   *
   * @return a new flip view.
   * @see #viewRowFlip()
   */
  def viewColumnFlip(): IntMatrix2D = {
    (view().vColumnFlip()).asInstanceOf[IntMatrix2D]
  }

  /**
   * Constructs and returns a new <i>dice (transposition) view</i>; Swaps
   * axes; example: 3 x 4 matrix --> 4 x 3 matrix. The view has both
   * dimensions exchanged; what used to be columns become rows, what used to
   * be rows become columns. In other words:
   * <tt>view.get(row,column)==this.get(column,row)</tt>. This is a zero-copy
   * transposition, taking O(1), i.e. constant time. The returned view is
   * backed by this matrix, so changes in the returned view are reflected in
   * this matrix, and vice-versa. Use idioms like
   * <tt>result = viewDice(A).copy()</tt> to generate an independent
   * transposed matrix.
   * <p>
   * <b>Example:</b>
   * <table border="0">
   * <tr nowrap>
   * <td valign="top">2 x 3 matrix: <br>
   * 1, 2, 3<br>
   * 4, 5, 6</td>
   * <td>transpose ==></td>
   * <td valign="top">3 x 2 matrix:<br>
   * 1, 4 <br>
   * 2, 5 <br>
   * 3, 6</td>
   * <td>transpose ==></td>
   * <td valign="top">2 x 3 matrix: <br>
   * 1, 2, 3<br>
   * 4, 5, 6</td>
   * </tr>
   * </table>
   *
   * @return a new dice view.
   */
  def viewDice(): IntMatrix2D = {
    (view().vDice()).asInstanceOf[IntMatrix2D]
  }

  /**
   * Constructs and returns a new <i>sub-range view</i> that is a
   * <tt>height x width</tt> sub matrix starting at <tt>[row,column]</tt>.
   *
   * Operations on the returned view can only be applied to the restricted
   * range. Any attempt to access coordinates not contained in the view will
   * throw an <tt>IndexOutOfBoundsException</tt>.
   * <p>
   * <b>Note that the view is really just a range restriction:</b> The
   * returned matrix is backed by this matrix, so changes in the returned
   * matrix are reflected in this matrix, and vice-versa.
   * <p>
   * The view contains the cells from <tt>[row,column]</tt> to
   * <tt>[row+height-1,column+width-1]</tt>, all inclusive. and has
   * <tt>view.rows() == height; view.columns() == width;</tt>. A view's legal
   * coordinates are again zero based, as usual. In other words, legal
   * coordinates of the view range from <tt>[0,0]</tt> to
   * <tt>[view.rows()-1==height-1,view.columns()-1==width-1]</tt>. As usual,
   * any attempt to access a cell at a coordinate
   * <tt>column&lt;0 || column&gt;=view.columns() || row&lt;0 || row&gt;=view.rows()</tt>
   * will throw an <tt>IndexOutOfBoundsException</tt>.
   *
   * @param row
   *            The index of the row-coordinate.
   * @param column
   *            The index of the column-coordinate.
   * @param height
   *            The height of the box.
   * @param width
   *            The width of the box.
   * @throws IndexOutOfBoundsException
   *             if
   *             <tt>column<0 || width<0 || column+width>columns() || row<0 || height<0 || row+height>rows()</tt>
   * @return the new view.
   *
   */
  def viewPart(row: Int,
      column: Int,
      height: Int,
      width: Int): IntMatrix2D = {
    (view().vPart(row, column, height, width)).asInstanceOf[IntMatrix2D]
  }

  /**
   * Constructs and returns a new <i>slice view</i> representing the columns
   * of the given row. The returned view is backed by this matrix, so changes
   * in the returned view are reflected in this matrix, and vice-versa. To
   * obtain a slice view on subranges, construct a sub-ranging view (
   * <tt>viewPart(...)</tt>), then apply this method to the sub-range view.
   * <p>
   * <b>Example:</b>
   * <table border="0">
   * <tr nowrap>
   * <td valign="top">2 x 3 matrix: <br>
   * 1, 2, 3<br>
   * 4, 5, 6</td>
   * <td>viewRow(0) ==></td>
   * <td valign="top">Matrix1D of size 3:<br>
   * 1, 2, 3</td>
   * </tr>
   * </table>
   *
   * @param row
   *            the row to fix.
   * @return a new slice view.
   * @throws IndexOutOfBoundsException
   *             if <tt>row < 0 || row >= rows()</tt>.
   * @see #viewColumn(int)
   */
  def viewRow(row: Int): IntMatrix1D = {
    checkRow(row)
    val viewSize = this.columns
    val viewZero = index(row, 0).toInt
    val viewStride = this.columnStride
    like1D(viewSize, viewZero, viewStride)
  }

  /**
   * Constructs and returns a new <i>flip view</i> aint the row axis. What
   * used to be row <tt>0</tt> is now row <tt>rows()-1</tt>, ..., what used to
   * be row <tt>rows()-1</tt> is now row <tt>0</tt>. The returned view is
   * backed by this matrix, so changes in the returned view are reflected in
   * this matrix, and vice-versa.
   * <p>
   * <b>Example:</b>
   * <table border="0">
   * <tr nowrap>
   * <td valign="top">2 x 3 matrix: <br>
   * 1, 2, 3<br>
   * 4, 5, 6</td>
   * <td>rowFlip ==></td>
   * <td valign="top">2 x 3 matrix:<br>
   * 4, 5, 6 <br>
   * 1, 2, 3</td>
   * <td>rowFlip ==></td>
   * <td valign="top">2 x 3 matrix: <br>
   * 1, 2, 3<br>
   * 4, 5, 6</td>
   * </tr>
   * </table>
   *
   * @return a new flip view.
   * @see #viewColumnFlip()
   */
  def viewRowFlip(): IntMatrix2D = {
    (view().vRowFlip()).asInstanceOf[IntMatrix2D]
  }

  /**
   * Constructs and returns a new <i>selection view</i> that is a matrix
   * holding all <b>rows</b> matching the given condition. Applies the
   * condition to each row and takes only those row where
   * <tt>condition.apply(viewRow(i))</tt> yields <tt>true</tt>. To match
   * columns, use a dice view.
   * <p>
   * <b>Example:</b> <br>
   *
   * <pre>
   *   // extract and view all rows which have a value &lt; threshold in the first column (representing &quot;age&quot;)
   *   final int threshold = 16;
   *   matrix.viewSelection(
   *      new IntMatrix1DProcedure() {
   *         public final boolean apply(IntMatrix1D m) { return m.get(0) &lt; threshold; }
   *      }
   *   );
   *
   *   // extract and view all rows with RMS &lt; threshold
   *   // The RMS (Root-Mean-Square) is a measure of the average &quot;size&quot; of the elements of a data sequence.
   *   matrix = 0 1 2 3
   *   final int threshold = 0.5;
   *   matrix.viewSelection(
   *      new IntMatrix1DProcedure() {
   *         public final boolean apply(IntMatrix1D m) { return Math.sqrt(m.aggregate(F.plus,F.square) / m.size()) &lt; threshold; }
   *      }
   *   );
   *
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
  def viewSelection(condition: IntMatrix1DProcedure): IntMatrix2D = {
    val matches = new IntArrayList()
    for (i <- 0 until rows if condition.apply(viewRow(i))) matches.add(i)
    matches.trimToSize()
    viewSelection(matches.elements(), null)
  }

  /**
   * Constructs and returns a new <i>selection view</i> that is a matrix
   * holding the indicated cells. There holds
   * <tt>view.rows() == rowIndexes.length, view.columns() == columnIndexes.length</tt>
   * and <tt>view.get(i,j) == this.get(rowIndexes[i],columnIndexes[j])</tt>.
   * Indexes can occur multiple times and can be in arbitrary order.
   * <p>
   * <b>Example:</b>
   *
   * <pre>
   *   this = 2 x 3 matrix:
   *   1, 2, 3
   *   4, 5, 6
   *   rowIndexes     = (0,1)
   *   columnIndexes  = (1,0,1,0)
   *   --&gt;
   *   view = 2 x 4 matrix:
   *   2, 1, 2, 1
   *   5, 4, 5, 4
   *
   * </pre>
   *
   * Note that modifying the index arguments after this call has returned has
   * no effect on the view. The returned view is backed by this matrix, so
   * changes in the returned view are reflected in this matrix, and
   * vice-versa.
   * <p>
   * To indicate "all" rows or "all columns", simply set the respective
   * parameter
   *
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
   *             if <tt>!(0 <= rowIndexes[i] < rows())</tt> for any
   *             <tt>i=0..rowIndexes.length()-1</tt>.
   * @throws IndexOutOfBoundsException
   *             if <tt>!(0 <= columnIndexes[i] < columns())</tt> for any
   *             <tt>i=0..columnIndexes.length()-1</tt>.
   */
  def viewSelection(rowIndexes: Array[Int], columnIndexes: Array[Int]): IntMatrix2D = {
    if (rowIndexes == null) {
      rowIndexes = Array.ofDim[Int](rows)
      for (i <- 0 until rows) rowIndexes(i) = i
    }
    if (columnIndexes == null) {
      columnIndexes = Array.ofDim[Int](columns)
      for (i <- 0 until columns) columnIndexes(i) = i
    }
    checkRowIndexes(rowIndexes)
    checkColumnIndexes(columnIndexes)
    val rowOffsets = Array.ofDim[Int](rowIndexes.length)
    val columnOffsets = Array.ofDim[Int](columnIndexes.length)
    for (i <- 0 until rowIndexes.length) {
      rowOffsets(i) = _rowOffset(_rowRank(rowIndexes(i)))
    }
    for (i <- 0 until columnIndexes.length) {
      columnOffsets(i) = _columnOffset(_columnRank(columnIndexes(i)))
    }
    viewSelectionLike(rowOffsets, columnOffsets)
  }

  def viewSelection(indexes: Set[Array[Int]]): IntMatrix2D = {
    val n = indexes.size
    val rowIndexes = Array.ofDim[Int](n)
    val columnIndexes = Array.ofDim[Int](n)
    var idx = 0
    var iterator = indexes.iterator()
    while (iterator.hasNext) {
      val is = iterator.next()
      rowIndexes(idx) = is(0)
      columnIndexes(idx) = is(1)
      idx += 1
    }
    checkRowIndexes(rowIndexes)
    checkColumnIndexes(columnIndexes)
    val rowOffsets = Array.ofDim[Int](rowIndexes.length)
    val columnOffsets = Array.ofDim[Int](columnIndexes.length)
    for (i <- 0 until rowIndexes.length) {
      rowOffsets(i) = _rowOffset(_rowRank(rowIndexes(i)))
    }
    for (i <- 0 until columnIndexes.length) {
      columnOffsets(i) = _columnOffset(_columnRank(columnIndexes(i)))
    }
    viewSelectionLike(rowOffsets, columnOffsets)
  }

  /**
   * Construct and returns a new selection view.
   *
   * @param rowOffsets
   *            the offsets of the visible elements.
   * @param columnOffsets
   *            the offsets of the visible elements.
   * @return a new view.
   */
  protected def viewSelectionLike(rowOffsets: Array[Int], columnOffsets: Array[Int]): IntMatrix2D

  /**
   * Sorts the matrix rows into ascending order, according to the <i>natural
   * ordering</i> of the matrix values in the given column. This sort is
   * guaranteed to be <i>stable</i>. For further information, see
   * {@link cern.colt.matrix.tint.algo.IntSorting#sort(IntMatrix2D,int)}. For
   * more advanced sorting functionality, see
   * {@link cern.colt.matrix.tint.algo.IntSorting}.
   *
   * @return a new sorted vector (matrix) view.
   * @throws IndexOutOfBoundsException
   *             if <tt>column < 0 || column >= columns()</tt>.
   */
  def viewSorted(column: Int): IntMatrix2D = {
    cern.colt.matrix.tint.algo.IntSorting.mergeSort.sort(this, column)
  }

  /**
   * Constructs and returns a new <i>stride view</i> which is a sub matrix
   * consisting of every i-th cell. More specifically, the view has
   * <tt>this.rows()/rowStride</tt> rows and
   * <tt>this.columns()/columnStride</tt> columns holding cells
   * <tt>this.get(i*rowStride,j*columnStride)</tt> for all
   * <tt>i = 0..rows()/rowStride - 1, j = 0..columns()/columnStride - 1</tt>.
   * The returned view is backed by this matrix, so changes in the returned
   * view are reflected in this matrix, and vice-versa.
   *
   * @param rowStride
   *            the row step factor.
   * @param columnStride
   *            the column step factor.
   * @return a new view.
   * @throws IndexOutOfBoundsException
   *             if <tt>rowStride<=0 || columnStride<=0</tt>.
   */
  def viewStrides(rowStride: Int, columnStride: Int): IntMatrix2D = {
    (view().vStrides(rowStride, columnStride)).asInstanceOf[IntMatrix2D]
  }

  /**
   * Linear algebraic matrix-vector multiplication; <tt>z = A * y</tt>;
   * Equivalent to <tt>return A.zMult(y,z,1,0);</tt>
   */
  def zMult(y: IntMatrix1D, z: IntMatrix1D): IntMatrix1D = {
    zMult(y, z, 1, (if (z == null) 1 else 0), false)
  }

  /**
   * Linear algebraic matrix-vector multiplication;
   * <tt>z = alpha * A * y + beta*z</tt>.
   * <tt>z[i] = alpha*Sum(A[i,j] * y[j]) + beta*z[i], i=0..A.rows()-1, j=0..y.size()-1</tt>
   * . Where <tt>A == this</tt>. <br>
   * Note: Matrix shape conformance is checked <i>after</i> potential
   * transpositions.
   *
   * @param y
   *            the source vector.
   * @param z
   *            the vector where results are to be stored. Set this parameter
   *            to <tt>null</tt> to indicate that a new result vector shall be
   *            constructed.
   * @return z (for convenience only).
   *
   * @throws IllegalArgumentException
   *             if <tt>A.columns() != y.size() || A.rows() > z.size())</tt>.
   */
  def zMult(y: IntMatrix1D,
      z: IntMatrix1D,
      alpha: Int,
      beta: Int,
      transposeA: Boolean): IntMatrix1D = {
    if (transposeA) return viewDice().zMult(y, z, alpha, beta, false)
    var z_loc: IntMatrix1D = null
    z_loc = if (z == null) new DenseIntMatrix1D(this.rows) else z
    if (columns != y.size || rows > z_loc.size) throw new IllegalArgumentException("Incompatible args: " + toShapeString() + ", " + y.toShapeString() +
      ", " +
      z_loc.toShapeString())
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
            for (r <- firstRow until lastRow) {
              var s = 0
              for (c <- 0 until columns) {
                s += getQuick(r, c) * y.getQuick(c)
              }
              z_loc.setQuick(r, alpha * s + beta * z_loc.getQuick(r))
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
    } else {
      for (r <- 0 until rows) {
        var s = 0
        for (c <- 0 until columns) {
          s += getQuick(r, c) * y.getQuick(c)
        }
        z_loc.setQuick(r, alpha * s + beta * z_loc.getQuick(r))
      }
    }
    z_loc
  }

  /**
   * Linear algebraic matrix-matrix multiplication; <tt>C = A x B</tt>;
   * Equivalent to <tt>A.zMult(B,C,1,0,false,false)</tt>.
   */
  def zMult(B: IntMatrix2D, C: IntMatrix2D): IntMatrix2D = {
    zMult(B, C, 1, (if (C == null) 1 else 0), false, false)
  }

  /**
   * Linear algebraic matrix-matrix multiplication;
   * <tt>C = alpha * A x B + beta*C</tt>.
   * <tt>C[i,j] = alpha*Sum(A[i,k] * B[k,j]) + beta*C[i,j], k=0..n-1</tt>. <br>
   * Matrix shapes: <tt>A(m x n), B(n x p), C(m x p)</tt>. <br>
   * Note: Matrix shape conformance is checked <i>after</i> potential
   * transpositions.
   *
   * @param B
   *            the second source matrix.
   * @param C
   *            the matrix where results are to be stored. Set this parameter
   *            to <tt>null</tt> to indicate that a new result matrix shall be
   *            constructed.
   * @return C (for convenience only).
   *
   * @throws IllegalArgumentException
   *             if <tt>B.rows() != A.columns()</tt>.
   * @throws IllegalArgumentException
   *             if
   *             <tt>C.rows() != A.rows() || C.columns() != B.columns()</tt>.
   * @throws IllegalArgumentException
   *             if <tt>A == C || B == C</tt>.
   */
  def zMult(B: IntMatrix2D,
      C: IntMatrix2D,
      alpha: Int,
      beta: Int,
      transposeA: Boolean,
      transposeB: Boolean): IntMatrix2D = {
    if (transposeA) return viewDice().zMult(B, C, alpha, beta, false, transposeB)
    if (transposeB) return this.zMult(B.viewDice(), C, alpha, beta, transposeA, false)
    val m = rows
    val n = columns
    val p = B.columns
    var C_loc: IntMatrix2D = null
    C_loc = if (C == null) new DenseIntMatrix2D(m, p) else C
    if (B.rows != n) throw new IllegalArgumentException("Matrix2D inner dimensions must agree:" + toShapeString() +
      ", " +
      B.toShapeString())
    if (C_loc.rows != m || C_loc.columns != p) throw new IllegalArgumentException("Incompatibe result matrix: " + toShapeString() + ", " +
      B.toShapeString() +
      ", " +
      C_loc.toShapeString())
    if (this == C_loc || B == C_loc) throw new IllegalArgumentException("Matrices must not be identical")
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_2D)) {
      nthreads = Math.min(nthreads, p)
      val futures = Array.ofDim[Future](nthreads)
      val k = p / nthreads
      for (j <- 0 until nthreads) {
        val firstIdx = j * k
        val lastIdx = if ((j == nthreads - 1)) p else firstIdx + k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            for (a <- firstIdx until lastIdx; b <- 0 until m) {
              var s = 0
              for (c <- 0 until n) {
                s += getQuick(b, c) * B.getQuick(c, a)
              }
              C_loc.setQuick(b, a, alpha * s + beta * C_loc.getQuick(b, a))
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
    } else {
      for (a <- 0 until p; b <- 0 until m) {
        var s = 0
        for (c <- 0 until n) {
          s += getQuick(b, c) * B.getQuick(c, a)
        }
        C_loc.setQuick(b, a, alpha * s + beta * C_loc.getQuick(b, a))
      }
    }
    C_loc
  }

  /**
   * Returns the sum of all cells; <tt>Sum( x[i,j] )</tt>.
   *
   * @return the sum.
   */
  def zSum(): Int = {
    if (size == 0) return 0
    aggregate(cern.jet.math.tint.IntFunctions.plus, cern.jet.math.tint.IntFunctions.identity)
  }
}
