package cern.colt.matrix.tint

import java.util.concurrent.Callable
import java.util.concurrent.ExecutionException
import java.util.concurrent.Future
import cern.colt.list.tint.IntArrayList
import cern.colt.matrix.AbstractMatrix1D
import edu.emory.mathcs.utils.ConcurrencyUtils
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Abstract base class for 1-d matrices (aka <i>vectors</i>) holding
 * <tt>int</tt> elements. First see the <a href="package-summary.html">package
 * summary</a> and javadoc <a href="package-tree.html">tree view</a> to get the
 * broad picture.
 * <p>
 * A matrix has a number of cells (its <i>size</i>), which are assigned upon
 * instance construction. Elements are accessed via zero based indexes. Legal
 * indexes are of the form <tt>[0..size()-1]</tt>. Any attempt to access an
 * element at a coordinate <tt>index&lt;0 || index&gt;=size()</tt> will throw an
 * <tt>IndexOutOfBoundsException</tt>.
 *
 * @author wolfgang.hoschek@cern.ch
 * @version 1.0, 09/24/99
 *
 * @author Piotr Wendykier (piotr.wendykier@gmail.com)
 *
 */
@SerialVersionUID(1L)
abstract class IntMatrix1D protected () extends AbstractMatrix1D {

  /**
   * Applies a function to each cell and aggregates the results. Returns a
   * value <tt>v</tt> such that <tt>v==a(size())</tt> where
   * <tt>a(i) == aggr( a(i-1), f(get(i)) )</tt> and terminators are
   * <tt>a(1) == f(get(0)), a(0)==Int.NaN</tt>.
   * <p>
   * <b>Example:</b>
   *
   * <pre>
   *   cern.jet.math.Functions F = cern.jet.math.Functions.functions;
   *   matrix = 0 1 2 3
   *
   *   // Sum( x[i]*x[i] )
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
    var a = f.apply(getQuick(0))
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_1D)) {
      nthreads = Math.min(nthreads, size)
      val futures = Array.ofDim[Future](nthreads)
      val k = size / nthreads
      for (j <- 0 until nthreads) {
        val firstIdx = j * k
        val lastIdx = if ((j == nthreads - 1)) size else firstIdx + k
        futures(j) = ConcurrencyUtils.submit(new Callable[Integer]() {

          def call(): java.lang.Integer = {
            var a = f.apply(getQuick(firstIdx))
            for (i <- firstIdx + 1 until lastIdx) {
              a = aggr.apply(a, f.apply(getQuick(i)))
            }
            return a
          }
        })
      }
      a = ConcurrencyUtils.waitForCompletion(futures, aggr)
    } else {
      for (i <- 1 until size) {
        a = aggr.apply(a, f.apply(getQuick(i)))
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
   * @param indexList
   *            indexes.
   *
   * @return the aggregated measure.
   * @see cern.jet.math.tint.IntFunctions
   */
  def aggregate(aggr: cern.colt.function.tint.IntIntFunction, f: cern.colt.function.tint.IntFunction, indexList: IntArrayList): Int = {
    if (size == 0) throw new IllegalArgumentException("size == 0")
    val size = indexList.size
    val indexElements = indexList.elements()
    var a = 0
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_1D)) {
      nthreads = Math.min(nthreads, size)
      val futures = Array.ofDim[Future](nthreads)
      val k = size / nthreads
      for (j <- 0 until nthreads) {
        val firstIdx = j * k
        val lastIdx = if ((j == nthreads - 1)) size else firstIdx + k
        futures(j) = ConcurrencyUtils.submit(new Callable[Integer]() {

          def call(): java.lang.Integer = {
            var a = f.apply(getQuick(indexElements(firstIdx)))
            var elem: Int = 0
            for (i <- firstIdx + 1 until lastIdx) {
              elem = getQuick(indexElements(i))
              a = aggr.apply(a, f.apply(elem))
            }
            return a
          }
        })
      }
      a = ConcurrencyUtils.waitForCompletion(futures, aggr)
    } else {
      var elem: Int = 0
      a = f.apply(getQuick(indexElements(0)))
      for (i <- 1 until size) {
        elem = getQuick(indexElements(i))
        a = aggr.apply(a, f.apply(elem))
      }
    }
    a
  }

  /**
   * Applies a function to each corresponding cell of two matrices and
   * aggregates the results. Returns a value <tt>v</tt> such that
   * <tt>v==a(size())</tt> where
   * <tt>a(i) == aggr( a(i-1), f(get(i),other.get(i)) )</tt> and terminators
   * are <tt>a(1) == f(get(0),other.get(0)), a(0)==Int.NaN</tt>.
   * <p>
   * <b>Example:</b>
   *
   * <pre>
   *   cern.jet.math.Functions F = cern.jet.math.Functions.functions;
   *   x = 0 1 2 3
   *   y = 0 1 2 3
   *
   *   // Sum( x[i]*y[i] )
   *   x.aggregate(y, F.plus, F.mult);
   *   --&gt; 14
   *
   *   // Sum( (x[i]+y[i])&circ;2 )
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
   *             if <tt>size() != other.size()</tt>.
   * @see cern.jet.math.tint.IntFunctions
   */
  def aggregate(other: IntMatrix1D, aggr: cern.colt.function.tint.IntIntFunction, f: cern.colt.function.tint.IntIntFunction): Int = {
    checkSize(other)
    if (size == 0) throw new IllegalArgumentException("size == 0")
    var a = f.apply(getQuick(0), other.getQuick(0))
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_1D)) {
      nthreads = Math.min(nthreads, size)
      val futures = Array.ofDim[Future](nthreads)
      val k = size / nthreads
      for (j <- 0 until nthreads) {
        val firstIdx = j * k
        val lastIdx = if ((j == nthreads - 1)) size else firstIdx + k
        futures(j) = ConcurrencyUtils.submit(new Callable[Integer]() {

          def call(): java.lang.Integer = {
            var a = f.apply(getQuick(firstIdx), other.getQuick(firstIdx))
            for (i <- firstIdx + 1 until lastIdx) {
              a = aggr.apply(a, f.apply(getQuick(i), other.getQuick(i)))
            }
            return a
          }
        })
      }
      a = ConcurrencyUtils.waitForCompletion(futures, aggr)
    } else {
      for (i <- 1 until size) {
        a = aggr.apply(a, f.apply(getQuick(i), other.getQuick(i)))
      }
    }
    a
  }

  /**
   * Assigns the result of a function to each cell;
   * <tt>x[i] = function(x[i])</tt>. (Iterates downwards from
   * <tt>[size()-1]</tt> to <tt>[0]</tt>).
   * <p>
   * <b>Example:</b>
   *
   * <pre>
   *   // change each cell to its sine
   *   matrix =   0.5      1.5      2.5       3.5
   *   matrix.assign(cern.jet.math.Functions.sin);
   *   --&gt;
   *   matrix ==  0.479426 0.997495 0.598472 -0.350783
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
  def assign(f: cern.colt.function.tint.IntFunction): IntMatrix1D = {
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_1D)) {
      nthreads = Math.min(nthreads, size)
      val futures = Array.ofDim[Future](nthreads)
      val k = size / nthreads
      for (j <- 0 until nthreads) {
        val firstIdx = j * k
        val lastIdx = if ((j == nthreads - 1)) size else firstIdx + k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            for (i <- firstIdx until lastIdx) {
              setQuick(i, f.apply(getQuick(i)))
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
    } else {
      for (i <- 0 until size) {
        setQuick(i, f.apply(getQuick(i)))
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
  def assign(cond: cern.colt.function.tint.IntProcedure, f: cern.colt.function.tint.IntFunction): IntMatrix1D = {
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_1D)) {
      nthreads = Math.min(nthreads, size)
      val futures = Array.ofDim[Future](nthreads)
      val k = size / nthreads
      for (j <- 0 until nthreads) {
        val firstIdx = j * k
        val lastIdx = if ((j == nthreads - 1)) size else firstIdx + k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            var elem: Int = 0
            for (i <- firstIdx until lastIdx) {
              elem = getQuick(i)
              if (cond.apply(elem) == true) {
                setQuick(i, f.apply(elem))
              }
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
    } else {
      var elem: Int = 0
      for (i <- 0 until size) {
        elem = getQuick(i)
        if (cond.apply(elem) == true) {
          setQuick(i, f.apply(elem))
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
  def assign(cond: cern.colt.function.tint.IntProcedure, value: Int): IntMatrix1D = {
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_1D)) {
      nthreads = Math.min(nthreads, size)
      val futures = Array.ofDim[Future](nthreads)
      val k = size / nthreads
      for (j <- 0 until nthreads) {
        val firstIdx = j * k
        val lastIdx = if ((j == nthreads - 1)) size else firstIdx + k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            var elem: Int = 0
            for (i <- firstIdx until lastIdx) {
              elem = getQuick(i)
              if (cond.apply(elem) == true) {
                setQuick(i, value)
              }
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
    } else {
      var elem: Int = 0
      for (i <- 0 until size) {
        elem = getQuick(i)
        if (cond.apply(elem) == true) {
          setQuick(i, value)
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
  def assign(value: Int): IntMatrix1D = {
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_1D)) {
      nthreads = Math.min(nthreads, size)
      val futures = Array.ofDim[Future](nthreads)
      val k = size / nthreads
      for (j <- 0 until nthreads) {
        val firstIdx = j * k
        val lastIdx = if ((j == nthreads - 1)) size else firstIdx + k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            for (i <- firstIdx until lastIdx) {
              setQuick(i, value)
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
    } else {
      for (i <- 0 until size) {
        setQuick(i, value)
      }
    }
    this
  }

  /**
   * Sets all cells to the state specified by <tt>values</tt>. <tt>values</tt>
   * is required to have the same number of cells as the receiver.
   * <p>
   * The values are copied. So subsequent changes in <tt>values</tt> are not
   * reflected in the matrix, and vice-versa.
   *
   * @param values
   *            the values to be filled into the cells.
   * @return <tt>this</tt> (for convenience only).
   * @throws IllegalArgumentException
   *             if <tt>values.length != size()</tt>.
   */
  def assign(values: Array[Int]): IntMatrix1D = {
    if (values.length != size) throw new IllegalArgumentException("Must have same number of cells: length=" + values.length +
      "size()=" +
      size)
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_1D)) {
      nthreads = Math.min(nthreads, size)
      val futures = Array.ofDim[Future](nthreads)
      val k = size / nthreads
      for (j <- 0 until nthreads) {
        val firstIdx = j * k
        val lastIdx = if ((j == nthreads - 1)) size else firstIdx + k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            for (i <- firstIdx until lastIdx) {
              setQuick(i, values(i))
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
    } else {
      for (i <- 0 until size) {
        setQuick(i, values(i))
      }
    }
    this
  }

  /**
   * Replaces all cell values of the receiver with the values of another
   * matrix. Both matrices must have the same size. If both matrices share the
   * same cells (as is the case if they are views derived from the same
   * matrix) and intersect in an ambiguous way, then replaces <i>as if</i>
   * using an intermediate auxiliary deep copy of <tt>other</tt>.
   *
   * @param other
   *            the source matrix to copy from (may be identical to the
   *            receiver).
   * @return <tt>this</tt> (for convenience only).
   * @throws IllegalArgumentException
   *             if <tt>size() != other.size()</tt>.
   */
  def assign(other: IntMatrix1D): IntMatrix1D = {
    if (other == this) return this
    checkSize(other)
    var other_loc: IntMatrix1D = null
    other_loc = if (haveSharedCells(other)) other.copy() else other
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_1D)) {
      nthreads = Math.min(nthreads, size)
      val futures = Array.ofDim[Future](nthreads)
      val k = size / nthreads
      for (j <- 0 until nthreads) {
        val firstIdx = j * k
        var lastIdx: Int = 0
        lastIdx = if (j == nthreads - 1) size else firstIdx + k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            for (i <- firstIdx until lastIdx) {
              setQuick(i, other_loc.getQuick(i))
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
    } else {
      for (i <- 0 until size) {
        setQuick(i, other_loc.getQuick(i))
      }
    }
    this
  }

  /**
   * Assigns the result of a function to each cell;
   * <tt>x[i] = function(x[i],y[i])</tt>.
   * <p>
   * <b>Example:</b>
   *
   * <pre>
   *   // assign x[i] = x[i]&lt;sup&gt;y[i]&lt;/sup&gt;
   *   m1 = 0 1 2 3;
   *   m2 = 0 2 4 6;
   *   m1.assign(m2, cern.jet.math.Functions.pow);
   *   --&gt;
   *   m1 == 1 1 16 729
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
   *             if <tt>size() != y.size()</tt>.
   * @see cern.jet.math.tint.IntFunctions
   */
  def assign(y: IntMatrix1D, function: cern.colt.function.tint.IntIntFunction): IntMatrix1D = {
    checkSize(y)
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_1D)) {
      nthreads = Math.min(nthreads, size)
      val futures = Array.ofDim[Future](nthreads)
      val k = size / nthreads
      for (j <- 0 until nthreads) {
        val firstIdx = j * k
        val lastIdx = if ((j == nthreads - 1)) size else firstIdx + k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            for (i <- firstIdx until lastIdx) {
              setQuick(i, function.apply(getQuick(i), y.getQuick(i)))
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
    } else {
      for (i <- 0 until size) {
        setQuick(i, function.apply(getQuick(i), y.getQuick(i)))
      }
    }
    this
  }

  /**
   * Assigns the result of a function to each cell;
   * <tt>x[i] = function(x[i],y[i])</tt>. (Iterates downwards from
   * <tt>[size()-1]</tt> to <tt>[0]</tt>).
   * <p>
   * <b>Example:</b>
   *
   * <pre>
   *   // assign x[i] = x[i]&lt;sup&gt;y[i]&lt;/sup&gt;
   *   m1 = 0 1 2 3;
   *   m2 = 0 2 4 6;
   *   m1.assign(m2, cern.jet.math.Functions.pow);
   *   --&gt;
   *   m1 == 1 1 16 729
   *
   *   // for non-standard functions there is no shortcut:
   *   m1.assign(m2,
   *      new IntIntFunction() {
   *         public int apply(int x, int y) { return Math.pow(x,y); }
   *      }
   *   );
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
   *            cell's value of <tt>y</tt>.
   * @param nonZeroIndexes
   *            list of indexes of non-zero values
   * @return <tt>this</tt> (for convenience only).
   * @throws IllegalArgumentException
   *             if <tt>size() != y.size()</tt>.
   * @see cern.jet.math.tint.IntFunctions
   */
  def assign(y: IntMatrix1D, function: cern.colt.function.tint.IntIntFunction, nonZeroIndexes: cern.colt.list.tint.IntArrayList): IntMatrix1D = {
    checkSize(y)
    val nonZeroElements = nonZeroIndexes.elements()
    if (function == cern.jet.math.tint.IntFunctions.mult) {
      var j = 0
      var index = nonZeroIndexes.size
      while (index >= 0) {
        val i = nonZeroElements(index)
        while (j < i) {setQuick(j, 0)j += 1
        }
        setQuick(i, getQuick(i) * y.getQuick(i))
        j += 1
      }
    } else if (function.isInstanceOf[cern.jet.math.tint.IntPlusMultSecond]) {
      val multiplicator = function.asInstanceOf[cern.jet.math.tint.IntPlusMultSecond].multiplicator
      if (multiplicator == 0) {
        return this
      } else if (multiplicator == 1) {
        var index = nonZeroIndexes.size
        while (index >= 0) {
          val i = nonZeroElements(index)
          setQuick(i, getQuick(i) + y.getQuick(i))
        }
      } else if (multiplicator == -1) {
        var index = nonZeroIndexes.size
        while (index >= 0) {
          val i = nonZeroElements(index)
          setQuick(i, getQuick(i) - y.getQuick(i))
        }
      } else {
        var index = nonZeroIndexes.size
        while (index >= 0) {
          val i = nonZeroElements(index)
          setQuick(i, getQuick(i) + multiplicator * y.getQuick(i))
        }
      }
    } else {
      return assign(y, function)
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
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_1D)) {
      nthreads = Math.min(nthreads, size)
      val futures = Array.ofDim[Future](nthreads)
      val results = Array.ofDim[Integer](nthreads)
      val k = size / nthreads
      for (j <- 0 until nthreads) {
        val firstIdx = j * k
        val lastIdx = if ((j == nthreads - 1)) size else firstIdx + k
        futures(j) = ConcurrencyUtils.submit(new Callable[Integer]() {

          def call(): java.lang.Integer = {
            var cardinality = 0
            for (i <- firstIdx until lastIdx if getQuick(i) != 0) cardinality += 1
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
      for (i <- 0 until size if getQuick(i) != 0) cardinality += 1
    }
    cardinality
  }

  /**
   * Returns the number of cells having non-zero values, but at most
   * maxCardinality; ignores tolerance.
   */
  protected def cardinality(maxCardinality: Int): Int = {
    var cardinality = 0
    val i = size
    while (i >= 0 && cardinality < maxCardinality) {
      if (getQuick(i) != 0) cardinality += 1
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
  def copy(): IntMatrix1D = {
    val copy = like()
    copy.assign(this)
    copy
  }

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
   * and is at least a <code>IntMatrix1D</code> object that has the same sizes
   * as the receiver and has exactly the same values at the same indexes.
   *
   * @param obj
   *            the object to compare with.
   * @return <code>true</code> if the objects are the same; <code>false</code>
   *         otherwise.
   */
  override def equals(obj: Any): Boolean = {
    if (this == obj) return true
    if (obj == null) return false
    if (!(obj.isInstanceOf[IntMatrix1D])) return false
    cern.colt.matrix.tint.algo.IntProperty.DEFAULT.==(this, obj.asInstanceOf[IntMatrix1D])
  }

  /**
   * Returns the matrix cell value at coordinate <tt>index</tt>.
   *
   * @param index
   *            the index of the cell.
   * @return the value of the specified cell.
   * @throws IndexOutOfBoundsException
   *             if <tt>index&lt;0 || index&gt;=size()</tt>.
   */
  def get(index: Int): Int = {
    if (index < 0 || index >= size) checkIndex(index)
    getQuick(index)
  }

  /**
   * Returns the content of this matrix if it is a wrapper; or <tt>this</tt>
   * otherwise. Override this method in wrappers.
   */
  protected def getStorageMatrix(): IntMatrix1D = this

  /**
   * Fills the coordinates and values of cells having negative values into the
   * specified lists. Fills into the lists, starting at index 0. After this
   * call returns the specified lists all have a new size, the number of
   * non-zero values.
   *
   * @param indexList
   *            the list to be filled with indexes, can have any size.
   * @param valueList
   *            the list to be filled with values, can have any size.
   */
  def getNegativeValues(indexList: IntArrayList, valueList: IntArrayList) {
    indexList.clear()
    valueList.clear()
    val rem = size % 2
    if (rem == 1) {
      val value = getQuick(0)
      if (value < 0) {
        indexList.add(0)
        valueList.add(value)
      }
    }
    var i = rem
    while (i < size) {
      var value = getQuick(i)
      if (value < 0) {
        indexList.add(i)
        valueList.add(value)
      }
      value = getQuick(i + 1)
      if (value < 0) {
        indexList.add(i + 1)
        valueList.add(value)
      }
      i += 2
    }
  }

  /**
   * Fills the coordinates and values of cells having non-zero values into the
   * specified lists. Fills into the lists, starting at index 0. After this
   * call returns the specified lists all have a new size, the number of
   * non-zero values.
   * <p>
   * In general, fill order is <i>unspecified</i>. This implementation fills
   * like: <tt>for (index = 0..size()-1)  do ... </tt>. However, subclasses
   * are free to us any other order, even an order that may change over time
   * as cell values are changed. (Of course, result lists indexes are
   * guaranteed to correspond to the same cell).
   * <p>
   * <b>Example:</b> <br>
   *
   * <pre>
   *   0, 0, 8, 0, 7
   *   --&gt;
   *   indexList  = (2,4)
   *   valueList  = (8,7)
   *
   * </pre>
   *
   * In other words, <tt>get(2)==8, get(4)==7</tt>.
   *
   * @param indexList
   *            the list to be filled with indexes, can have any size.
   * @param valueList
   *            the list to be filled with values, can have any size.
   */
  def getNonZeros(indexList: IntArrayList, valueList: IntArrayList) {
    indexList.clear()
    valueList.clear()
    val rem = size % 2
    if (rem == 1) {
      val value = getQuick(0)
      if (value != 0) {
        indexList.add(0)
        valueList.add(value)
      }
    }
    var i = rem
    while (i < size) {
      var value = getQuick(i)
      if (value != 0) {
        indexList.add(i)
        valueList.add(value)
      }
      value = getQuick(i + 1)
      if (value != 0) {
        indexList.add(i + 1)
        valueList.add(value)
      }
      i += 2
    }
  }

  /**
   * Fills the coordinates and values of the first <tt>maxCardinality</tt>
   * cells having non-zero values into the specified lists. Fills into the
   * lists, starting at index 0. After this call returns the specified lists
   * all have a new size, the number of non-zero values.
   * <p>
   * In general, fill order is <i>unspecified</i>. This implementation fills
   * like: <tt>for (index = 0..size()-1)  do ... </tt>. However, subclasses
   * are free to us any other order, even an order that may change over time
   * as cell values are changed. (Of course, result lists indexes are
   * guaranteed to correspond to the same cell).
   * <p>
   * <b>Example:</b> <br>
   *
   * <pre>
   *   0, 0, 8, 0, 7
   *   --&gt;
   *   indexList  = (2,4)
   *   valueList  = (8,7)
   *
   * </pre>
   *
   * In other words, <tt>get(2)==8, get(4)==7</tt>.
   *
   * @param indexList
   *            the list to be filled with indexes, can have any size.
   * @param valueList
   *            the list to be filled with values, can have any size.
   * @param maxCardinality
   *            maximal cardinality
   */
  def getNonZeros(indexList: IntArrayList, valueList: IntArrayList, maxCardinality: Int) {
    val fillIndexList = indexList != null
    val fillValueList = valueList != null
    if (fillIndexList) indexList.clear()
    if (fillValueList) valueList.clear()
    val s = size
    var currentSize = 0
    for (i <- 0 until s) {
      val value = getQuick(i)
      if (value != 0) {
        if (fillIndexList) indexList.add(i)
        if (fillValueList) valueList.add(value)
        currentSize += 1
      }
      if (currentSize >= maxCardinality) {
        //break
      }
    }
  }

  /**
   * Fills the coordinates and values of cells having positive values into the
   * specified lists. Fills into the lists, starting at index 0. After this
   * call returns the specified lists all have a new size, the number of
   * non-zero values.
   *
   * @param indexList
   *            the list to be filled with indexes, can have any size.
   * @param valueList
   *            the list to be filled with values, can have any size.
   */
  def getPositiveValues(indexList: IntArrayList, valueList: IntArrayList) {
    indexList.clear()
    valueList.clear()
    val rem = size % 2
    if (rem == 1) {
      val value = getQuick(0)
      if (value > 0) {
        indexList.add(0)
        valueList.add(value)
      }
    }
    var i = rem
    while (i < size) {
      var value = getQuick(i)
      if (value > 0) {
        indexList.add(i)
        valueList.add(value)
      }
      value = getQuick(i + 1)
      if (value > 0) {
        indexList.add(i + 1)
        valueList.add(value)
      }
      i += 2
    }
  }

  /**
   * Returns the matrix cell value at coordinate <tt>index</tt>.
   *
   * <p>
   * Provided with invalid parameters this method may return invalid objects
   * without throwing any exception. <b>You should only use this method when
   * you are absolutely sure that the coordinate is within bounds.</b>
   * Precondition (unchecked): <tt>index&lt;0 || index&gt;=size()</tt>.
   *
   * @param index
   *            the index of the cell.
   * @return the value of the specified cell.
   */
  def getQuick(index: Int): Int

  /**
   * Returns <tt>true</tt> if both matrices share at least one identical cell.
   */
  protected def haveSharedCells(other: IntMatrix1D): Boolean = {
    if (other == null) return false
    if (this == other) return true
    getStorageMatrix.haveSharedCellsRaw(other.getStorageMatrix)
  }

  /**
   * Returns <tt>true</tt> if both matrices share at least one identical cell.
   */
  protected def haveSharedCellsRaw(other: IntMatrix1D): Boolean = false

  /**
   * Construct and returns a new empty matrix <i>of the same dynamic type</i>
   * as the receiver, having the same size. For example, if the receiver is an
   * instance of type <tt>DenseIntMatrix1D</tt> the new matrix must also be of
   * type <tt>DenseIntMatrix1D</tt>, if the receiver is an instance of type
   * <tt>SparseIntMatrix1D</tt> the new matrix must also be of type
   * <tt>SparseIntMatrix1D</tt>, etc. In general, the new matrix should have
   * internal parametrization as similar as possible.
   *
   * @return a new empty matrix of the same dynamic type.
   */
  def like(): IntMatrix1D = like(size)

  /**
   * Construct and returns a new empty matrix <i>of the same dynamic type</i>
   * as the receiver, having the specified size. For example, if the receiver
   * is an instance of type <tt>DenseIntMatrix1D</tt> the new matrix must also
   * be of type <tt>DenseIntMatrix1D</tt>, if the receiver is an instance of
   * type <tt>SparseIntMatrix1D</tt> the new matrix must also be of type
   * <tt>SparseIntMatrix1D</tt>, etc. In general, the new matrix should have
   * internal parametrization as similar as possible.
   *
   * @param size
   *            the number of cell the matrix shall have.
   * @return a new empty matrix of the same dynamic type.
   */
  def like(size: Int): IntMatrix1D

  /**
   * Construct and returns a new 2-d matrix <i>of the corresponding dynamic
   * type</i>, entirelly independent of the receiver. For example, if the
   * receiver is an instance of type <tt>DenseIntMatrix1D</tt> the new matrix
   * must be of type <tt>DenseIntMatrix2D</tt>, if the receiver is an instance
   * of type <tt>SparseIntMatrix1D</tt> the new matrix must be of type
   * <tt>SparseIntMatrix2D</tt>, etc.
   *
   * @param rows
   *            the number of rows the matrix shall have.
   * @param columns
   *            the number of columns the matrix shall have.
   * @return a new matrix of the corresponding dynamic type.
   */
  def like2D(rows: Int, columns: Int): IntMatrix2D

  /**
   * Return the maximum value of this matrix together with its location
   *
   * @return { maximum_value, location };
   */
  def getMaxLocation(): Array[Int] = {
    var location = 0
    var maxValue = 0
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_1D)) {
      nthreads = Math.min(nthreads, size)
      val futures = Array.ofDim[Future](nthreads)
      val results = Array.ofDim[Int](nthreads, 2)
      val k = size / nthreads
      for (j <- 0 until nthreads) {
        val firstIdx = j * k
        val lastIdx = if ((j == nthreads - 1)) size else firstIdx + k
        futures(j) = ConcurrencyUtils.submit(new Callable[Array[Int]]() {

          def call(): Array[Int] = {
            var location = firstIdx
            var maxValue = getQuick(location)
            var elem: Int = 0
            for (i <- firstIdx + 1 until lastIdx) {
              elem = getQuick(i)
              if (maxValue < elem) {
                maxValue = elem
                location = i
              }
            }
            return Array(maxValue, location)
          }
        })
      }
      try {
        for (j <- 0 until nthreads) {
          results(j) = futures(j).get.asInstanceOf[Array[Int]]
        }
        maxValue = results(0)(0)
        location = results(0)(1).toInt
        for (j <- 1 until nthreads if maxValue < results(j)(0)) {
          maxValue = results(j)(0)
          location = results(j)(1).toInt
        }
      } catch {
        case ex: ExecutionException => ex.printStackTrace()
        case e: InterruptedException => e.printStackTrace()
      }
    } else {
      maxValue = getQuick(location)
      var elem: Int = 0
      for (i <- 1 until size) {
        elem = getQuick(i)
        if (maxValue < elem) {
          maxValue = elem
          location = i
        }
      }
    }
    Array(maxValue, location)
  }

  /**
   * Return the minimum value of this matrix together with its location
   *
   * @return { minimum_value, location };
   */
  def getMinLocation(): Array[Int] = {
    var location = 0
    var minValue = 0
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_1D)) {
      nthreads = Math.min(nthreads, size)
      val futures = Array.ofDim[Future](nthreads)
      val results = Array.ofDim[Int](nthreads, 2)
      val k = size / nthreads
      for (j <- 0 until nthreads) {
        val firstIdx = j * k
        val lastIdx = if ((j == nthreads - 1)) size else firstIdx + k
        futures(j) = ConcurrencyUtils.submit(new Callable[Array[Int]]() {

          def call(): Array[Int] = {
            var location = firstIdx
            var minValue = getQuick(location)
            var elem: Int = 0
            for (i <- firstIdx + 1 until lastIdx) {
              elem = getQuick(i)
              if (minValue > elem) {
                minValue = elem
                location = i
              }
            }
            return Array(minValue, location)
          }
        })
      }
      try {
        for (j <- 0 until nthreads) {
          results(j) = futures(j).get.asInstanceOf[Array[Int]]
        }
        minValue = results(0)(0)
        location = results(0)(1).toInt
        for (j <- 1 until nthreads if minValue > results(j)(0)) {
          minValue = results(j)(0)
          location = results(j)(1).toInt
        }
      } catch {
        case ex: ExecutionException => ex.printStackTrace()
        case e: InterruptedException => e.printStackTrace()
      }
    } else {
      minValue = getQuick(location)
      var elem: Int = 0
      for (i <- 1 until size) {
        elem = getQuick(i)
        if (minValue > elem) {
          minValue = elem
          location = i
        }
      }
    }
    Array(minValue, location)
  }

  /**
   * Returns new IntMatrix2D of size rows x columns whose elements are taken
   * column-wise from this matrix.
   *
   * @param rows
   *            number of rows
   * @param columns
   *            number of columns
   * @return new 2D matrix with columns being the elements of this matrix.
   */
  def reshape(rows: Int, columns: Int): IntMatrix2D

  /**
   * Returns new IntMatrix3D of size slices x rows x columns, whose elements
   * are taken column-wise from this matrix.
   *
   * @param rows
   *            number of rows
   * @param columns
   *            number of columns
   * @return new 2D matrix with columns being the elements of this matrix.
   */
  def reshape(slices: Int, rows: Int, columns: Int): IntMatrix3D

  /**
   * Sets the matrix cell at coordinate <tt>index</tt> to the specified value.
   *
   * @param index
   *            the index of the cell.
   * @param value
   *            the value to be filled into the specified cell.
   * @throws IndexOutOfBoundsException
   *             if <tt>index&lt;0 || index&gt;=size()</tt>.
   */
  def set(index: Int, value: Int) {
    if (index < 0 || index >= size) checkIndex(index)
    setQuick(index, value)
  }

  /**
   * Sets the matrix cell at coordinate <tt>index</tt> to the specified value.
   *
   * <p>
   * Provided with invalid parameters this method may access illegal indexes
   * without throwing any exception. <b>You should only use this method when
   * you are absolutely sure that the coordinate is within bounds.</b>
   * Precondition (unchecked): <tt>index&lt;0 || index&gt;=size()</tt>.
   *
   * @param index
   *            the index of the cell.
   * @param value
   *            the value to be filled into the specified cell.
   */
  def setQuick(index: Int, value: Int): Unit

  /**
   * Sets the size of this matrix.
   *
   * @param size
   */
  def setSize(size: Int) {
    this.size = size
  }

  /**
   * Swaps each element <tt>this[i]</tt> with <tt>other[i]</tt>.
   *
   * @throws IllegalArgumentException
   *             if <tt>size() != other.size()</tt>.
   */
  def swap(other: IntMatrix1D) {
    checkSize(other)
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_1D)) {
      nthreads = Math.min(nthreads, size)
      val futures = Array.ofDim[Future](nthreads)
      val k = size / nthreads
      for (j <- 0 until nthreads) {
        val firstIdx = j * k
        val lastIdx = if ((j == nthreads - 1)) size else firstIdx + k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            for (i <- firstIdx until lastIdx) {
              var tmp = getQuick(i)
              setQuick(i, other.getQuick(i))
              other.setQuick(i, tmp)
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
    } else {
      for (i <- 0 until size) {
        val tmp = getQuick(i)
        setQuick(i, other.getQuick(i))
        other.setQuick(i, tmp)
      }
    }
  }

  /**
   * Constructs and returns a 1-dimensional array containing the cell values.
   * The values are copied. So subsequent changes in <tt>values</tt> are not
   * reflected in the matrix, and vice-versa. The returned array
   * <tt>values</tt> has the form <br>
   * <tt>for (int i=0; i < size(); i++) values[i] = get(i);</tt>
   *
   * @return an array filled with the values of the cells.
   */
  def toArray(): Array[Int] = {
    val values = Array.ofDim[Int](size)
    toArray(values)
    values
  }

  /**
   * Fills the cell values into the specified 1-dimensional array. The values
   * are copied. So subsequent changes in <tt>values</tt> are not reflected in
   * the matrix, and vice-versa. After this call returns the array
   * <tt>values</tt> has the form <br>
   * <tt>for (int i=0; i < size(); i++) values[i] = get(i);</tt>
   *
   * @throws IllegalArgumentException
   *             if <tt>values.length < size()</tt>.
   */
  def toArray(values: Array[Int]) {
    if (values.length < size) throw new IllegalArgumentException("values too small")
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_1D)) {
      nthreads = Math.min(nthreads, size)
      val futures = Array.ofDim[Future](nthreads)
      val k = size / nthreads
      for (j <- 0 until nthreads) {
        val firstIdx = j * k
        val lastIdx = if ((j == nthreads - 1)) size else firstIdx + k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            for (i <- firstIdx until lastIdx) {
              values(i) = getQuick(i)
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
    } else {
      for (i <- 0 until size) {
        values(i) = getQuick(i)
      }
    }
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
  protected def view(): IntMatrix1D = clone().asInstanceOf[IntMatrix1D]

  /**
   * Constructs and returns a new <i>flip view</i>. What used to be index
   * <tt>0</tt> is now index <tt>size()-1</tt>, ..., what used to be index
   * <tt>size()-1</tt> is now index <tt>0</tt>. The returned view is backed by
   * this matrix, so changes in the returned view are reflected in this
   * matrix, and vice-versa.
   *
   * @return a new flip view.
   */
  def viewFlip(): IntMatrix1D = {
    (view().vFlip()).asInstanceOf[IntMatrix1D]
  }

  /**
   * Constructs and returns a new <i>sub-range view</i> that is a
   * <tt>width</tt> sub matrix starting at <tt>index</tt>.
   *
   * Operations on the returned view can only be applied to the restricted
   * range. Any attempt to access coordinates not contained in the view will
   * throw an <tt>IndexOutOfBoundsException</tt>.
   * <p>
   * <b>Note that the view is really just a range restriction:</b> The
   * returned matrix is backed by this matrix, so changes in the returned
   * matrix are reflected in this matrix, and vice-versa.
   * <p>
   * The view contains the cells from <tt>index..index+width-1</tt>. and has
   * <tt>view.size() == width</tt>. A view's legal coordinates are again zero
   * based, as usual. In other words, legal coordinates of the view are
   * <tt>0 .. view.size()-1==width-1</tt>. As usual, any attempt to access a
   * cell at other coordinates will throw an
   * <tt>IndexOutOfBoundsException</tt>.
   *
   * @param index
   *            The index of the first cell.
   * @param width
   *            The width of the range.
   * @throws IndexOutOfBoundsException
   *             if <tt>index<0 || width<0 || index+width>size()</tt>.
   * @return the new view.
   *
   */
  def viewPart(index: Int, width: Int): IntMatrix1D = {
    (view().vPart(index, width)).asInstanceOf[IntMatrix1D]
  }

  /**
   * Constructs and returns a new <i>selection view</i> that is a matrix
   * holding the cells matching the given condition. Applies the condition to
   * each cell and takes only those cells where
   * <tt>condition.apply(get(i))</tt> yields <tt>true</tt>.
   * <p>
   * <b>Example:</b> <br>
   *
   * <pre>
   *   // extract and view all cells with even value
   *   matrix = 0 1 2 3
   *   matrix.viewSelection(
   *      new IntProcedure() {
   *         public final boolean apply(int a) { return a % 2 == 0; }
   *      }
   *   );
   *   --&gt;
   *   matrix ==  0 2
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
  def viewSelection(condition: cern.colt.function.tint.IntProcedure): IntMatrix1D = {
    val matches = new IntArrayList()
    for (i <- 0 until size if condition.apply(getQuick(i))) matches.add(i)
    matches.trimToSize()
    viewSelection(matches.elements())
  }

  /**
   * Constructs and returns a new <i>selection view</i> that is a matrix
   * holding the indicated cells. There holds
   * <tt>view.size() == indexes.length</tt> and
   * <tt>view.get(i) == this.get(indexes[i])</tt>. Indexes can occur multiple
   * times and can be in arbitrary order.
   * <p>
   * <b>Example:</b> <br>
   *
   * <pre>
   *   this     = (0,0,8,0,7)
   *   indexes  = (0,2,4,2)
   *   --&gt;
   *   view     = (0,8,7,8)
   *
   * </pre>
   *
   * Note that modifying <tt>indexes</tt> after this call has returned has no
   * effect on the view. The returned view is backed by this matrix, so
   * changes in the returned view are reflected in this matrix, and
   * vice-versa.
   *
   * @param indexes
   *            The indexes of the cells that shall be visible in the new
   *            view. To indicate that <i>all</i> cells shall be visible,
   *            simply set this parameter to <tt>null</tt>.
   * @return the new view.
   * @throws IndexOutOfBoundsException
   *             if <tt>!(0 <= indexes[i] < size())</tt> for any
   *             <tt>i=0..indexes.length()-1</tt>.
   */
  def viewSelection(indexes: Array[Int]): IntMatrix1D = {
    if (indexes == null) {
      indexes = Array.ofDim[Int](size)
      for (i <- 0 until size) indexes(i) = i
    }
    checkIndexes(indexes)
    val offsets = Array.ofDim[Int](indexes.length)
    for (i <- 0 until indexes.length) {
      offsets(i) = index(indexes(i)).toInt
    }
    viewSelectionLike(offsets)
  }

  /**
   * Construct and returns a new selection view.
   *
   * @param offsets
   *            the offsets of the visible elements.
   * @return a new view.
   */
  protected def viewSelectionLike(offsets: Array[Int]): IntMatrix1D

  /**
   * Sorts the vector into ascending order, according to the <i>natural
   * ordering</i>. This sort is guaranteed to be <i>stable</i>. For further
   * information, see
   * {@link cern.colt.matrix.tint.algo.IntSorting#sort(IntMatrix1D)}. For more
   * advanced sorting functionality, see
   * {@link cern.colt.matrix.tint.algo.IntSorting}.
   *
   * @return a new sorted vector (matrix) view.
   */
  def viewSorted(): IntMatrix1D = {
    cern.colt.matrix.tint.algo.IntSorting.mergeSort.sort(this)
  }

  /**
   * Constructs and returns a new <i>stride view</i> which is a sub matrix
   * consisting of every i-th cell. More specifically, the view has size
   * <tt>this.size()/stride</tt> holding cells <tt>this.get(i*stride)</tt> for
   * all <tt>i = 0..size()/stride - 1</tt>.
   *
   * @param stride
   *            the step factor.
   * @throws IndexOutOfBoundsException
   *             if <tt>stride <= 0</tt>.
   * @return the new view.
   *
   */
  def viewStrides(stride: Int): IntMatrix1D = {
    (view().vStrides(stride)).asInstanceOf[IntMatrix1D]
  }

  /**
   * Returns the dot product of two vectors x and y, which is
   * <tt>Sum(x[i]*y[i])</tt>. Where <tt>x == this</tt>. Operates on cells at
   * indexes <tt>0 .. Math.min(size(),y.size())</tt>.
   *
   * @param y
   *            the second vector.
   * @return the sum of products.
   */
  def zDotProduct(y: IntMatrix1D): Int = zDotProduct(y, 0, size)

  /**
   * Returns the dot product of two vectors x and y, which is
   * <tt>Sum(x[i]*y[i])</tt>. Where <tt>x == this</tt>. Operates on cells at
   * indexes <tt>from .. Min(size(),y.size(),from+length)-1</tt>.
   *
   * @param y
   *            the second vector.
   * @param from
   *            the first index to be considered.
   * @param length
   *            the number of cells to be considered.
   * @return the sum of products; zero if <tt>from<0 || length<0</tt>.
   */
  def zDotProduct(y: IntMatrix1D, from: Int, length: Int): Int = {
    if (from < 0 || length <= 0) return 0
    var tail = from + length
    if (size < tail) tail = size
    if (y.size < tail) tail = y.size
    length = tail - from
    var sum = 0
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_1D)) {
      nthreads = Math.min(nthreads, length)
      val futures = Array.ofDim[Future](nthreads)
      val results = Array.ofDim[Integer](nthreads)
      val k = length / nthreads
      for (j <- 0 until nthreads) {
        val firstIdx = j * k
        val lastIdx = if ((j == nthreads - 1)) length else firstIdx + k
        futures(j) = ConcurrencyUtils.submit(new Callable[Integer]() {

          def call(): java.lang.Integer = {
            var sum = 0
            var idx: Int = 0
            for (k <- firstIdx until lastIdx) {
              idx = k + from
              sum += getQuick(idx) * y.getQuick(idx)
            }
            return sum
          }
        })
      }
      try {
        for (j <- 0 until nthreads) {
          results(j) = futures(j).get.asInstanceOf[java.lang.Integer]
        }
        sum = results(0)
        for (j <- 1 until nthreads) {
          sum += results(j)
        }
      } catch {
        case ex: ExecutionException => ex.printStackTrace()
        case e: InterruptedException => e.printStackTrace()
      }
    } else {
      var i = tail - 1
      var k = length
      while (k >= 0) {
        sum += getQuick(i) * y.getQuick(i)
        i -= 1
      }
    }
    sum
  }

  /**
   * Returns the dot product of two vectors x and y, which is
   * <tt>Sum(x[i]*y[i])</tt>. Where <tt>x == this</tt>.
   *
   * @param y
   *            the second vector.
   * @param nonZeroIndexes
   *            the indexes of cells in <tt>y</tt>having a non-zero value.
   * @return the sum of products.
   */
  def zDotProduct(y: IntMatrix1D,
      from: Int,
      length: Int,
      nonZeroIndexes: IntArrayList): Int = {
    if (from < 0 || length <= 0) return 0
    var tail = from + length
    if (size < tail) tail = size
    if (y.size < tail) tail = y.size
    length = tail - from
    if (length <= 0) return 0
    val indexesCopy = nonZeroIndexes.copy()
    indexesCopy.trimToSize()
    indexesCopy.quickSort()
    val nonZeroIndexElements = indexesCopy.elements()
    var index = 0
    val s = indexesCopy.size
    while ((index < s) && nonZeroIndexElements(index) < from) index += 1
    var i: Int = 0
    var sum = 0
    while ((length >= 0) && (index < s) && ((i = nonZeroIndexElements(index)) < tail)) {
      sum += getQuick(i) * y.getQuick(i)
      index += 1
    }
    sum
  }

  /**
   * Returns the dot product of two vectors x and y, which is
   * <tt>Sum(x[i]*y[i])</tt>. Where <tt>x == this</tt>.
   *
   * @param y
   *            the second vector.
   * @param nonZeroIndexes
   *            the indexes of cells in <tt>y</tt>having a non-zero value.
   * @return the sum of products.
   */
  protected def zDotProduct(y: IntMatrix1D, nonZeroIndexes: IntArrayList): Int = zDotProduct(y, 0, size, nonZeroIndexes)

  /**
   * Returns the sum of all cells; <tt>Sum( x[i] )</tt>.
   *
   * @return the sum.
   */
  def zSum(): Int = {
    if (size == 0) return 0
    aggregate(cern.jet.math.tint.IntFunctions.plus, cern.jet.math.tint.IntFunctions.identity)
  }
}
