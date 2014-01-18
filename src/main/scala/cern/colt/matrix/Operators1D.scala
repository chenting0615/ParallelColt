package cern.colt.matrix

import cern.jet.math.NumericFunctions
import hep.aida.bin.StaticBin1D
import cern.colt.matrix.MatrixTypes._
import cern.colt.matrix.impl.WrappedDiagonalMatrix2D
import cern.colt.function._
import cern.colt.function.ProcedureTypes._
import cern.colt.list.ArrayTypes._
import cern.colt.function.FunctionTypes.IntFunction
import cern.colt.matrix.MatrixNumeric._
import cern.colt.matrix.algo.MatrixMultiply

case class Operators1D[@specialized T: Manifest: MatrixNumeric: MatrixMultiply](m: Matrix1D[T]) {

  val numeric = implicitly[MatrixNumeric[T]]
  val multiplyEngine = implicitly[MatrixMultiply[T]]

  /**
   * Applies a function to each cell and aggregates the results. Returns a
   * value <tt>v</tt> such that <tt>v==a(size())</tt> where
   * <tt>a(i) == aggr( a(i-1), f(get(row,column)) )</tt> and terminators are
   * <tt>a(1) == f(get(0,0)), a(0)==Double.NaN</tt>.
   * <p>
   * <b>Example:</b>
   *
   * <pre>
   * 	 cern.jet.math.Functions F = cern.jet.math.Functions.functions;
   * 	 2 x 2 matrix
   * 	 0 1
   * 	 2 3
   *
   * 	 // Sum( x[row,col]*x[row,col] )
   * 	 matrix.aggregate(F.plus,F.square);
   * 	 --&gt; 14
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
   * @param cellFunction
   *            a function transforming the current cell value.
   * @return the aggregated measure.
   */
  def aggregate(aggr: Function2[T, T, T], cellFunction: Function1[T, T]): T = {
    if (m.size == 0)
      m.numeric.zero
    else
      MatrixProcessor.singleton.aggregateCells(m, aggr, cellFunction, null)
  }

  /**
   * Applies a function to each cell that satisfies a condition and aggregates
   * the results.
   *
   * @param aggr
   *            an aggregation function taking as first argument the current
   *            aggregation and as second argument the transformed current
   *            cell value.
   * @param cellFunction
   *            a function transforming the current cell value.
   * @param cond
   *            a condition.
   * @return the aggregated measure.
   */
  def aggregate(aggr: Function2[T, T, T], cellFunction: Function2[Int, T, T], cond: Procedure1[T]): T = {
    if (m.size == 0)
      m.numeric.zero
    else
      MatrixProcessor.singleton.aggregateCells(m, aggr, cellFunction, cond)
  }

  /**
   * Applies a function to each corresponding cell of two matrices and
   * aggregates the results. Returns a value <tt>v</tt> such that
   * <tt>v==a(size())</tt> where
   * <tt>a(i) == aggr( a(i-1), f(get(row,column),other.get(row,column)) )</tt>
   * and terminators are
   * <tt>a(1) == f(get(0,0),other.get(0,0)), a(0)==Double.NaN</tt>.
   * <p>
   * <b>Example:</b>
   *
   * <pre>
   * 	 cern.jet.math.Functions F = cern.jet.math.Functions.functions;
   * 	 x == 2 x 2 matrix
   * 	 0 1
   * 	 2 3
   *
   * 	 y == 2 x 2 matrix
   * 	 0 1
   * 	 2 3
   *
   * 	 // Sum( x[row,col] * y[row,col] )
   * 	 x.aggregate(y, F.plus, F.mult);
   * 	 --&gt; 14
   *
   * 	 // Sum( (x[row,col] + y[row,col])&circ;2 )
   * 	 x.aggregate(y, F.plus, F.chain(F.square,F.plus));
   * 	 --&gt; 56
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
   * @param cellFunction
   *            a function transforming the current cell values.
   * @return the aggregated measure.
   * @throws IllegalArgumentException
   *             if
   *             <tt>columns != other.columns || rows != other.rows</tt>
   * @see cern.jet.math.tdouble.DoubleFunctions
   */
  def aggregate(other: Matrix1D[T], aggr: Function2[T, T, T], cellFunction: Function2[T, T, T]): T = {
    m.checkSize(other)
    if (m.size == 0)
      m.numeric.zero
    else {
      MatrixProcessor.singleton.aggregateCells(m, aggr, new Function2[Int, T, T]() {
        def apply(idx: Int, value: T) = {
          cellFunction.apply(value, other.getQuick(idx))
        }
      }, null)
    }
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
   * @see cern.jet.math.tdouble.DoubleFunctions
   */
  def assign(cond: Procedure1[T], f: Function1[T, T]) = {
    MatrixProcessor.singleton.processCells(m, new Function2[Int, T, T]() {
      def apply(idx: Int, value: T) = {
        if (cond.apply(value))
          f.apply(value)
        else
          value
      }
    })
    m
  }

  /**
   * Assigns a value to all cells that satisfy a condition.
   *
   * @param cond
   *            a condition.
   *
   * @param newValue
   *            a value.
   * @return <tt>this</tt> (for convenience only).
   *
   */
  def assign(cond: Procedure1[T], newValue: T) = {
    MatrixProcessor.singleton.processCells(m, new Function2[Int, T, T]() {
      def apply(idx: Int, value: T) = {
        if (cond.apply(value))
          newValue
        else
          value
      }
    })
    m
  }

  def assign(f: Function[T, T]) = {
    MatrixProcessor.singleton.processCells(m, f)
    m
  }

  /**
   * Assigns the result of a function to each cell;
   * <tt>x[i] = function(x[i],y[i])</tt>.
   * <p>
   * <b>Example:</b>
   *
   * <pre>
   * 	 // assign x[i] = x[i]&lt;sup&gt;y[i]&lt;/sup&gt;
   * 	 m1 = 0 1 2 3;
   * 	 m2 = 0 2 4 6;
   * 	 m1.assign(m2, cern.jet.math.Functions.pow);
   * 	 --&gt;
   * 	 m1 == 1 1 16 729
   *
   * </pre>
   *
   * For further examples, see the <a
   * href="package-summary.html#FunctionObjects">package doc</a>.
   *
   * @param other
   *            the secondary matrix to operate on.
   * @param function
   *            a function object taking as first argument the current cell's
   *            value of <tt>this</tt>, and as second argument the current
   *            cell's value of <tt>y</tt>,
   * @return <tt>this</tt> (for convenience only).
   * @throws IllegalArgumentException
   *             if <tt>size() != y.size()</tt>.
   */
  def assign(other: Matrix1D[T], function: Function2[T, T, T]) = {
    m.checkSize(other)
    MatrixProcessor.singleton.processCells(m, new Function2[Int, T, T]() {
      def apply(idx: Int, value: T) = {
        function.apply(value, other.getQuick(idx))
      }
    })
    m
  }

  def mean: Double = {
    val num = m.numeric
    val count = m.size.toInt
    if (count == 0)
        num.toDouble(num.zero)
    else
        num.toDouble(aggregate(NumericFunctions.plus(num), NumericFunctions.identity)) / count.toDouble
  }

  def aggregateStats() = {
    val bin = new StaticBin1D[T]()
    (0 until m.size.toInt).foreach(colIdx => {
      bin.add( m.getQuick(colIdx) )
    })
    bin
  }

  def max = {
    aggregateStats().max
  }

  def min = {
    aggregateStats().min
  }

  def stdDev = {
    aggregateStats().standardDeviation
  }

  /**
   * Returns the one-norm of vector <tt>x</tt>, which is
   * <tt>Sum(abs(x[i]))</tt>.
   */
  def norm1: Double = {
    val num = m.numeric
    if (m.size == 0)
        num.toDouble(num.zero)
    num.toDouble( aggregate(NumericFunctions.plus(num), NumericFunctions.abs(num)) )
  }

  /**
   * Returns the two-norm (aka <i>euclidean norm</i>) of vector <tt>x</tt>;
   * equivalent to <tt>Sqrt(mult(x,x))</tt>.
   */
  def norm2: Double = {
    val num = m.numeric
    if (m.size == 0)
        num.toDouble(num.zero)
    Math.sqrt( num.toDouble( aggregate(NumericFunctions.plus(num), NumericFunctions.square(num)) ) )
  }

  /**
   * Returns the infinity norm of vector <tt>x</tt>, which is
   * <tt>Max(abs(x[i]))</tt>.
   */
  def normInfinity: Double = {
    val num = m.numeric
    if (m.size == 0)
        num.toDouble(num.zero)
    num.toDouble( aggregate(NumericFunctions.max(num), NumericFunctions.abs(num)) )
  }

  def sumAllCells = {
    val num = m.numeric
    var result = num.zero
    m.forEachNonZero(new Function2[Int, T, T]() {
      def apply(v1: Int, value: T) = {
        result = num.plus(result, value)
        value
      }
    })
    result
  }

  def getMinLocation: (Int, T) = {
    val num = m.numeric
    if (m.size == 0)
      return (-1, num.zero)
    var savedIdx = -1
    var savedValue = num.zero
    m.forEachNonZero(new Function2[Int, T, T]() {
      def apply(idx: Int, value: T) = {
        if (savedIdx < 0 || num.lt(value, savedValue)) {
          savedIdx = idx
          savedValue = value
        }
        value
      }
    })
    (savedIdx, savedValue)
  }

  def getMaxLocation: (Int, T) = {
    val num = m.numeric
    if (m.size == 0)
      return (-1, num.zero)
    var savedIdx = -1
    var savedValue = num.zero
    m.forEachNonZero(new Function2[Int, T, T]() {
      def apply(idx: Int, value: T) = {
        if (savedIdx < 0 || num.gt(value, savedValue)) {
          savedIdx = idx
          savedValue = value
        }
        value
      }
    })
    (savedIdx, savedValue)
  }

  def *(value: T) = {
    new Operators1D[T](m.copy()).assign(NumericFunctions.mult(m.numeric, value))
  }

  def *=(factor: T) = {
    assign(NumericFunctions.mult(m.numeric, factor))
  }

  def -(value: T) = {
    new Operators1D[T](m.copy()).assign(NumericFunctions.minus(m.numeric, value))
  }

  def -=(value: T) = {
    assign(NumericFunctions.minus(m.numeric, value))
  }

  def +(value: T) = {
    new Operators1D[T](m.copy()).assign(NumericFunctions.plus(m.numeric, value))
  }

  def +=(value: T) = {
    assign(NumericFunctions.plus(m.numeric, value))
  }

  def -(value: Matrix1D[T]) = {
    new Operators1D[T](m.copy()).assign(value, NumericFunctions.minus(m.numeric))
  }

  def -=(value: Matrix1D[T]) = {
    assign(value, NumericFunctions.minus(m.numeric))
  }

  def +(value: Matrix1D[T]): Matrix1D[T] = {
    new Operators1D[T](m.copy()).assign(value, NumericFunctions.plus(m.numeric))
  }

  def +=(value: Matrix1D[T]) = {
    assign(value, NumericFunctions.plus(m.numeric))
  }

  def assignByIndex(func: Function1[Int, T]) = {
    (0 until m.size.toInt).foreach(idx => {
      m.setQuick(idx, func.apply(idx))
    })
    m
  }

  def ==(value: T): IntMatrix1D = {
    // TODO: Use a factory?
    // TODO: Always use sparse matrix?
    val result = new DenseIntMatrix1D(m.size.toInt)
    new Operators1D[Int](result).assignByIndex(new IntFunction() {
      def apply(idx: Int): Int =  {
        if (m.getQuick(idx) == value) 1 else 0
      }
    })
    result
  }

  def !=(value: T): IntMatrix1D = {
    val result = new DenseIntMatrix1D(m.size.toInt)
    new Operators1D[Int](result).assignByIndex(new IntFunction() {
      def apply(idx: Int): Int =  {
        if (m.getQuick(idx) != value) 1 else 0
      }
    })
    result
  }

  def >(value: T): IntMatrix1D = {
    val num = m.numeric
    val result = new DenseIntMatrix1D(m.size.toInt)
    new Operators1D[Int](result).assignByIndex(new IntFunction() {
      def apply(idx: Int): Int =  {
        if (num.gt(m.getQuick(idx), value)) 1 else 0
      }
    })
    result
  }

  def >=(value: T): IntMatrix1D = {
    val num = m.numeric
    val result = new DenseIntMatrix1D(m.size.toInt)
    new Operators1D[Int](result).assignByIndex(new IntFunction() {
      def apply(idx: Int): Int =  {
        if (num.gteq(m.getQuick(idx), value)) 1 else 0
      }
    })
    result
  }

  def <(value: T): IntMatrix1D = {
    val num = m.numeric
    val result = new DenseIntMatrix1D(m.size.toInt)
    new Operators1D[Int](result).assignByIndex(new IntFunction() {
      def apply(idx: Int): Int =  {
        if (num.lt(m.getQuick(idx), value)) 1 else 0
      }
    })
    result
  }

  def <=(value: T): IntMatrix1D = {
    val num = m.numeric
    val result = new DenseIntMatrix1D(m.size.toInt)
    new Operators1D[Int](result).assignByIndex(new IntFunction() {
      def apply(idx: Int): Int =  {
        if (num.lteq(m.getQuick(idx), value)) 1 else 0
      }
    })
    result
  }

  def dot(other: Matrix1D[T]) = {
    multiplyEngine.dot(m, other)
  }

  def dot(other: Matrix1D[T], start: Int, end: Int) = {
    multiplyEngine.dot(m, other, start, end)
  }


  def toDiagonal: Matrix2D[T] = {
    new WrappedDiagonalMatrix2D[T](m)
  }

  /**
   * Constructs and returns a new <i>selection view</i> that is a matrix
   * holding all <b>rows</b> whose index matches the given condition. Applies the
   * condition to each row and takes only those row where
   * <tt>condition.apply(i)</tt> yields <tt>true</tt>. To match
   * columns, use a dice view.
   * @param condition
   * The condition to be matched.
   * @return the new view.
   */
  def viewSelection(condition: IntProcedure): Matrix1D[T] = {
    val matches: IntArrayList = new IntArrayList(m.size.toInt)
    (0 until m.size.toInt).foreach(colIdx => {
      if (condition.apply(colIdx))
        matches.add(colIdx)
    })
    matches.trimToSize()
    m.viewSelection(matches.elements())
  }
}
