package cern.colt.matrix

import cern.colt.function.Procedure1
import cern.jet.math.NumericFunctions
import cern.colt.matrix.MatrixTypes._
import cern.colt.matrix.impl.{WrappedDiagonalMatrix1D, DenseMatrix1D}
import hep.aida.bin.{StatsResult, StaticBin1D}
import cern.colt.function.ProcedureTypes._
import cern.colt.list.ArrayTypes._
import cern.colt.matrix.Norm.Norm
import MatrixChecks._
import cern.colt.matrix.algo.MatrixMultiply
import cern.colt.matrix.MatrixNumeric._

class Operators2D[@specialized T: Manifest: MatrixNumeric: MatrixMultiply](m: Matrix2D[T]) extends Iterable[Matrix1D[T]] {

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
   * @see cern.jet.math.tdouble.DoubleFunctions
   */
  def aggregate(aggr: Function2[T, T, T], cellFunction: Function1[T, T]): T = {
    if (m.size == 0)
      numeric.zero
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
  def aggregate(aggr: Function2[T, T, T], cellFunction: Function1[T, T], cond: Procedure1[T]): T = {
    if (m.size == 0)
      numeric.zero
    else
      MatrixProcessor.singleton.aggregateCells(m, aggr, cellFunction, cond)
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
   * @see cern.jet.math.tdouble.DoubleFunctions
   */
  def aggregate(aggr: Function2[T, T, T], cellFunction: Function3[Int, Int, T, T], cond: Procedure1[T]): T = {
    if (m.size == 0)
      numeric.zero
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
  def aggregate(other: Matrix2D[T], aggr: Function2[T, T, T], cellFunction: Function2[T, T, T]): T = {
    m.checkShape(other)
    if (m.size == 0)
      numeric.zero
    else {
      MatrixProcessor.singleton.aggregateCells(m, aggr, new Function3[Int, Int, T, T]() {
        def apply(rowIdx: Int, colIdx: Int, value: T) = {
          cellFunction.apply(value, other.getQuick(rowIdx, colIdx))
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
    MatrixProcessor.singleton.processCells(m, new Function3[Int, Int, T, T]() {
      def apply(rowIdx: Int, colIdx: Int, value: T) = {
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
    MatrixProcessor.singleton.processCells(m, new Function3[Int, Int, T, T]() {
      def apply(rowIdx: Int, colIdx: Int, value: T) = {
        if (cond.apply(value))
          newValue
        else
          value
      }
    })
    m
  }

  def dot(other: Matrix2D[T]) = {
    multiplyEngine.multiply(m, other)
  }

  def dot(other: Matrix2D[T], transposeOther: Boolean) = {
    multiplyEngine.multiply2D(m, other, null, transposeB=transposeOther)
  }

/*
  def dot(other: Matrix2D[T], alpha: T) = {
    multiplyEngine.multiply2D(m, other, null, alpha=alpha)
  }
*/

  def dot(other: Matrix2D[T], alpha: T, transposeSelf: Boolean, transposeOther: Boolean) = {
    multiplyEngine.multiply2D(m, other, null, alpha=alpha, transposeA=transposeSelf, transposeB=transposeOther)
  }

  def dot(other: Matrix2D[T], C: Matrix2D[T], alpha: T, beta: T, transposeSelf: Boolean, transposeOther: Boolean) = {
    multiplyEngine.multiply2D(m, other, C, alpha, beta, transposeA=transposeSelf, transposeB=transposeOther)
  }

  def dot(other: Matrix1D[T]) = {
    multiplyEngine.multiply(m, other)
  }

  def dot(other: Matrix1D[T], alpha: T) = {
    multiplyEngine.multiply1D(m, other, null, alpha=alpha)
  }

  def dot(other: Matrix1D[T], alpha: T, transposeSelf: Boolean) = {
    multiplyEngine.multiply1D(m, other, null, alpha=alpha, transposeA=transposeSelf)
  }

  def dot(y: Matrix1D[T], z: Matrix1D[T], alpha: T, beta: T, transposeSelf: Boolean) = {
    multiplyEngine.multiply1D(m, y, z, alpha, beta, transposeSelf)
  }

  def mean: Double = {
    val count = m.rows * m.columns
    if (count == 0)
        0.0
    else
        numeric.toDouble(aggregate(NumericFunctions.plus(numeric), NumericFunctions.identity)) / count
  }

  def meanOfEachRow: DoubleMatrix1D = {
    val result = new DenseDoubleMatrix1D(m.rows)
    statsOfEachRow(new StatsResult[T]() {
      def apply(idx: Int, bin: StaticBin1D[T]) {
        result.setQuick(idx, bin.mean)
      }
    })
    result
  }

  def meanOfEachColumn: DoubleMatrix1D = {
    val result = new DenseDoubleMatrix1D(m.columns)
    statsOfEachColumn(new StatsResult[T]() {
      def apply(idx: Int, bin: StaticBin1D[T]) {
        result.setQuick(idx, bin.mean)
      }
    })
    result
  }

  def setRow(rowIdx: Int, values: Matrix1D[T]) {
    checkColumnsEqualsSize(m, values)
    (0 until values.size.toInt).foreach(colIdx => {
      m.set(rowIdx, colIdx, values.getQuick(colIdx))
    })
  }

  def setColumn(colIdx: Int, values: Matrix1D[T]) {
    checkRowsEqualsSize(m, values)
    (0 until values.size.toInt).foreach(rowIdx => {
      m.set(rowIdx, colIdx, values.getQuick(rowIdx))
    })
  }

  def assign(f1: Function1[T, T]) = {
    MatrixProcessor.singleton.processCells(m, f1)
    m
  }

  def *(factor: T) = {
    new Operators2D[T](m.copy()).assign(NumericFunctions.mult(numeric, factor))
  }

  def *=(factor: T) = {
    assign(NumericFunctions.mult(numeric, factor))
    m
  }

  def *(value: Matrix1D[T]) = {
    multiplyByRow(value)
  }

  def *=(value: Matrix1D[T]) = {
    multiplyEqualsByRow(value)
  }

  def multiplyByRow(value: Matrix1D[T]) = {
    new Operators2D[T](m.copy()).multiplyEqualsByRow(value)
  }

  def multiplyEqualsByRow(value: Matrix1D[T]) = {
    checkColumnsEqualsSize(m, value)
    MatrixProcessor.singleton.processNonZeroCells(m, m, new Function3[Int, Int, T, T]() {
      def apply(rowIdx: Int, colIdx: Int, oldValue: T) = {
        numeric.times(oldValue, value.getQuick(colIdx))
      }
    })
    m
  }

  def multiplyByColumn(value: Matrix1D[T]) = {
    new Operators2D[T](m.copy()).multiplyEqualsByColumn(value)
  }

  def multiplyEqualsByColumn(value: Matrix1D[T]) = {
    checkRowsEqualsSize(m, value)
    MatrixProcessor.singleton.processNonZeroCells(m, m, new Function3[Int, Int, T, T]() {
      def apply(rowIdx: Int, colIdx: Int, oldValue: T) = {
        numeric.times(oldValue, value.getQuick(rowIdx))
      }
    })
    m
  }

  def assign(other: Matrix2D[T], f: Function2[T, T, T]) = {
    checkRowsEqual(m, other)
    checkColumnsEqual(m, other)
    MatrixProcessor.singleton.processCells(m, new Function3[Int, Int, T, T]() {
      def apply(row: Int, col: Int, value: T) = {
        f(value, other.getQuick(row, col))
      }
    })
    m
  }

  def *(value: Matrix2D[T]) = {
    new Operators2D[T](m.copy()).assign(value, NumericFunctions.mult(numeric))
  }

  def *=(value: Matrix2D[T]) = {
    assign(value, NumericFunctions.mult(numeric))
  }

  def -(value: T) = {
    new Operators2D[T](m.copy()).assign(NumericFunctions.minus(numeric, value))
  }

  def -=(value: T) = {
    assign(NumericFunctions.minus(numeric, value))
  }

  def +(value: T) = {
    new Operators2D[T](m.copy()).assign(NumericFunctions.plus(numeric, value))
  }

  def +=(value: T) = {
    assign(NumericFunctions.plus(numeric, value))
  }

  def -(value: Matrix1D[T]) = {
    new Operators2D[T](m.copy()).assign(value, NumericFunctions.minus(numeric))
  }

  def -=(value: Matrix1D[T]) = {
    assign(value, NumericFunctions.minus(numeric))
  }

  def -(value: Matrix2D[T]) = {
    new Operators2D[T](m.copy()).assign(value, NumericFunctions.minus(numeric))
  }

  def -=(value: Matrix2D[T]) = {
    assign(value, NumericFunctions.minus(numeric))
  }

  def minusByColumn(value: Matrix1D[T]) = {
    new Operators2D[T](m.copy()).minusEqualsByColumn(value)
  }

  def minusEqualsByColumn(value: Matrix1D[T]) = {
    checkRowsEqualsSize(m, value)
    MatrixProcessor.singleton.processCells(m, new IntIntProcedure() {
      def apply(rowIdx: Int, colIdx: Int) = {
        val newValue = numeric.minus(m.getQuick(rowIdx, colIdx), value.getQuick(rowIdx))
        m.setQuick(rowIdx, colIdx, newValue)
        true
      }
    })
    m
  }

  def minusByRow(value: Matrix1D[T]) = {
    new Operators2D[T](m.copy()).minusEqualsByRow(value)
  }

  def minusEqualsByRow(value: Matrix1D[T]) = {
    checkColumnsEqualsSize(m, value)
    MatrixProcessor.singleton.processCells(m, new IntIntProcedure() {
      def apply(rowIdx: Int, colIdx: Int) = {
        val newValue = numeric.minus(m.getQuick(rowIdx, colIdx), value.getQuick(colIdx))
        m.setQuick(rowIdx, colIdx, newValue)
        true
      }
    })
    m
  }

  def assign(value: Matrix1D[T], func: Function2[T, T, T]) = {
    checkColumnsEqual(m, value)
    MatrixProcessor.singleton.processCells(m, new IntIntProcedure() {
      def apply(rowIdx: Int, colIdx: Int) = {
        val newValue = func.apply(m.getQuick(rowIdx, colIdx), value.getQuick(colIdx))
        m.setQuick(rowIdx, colIdx, newValue)
        true
      }
    })
    m
  }

  def +(value: Matrix1D[T]) = {
    new Operators2D[T](m.copy()).assign(value, NumericFunctions.plus(numeric))
  }

  def +=(value: Matrix1D[T]) = {
    assign(value, NumericFunctions.plus(numeric))
  }

  def +=(value: Matrix2D[T]) = {
    assign(value, NumericFunctions.plus(numeric))
  }

  def plusByColumn(value: Matrix1D[T]) = {
    new Operators2D[T](m.copy()).plusEqualsByColumn(value)
  }

  def plusEqualsByColumn(value: Matrix1D[T]) = {
    checkRowsEqualsSize(m, value)
    MatrixProcessor.singleton.processCells(m, new IntIntProcedure() {
      def apply(rowIdx: Int, colIdx: Int) = {
        val newValue = numeric.plus(m.getQuick(rowIdx, colIdx), value.getQuick(rowIdx))
        m.setQuick(rowIdx, colIdx, newValue)
        true
      }
    })
    m
  }

  // Transpose
  def T = m.viewTranspose()

  def statsOfEachColumn(saver: StatsResult[T]) {
    MatrixProcessor.singleton.processColumns(m, new IntProcedure() {
      def apply(colIdx: Int) = {
        val bin = new StaticBin1D[T]()
        (0 until m.rows).foreach(rowIdx => {
          bin.add( m.getQuick(rowIdx, colIdx) )
        })
        saver.apply(colIdx, bin)
        true
      }
    })
  }

  def statsOfEachRow(saver: StatsResult[T]) {
    MatrixProcessor.singleton.processRows(m, new IntProcedure() {
      def apply(rowIdx: Int) = {
        val bin = new StaticBin1D[T]()
        (0 until m.columns).foreach(colIdx => {
          bin.add( m.getQuick(rowIdx, colIdx) )
        })
        saver.apply(rowIdx, bin)
        true
      }
    })
  }

  def stdDevOfEachColumn: DoubleMatrix1D = {
    stdDevOfEachColumn(1.0)
  }

  def stdDevOfEachColumn(defaultVal: Double) = {
    val result = new DenseDoubleMatrix1D(m.columns)
    val saver = new StatsResult[T]() {
      def apply(idx: Int, bin: StaticBin1D[T]) {
        var std = bin.standardDeviation
        if (std == 0.0 || std.isNaN)
          std = defaultVal
        result.setQuick(idx, std)
      }
    }
    statsOfEachColumn(saver)
    result
  }

  def stdDevOfEachRow: DoubleMatrix1D = {
    stdDevOfEachRow(1.0)
  }

  def stdDevOfEachRow(defaultVal: Double) = {
    val result = new DenseDoubleMatrix1D(m.rows)
    statsOfEachRow(new StatsResult[T]() {
      def apply(idx: Int, bin: StaticBin1D[T]) {
        var std = bin.standardDeviation
        if (std == 0.0 || std.isNaN)
          std = defaultVal
        result.setQuick(idx, std)
      }
    })
    result
  }

  def sumAllCells = {
    var result = numeric.zero
    m.forEachNonZeroRowMajor(new Function3[Int, Int, T, T]() {
      def apply(v1: Int, v2: Int, value: T) = {
        result = numeric.plus(result, value)
        value
      }
    })
    result
  }

  def sumOfEachColumn = {
    val result = new DenseMatrix1D[T](m.columns)
    MatrixProcessor.singleton.processColumns(m, new IntProcedure() {
      def apply(colIdx: Int) = {
        var sum: T = numeric.zero
        for (rowIdx <- 0 until m.rows) {
          sum = numeric.plus(sum, m.getQuick(rowIdx, colIdx))
        }
        result.setQuick(colIdx, sum)
        true
      }
    })
    result
  }

  def sumOfEachColumn(f: Function2[Int, T, T]) = {
    MatrixProcessor.singleton.processColumns(m, new IntProcedure() {
      def apply(colIdx: Int) = {
        val col = m.viewColumn(colIdx)
        var sum: T = numeric.zero
        col.forEachNonZero(new Function2[Int, T, T] {
          def apply(colIdx: Int, value: T): T = {
            sum = numeric.plus(sum, value)
            value
          }
        })
        f.apply(colIdx, sum)
        true
      }
    })
  }

  def sumOfEachRow = {
    val result = new DenseMatrix1D[T](m.rows)
    MatrixProcessor.singleton.processRows(m, new IntProcedure() {
      def apply(rowIdx: Int) = {
        var sum: T = numeric.zero
        for (colIdx <- 0 until m.columns) {
          sum = numeric.plus(sum, m.getQuick(rowIdx, colIdx))
        }
        result.setQuick(rowIdx, sum)
        true
      }
    })
    result
  }

  def sumOfEachRow(f: Function2[Int, T, T]) = {
    MatrixProcessor.singleton.processRows(m, new IntProcedure() {
      def apply(rowIdx: Int) = {
        val row = m.viewRow(rowIdx)
        var sum: T = numeric.zero
        row.forEachNonZero(new Function2[Int, T, T] {
          def apply(colIdx: Int, value: T): T = {
            sum = numeric.plus(sum, value)
            value
          }
        })
        f.apply(rowIdx, sum)
        true
      }
    })
  }

  def maxOfEachColumn = {
    val result = new DenseMatrix1D[T](m.columns)
    statsOfEachColumn(new StatsResult[T]() {
      def apply(idx: Int, bin: StaticBin1D[T]) {
        result.setQuick(idx, bin.max)
      }
    })
    result
  }

  def maxOfEachRow = {
    val result = new DenseMatrix1D[T](m.rows)
    statsOfEachRow(new StatsResult[T]() {
      def apply(idx: Int, bin: StaticBin1D[T]) {
        result.setQuick(idx, bin.max)
      }
    })
    result
  }

  def indexMaxOfEachRow: IntMatrix1D = {
    val result = new DenseIntMatrix1D(m.rows)
    MatrixProcessor.singleton.processRows(m, new IntProcedure() {
      def apply(rowIdx: Int): Boolean = {
        var index = -1
        var max = numeric.zero
        for(colIdx <- 0 until m.columns) {
          val value = m.getQuick(rowIdx, colIdx)
          if (index < 0 || numeric.gt(value, max)) {
            index = colIdx
            max = value
          }
        }
        result.setQuick(rowIdx, index)
        true
      }
    })
    result
  }

  def indexMaxOfEachColumn = {
    val result = new DenseIntMatrix1D(m.columns)
    MatrixProcessor.singleton.processColumns(m, new IntProcedure() {
      def apply(colIdx: Int): Boolean = {
        var index = -1
        var max = numeric.zero
        for(rowIdx <- 0 until m.rows) {
          val value = m.getQuick(rowIdx, colIdx)
          if (index < 0 || numeric.gt(value, max)) {
            index = rowIdx
            max = value
          }
        }
        result.setQuick(colIdx, index)
        true
      }
    })
    result
  }

  def getMinLocation: (Int, Int, T) = {
    if (m.size == 0)
      return (-1, -1, numeric.zero)
    var savedRowIdx = -1
    var savedColIdx = -1
    var savedValue = numeric.zero
    var firstRowIdx = -1
    var firstColIdx = -1
    m.forEachNonZeroRowMajor(new Function3[Int, Int, T, T]() {
      def apply(rowIdx: Int, colIdx: Int, value: T) = {
        if (savedRowIdx < 0 || numeric.lt(value, savedValue)) {
          savedRowIdx = rowIdx
          savedColIdx = colIdx
          savedValue = value
          if (firstRowIdx < 0) {
            firstRowIdx = rowIdx
            firstColIdx = colIdx
          }
        }
        value
      }
    })
    if (numeric.gt(savedValue, numeric.zero)) {
      firstColIdx -= 1
      if (firstColIdx < 0) {
        firstColIdx = m.columns-1
        firstRowIdx -= 1
      }
      savedRowIdx = firstRowIdx
      savedColIdx = firstColIdx
      savedValue = numeric.zero
    }
    (savedRowIdx, savedColIdx, savedValue)
  }

  def getMaxLocation: (Int, Int, T) = {
    if (m.size == 0)
      return (-1, -1, numeric.zero)
    var savedRowIdx = -1
    var savedColIdx = -1
    var savedValue = numeric.zero
    var firstRowIdx = -1
    var firstColIdx = -1
    m.forEachNonZeroRowMajor(new Function3[Int, Int, T, T]() {
      def apply(rowIdx: Int, colIdx: Int, value: T) = {
        if (savedRowIdx < 0 || numeric.gt(value, savedValue)) {
          savedRowIdx = rowIdx
          savedColIdx = colIdx
          savedValue = value
          if (firstRowIdx < 0) {
            firstRowIdx = rowIdx
            firstColIdx = colIdx
          }
        }
        value
      }
    })
    if (numeric.lt(savedValue, numeric.zero)) {
      firstColIdx -= 1
      if (firstColIdx < 0) {
        firstColIdx = m.columns-1
        firstRowIdx -= 1
      }
      savedRowIdx = firstRowIdx
      savedColIdx = firstColIdx
      savedValue = numeric.zero
    }
    (savedRowIdx, savedColIdx, savedValue)
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
  def viewIndexSelection(condition: IntProcedure): Matrix2D[T] = {
    val matches: IntArrayList = new IntArrayList(m.rows)
    (0 until m.rows).foreach(rowIdx => {
      if (condition.apply(rowIdx))
        matches.add(rowIdx)
    })
    matches.trimToSize()
    m.viewSelection(matches.elements(), null)
  }

  def ==(value: T): IntMatrix2D = {
    val result = new SparseRCIntMatrix2D(m.rows, m.columns)
    for(rowIdx <- 0 until m.rows; colIdx <- 0 until m.columns) {
      result.setQuick(rowIdx, colIdx, if (m.getQuick(rowIdx, colIdx) == value) 1 else 0)
    }
    result
  }

  def !=(value: T): IntMatrix2D = {
    val result = new SparseRCIntMatrix2D(m.rows, m.columns)
    for(rowIdx <- 0 until m.rows; colIdx <- 0 until m.columns) {
      result.setQuick(rowIdx, colIdx, if (m.getQuick(rowIdx, colIdx) != value) 1 else 0)
    }
    result
  }

  def >(value: T): IntMatrix2D = {
    val result = new SparseRCIntMatrix2D(m.rows, m.columns)
    for(rowIdx <- 0 until m.rows; colIdx <- 0 until m.columns) {
      result.setQuick(rowIdx, colIdx, if (numeric.gt(m.getQuick(rowIdx, colIdx), value)) 1 else 0)
    }
    result
  }

  def >=(value: T): IntMatrix2D = {
    val result = new SparseRCIntMatrix2D(m.rows, m.columns)
    for(rowIdx <- 0 until m.rows; colIdx <- 0 until m.columns) {
      result.setQuick(rowIdx, colIdx, if (numeric.gteq(m.getQuick(rowIdx, colIdx), value)) 1 else 0)
    }
    result
  }

  def <(value: T): IntMatrix2D = {
    val result = new SparseRCIntMatrix2D(m.rows, m.columns)
    for(rowIdx <- 0 until m.rows; colIdx <- 0 until m.columns) {
      result.setQuick(rowIdx, colIdx, if (numeric.lt(m.getQuick(rowIdx, colIdx), value)) 1 else 0)
    }
    result
  }

  def <=(value: T): IntMatrix2D = {
    val result = new SparseRCIntMatrix2D(m.rows, m.columns)
    for(rowIdx <- 0 until m.rows; colIdx <- 0 until m.columns) {
      result.setQuick(rowIdx, colIdx, if (numeric.lteq(m.getQuick(rowIdx, colIdx), value)) 1 else 0)
    }
    result
  }

  def viewRows(start: Int, end: Int) = {
    m.viewPart(start, 0, end-start, m.columns)
  }

  def viewColumns(start: Int, end: Int) = {
    m.viewPart(0, start, m.rows, end-start)
  }

  def getDiagonal: Matrix1D[T] = {
    new WrappedDiagonalMatrix1D[T](m)
  }

  def setDiagonal(diag: Matrix1D[T]) {
    val maxIdx = math.min(math.min(m.rows, m.columns), diag.size.toInt)
    (0 until maxIdx).foreach(idx => {
      m.setQuick(idx, idx, diag.getQuick(idx))
    })
  }

   /**
    * Returns the one-norm of matrix <tt>A</tt>, which is the maximum absolute column sum.
    */
   def norm1: Double = {
     var max = 0.0
     for(c <- 0 until m.columns) {
       max = Math.max(max, new Operators1D[T](m.viewColumn(c)).norm1)
     }
     max
   }

  /**
   * Returns the Frobenius norm of matrix <tt>A</tt>, which is
   * <tt>Sqrt(Sum(A[i,j]**2))</tt>.
   */
  def normF: Double = {
    if (m.size == 0)
        return 0.0
    Math.sqrt( numeric.toDouble( aggregate(NumericFunctions.plus(numeric), NumericFunctions.square(numeric)) ) )
  }

  /**
   * Returns the infinity norm of matrix <tt>A</tt>, which is the maximum absolute row sum.
   */
  def normInfinity: Double = {
    var max = 0.0
    for(r <- 0 until m.rows) {
      max = Math.max(max, new Operators1D[T](m.viewRow(r)).norm1)
    }
    max
  }

  def normalize(norm: Norm) = {
    new Operators2D[T](m.copy()).normalizeSelf(norm)
  }

  def normalizeSelf(norm: Norm) = {
    if (norm == null) {
      // nothing to do.
    }
    else if (norm == Norm.One) {
      MatrixProcessor.singleton.processRows(m, new IntProcedure() {
        def apply(rowIdx: Int) = {
          val row = m.viewRow(rowIdx)
          var sum = numeric.zero
          row.forEachNonZero(new Function2[Int, T, T] {
            def apply(colIdx: Int, value: T): T = {
              sum = numeric.plus(sum, value)
              value
            }
          })
          row.forEachNonZero(new Function2[Int, T, T] {
            def apply(colIdx: Int, value: T): T = {
              numeric.div(value, sum)
            }
          })
          true
        }
      })
    }
    else if (norm == Norm.Two) {
      MatrixProcessor.singleton.processRows(m, new IntProcedure() {
        def apply(rowIdx: Int) = {
          val row = m.viewRow(rowIdx)
          var sum = numeric.zero
          row.forEachNonZero(new Function2[Int, T, T] {
            def apply(colIdx: Int, value: T): T = {
              sum = numeric.plus(sum, numeric.times(value, value))
              value
            }
          })
          val divisor = numeric.fromDouble(Math.sqrt(numeric.toDouble(sum)))
          row.forEachNonZero(new Function2[Int, T, T] {
            def apply(colIdx: Int, value: T): T = {
              numeric.div(value, divisor)
            }
          })
          true
        }
      })
    }
    else {
      throw new UnsupportedOperationException("Unsupported norm value: " + norm)
    }
    m
  }

  def iterator: Iterator[Matrix1D[T]] = {
    new Matrix2DIterator[T](m)
  }

  def hasNaN: Boolean = {
    (0 until m.rows).foreach(rowIdx => {
      (0 until m.columns).foreach(colIdx => {
        val value = numeric.toDouble(m.getQuick(rowIdx, colIdx))
        if (value.isNaN || value.isInfinite)
          return true
      })
    })
    false
  }

  def locateNaNs(limit: Int = 10): String = {
    var count = 0
    val buf = new StringBuilder()
    for(rowIdx <- 0 until m.rows; colIdx <- 0 until m.columns) {
      val value = numeric.toDouble(m.getQuick(rowIdx, colIdx))
      if (value.isNaN || value.isInfinite) {
        count += 1
        if (count > limit) {
          buf.append(" ...")
          return buf.toString()
        }
        else {
          if (count > 1)
            buf.append(", ")
          buf.append( "(" + rowIdx + ", " + colIdx + ")")
        }
      }
    }
    if (count == 0)
      "None"
    else
      buf.toString()
  }
}

