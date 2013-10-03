package cern.colt.matrix

import MatrixTypes._
import cern.colt.function.FunctionTypes._
import cern.jet.math.tdouble.DoubleFunctions
import cern.colt.matrix.algo.{MatrixMultiply, DenseDoubleSingularValueDecomposition}
import cern.jet.math.tint.IntFunctions
import java.io.{ObjectInputStream, ObjectOutputStream}
import cern.colt.function.ProcedureTypes.{DoubleProcedure, IntIntProcedure, IntProcedure}
import cern.colt.list.ArrayTypes.IntArrayList
import hep.aida.bin.StaticBin1D
import cern.colt.matrix.Norm.Norm
import cern.colt.matrix.impl.{WrappedDiagonalMatrix2D, WrappedDiagonalMatrix1D}

/**
  */
object MatrixOperators {

  type StaticDoubleBin1D = StaticBin1D[Double]

  implicit def matrix2operators(m : DoubleMatrix2D) = new Double2DOperators(m)
  implicit def matrix2operators(m : DoubleMatrix1D) = new Double1DOperators(m)
  implicit def matrix2operators(m : IntMatrix1D) = new Int1DOperators(m)
  implicit def matrix2operators(m : IntMatrix2D) = new Int2DOperators(m)
  implicit def matrix2operators(m : IntArrayList) = new IntArrayOperators(m)

  def checkSizesEqual(a: Matrix1D[_], b: Matrix1D[_]) {
    if (a.size != b.size)
      throw new IllegalArgumentException("Sizes are not equal (" + a.size + " and " + b.size + ")")
  }

  def checkSizeEquals(a: Matrix1D[_], b: Int) {
    if (a.size.toInt != b)
      throw new IllegalArgumentException("Sizes are not equal (" + a.size + " and " + b + ")")
  }

  def checkSizeAtLeast(a: Matrix1D[_], b: Int) {
    if (a.size.toInt < b)
      throw new IllegalArgumentException("Size is not big enough (" + a.size + "); should be at least " + b)
  }

  def checkColumnsEqual(a: Matrix2D[_], b: Int) {
    if (a.columns != b)
      throw new IllegalArgumentException("Columns are not equal (" + a.columns + " and " + b + ")")
  }

  def checkColumnsEqual(a: Matrix2D[_], b: Matrix2D[_]) {
    if (a.columns != b.columns)
      throw new IllegalArgumentException("Columns are not equal (" + a.columns + " and " + b.columns + ")")
  }

  def checkColumnsEqual(a: Matrix2D[_], b: Matrix1D[_]) {
    if (a.columns != b.size.toInt)
      throw new IllegalArgumentException("Columns and size are not equal (" + a.columns + " and " + b.size + ")")
  }

  def checkRowsEqual(a: Matrix2D[_], b: Int) {
    if (a.rows != b)
      throw new IllegalArgumentException("Rows are not equal (" + a.rows + " and " + b + ")")
  }

  def checkRowsEqual(a: Matrix2D[_], b: Matrix2D[_]) {
    if (a.rows != b.rows)
      throw new IllegalArgumentException("Rows are not equal (" + a.rows + " and " + b.rows + ")")
  }

  def checkRowsEqualsSize(a: Matrix2D[_], b: Matrix1D[_]) {
    if (a.rows != b.size.toInt)
      throw new IllegalArgumentException("Rows do not equal size (" + a.rows + " and " + b.size + ")")
  }

  def checkColumnsEqualsSize(a: Matrix2D[_], b: Matrix1D[_]) {
    if (a.columns != b.size.toInt)
      throw new IllegalArgumentException("Columns do not equal size (" + a.columns + " and " + b.size + ")")
  }

  def checkColumnsEqualsRows(a: Matrix2D[_], b: Matrix2D[_]) {
    if (a.columns != b.rows)
      throw new IllegalArgumentException("Columns do not equal rows (" + a.columns + " and " + b.rows + ")")
  }

  def checkColumnsEqualsRows(a: Matrix2D[_]) {
    if (a.columns != a.rows)
      throw new IllegalArgumentException("Columns do not equal rows (" + a.columns + " and " + a.rows + ")")
  }

  def getDensityInfo(a: Matrix[_]): String = {
    if (a == null)
      "null"
    else {
      a match {
        case v: Matrix1D[_] => getDensityInfo(v)
        case m: Matrix2D[_] => getDensityInfo(m)
        case _ => "Matrix type: " + a.getClass.getSimpleName + ", size: " + a.size    // Unsupported details case...
      }
    }
  }

  def getDensityInfo(m: Matrix1D[_]): String = {
    val rows = 1
    val columns = m.size.toInt
    var nonZero = m.numNonZero
    "Matrix type: " + m.getClass.getSimpleName + ", size: " + rows + "x" + columns + ", non-zero: " + nonZero + ", density: " + ((nonZero*100.0)/(rows.toDouble*columns)).formatted("%.2f") + "%"
  }

  def getDensityInfo(m: Matrix2D[_]): String = {
    val rows = m.rows
    val columns = m.columns
    var nonZero = m.numNonZero
    "Matrix type: " + m.getClass.getSimpleName + ", size: " + rows + "x" + columns + ", non-zero: " + nonZero + ", density: " + ((nonZero*100.0)/(rows.toDouble*columns)).formatted("%.2f") + "%"
  }

  def writeMatrix(out: ObjectOutputStream, matrix: DoubleMatrix2D) {
    if (matrix == null)
      out.writeObject(null)
    else
      out.writeObject(matrix.toArray)
  }

  def writeMatrix(out: ObjectOutputStream, matrix: DoubleMatrix1D) {
    if (matrix == null)
      out.writeObject(null)
    else
      out.writeObject(matrix.toArray)
  }

  def readMatrix2D(in: ObjectInputStream): DoubleMatrix2D = {
    val doubles: Array[Array[Double]] = in.readObject().asInstanceOf[Array[Array[Double]]]
    if (doubles == null)
      null
    else
      new DenseDoubleMatrix2D(doubles)
  }

  def readMatrix1D(in: ObjectInputStream): DoubleMatrix1D = {
    val doubles: Array[Double] = in.readObject().asInstanceOf[Array[Double]]
    if (doubles == null)
      null
    else
      new DenseDoubleMatrix1D(doubles)
  }

  def DoubleMatrix2D( values: Array[Array[Double]] ) = {
    var maxWidth = 0
    values.foreach(row => if (row.size > maxWidth) maxWidth = row.size)
    val result = new DenseDoubleMatrix2D(values.size, maxWidth)
    var rowIdx = 0
    values.foreach(row => {
      var colIdx = 0
      row.foreach(value => {
        result.setQuick(rowIdx, colIdx, value)
        colIdx += 1
      })
      rowIdx += 1
    })
    result
  }

  def DoubleMatrix1D( values: Array[Double] ) = {
    val result = new DenseDoubleMatrix1D(values.size)
    result.assign(values)
  }

  def IntMatrix1D( values: Array[Int] ) = {
    val result = new DenseIntMatrix1D(values.size)
    result.assign(values)
  }

  abstract class StatsResult {
    def apply(idx: Int, bin: StaticDoubleBin1D)
  }

  case class Double2DOperators(m: DoubleMatrix2D) extends Iterable[DoubleMatrix1D] {

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
    def aggregate(aggr: DoubleDoubleFunction, cellFunction: DoubleFunction): Double = {
      if (size == 0)
          return Double.NaN
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
     * @see cern.jet.math.tdouble.DoubleFunctions
     */
    def aggregate(aggr: DoubleDoubleFunction, cellFunction: DoubleFunction, cond: DoubleProcedure): Double = {
      if (size == 0)
          return Double.NaN
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
    def aggregate(aggr: DoubleDoubleFunction, cellFunction: IntIntDoubleFunction, cond: DoubleProcedure): Double = {
      if (size == 0)
          return Double.NaN
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
    def aggregate(other: DoubleMatrix2D, aggr: DoubleDoubleFunction, cellFunction: DoubleDoubleFunction): Double = {
      m.checkShape(other)
      if (size == 0)
          return Double.NaN
      MatrixProcessor.singleton.aggregateCells(m, aggr, new Function3[Int, Int, Double, Double]() {
        def apply(rowIdx: Int, colIdx: Int, value: Double) = {
          cellFunction.apply(value, other.getQuick(rowIdx, colIdx))
        }
      }, null)
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
    def assign(cond: DoubleProcedure, f: DoubleFunction) = {
      MatrixProcessor.singleton.processCells(m, new IntIntDoubleFunction() {
        def apply(rowIdx: Int, colIdx: Int, value: Double) = {
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
    def assign(cond: DoubleProcedure, newValue: Double) = {
      MatrixProcessor.singleton.processCells(m, new IntIntDoubleFunction() {
        def apply(rowIdx: Int, colIdx: Int, value: Double) = {
          if (cond.apply(value))
            newValue
          else
            value
        }
      })
      m
    }

    def mean: Double = {
      val count = m.rows * m.columns
      if (count == 0)
          0
      else
          aggregate(DoubleFunctions.plus, DoubleFunctions.identity) / count
    }

    def meanOfEachRow: DoubleMatrix1D = {
      val result = new DenseDoubleMatrix1D(m.rows)
      statsOfEachRow(new StatsResult() {
        def apply(idx: Int, bin: StaticDoubleBin1D) {
          result.setQuick(idx, bin.mean)
        }
      })
      result
    }

    def meanOfEachColumn: DoubleMatrix1D = {
      val result = new DenseDoubleMatrix1D(m.columns)
      statsOfEachColumn(new StatsResult() {
        def apply(idx: Int, bin: StaticDoubleBin1D) {
          result.setQuick(idx, bin.mean)
        }
      })
      result
    }

    def setRow(rowIdx: Int, values: DoubleMatrix1D) {
      checkColumnsEqualsSize(m, values)
      (0 until values.size.toInt).foreach(colIdx => {
        m.set(rowIdx, colIdx, values.getQuick(colIdx))
      })
    }

    def setColumn(colIdx: Int, values: DoubleMatrix1D) {
      checkRowsEqualsSize(m, values)
      (0 until values.size.toInt).foreach(rowIdx => {
        m.set(rowIdx, colIdx, values.getQuick(rowIdx))
      })
    }

    def assign(f1: DoubleFunction): DoubleMatrix2D = {
      // TODO:
      m
    }

    def *(factor: Double) = {
      val result = m.copy()
      result.assign(DoubleFunctions.mult(factor))
      result
    }

    def *=(factor: Double) = {
      m.assign(DoubleFunctions.mult(factor))
      m
    }

    def /(divisor: Double) = {
      val result = m.copy()
      result.assign(DoubleFunctions.div(divisor))
      result
    }

    def /=(divisor: Double) = {
      m.assign(DoubleFunctions.div(divisor))
    }

    def /=(divisor: Int) = {
      m.assign(DoubleFunctions.div(divisor.toDouble))
    }

    def /(divisor: DoubleMatrix1D) = {
      divByRow(divisor)
    }

    def /=(divisor: DoubleMatrix1D) = {
      divEqualsByRow(divisor)
    }

    def divByRow(value: DoubleMatrix1D) = {
      val result = m.copy()
      result.divEqualsByRow(value)
    }

    def divEqualsByRow(value: DoubleMatrix1D) = {
      checkColumnsEqualsSize(m, value)
      MatrixProcessor.singleton.processNonZeroCells(m, m, new IntIntDoubleFunction() {
        def apply(rowIdx: Int, colIdx: Int, oldValue: Double) = {
          oldValue / value.getQuick(colIdx)
        }
      })
      m
    }

    def divByColumn(value: DoubleMatrix1D) = {
      val result = m.copy()
      result.divEqualsByColumn(value)
    }

    def divEqualsByColumn(value: DoubleMatrix1D) = {
      checkRowsEqualsSize(m, value)
      MatrixProcessor.singleton.processNonZeroCells(m, m, new IntIntDoubleFunction() {
        def apply(rowIdx: Int, colIdx: Int, oldValue: Double) = {
          oldValue / value.getQuick(rowIdx)
        }
      })
      m
    }

    def *(value: DoubleMatrix1D) = {
      multiplyByRow(value)
    }

    def *=(value: DoubleMatrix1D) = {
      multiplyEqualsByRow(value)
    }

    def multiplyByRow(value: DoubleMatrix1D) = {
      val result = m.copy()
      result.multiplyEqualsByRow(value)
    }

    def multiplyEqualsByRow(value: DoubleMatrix1D) = {
      checkColumnsEqualsSize(m, value)
      MatrixProcessor.singleton.processNonZeroCells(m, m, new IntIntDoubleFunction() {
        def apply(rowIdx: Int, colIdx: Int, oldValue: Double) = {
          oldValue * value.getQuick(colIdx)
        }
      })
      m
    }

    def multiplyByColumn(value: DoubleMatrix1D) = {
      val result = m.copy()
      result.multiplyEqualsByColumn(value)
    }

    def multiplyEqualsByColumn(value: DoubleMatrix1D) = {
      checkRowsEqualsSize(m, value)
      MatrixProcessor.singleton.processNonZeroCells(m, m, new IntIntDoubleFunction() {
        def apply(rowIdx: Int, colIdx: Int, oldValue: Double) = {
          oldValue * value.getQuick(rowIdx)
        }
      })
      m
    }

    def /(divisor: DoubleMatrix2D) = {
      val result = m.copy()
      result /= divisor
    }

    def assign(other: DoubleMatrix2D, f: DoubleDoubleFunction) {
      //TODO:
    }

    def /=(divisor: DoubleMatrix2D) = {
      m.assign(divisor, DoubleFunctions.div)
    }

    def *(value: DoubleMatrix2D) = {
      val result = m.copy()
      result *= value
    }

    def *=(value: DoubleMatrix2D) = {
      m.assign(value, DoubleFunctions.mult)
    }

    def **(value: Double) = {
      val result = m.copy()
      result **= value
    }

    def **=(value: Double) = {
      m.assign( new DoubleFunction() {
        def apply(x: Double) = math.pow(x, value)
      })
    }

    def -(value: Double) = {
      val result = m.copy()
      result += value
    }

    def -=(subtractor: Double) = {
      m.assign(DoubleFunctions.minus(subtractor))
    }

    def +(value: Double) = {
      val result = m.copy()
      result += value
    }

    def +=(value: Double) = {
      m.assign(DoubleFunctions.plus(value))
    }

    def -(value: DoubleMatrix1D) = {
      val result = m.copy()
      result -= value
    }

    def -=(value: DoubleMatrix1D) = {
      m.assign(value, DoubleFunctions.minus)
    }

    def -(value: DoubleMatrix2D) = {
      val result = m.copy()
      result -= value
    }

    def -=(value: DoubleMatrix2D) = {
      m.assign(value, DoubleFunctions.minus)
    }

    def minusByColumn(value: DoubleMatrix1D) = {
      val result = m.copy()
      result.minusEqualsByColumn(value)
    }

    def minusEqualsByColumn(value: DoubleMatrix1D) = {
      MatrixProcessor.singleton.processColumns(m, new IntProcedure() {
        def apply(colIdx: Int) = {
          val column = m.viewColumn(colIdx)
          column.assign(value, DoubleFunctions.minus)
          true
        }
      })
      m
    }

    def +(value: DoubleMatrix1D) = {
      val result = m.copy()
      result += value
      result
    }

    def assign(value: DoubleMatrix1D, func: DoubleDoubleFunction) = {
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

    def +=(value: DoubleMatrix1D) = {
      m.assign(value, DoubleFunctions.plus)
    }

    def +=(value: DoubleMatrix2D) = {
      m.assign(value, DoubleFunctions.plus)
    }

    def plusByColumn(value: DoubleMatrix1D) = {
      val result = m.copy()
      result.plusEqualsByColumn(value)
    }

    def plusEqualsByColumn(value: DoubleMatrix1D) = {
      MatrixProcessor.singleton.processColumns(m, new IntProcedure() {
        def apply(colIdx: Int) = {
          val column = m.viewColumn(colIdx)
          column.assign(value, DoubleFunctions.plus)
          true
        }
      })
      m
    }

    def sqrt = {
      val result = m.copy()
      result.sqrtOnSelf
    }

    def sqrtOnSelf = {
      m.assign(DoubleFunctions.sqrt)
    }

    def exp = {
      val result = m.copy()
      result.expOnSelf
    }

    def expOnSelf = {
      m.assign(DoubleFunctions.exp)
    }

    def log = {
      val result = m.copy()
      result.logOnSelf
    }

    def logOnSelf = {
      m.assign(DoubleFunctions.log)
    }

    // Transpose
    def T = m.viewTranspose()

    def dot(other: DoubleMatrix2D) = {
      MatrixMultiply.multiply(m, other)
    }

    def dot(other: DoubleMatrix2D, transposeOther: Boolean) = {
      MatrixMultiply.multiply2D(m, other, null, transposeB=transposeOther)
    }

    def dot(other: DoubleMatrix2D, alpha: Double) = {
      MatrixMultiply.multiply2D(m, other, null, alpha=alpha)
    }

    def dot(other: DoubleMatrix2D, alpha: Double, transposeSelf: Boolean, transposeOther: Boolean) = {
      MatrixMultiply.multiply2D(m, other, null, alpha=alpha, transposeA=transposeSelf, transposeB=transposeOther)
    }

    def dot(other: DoubleMatrix2D, C: DoubleMatrix2D, alpha: Double, beta: Double, transposeSelf: Boolean, transposeOther: Boolean) = {
      MatrixMultiply.multiply2D(m, other, C, alpha, beta, transposeA=transposeSelf, transposeB=transposeOther)
    }

    def dot(other: DoubleMatrix1D) = {
      MatrixMultiply.multiply(m, other)
    }

    def dot(other: DoubleMatrix1D, alpha: Double) = {
      MatrixMultiply.multiply1D(m, other, null, alpha=alpha)
    }

    def dot(other: DoubleMatrix1D, alpha: Double, transposeSelf: Boolean) = {
      MatrixMultiply.multiply1D(m, other, null, alpha=alpha, transposeA=transposeSelf)
    }

    def dot(y: DoubleMatrix1D, z: DoubleMatrix1D, alpha: Double, beta: Double, transposeSelf: Boolean) = {
      MatrixMultiply.multiply1D(m, y, z, alpha, beta, transposeSelf)
    }

    def statsOfEachColumn(saver: StatsResult) {
      MatrixProcessor.singleton.processColumns(m, new IntProcedure() {
        def apply(colIdx: Int) = {
          val bin = new StaticDoubleBin1D()
          (0 until m.rows).foreach(rowIdx => {
            bin.add( m.getQuick(rowIdx, colIdx) )
          })
          saver.apply(colIdx, bin)
          true
        }
      })
    }

    def statsOfEachRow(saver: StatsResult) {
      MatrixProcessor.singleton.processRows(m, new IntProcedure() {
        def apply(rowIdx: Int) = {
          val bin = new StaticDoubleBin1D()
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
      val saver = new StatsResult() {
        def apply(idx: Int, bin: StaticDoubleBin1D) {
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
      statsOfEachRow(new StatsResult() {
        def apply(idx: Int, bin: StaticDoubleBin1D) {
          var std = bin.standardDeviation
          if (std == 0.0 || std.isNaN)
            std = defaultVal
          result.setQuick(idx, std)
        }
      })
      result
    }

    def sumAllCells = {
      var result = 0.0
      m.forEachNonZero(new Function3[Int, Int, Double, Double]() {
        def apply(v1: Int, v2: Int, value: Double) = {
          result += value
          value
        }
      })
      result
    }

    def sumOfEachColumn = {
      val result = new DenseDoubleMatrix1D(m.columns)
      statsOfEachColumn(new StatsResult() {
        def apply(idx: Int, bin: StaticDoubleBin1D) {
          result.setQuick(idx, bin.sum)
        }
      })
      result
    }

    def sumOfEachColumn(f: IntDoubleFunction) = {
      MatrixProcessor.singleton.processColumns(m, new IntProcedure() {
        def apply(colIdx: Int) = {
          val col = m.viewColumn(colIdx)
          var sum = 0.0
          col.forEachNonZero(new IntDoubleFunction {
            def apply(colIdx: Int, value: Double): Double = {
              sum += value
              value
            }
          })
          f.apply(colIdx, sum)
          true
        }
      })
    }

    def sumOfEachRow = {
      val result = new DenseDoubleMatrix1D(m.rows)
      statsOfEachRow(new StatsResult() {
        def apply(idx: Int, bin: StaticDoubleBin1D) {
          result.setQuick(idx, bin.sum)
        }
      })
      result
    }

    def sumOfEachRow(f: IntDoubleFunction) = {
      MatrixProcessor.singleton.processRows(m, new IntProcedure() {
        def apply(rowIdx: Int) = {
          val row = m.viewRow(rowIdx)
          var sum = 0.0
          row.forEachNonZero(new IntDoubleFunction {
            def apply(colIdx: Int, value: Double): Double = {
              sum += value
              value
            }
          })
          f.apply(rowIdx, sum)
          true
        }
      })
    }

    def maxOfEachColumn = {
      val result = new DenseDoubleMatrix1D(m.columns)
      statsOfEachColumn(new StatsResult() {
        def apply(idx: Int, bin: StaticDoubleBin1D) {
          result.setQuick(idx, bin.max)
        }
      })
      result
    }

    def maxOfEachRow = {
      val result = new DenseDoubleMatrix1D(m.rows)
      statsOfEachRow(new StatsResult() {
        def apply(idx: Int, bin: StaticDoubleBin1D) {
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
          var max = Double.NegativeInfinity
          (0 until m.columns).foreach(colIdx => {
            val value = m.getQuick(rowIdx, colIdx)
            if (value > max) {
              index = colIdx
              max = value
            }
          })
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
          var max = Double.NegativeInfinity
          (0 until m.rows).foreach(rowIdx => {
            val value = m.getQuick(rowIdx, colIdx)
            if (value > max) {
              index = rowIdx
              max = value
            }
          })
          result.setQuick(colIdx, index)
          true
        }
      })
      result
    }

    def getMinLocation: (Int, Int, Double) = {
      if (m.size == 0)
        return (-1, -1, Double.NaN)
      var savedRowIdx = -1
      var savedColIdx = -1
      var savedValue = Double.MaxValue
      var firstRowIdx = -1
      var firstColIdx = -1
      m.forEachNonZero(new Function3[Int, Int, Double, Double]() {
        def apply(rowIdx: Int, colIdx: Int, value: Double) = {
          if (value < savedValue) {
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
      if (savedValue > 0 && (firstRowIdx > 0 || firstColIdx > 0)) {
        firstColIdx -= 1
        if (firstColIdx < 0) {
          firstColIdx = m.columns-1
          firstRowIdx -= 1
        }
        savedRowIdx = firstRowIdx
        savedColIdx = firstColIdx
        savedValue = 0
      }
      (savedRowIdx, savedColIdx, savedValue)
    }

    def getMaxLocation: (Int, Int, Double) = {
      if (m.size == 0)
        return (-1, -1, Double.NaN)
      var savedRowIdx = -1
      var savedColIdx = -1
      var savedValue = Double.MinValue
      var firstRowIdx = -1
      var firstColIdx = -1
      m.forEachNonZero(new Function3[Int, Int, Double, Double]() {
        def apply(rowIdx: Int, colIdx: Int, value: Double) = {
          if (value > savedValue) {
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
      if (savedValue < 0 && (firstRowIdx > 0 || firstColIdx > 0)) {
        firstColIdx -= 1
        if (firstColIdx < 0) {
          firstColIdx = m.columns-1
          firstRowIdx -= 1
        }
        savedRowIdx = firstRowIdx
        savedColIdx = firstColIdx
        savedValue = 0
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
    def viewIndexSelection(condition: IntProcedure): DoubleMatrix2D = {
      val matches: IntArrayList = new IntArrayList(m.rows)
      (0 until m.rows).foreach(rowIdx => {
        if (condition.apply(rowIdx))
          matches.add(rowIdx)
      })
      matches.trimToSize()
      m.viewSelection(matches.elements(), null)
    }

    def ==(value: Double): IntMatrix2D = {
      val result = new SparseRCIntMatrix2D(m.rows, m.columns)
      result.assignByIndex(new IntIntFunction() {
        def apply(x: Int, y: Int): Int = {
          if (m.getQuick(x, y) == value) 1 else 0
        }
      })
      result
    }

    def !=(value: Double): IntMatrix2D = {
      val result = new SparseRCIntMatrix2D(m.rows, m.columns)
      result.assignByIndex(new IntIntFunction() {
        def apply(x: Int, y: Int): Int = {
          if (m.getQuick(x, y) != value) 1 else 0
        }
      })
      result
    }

    def >(value: Double): IntMatrix2D = {
      val result = new SparseRCIntMatrix2D(m.rows, m.columns)
      result.assignByIndex(new IntIntFunction() {
        def apply(x: Int, y: Int): Int = {
          if (m.getQuick(x, y) > value) 1 else 0
        }
      })
      result
    }

    def >=(value: Double): IntMatrix2D = {
      val result = new SparseRCIntMatrix2D(m.rows, m.columns)
      result.assignByIndex(new IntIntFunction() {
        def apply(x: Int, y: Int): Int = {
          if (m.getQuick(x, y) >= value) 1 else 0
        }
      })
      result
    }

    def <(value: Double): IntMatrix2D = {
      val result = new SparseRCIntMatrix2D(m.rows, m.columns)
      result.assignByIndex(new IntIntFunction() {
        def apply(x: Int, y: Int): Int = {
          if (m.getQuick(x, y) < value) 1 else 0
        }
      })
      result
    }

    def <=(value: Double): IntMatrix2D = {
      val result = new SparseRCIntMatrix2D(m.rows, m.columns)
      result.assignByIndex(new IntIntFunction() {
        def apply(x: Int, y: Int): Int = {
          if (m.getQuick(x, y) <= value) 1 else 0
        }
      })
      result
    }

    def viewRows(start: Int, end: Int) = {
      m.viewPart(start, 0, end-start, m.columns)
    }

    def viewColumns(start: Int, end: Int) = {
      m.viewPart(0, start, m.rows, end-start)
    }

    def svd(wantUV: Boolean = true, wantWholeUV: Boolean = false) = {
      // TODO: Support non-dense approaches
      new DenseDoubleSingularValueDecomposition(m, wantUV, wantWholeUV)
    }

    def getDiagonal: DoubleMatrix1D = {
      new WrappedDiagonalMatrix1D[Double](m)
    }

    def setDiagonal(diag: DoubleMatrix1D) {
      val maxIdx = math.min(math.min(m.rows, m.columns), diag.size.toInt)
      (0 until maxIdx).foreach(idx => {
        m.setQuick(idx, idx, diag.getQuick(idx))
      })
    }

     /**
      * Returns the one-norm of matrix <tt>A</tt>, which is the maximum absolute
      * column sum.
      */
     def norm1: Double = {
       var max = 0.0
       for(c <- 0 until m.columns) {
         max = Math.max(max, m.viewColumn(c).norm1)
       }
       max
     }

     /**
      * Returns the two-norm (aka <i>euclidean norm</i>) of vector
      * <tt>X.vectorize()</tt>;
      */
     def vectorNorm2: Double = {
       Math.sqrt(aggregate(DoubleFunctions.plus, DoubleFunctions.square))
     }

    /**
     * Returns the two-norm of matrix <tt>A</tt>, which is the maximum singular
     * value; obtained from SVD.
     */
    def norm2: Double = {
        svd().norm2
    }

    /**
     * Returns the Frobenius norm of matrix <tt>A</tt>, which is
     * <tt>Sqrt(Sum(A[i,j]<sup>2</sup>))</tt>.
     */
    def normF: Double = {
      if (m.size == 0)
          return 0
      aggregate(DoubleFunctions.hypot, DoubleFunctions.identity)
    }

    /**
     * Returns the infinity norm of matrix <tt>A</tt>, which is the maximum
     * absolute row sum.
     */
    def normInfinity: Double = {
      var max = 0.0
      for(r <- 0 until m.rows) {
        max = Math.max(max, m.viewRow(r).norm1)
      }
      max
    }

    def normalize(norm: Norm) = {
      val result = m.copy()
      result.normalizeSelf(norm)
    }

    def normalizeSelf(norm: Norm) = {
      if (norm == null) {
        // nothing to do.
      }
      else if (norm == Norm.One) {
        MatrixProcessor.singleton.processRows(m, new IntProcedure() {
          def apply(rowIdx: Int) = {
            val row = m.viewRow(rowIdx)
            var sum = 0.0
            row.forEachNonZero(new IntDoubleFunction {
              def apply(colIdx: Int, value: Double): Double = {
                sum += value
                value
              }
            })
            row.forEachNonZero(new IntDoubleFunction {
              def apply(colIdx: Int, value: Double): Double = {
                value / sum
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
            var sum = 0.0
            row.forEachNonZero(new IntDoubleFunction {
              def apply(colIdx: Int, value: Double): Double = {
                sum += value*value
                value
              }
            })
            sum = Math.sqrt(sum)
            row.forEachNonZero(new IntDoubleFunction {
              def apply(colIdx: Int, value: Double): Double = {
                value / sum
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

    def iterator: Iterator[DoubleMatrix1D] = {
      new DoubleMatrix2DIterator(m)
    }

    def hasNaN: Boolean = {
      (0 until m.rows).foreach(rowIdx => {
        (0 until m.columns).foreach(colIdx => {
          val value = m.getQuick(rowIdx, colIdx)
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
        val value = m.getQuick(rowIdx, colIdx)
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

  class DoubleMatrix2DIterator(m: DoubleMatrix2D) extends Iterator[DoubleMatrix1D] {

    private var currentRow = -1

    def hasNext: Boolean = currentRow < m.rows

    def next(): DoubleMatrix1D = {
      if (hasNext) {
        currentRow += 1
        m.viewRow(currentRow)
      }
      else {
        null
      }
    }
  }

  case class Double1DOperators(m: DoubleMatrix1D) {

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
    def aggregate(aggr: DoubleDoubleFunction, cellFunction: DoubleFunction): Double = {
      if (m.size == 0)
          return Double.NaN
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
     * @see cern.jet.math.tdouble.DoubleFunctions
     */
    def aggregate(aggr: DoubleDoubleFunction, cellFunction: IntDoubleFunction, cond: DoubleProcedure): Double = {
      if (m.size == 0)
          return Double.NaN
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
    def aggregate(other: DoubleMatrix1D, aggr: DoubleDoubleFunction, cellFunction: DoubleDoubleFunction): Double = {
      m.checkSize(other)
      if (m.size == 0)
          return Double.NaN
      MatrixProcessor.singleton.aggregateCells(m, aggr, new IntDoubleFunction() {
        def apply(idx: Int, value: Double) = {
          cellFunction.apply(value, other.getQuick(idx))
        }
      }, null)
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
    def assign(cond: DoubleProcedure, f: DoubleFunction) = {
      MatrixProcessor.singleton.processCells(m, new IntDoubleFunction() {
        def apply(idx: Int, value: Double) = {
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
    def assign(cond: DoubleProcedure, newValue: Double) = {
      MatrixProcessor.singleton.processCells(m, new IntDoubleFunction() {
        def apply(idx: Int, value: Double) = {
          if (cond.apply(value))
            newValue
          else
            value
        }
      })
      m
    }

    def assign(f: DoubleFunction) = {
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
     * @see cern.jet.math.tdouble.DoubleFunctions
     */
    def assign(other: DoubleMatrix1D, function: DoubleDoubleFunction) = {
      m.checkSize(other)
      MatrixProcessor.singleton.processCells(m, new IntDoubleFunction() {
        def apply(idx: Int, value: Double) = {
          function.apply(value, other.getQuick(idx))
        }
      })
      m
    }

    def mean: Double = {
      val count = m.size.toInt
      if (count == 0)
          0
      else
          aggregate(DoubleFunctions.plus, DoubleFunctions.identity) / count
    }

    def aggregateStats() = {
      val bin = new StaticDoubleBin1D()
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
      if (m.size == 0)
        return 0
      aggregate(DoubleFunctions.plus, DoubleFunctions.abs)
    }

    /**
     * Returns the two-norm (aka <i>euclidean norm</i>) of vector <tt>x</tt>;
     * equivalent to <tt>Sqrt(mult(x,x))</tt>.
     */
    def norm2: Double = {
      Math.sqrt( dot(m) )
    }

    /**
     * Returns the Frobenius norm of matrix <tt>A</tt>, which is
     * <tt>Sqrt(Sum(A[i]<sup>2</sup>))</tt>.
     */
    def normF: Double = {
      if (m.size == 0)
          return 0
      aggregate(DoubleFunctions.hypot, DoubleFunctions.identity)
    }

    /**
     * Returns the infinity norm of vector <tt>x</tt>, which is
     * <tt>Max(abs(x[i]))</tt>.
     */
    def normInfinity: Double = {
        if (m.size == 0)
            return 0
        aggregate(DoubleFunctions.max, DoubleFunctions.abs)
    }

    def sumAllCells = {
      var result = 0.0
      m.forEachNonZero(new Function2[Int, Double, Double]() {
        def apply(v1: Int, value: Double) = {
          result += value
          value
        }
      })
      result
    }

    def getMinLocation: (Int, Double) = {
      if (m.size == 0)
        return (-1, Double.NaN)
      var savedIdx = -1
      var savedValue = Double.MaxValue
      m.forEachNonZero(new Function2[Int, Double, Double]() {
        def apply(idx: Int, value: Double) = {
          if (value < savedValue) {
            savedIdx = idx
            savedValue = value
          }
          value
        }
      })
      (savedIdx, savedValue)
    }

    def getMaxLocation: (Int, Double) = {
      if (m.size == 0)
        return (-1, Double.NaN)
      var savedIdx = -1
      var savedValue = Double.MinValue
      m.forEachNonZero(new Function2[Int, Double, Double]() {
        def apply(idx: Int, value: Double) = {
          if (value > savedValue) {
            savedIdx = idx
            savedValue = value
          }
          value
        }
      })
      (savedIdx, savedValue)
    }

    def *(factor: Double) = {
      val result = m.copy()
      result *= factor
    }

    def *=(factor: Double) = {
      m.assign(DoubleFunctions.mult(factor))
    }

    def /(divisor: Double) : DoubleMatrix1D = {
      val result = m.copy()
      result /= divisor
    }

    def /=(divisor: Double) : DoubleMatrix1D = {
      m.assign(DoubleFunctions.div(divisor))
    }

    def /(divisor: Int) : DoubleMatrix1D  = {
      m / divisor.toDouble
    }

    def /=(divisor: Int)  : DoubleMatrix1D = {
      m /= divisor.toDouble
    }

    def -(value: Double) = {
      val result = m.copy()
      result -= value
    }

    def -=(subtractor: Double) = {
      m.assign(DoubleFunctions.minus(subtractor))
    }

    def +(value: Double) = {
      val result = m.copy()
      result += value
    }

    def +=(value: Double) = {
      m.assign(DoubleFunctions.plus(value))
    }

    def -(value: DoubleMatrix1D) = {
      val result = m.copy()
      result -= value
    }

    def -=(value: DoubleMatrix1D) = {
      m.assign(value, DoubleFunctions.minus)
    }

    def +(value: DoubleMatrix1D) = {
      val result = m.copy()
      result += value
    }

    def +=(value: DoubleMatrix1D) = {
      m.assign(value, DoubleFunctions.plus)
    }

    def **(value: Double)  : DoubleMatrix1D = {
      val result = m.copy()
      result **= value
    }

    def **=(value: Double)  : DoubleMatrix1D = {
      m.assign(new DoubleFunction() {
        def apply(x: Double) = math.pow(x, value)
      })
    }

    def **(value: Int)  : DoubleMatrix1D = {
      m ** value.toDouble
    }

    def **=(value: Int) : DoubleMatrix1D  = {
      m **= value.toDouble
    }

    def ==(value: Double): IntMatrix1D = {
      // TODO: Use a factory?
      // TODO: Always use sparse matrix?
      val result = new DenseIntMatrix1D(m.size.toInt)
      result.assignByIndex(new IntFunction() {
        def apply(idx: Int): Int =  {
          if (m.getQuick(idx) == value) 1 else 0
        }
      })
      result
    }

    def !=(value: Double): IntMatrix1D = {
      val result = new DenseIntMatrix1D(m.size.toInt)
      result.assignByIndex(new IntFunction() {
        def apply(idx: Int): Int =  {
          if (m.getQuick(idx) != value) 1 else 0
        }
      })
      result
    }

    def greaterThanValue(value: Double): IntMatrix1D = {
      val result = new DenseIntMatrix1D(m.size.toInt)
      result.assignByIndex(new IntFunction() {
        def apply(idx: Int): Int =  {
          if (m.getQuick(idx) > value) 1 else 0
        }
      })
      result
    }

    def >=(value: Double): IntMatrix1D = {
      val result = new DenseIntMatrix1D(m.size.toInt)
      result.assignByIndex(new IntFunction() {
        def apply(idx: Int): Int =  {
          if (m.getQuick(idx) >= value) 1 else 0
        }
      })
      result
    }

    def <(value: Double): IntMatrix1D = {
      val result = new DenseIntMatrix1D(m.size.toInt)
      result.assignByIndex(new IntFunction() {
        def apply(idx: Int): Int =  {
          if (m.getQuick(idx) < value) 1 else 0
        }
      })
      result
    }

    def <=(value: Double): IntMatrix1D = {
      val result = new DenseIntMatrix1D(m.size.toInt)
      result.assignByIndex(new IntFunction() {
        def apply(idx: Int): Int =  {
          if (m.getQuick(idx) <= value) 1 else 0
        }
      })
      result
    }

    def dot(other: DoubleMatrix1D) = {
      MatrixMultiply.dot(m, other)
    }

    def dot(other: DoubleMatrix1D, start: Int, end: Int) = {
      MatrixMultiply.dot(m, other, start, end)
    }

    def sqrt = {
      val result = m.copy()
      result.sqrtOnSelf
    }

    def sqrtOnSelf = {
      m.assign(DoubleFunctions.sqrt)
    }

    def exp = {
      val result = m.copy()
      result.expOnSelf
    }

    def expOnSelf = {
      m.assign(DoubleFunctions.exp)
    }

    def log = {
      val result = m.copy()
      result.logOnSelf
    }

    def logOnSelf = {
      m.assign(DoubleFunctions.log)
    }

    def toDiagonal: DoubleMatrix2D = {
      new WrappedDiagonalMatrix2D[Double](m)
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
    def viewSelection(condition: IntProcedure): DoubleMatrix1D = {
      val matches: IntArrayList = new IntArrayList(m.size.toInt)
      (0 until m.size.toInt).foreach(colIdx => {
        if (condition.apply(colIdx))
          matches.add(colIdx)
      })
      matches.trimToSize()
      m.viewSelection(matches.elements())
    }

    def hasNaN: Boolean = {
      (0 until m.size.toInt).foreach(idx => {
        if (m.getQuick(idx).isNaN) return true
      })
      false
    }

    def locateNaNs(limit: Int = 10): String = {
      var count = 0
      val buf = new StringBuilder()
      (0 until m.size.toInt).foreach(idx => {
        if (m.getQuick(idx).isNaN) {
          count += 1
          if (count > limit) {
            buf.append(" ...")
            return buf.toString()
          }
          else {
            if (count > 1)
              buf.append(", ")
            buf.append( idx )
          }
        }
      })
      if (count == 0)
        "None"
      else
        buf.toString()
    }
  }

  class Int1DOperators(m: IntMatrix1D) {

    def take(indexes: DenseIntMatrix1D) = {
      val result = new DenseIntMatrix1D(indexes.size.toInt)
      // No way to pass in the index using assign(), so just write it out.
      (0 until indexes.size.toInt).foreach(colIdx => {
        result.setQuick(colIdx, m.get(indexes.getQuick(colIdx)))
      })
      result
    }

    /**
     * Constructs and returns a new <i>selection view</i> that is a matrix
     * holding all <b>elements</b> whose index matches the given condition. Applies the
     * condition to each element and takes only those element where
     * <tt>condition.apply(i)</tt> yields <tt>true</tt>.
     * @param condition
     * The condition to be matched.
     * @return the new view.
     */
    def viewIndexSelection(condition: IntProcedure) = {
      val matches = new IntArrayList(m.size.toInt)
      (0 until m.size.toInt).foreach(colIdx => {
        if (condition.apply(colIdx))
          matches.add(colIdx)
      })
      matches.trimToSize()
      m.viewSelection(matches.elements())
    }

    def viewEqualValues(other: IntMatrix1D) = {
      val otherSize = other.size.toInt
      val result = m.viewIndexSelection(new IntProcedure() {
        def apply(idx: Int): Boolean = {
          idx < otherSize && m.getQuick(idx) == other.getQuick(idx)
        }
      })
      result
    }

    def viewUnequalValues(other: IntMatrix1D) = {
      val otherSize = other.size.toInt
      val result = m.viewIndexSelection(new IntProcedure() {
        def apply(idx: Int): Boolean = {
          idx < otherSize && m.getQuick(idx) != other.getQuick(idx)
        }
      })
      result
    }

    def assignByIndex(func: IntFunction) = {
      (0 until m.size.toInt).foreach(idx => {
        m.setQuick(idx, func.apply(idx))
      })
      m
    }

    def *(factor: Int) = {
      val result = m.copy()
      result *= factor
    }

    def assign(f: IntFunction) {
      //TODO:
    }
    def *=(factor: Int) = {
      m.assign(IntFunctions.mult(factor))
    }

    def /(divisor: Int) = {
      val result = m.copy()
      result /= divisor
    }

    def /=(divisor: Int) = {
      m.assign(IntFunctions.div(divisor))
    }

    def -(value: Int) = {
      val result = m.copy()
      result -= value
    }

    def -=(subtractor: Int) = {
      m.assign(IntFunctions.minus(subtractor))
    }

    def +(value: Int) = {
      val result = m.copy()
      result += value
    }

    def +=(value: Int) = {
      m.assign(IntFunctions.plus(value))
    }
  }

  class Int2DOperators(m: IntMatrix2D) {

    def assignByIndex(func: IntIntFunction) = {
      (0 until m.rows).foreach(rowIdx => {
        (0 until m.columns).foreach(colIdx => {
          m.setQuick(rowIdx, colIdx, func.apply(rowIdx, colIdx))
        })
      })
      m
    }

    def all(): Boolean = {
      (0 until m.rows).foreach(rowIdx => {
        (0 until m.columns).foreach(colIdx => {
          if (m.getQuick(rowIdx, colIdx) == 0)
            return false
        })
      })
      true
    }
  }

  class IntArrayOperators(m: IntArrayList) {

    def take(indexes: IntMatrix1D) = {
      val result = new DenseIntMatrix1D(indexes.size.toInt)
      // No way to pass in the index using assign(), so just write it out.
      (0 until indexes.size.toInt).foreach(colIdx => {
        result.setQuick(colIdx, m.get(indexes.getQuick(colIdx)))
      })
      result
    }
  }
}
