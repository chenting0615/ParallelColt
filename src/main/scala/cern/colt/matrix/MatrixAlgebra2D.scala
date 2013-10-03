package cern.colt.matrix

import cern.colt.matrix.MatrixTypes.{IntMatrix2D, IntMatrix1D}
import cern.colt.function.FunctionTypes._
import cern.colt.matrix.MatrixOperators.StatsResult
import cern.colt.function.ProcedureTypes.IntProcedure
import cern.colt.matrix.algo.DenseDoubleSingularValueDecomposition
import cern.colt.function.Procedure2

/**
  */
trait MatrixAlgebra2D[T] extends MatrixAlgebra[T, Matrix2D] {

  def assignByIndex(func: Function2[Int, Int, T])

  def meanOfEachRow: Matrix1D[T]

  def meanOfEachColumn: Matrix1D[T]

  def divByRow(value: Matrix1D[T]): Matrix2D[T]

  def divSelfByRow(value: Matrix1D[T]): Matrix2D[T]

  def divByColumn(value: Matrix1D[T]): Matrix2D[T]

  def divSelfByColumn(value: Matrix1D[T]): Matrix2D[T]

  def multiplyByRow(value: Matrix1D[T]): Matrix2D[T]

  def multiplyEqualsByRow(value: Matrix1D[T]): Matrix2D[T]

  def multiplyByColumn(value: Matrix1D[T]): Matrix2D[T]

  def multiplyEqualsByColumn(value: Matrix1D[T]): Matrix2D[T]

  def /(divisor: Matrix2D[T]): Matrix2D[T]

  def /=(divisor: Matrix2D[T]): Matrix2D[T]

  def *(value: Matrix2D[T]) : Matrix2D[T]

  def *=(value: Matrix2D[T]): Matrix2D[T]

  def -(value: Matrix2D[T]): Matrix2D[T]

  def -=(value: Matrix2D[T]): Matrix2D[T]

  def minusByColumn(value: Matrix1D[T]): Matrix2D[T]

  def minusEqualsByColumn(value: Matrix1D[T]): Matrix2D[T]

  def assign(value: Matrix1D[T], func: DoubleDoubleFunction): Matrix2D[T]

  def +(value: Matrix2D[T]): Matrix2D[T]

  def +=(value: Matrix2D[T]): Matrix2D[T]

  def plusByColumn(value: Matrix1D[T]): Matrix2D[T]

  def plusSelfByColumn(value: Matrix1D[T]): Matrix2D[T]

  // Transpose
  def T: Matrix2D[T]

  def dot(other: Matrix2D[T]): Matrix2D[T]

  def statsOfEachColumn(saver: StatsResult)

  def statsOfEachRow(saver: StatsResult)

  def stdDevOfEachColumn: Matrix1D[T]

  def stdDevOfEachColumn(defaultVal: T): Matrix1D[T]

  def stdDevOfEachRow: Matrix1D[T]

  def stdDevOfEachRow(defaultVal: T): Matrix1D[T]

  def sumOfEachColumn: Matrix1D[T]

  def sumOfEachColumn(f: Procedure2[Int, T])

  def sumOfEachRow: Matrix1D[T]

  def sumOfEachRow(f: Procedure2[Int, T])

  def maxOfEachColumn: Matrix1D[T]

  def maxOfEachRow: Matrix1D[T]

  def indexMinOfEachRow: IntMatrix1D

  def indexMinOfEachColumn: IntMatrix1D

  def indexMaxOfEachRow: IntMatrix1D

  def indexMaxOfEachColumn: IntMatrix1D

  def take(indexes: IntMatrix2D)

  /**
   * Constructs and returns a new <i>selection view</i> that is a matrix
   * holding all <b>rows</b> whose index matches the given condition. Applies the
   * condition to each row and takes only those rows where
   * <tt>condition.apply(i)</tt> yields <tt>true</tt>. To match
   * columns, use a dice view.
   * @param condition
   * The condition to be matched.
   * @return the new view.
   */
  def viewRowSelection(condition: IntProcedure): Matrix2D[T]

  /**
   * Constructs and returns a new <i>selection view</i> that is a matrix
   * holding all <b>columns</b> whose index matches the given condition. Applies the
   * condition to each column and takes only those columns where
   * <tt>condition.apply(i)</tt> yields <tt>true</tt>. To match
   * columns, use a dice view.
   * @param condition
   * The condition to be matched.
   * @return the new view.
   */
  def viewColumnSelection(condition: IntProcedure): Matrix2D[T]

  def viewRows(start: Int, end: Int): Matrix2D[T]

  def viewColumns(start: Int, end: Int): Matrix2D[T]

  def viewEqualValues(other: Matrix2D[T])

  def viewNotEqualValues(other: Matrix2D[T])

  def svd(wantUV: Boolean = true, wantWholeUV: Boolean = false): DenseDoubleSingularValueDecomposition

  def getDiagonal: Matrix1D[T]

  def setDiagonal(diag: Matrix1D[T]): Matrix2D[T]

  def iterateRows: Iterator[Matrix1D[T]]

  def iterateColumns: Iterator[Matrix1D[T]]
}
