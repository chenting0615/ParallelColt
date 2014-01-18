package cern.colt.matrix.algo

import cern.colt.matrix.{MatrixProcessor, Matrix2D, Matrix1D}
import cern.colt.function.ProcedureTypes._
import cern.colt.matrix.MatrixNumeric._

class MultiplyForEachNonZero[T: MatrixNumeric : Manifest] extends MultiplyAlgorithm[T] {

  val numeric = implicitly[MatrixNumeric[T]]

  def dot(A: Matrix1D[T], B: Matrix1D[T]): T = {
    var sum = numeric.zero
    A.forEachNonZero(new Function2[Int, T, T]() {
          def apply(idx: Int, value: T) = {
            sum = numeric.plus(sum, numeric.times(value, B.getQuick(idx)))
            value
          }
        })
    sum
  }

  def dot(A: Matrix1D[T], B: Matrix1D[T], start: Int, end: Int): T = {
    var sum = numeric.zero
    A.forEachNonZero(new Function2[Int, T, T]() {
      def apply(idx: Int, value: T) = {
        if (idx >= start && idx < end)
          sum = numeric.plus(sum, numeric.times(value, B.getQuick(idx)))
        value
      }
    })
    sum
  }

  def multiply(A: Matrix2D[T], y: Matrix1D[T], z: Matrix1D[T], alpha: T, beta_p: T): Matrix1D[T] = {
    val beta = if (z == null) numeric.zero else beta_p
    val zz: Matrix1D[T] = if (z == null) y.like1D(A.rows) else z

    MatrixProcessor.singleton.processRows[T](A, zz, new Function1[Int, T]() {
      def apply(rowIdx: Int) = {
        var sum = numeric.zero
        A.forEachNonZeroInRow(rowIdx, new Function3[Int, Int, T, T]() {
              def apply(rowIdx: Int, colIdx: Int, value: T) = {
                sum = numeric.plus(sum, numeric.times(value, y.getQuick(colIdx)))
                value
              }
            })
        if (beta == numeric.zero)
          numeric.times(alpha, sum)
        else
          numeric.plus(numeric.times(alpha, sum), numeric.times(beta, zz.getQuick(rowIdx)))
      }
    })
    zz
  }

  def multiply(A: Matrix2D[T], B: Matrix2D[T], C: Matrix2D[T], alpha: T, beta_p: T): Matrix2D[T] = {
    val beta = if (C == null) numeric.zero else beta_p
    val CC = if (C == null) A.like2D(A.rows, B.columns) else C

    MatrixProcessor.singleton.processIndexes(B.columns, new IntProcedure(){
      def apply(bColIdx: Int) = {
        for (aRowIdx <- 0 until A.rows) {
          var sum = numeric.zero
          A.forEachNonZeroInRow(aRowIdx, new Function3[Int, Int, T, T]() {
            def apply(rowIdx: Int, commonIdx: Int, value: T) = {
              sum = numeric.plus(sum, numeric.times(value, B.getQuick(commonIdx, bColIdx)))
              value
            }
          })
          if (beta == numeric.zero)
            CC.setQuick(aRowIdx, bColIdx, numeric.times(alpha, sum))
          else
            CC.setQuick(aRowIdx, bColIdx, numeric.plus(numeric.times(alpha, sum), numeric.times(beta, CC.getQuick(aRowIdx, bColIdx))))
        }
        true
      }
    }, parallelAllowed = MatrixProcessor.singleton.canWriteInParallel(CC))
    CC
  }
}

