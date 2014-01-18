package cern.colt.matrix.algo

import cern.colt.matrix.{MatrixProcessor, Matrix2D, Matrix1D}
import cern.colt.function.ProcedureTypes._
import cern.colt.matrix.MatrixNumeric._


class MultiplySimple[T: MatrixNumeric: Manifest] extends MultiplyAlgorithm[T] {

  val numeric = implicitly[MatrixNumeric[T]]

  override def predicate(A: Matrix1D[T], B: Matrix1D[T]) = {
    ! A.isSparse && ! B.isSparse && ! A.isView && ! B.isView
  }

  override def predicate(A: Matrix2D[T], B: Matrix1D[T], C: Matrix1D[T], transposeA: Boolean) = {
    ! transposeA && ! A.isSparse && ! B.isSparse && ! A.isView && ! B.isView
  }

  override def predicate(A: Matrix2D[T], B: Matrix2D[T], C: Matrix2D[T], transposeA: Boolean, transposeB: Boolean) = {
    ! transposeA && ! transposeB && ! A.isSparse && ! B.isSparse && ! A.isView && ! B.isView
  }

  def dot(A: Matrix1D[T], B: Matrix1D[T]): T = {
    var sum = numeric.zero
    for (idx <- 0 until A.size.toInt) {
       sum = numeric.plus(sum, numeric.times(A.getQuick(idx), B.getQuick(idx)))
    }
    sum
  }

  def dot(A: Matrix1D[T], B: Matrix1D[T], start: Int, end: Int): T = {
    var sum = numeric.zero
    for (idx <- start until end) {
      sum = numeric.plus(sum, numeric.times(A.getQuick(idx), B.getQuick(idx)))
    }
    sum
  }

  def multiply(A: Matrix2D[T], y: Matrix1D[T], z: Matrix1D[T], alpha: T, beta_p: T): Matrix1D[T] = {
    val beta = if (z == null) numeric.zero else beta_p
    val zz: Matrix1D[T] = if (z == null) y.like1D(A.rows) else z

    MatrixProcessor.singleton.processRows[T](A, zz, new Function1[Int, T]() {
      def apply(rowIdx: Int) = {
        var s = numeric.zero
        for (colIdx <- 0 until A.columns) {
          s = numeric.plus(s, numeric.times(A.getQuick(rowIdx, colIdx), y.getQuick(colIdx)))
        }
        numeric.plus(numeric.times(alpha, s), numeric.times(beta, zz.getQuick(rowIdx)))
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
            for (commonIdx <- 0 until A.columns) {
                sum = numeric.plus(sum, numeric.times(A.getQuick(aRowIdx, commonIdx), B.getQuick(commonIdx, bColIdx)))
            }
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

