package cern.colt.matrix.algo

import cern.colt.matrix._
import cern.colt.matrix.MatrixNumeric._

class MultiplyNonZeroIterator[T: MatrixNumeric : Manifest] extends MultiplyAlgorithm[T] {

  val numeric = implicitly[MatrixNumeric[T]]

  @inline
  def compareColumnToRow(iter1: IndexIterator2D[T], iter2: IndexIterator2D[T]): Int = {
    if (iter1.column < iter2.row)
      -1
    else if (iter1.column > iter2.row)
      1
    else
      0
  }

  @inline
  def compareColumnToIndex(iter1: IndexIterator2D[T], iter2: IndexIterator1D[T]): Int = {
    if (iter1.column < iter2.index)
      -1
    else if (iter1.column > iter2.index)
      1
    else
      0
  }

  @inline
  def compareIndexToIndex(iter1: IndexIterator1D[T], iter2: IndexIterator1D[T]): Int = {
    if (iter1.index < iter2.index)
      -1
    else if (iter1.index > iter2.index)
      1
    else
      0
  }

  def dot(A: Matrix1D[T], B: Matrix1D[T]): T = {
    val iterA = A.iteratorNonZeros
    val iterB = B.iteratorNonZeros
    var sum = numeric.zero
    while(iterA.moreValues && iterB.moreValues) {
      compareIndexToIndex(iterA, iterB) match {
        case -1 => {
          if ( ! iterA.setIndex(iterB.index) )
            iterA.increment()
        }
        case 1 => {
          if ( ! iterB.setIndex(iterA.index) )
            iterB.increment()
        }
        case 0 => {
          sum = numeric.plus(sum, numeric.times(iterA.value, iterB.value))
          iterA.increment()
          iterB.increment()
        }
      }
    }
    sum
  }

  def dot(A: Matrix1D[T], B: Matrix1D[T], start: Int, end: Int): T = {
    val iterA = A.iteratorNonZeros
    iterA.setIndex(start)
    val iterB = B.iteratorNonZeros
    iterB.setIndex(start)
    var sum = numeric.zero
    while(iterA.moreValues && iterB.moreValues && iterA.index < end && iterB.index < end) {
      compareIndexToIndex(iterA, iterB) match {
        case -1 => {
          if ( ! iterA.setIndex(iterB.index) )
            iterA.increment()
        }
        case 1 => {
          if ( ! iterB.setIndex(iterA.index) )
            iterB.increment()
        }
        case 0 => {
          sum = numeric.plus(sum, numeric.times(iterA.value, iterB.value))
          iterA.increment()
          iterB.increment()
        }
      }
    }
    sum
  }

  def multiply(A: Matrix2D[T], B: Matrix1D[T], C: Matrix1D[T], alpha: T, beta_p: T): Matrix1D[T] = {
    val beta = if (C == null) numeric.zero else beta_p
    val CC = if (C == null) A.like1D(A.rows) else C

    MatrixProcessor.singleton.processCellIndexes(CC, new Function1[Int, T]() {
      def apply(rowIdx: Int): T = {
        val iterA = A.iteratorNonZerosInRow(rowIdx)
        val iterB = B.iteratorNonZeros
        var sum = numeric.zero
        while(iterA.moreValues && iterB.moreValues) {
          compareColumnToIndex(iterA, iterB) match {
            case -1 => {
              if ( ! iterA.setColumn(iterB.index) )
                iterA.increment()
            }
            case 1 => {
              if ( ! iterB.setIndex(iterA.column) )
                iterB.increment()
            }
            case 0 => {
              sum = numeric.plus(sum, numeric.times(iterA.value, iterB.value))
              iterA.increment()
              iterB.increment()
            }
          }
        }
        if (beta != numeric.zero)
          numeric.plus(numeric.times(sum, alpha), numeric.times(beta, CC.getQuick(rowIdx)))
        else
          numeric.times(sum, alpha)
      }
    })
    CC
  }

  def multiply(A: Matrix2D[T], B: Matrix2D[T], C: Matrix2D[T], alpha: T, beta_p: T): Matrix2D[T] = {
    val beta = if (C == null) numeric.zero else beta_p
    val CC = if (C == null) A.like2D(A.rows, B.columns) else C

    MatrixProcessor.singleton.processCells[T](CC, new Function2[Int, Int, T]() {
      def apply(rowIdx: Int, colIdx: Int): T = {
        val iterA = A.iteratorNonZerosInRow(rowIdx)
        val iterB = B.iteratorNonZerosInColumn(colIdx)
        var sum = numeric.zero
        while(iterA.moreValues && iterB.moreValues) {
          compareColumnToRow(iterA, iterB) match {
            case -1 => {
              if ( ! iterA.setColumn(iterB.row) )
                iterA.increment()
            }
            case 1 => {
              if ( ! iterB.setRow(iterA.column) )
                iterB.increment()
            }
            case 0 => {
              sum = numeric.plus(sum, numeric.times(iterA.value, iterB.value))
              iterA.increment()
              iterB.increment()
            }
          }
        }
        if (beta != numeric.zero)
          numeric.plus(numeric.times(sum, alpha), numeric.times(beta, CC.getQuick(rowIdx, colIdx)))
        else
          numeric.times(sum, alpha)
      }
    })
    CC
  }
}
