package cern.colt.matrix

import java.util.concurrent.{Callable, Future}
import java.util.logging.Logger
import cern.colt.function.{VectorFunction, VectorVectorFunction}
import cern.colt.function.ProcedureTypes._
import MatrixOperators._
import cern.colt.matrix.MatrixTypes.{DoubleMatrix1D, DoubleMatrix2D}
import cern.colt.function.FunctionTypes.{IntDoubleFunction, DoubleFunction, IntIntDoubleFunction, DoubleDoubleFunction}

/**
 * This class contains logic for processing rows and/or columns of a matrix.
 * A supplied function gives the actual calculation to perform.
 * Depending on configuration, this class may run the calculations in parallel or sequentially.
 *
 * If you want to control the parallelism, derive from this class and override the
 * calculate*InParallel() methods, as appropriate.
 */
class MatrixProcessor {

  val logger = Logger.getLogger(this.getClass.getCanonicalName)

  def calculateRowsInParallel(X: Matrix2D[_]): Boolean = {
    ConcurrencyParams.calculateRowsInParallel(X)
  }

  def calculateColumnsInParallel(X: Matrix2D[_]): Boolean = {
    ConcurrencyParams.calculateColumnsInParallel(X)
  }

  def calculateRowsInParallel(X: Matrix2D[_], Y: Matrix2D[_]): Boolean = {
    ConcurrencyParams.calculateRowsInParallel(X, Y)
  }

  def calculateIndexesInParallel(X: Int, Y: Int): Boolean = {
    ConcurrencyParams.calculateInParallel(X, Y)
  }

  def calculateIndexesInParallel(X: Int): Boolean = {
    ConcurrencyParams.calculateInParallel(X)
  }

  def canReadInParallel(X: Matrix2D[_]): Boolean = {
    ConcurrencyParams.canReadInParallel(X)
  }

  def canReadInParallel(X: Matrix1D[_]): Boolean = {
    ConcurrencyParams.canReadInParallel(X)
  }

  def canWriteInParallel(X: Matrix2D[_]): Boolean = {
    ConcurrencyParams.canWriteInParallel(X)
  }

  def canWriteInParallel(X: Matrix1D[_]): Boolean = {
    ConcurrencyParams.canWriteInParallel(X)
  }

  def processRows[T](X: Matrix2D[T], Y: Matrix2D[T], rowFunction: Function2[Int, Int, T]): Matrix2D[T] = {
    processRows(X, Y, X.like2D(X.rows, Y.rows), rowFunction)
  }

  def processRows[T](X: Matrix2D[T], Y: Matrix2D[T], result: Matrix2D[T], rowFunction: Function2[Int, Int, T]): Matrix2D[T] = {
    checkColumnsEqual(X, Y)
    processRows(X.rows, Y.rows, result, rowFunction, parallelAllowed = canReadInParallel(X) && canReadInParallel(Y))
  }

  def processRows[T](xItemCount: Int, yItemCount: Int, result: Matrix2D[T], rowFunction: Function2[Int, Int, T], parallelAllowed: Boolean): Matrix2D[T] = {
    checkRowsEqual(result, xItemCount)
    checkColumnsEqual(result, yItemCount)

    if (parallelAllowed && canWriteInParallel(result) && calculateIndexesInParallel(xItemCount, yItemCount)) {
      logger.finest("Processing in parallel (" + xItemCount + "x" + yItemCount + ")...")

      val totalItemCount = xItemCount*yItemCount

      val threadCount = math.min(ConcurrencyParams.getNumberOfThreads, totalItemCount)
      val futures = new Array[Future[_]](threadCount)
      // This rounds down items-per-iteration.  It leaves any extra items in the last iteration.
      val itemsPerIteration = totalItemCount / threadCount

      (0 until threadCount).foreach(threadIdx => {
        val iterationStart = threadIdx * itemsPerIteration
        val iterationEnd = if (threadIdx == threadCount - 1) totalItemCount else iterationStart + itemsPerIteration
        futures(threadIdx) = ConcurrencyParams.submit(new Runnable() {
            def run() {
              (iterationStart until iterationEnd).foreach(rawIdx => {
                val xIndex = rawIdx / yItemCount
                val yIndex = rawIdx % yItemCount
                val cellValue = rowFunction(xIndex, yIndex)
                result.setQuick(xIndex, yIndex, cellValue)
              })
            }
        })
      })
      ConcurrencyParams.waitForCompletion(futures)
    }
    else {
      logger.finest("Processing sequentially (" + xItemCount + "x" + yItemCount + ")...")

      (0 until xItemCount).foreach(xIndex => {
        (0 until yItemCount).foreach(yIndex => {
          val cellValue = rowFunction(xIndex, yIndex)
          result.setQuick(xIndex, yIndex, cellValue)
        })
      })
    }
    result
  }

  def processSymmetricRowsAgainstRows[T](X: Matrix2D[T], rowFunction: VectorVectorFunction[T]): Matrix2D[T] = {
    processSymmetricRowsAgainstRows(X, X.like2D(X.rows, X.rows), rowFunction)
  }

  // Given N, the # rows in triangular matrix, and the row-num, compute the raw-index of the
  // first item in that row.
  private def getRowIndexStart(N: Int, rowIdx: Int): Int = {
    (N*(N+1)/2) - ((N-rowIdx)*(N-rowIdx+1)/2)
  }

  /**
   * Compute an operation on the rows of X against itself, where the operation is symmetric. That is
   *   f(row1, row2) == f(row2, row1)
   * This avoids doing the duplicate operations, and just saves the result for both index combinations.
   *
   * This method will run the calculations in parallel if configured to do so.  If not, it will run
   * the calculations sequentially.
   * @param X The matrix to compute on rows against itself.
   * @param result The result matrix where values are stored.  That is:
   *               result(x1, x2) = result(x2, x1) = f(X.getRow(x1), X.getRow(x2)).
   * @param rowFunction The function to run for each row combination
   * @return Returns "result" matrix.
   */
  def processSymmetricRowsAgainstRows[T](X: Matrix2D[T], result: Matrix2D[T], rowFunction: VectorVectorFunction[T]): Matrix2D[T] = {
    checkRowsEqual(X, result)
    checkColumnsEqualsRows(result, X)

    if (calculateRowsInParallel(X) && canWriteInParallel(result)) {
      logger.finest("Processing in parallel (" + X.rows + "x" + X.columns + ")...")

      val xItemCount = X.rows
      val totalItemCount = xItemCount*(xItemCount+1)/2

      val threadCount = math.min(ConcurrencyParams.getNumberOfThreads, totalItemCount)
      val futures = new Array[Future[_]](threadCount)
      // This rounds down items-per-iteration.  It leaves any extra items in the last iteration.
      val itemsPerIteration = totalItemCount / threadCount

      (0 until threadCount).foreach(threadIdx => {
        val iterationStart = threadIdx * itemsPerIteration
        val iterationEnd = if (threadIdx == threadCount - 1) totalItemCount else iterationStart + itemsPerIteration
        futures(threadIdx) = ConcurrencyParams.submit(new Runnable() {
            def run() {
              // Save the major row view, so we don't constantly get it over and over.
              var savedMajorRowIndex = -1
              var majorRow: Matrix1D[T] = null
              (iterationStart until iterationEnd).foreach(rawIdx => {
                // The raw-indexes are laid out like a triangular matrix relative to the rows and columns:
                //     0   1   2  3  ...  N
                //   -----------------------
                // 0 | 0   1   2  3  ...  N
                // 1 |    N+1 N+2 N+3 ... 2N-1
                // 2 |        2N 2N+1 ... 3N-2
                // 3 |           3N-1 ... 4N-3
                // . |                ...
                // . |                 ...
                // . |                  ...
                // N |                    N*(N+1)/2 - 1
                //
                // where N == xItemCount
                //
                // Turn the raw-index into majorRowIdx and minorRowIdx.
                // The algebraic calculation for this is really complicated, but the algorithmic
                // approach is simple, so use the algorithmic approach.
                // Subtract the number of items in each row until we have less than the items
                // in the current row.
                var total = rawIdx
                var majorRowIdx = 0
                while(total >= xItemCount - majorRowIdx) {
                  total -= xItemCount - majorRowIdx
                  majorRowIdx += 1
                }
                // Now get the minor row index.  We skip majorRowIdx items in each row
                // since its a triangular matrix.
                val minorRowIdx = majorRowIdx + rawIdx - getRowIndexStart(xItemCount, majorRowIdx)
                // If we switched major rows, get a new view on the row.
                if (majorRowIdx != savedMajorRowIndex) {
                  majorRow = X.viewRow(majorRowIdx)
                  savedMajorRowIndex = majorRowIdx
                }
                // Do the calc
                val cellValue = rowFunction(majorRow, X.viewRow(minorRowIdx))
                // Save the result in both index combinations.
                result.setQuick(majorRowIdx, minorRowIdx, cellValue)
                if (majorRowIdx != minorRowIdx)
                  result.setQuick(minorRowIdx, majorRowIdx, cellValue)
              })
            }
        })
      })
      ConcurrencyParams.waitForCompletion(futures)
    }
    else {
      logger.finest("Processing sequentially" + X.rows + "x" + X.columns + ")...")

      (0 until X.rows).foreach(row1Idx => {
        val majorRow = X.viewRow(row1Idx)
        (row1Idx until X.rows).foreach(row2Idx => {
          val minorRow = X.viewRow(row2Idx)
          val cellValue = rowFunction(majorRow, minorRow)
          result.setQuick(row1Idx, row2Idx, cellValue)
          if (row1Idx != row2Idx)
            result.setQuick(row2Idx, row1Idx, cellValue)
        })
      })
    }
    result
  }

  def processRows[T](X: Matrix2D[T], Y: Matrix2D[T], rowFunction: VectorVectorFunction[T]): Matrix2D[T] = {
    processRows(X, Y, X.like2D(X.rows, Y.rows), rowFunction)
  }

  def processRows[T](X: Matrix2D[T], Y: Matrix2D[T], result: Matrix2D[T], rowFunction: VectorVectorFunction[T]): Matrix2D[T] = {
    checkColumnsEqual(X, Y)
    checkRowsEqual(X, result)
    checkColumnsEqualsRows(result, Y)

    if (calculateRowsInParallel(X, Y) && canWriteInParallel(result)) {
      logger.finest("Processing in parallel (" + X.rows + "x" + Y.rows + ")...")

      val xItemCount = X.rows
      val yItemCount = Y.rows
      val totalItemCount = xItemCount*yItemCount

      val threadCount = math.min(ConcurrencyParams.getNumberOfThreads, totalItemCount)
      val futures = new Array[Future[_]](threadCount)
      // This rounds down items-per-iteration.  It leaves any extra items in the last iteration.
      val itemsPerIteration = totalItemCount / threadCount

      (0 until threadCount).foreach(threadIdx => {
        val iterationStart = threadIdx * itemsPerIteration
        val iterationEnd = if (threadIdx == threadCount - 1) totalItemCount else iterationStart + itemsPerIteration
        futures(threadIdx) = ConcurrencyParams.submit(new Runnable() {
            def run() {
              var previousXIndex = -1
              var xRow: Matrix1D[T] = null
              (iterationStart until iterationEnd).foreach(rawIdx => {
                val xIndex = rawIdx / yItemCount
                val yIndex = rawIdx % yItemCount
                if (xIndex != previousXIndex) {
                  xRow = X.viewRow(xIndex)
                  previousXIndex = xIndex
                }
                val cellValue = rowFunction(xRow, Y.viewRow(yIndex))
                result.setQuick(xIndex, yIndex, cellValue)
              })
            }
        })
      })
      ConcurrencyParams.waitForCompletion(futures)
    }
    else {
      logger.finest("Processing sequentially (" + X.rows + "x" + Y.rows + ")...")

      (0 until X.rows).foreach(xIndex => {
        val xRow = X.viewRow(xIndex)
        (0 until Y.rows).foreach(yIndex => {
          val yRow = Y.viewRow(yIndex)
          val cellValue = rowFunction(xRow, yRow)
          result.setQuick(xIndex, yIndex, cellValue)
        })
      })
    }
    result
  }

  def processRows[T](X: Matrix2D[T], rowFunction: VectorFunction[T]): Matrix1D[T] = {
    processRows[T](X, X.like1D(X.rows), rowFunction)
  }

  def processRows[T](X: Matrix2D[T], result: Matrix1D[T], rowFunction: VectorFunction[T]): Matrix1D[T] = {
    checkRowsEqualsSize(X, result)

    if (canWriteInParallel(result) && calculateRowsInParallel(X)) {
      logger.finest("Processing rows in parallel (" + X.rows + "x" + X.columns + ")...")

      val totalItemCount = X.rows

      val threadCount = math.min(ConcurrencyParams.getNumberOfThreads, totalItemCount)
      val futures = new Array[Future[_]](threadCount)
      // This rounds down items-per-iteration.  It leaves any extra items in the last iteration.
      val itemsPerIteration = totalItemCount / threadCount

      (0 until threadCount).foreach(threadIdx => {
        val iterationStart = threadIdx * itemsPerIteration
        val iterationEnd = if (threadIdx == threadCount - 1) totalItemCount else iterationStart + itemsPerIteration
        futures(threadIdx) = ConcurrencyParams.submit(new Runnable() {
          def run() {
            (iterationStart until iterationEnd).foreach(rowIdx => {
              val cellValue = rowFunction(rowIdx, X.viewRow(rowIdx))
              result.setQuick(rowIdx, cellValue)
            })
          }
        })
      })
      ConcurrencyParams.waitForCompletion(futures)
    }
    else {
      logger.finest("Processing rows sequentially (" + X.rows + "x" + X.columns + ")...")

      (0 until X.rows).foreach(rowIdx => {
        val cellValue = rowFunction(rowIdx, X.viewRow(rowIdx))
        result.setQuick(rowIdx, cellValue)
      })
    }
    result
  }

  def processColumns[T](X: Matrix2D[T], columnFunction: VectorFunction[T]): Matrix1D[T] = {
    processColumns(X, X.like1D(X.columns), columnFunction)
  }

  def processColumns[T](X: Matrix2D[T], result: Matrix1D[T], columnFunction: VectorFunction[T]): Matrix1D[T] = {
    checkColumnsEqualsSize(X, result)

    if (canWriteInParallel(result) && calculateColumnsInParallel(X)) {
      logger.finest("Processing columns in parallel (" + X.rows + "x" + X.columns + ")...")

      val totalItemCount = X.columns

      val threadCount = math.min(ConcurrencyParams.getNumberOfThreads, totalItemCount)
      val futures = new Array[Future[_]](threadCount)
      // This rounds down items-per-iteration.  It leaves any extra items in the last iteration.
      val itemsPerIteration = totalItemCount / threadCount

      (0 until threadCount).foreach(threadIdx => {
        val iterationStart = threadIdx * itemsPerIteration
        val iterationEnd = if (threadIdx == threadCount - 1) totalItemCount else iterationStart + itemsPerIteration
        futures(threadIdx) = ConcurrencyParams.submit(new Runnable() {
          def run() {
            (iterationStart until iterationEnd).foreach(colIdx => {
              val cellValue = columnFunction(colIdx, X.viewColumn(colIdx))
              result.setQuick(colIdx, cellValue)
            })
          }
        })
      })
      ConcurrencyParams.waitForCompletion(futures)
    }
    else {
      logger.finest("Processing columns sequentially (" + X.rows + "x" + X.columns + ")...")

      (0 until X.columns).foreach(colIdx => {
        val cellValue = columnFunction(colIdx, X.viewColumn(colIdx))
        result.setQuick(colIdx, cellValue)
      })
    }
    result
  }

  def processColumns[T](X: Matrix2D[T], columnFunction: IntProcedure) {
    processIndexes(X.columns, columnFunction, parallelAllowed = canReadInParallel(X) && canWriteInParallel(X))
  }

  def processRows[T](X: Matrix2D[T], rowFunction: IntProcedure) {
    processIndexes(X.rows, rowFunction, parallelAllowed = canReadInParallel(X) && canWriteInParallel(X))
  }

  def processRows[T](X: Matrix2D[T], result: Matrix1D[T], rowFunction: Function1[Int, T]) {
    processIndexes(X.rows, result, rowFunction, parallelAllowed = canReadInParallel(X) && canWriteInParallel(result))
  }

  def processIndexes[T](totalItemCount: Int, result: Matrix1D[T], rowFunction: Function1[Int, T]): Matrix1D[T] = {
    processIndexes(totalItemCount, result, rowFunction, parallelAllowed=true)
  }

  def processIndexes[T](totalItemCount: Int, result: Matrix1D[T], rowFunction: Function1[Int, T], parallelAllowed: Boolean): Matrix1D[T] = {
    checkSizeAtLeast(result, totalItemCount)

    if (parallelAllowed && canWriteInParallel(result) && calculateIndexesInParallel(totalItemCount)) {
      logger.finest("Processing in parallel (" + totalItemCount + "x?)...")

      val threadCount = math.min(ConcurrencyParams.getNumberOfThreads, totalItemCount)
      val futures = new Array[Future[_]](threadCount)
      // This rounds down items-per-iteration.  It leaves any extra items in the last iteration.
      val itemsPerIteration = totalItemCount / threadCount

      (0 until threadCount).foreach(threadIdx => {
        val iterationStart = threadIdx * itemsPerIteration
        val iterationEnd = if (threadIdx == threadCount - 1) totalItemCount else iterationStart + itemsPerIteration
        futures(threadIdx) = ConcurrencyParams.submit(new Runnable() {
            def run() {
              (iterationStart until iterationEnd).foreach(rowIdx => {
                val cellValue = rowFunction(rowIdx)
                result.setQuick(rowIdx, cellValue)
              })
            }
        })
      })
      ConcurrencyParams.waitForCompletion(futures)
    }
    else {
      logger.finest("Processing sequentially (" + totalItemCount + "x?)...")

      (0 until totalItemCount).foreach(rowIdx => {
        val cellValue = rowFunction(rowIdx)
        result.setQuick(rowIdx, cellValue)
      })
    }
    result
  }

  def processIndexes(totalItemCount: Int, func: IntProcedure, parallelAllowed: Boolean) {

    if (parallelAllowed && calculateIndexesInParallel(totalItemCount)) {
      logger.finest("Processing in parallel (" + totalItemCount + "x?)...")

      val threadCount = math.min(ConcurrencyParams.getNumberOfThreads, totalItemCount)
      val futures = new Array[Future[_]](threadCount)
      // This rounds down items-per-iteration.  It leaves any extra items in the last iteration.
      val itemsPerIteration = totalItemCount / threadCount

      (0 until threadCount).foreach(threadIdx => {
        val iterationStart = threadIdx * itemsPerIteration
        val iterationEnd = if (threadIdx == threadCount - 1) totalItemCount else iterationStart + itemsPerIteration
        futures(threadIdx) = ConcurrencyParams.submit(new Runnable() {
            def run() {
              (iterationStart until iterationEnd).foreach(idx => {
                func(idx)
              })
            }
        })
      })
      ConcurrencyParams.waitForCompletion(futures)
    }
    else {
      logger.finest("Processing sequentially (" + totalItemCount + "x?)...")

      (0 until totalItemCount).foreach(idx => {
        func(idx)
      })
    }
  }

  def processCells[T](result: Matrix2D[T], cellFunction: Function2[Int, Int, T]): Matrix2D[T] = {
    processCells(result.rows, result.columns, result, cellFunction, parallelAllowed = canReadInParallel(result))
  }

  def processCells[T](X: Matrix2D[T], result: Matrix2D[T], cellFunction: Function2[Int, Int, T]): Matrix2D[T] = {
    processCells(X.rows, X.columns, result, cellFunction, parallelAllowed = canReadInParallel(X))
  }

  def processNonZeroCells[T](X: Matrix2D[T], cellFunction: Function3[Int, Int, T, T]): Matrix2D[T] = {
    processNonZeroCells(X, X.like2D(X.rows, X.columns), cellFunction)
  }

  def processNonZeroCells[T](X: Matrix2D[T], result: Matrix2D[T], cellFunction: Function3[Int, Int, T, T]): Matrix2D[T] = {
    val xItemCount = X.rows
    val yItemCount = X.columns

    checkRowsEqual(result, xItemCount)
    checkColumnsEqual(result, yItemCount)

    if (canReadInParallel(X) && canWriteInParallel(result) && calculateIndexesInParallel(xItemCount, yItemCount)) {
      logger.finest("Processing non-zeros in parallel (" + X.rows + "x" + X.columns + ")...")

      val totalItemCount = xItemCount

      val threadCount = math.min(ConcurrencyParams.getNumberOfThreads, totalItemCount)
      val futures = new Array[Future[_]](threadCount)
      // This rounds down items-per-iteration.  It leaves any extra items in the last iteration.
      val itemsPerIteration = totalItemCount / threadCount

      (0 until threadCount).foreach(threadIdx => {
        val iterationStart = threadIdx * itemsPerIteration
        val iterationEnd = if (threadIdx == threadCount - 1) totalItemCount else iterationStart + itemsPerIteration
        futures(threadIdx) = ConcurrencyParams.submit(new Runnable() {
            def run() {
              (iterationStart until iterationEnd).foreach(xIndex => {
                X.forEachNonZeroInRow(xIndex, new Function3[Int, Int, T, T] {
                  def apply(rowIdx: Int, colIdx: Int, oldValue: T): T = {
                    val newValue = cellFunction(rowIdx, colIdx, oldValue)
                    result.setQuick(rowIdx, colIdx, newValue)
                    oldValue    // So nothing is changed in the original matrix
                  }
                })
              })
            }
        })
      })
      ConcurrencyParams.waitForCompletion(futures)
    }
    else {
      logger.finest("Processing non-zeros sequentially (" + X.rows + "x" + X.columns + ")...")

      (0 until xItemCount).foreach(xIndex => {
        X.forEachNonZeroInRow(xIndex, new Function3[Int, Int, T, T] {
          def apply(rowIdx: Int, colIdx: Int, oldValue: T): T = {
            val newValue = cellFunction(rowIdx, colIdx, oldValue)
            result.setQuick(rowIdx, colIdx, newValue)
            oldValue    // So nothing is changed in the original matrix
          }
        })
      })
    }
    result
  }

  def processCells[T](xItemCount: Int, yItemCount: Int, result: Matrix2D[T], cellFunction: Function2[Int, Int, T], parallelAllowed: Boolean): Matrix2D[T] = {
    checkRowsEqual(result, xItemCount)
    checkColumnsEqual(result, yItemCount)

    if (parallelAllowed && canWriteInParallel(result) && calculateIndexesInParallel(xItemCount, yItemCount)) {
      logger.finest("Processing in parallel (" + xItemCount + "x" + yItemCount + ")...")

      val totalItemCount = xItemCount*yItemCount

      val threadCount = math.min(ConcurrencyParams.getNumberOfThreads, totalItemCount)
      val futures = new Array[Future[_]](threadCount)
      // This rounds down items-per-iteration.  It leaves any extra items in the last iteration.
      val itemsPerIteration = totalItemCount / threadCount

      (0 until threadCount).foreach(threadIdx => {
        val iterationStart = threadIdx * itemsPerIteration
        val iterationEnd = if (threadIdx == threadCount - 1) totalItemCount else iterationStart + itemsPerIteration
        futures(threadIdx) = ConcurrencyParams.submit(new Runnable() {
            def run() {
              (iterationStart until iterationEnd).foreach(rawIdx => {
                val xIndex = rawIdx / yItemCount
                val yIndex = rawIdx % yItemCount
                val cellValue = cellFunction(xIndex, yIndex)
                result.setQuick(xIndex, yIndex, cellValue)
              })
            }
        })
      })
      ConcurrencyParams.waitForCompletion(futures)
    }
    else {
      logger.finest("Processing sequentially (" + xItemCount + "x" + yItemCount + ")...")

      (0 until xItemCount).foreach(xIndex => {
        (0 until yItemCount).foreach(yIndex => {
          val cellValue = cellFunction(xIndex, yIndex)
          result.setQuick(xIndex, yIndex, cellValue)
        })
      })
    }
    result
  }

  def processCells[T](result: Matrix1D[T], cellFunction: Function2[Int, T, T]) {
    val totalItemCount = result.size.toInt

    val parallelAllowed = canReadInParallel(result) && canWriteInParallel(result)
    if (parallelAllowed && calculateIndexesInParallel(totalItemCount)) {
      logger.finest("Processing in parallel (" + totalItemCount + ")...")

      val threadCount = math.min(ConcurrencyParams.getNumberOfThreads, totalItemCount)
      val futures = new Array[Future[_]](threadCount)
      // This rounds down items-per-iteration.  It leaves any extra items in the last iteration.
      val itemsPerIteration = totalItemCount / threadCount

      (0 until threadCount).foreach(threadIdx => {
        val iterationStart = threadIdx * itemsPerIteration
        val iterationEnd = if (threadIdx == threadCount - 1) totalItemCount else iterationStart + itemsPerIteration
        futures(threadIdx) = ConcurrencyParams.submit(new Runnable() {
            def run() {
              (iterationStart until iterationEnd).foreach(rawIdx => {
                val oldValue = result.getQuick(rawIdx)
                val newValue = cellFunction(rawIdx, oldValue)
                if (oldValue != newValue)
                  result.setQuick(rawIdx, newValue)
              })
            }
        })
      })
      ConcurrencyParams.waitForCompletion(futures)
    }
    else {
      logger.finest("Processing sequentially (" + totalItemCount + ")...")

      (0 until totalItemCount).foreach(rawIndex => {
        val oldValue = result.getQuick(rawIndex)
        val newValue = cellFunction(rawIndex, oldValue)
        if (oldValue != newValue)
          result.setQuick(rawIndex, newValue)
      })
    }
  }

  def processCellIndexes[T](result: Matrix1D[T], cellFunction: Function1[Int, T]) {
    val totalItemCount = result.size.toInt

    val parallelAllowed = canWriteInParallel(result)
    if (parallelAllowed && calculateIndexesInParallel(totalItemCount)) {
      logger.finest("Processing in parallel (" + totalItemCount + ")...")

      val threadCount = math.min(ConcurrencyParams.getNumberOfThreads, totalItemCount)
      val futures = new Array[Future[_]](threadCount)
      // This rounds down items-per-iteration.  It leaves any extra items in the last iteration.
      val itemsPerIteration = totalItemCount / threadCount

      (0 until threadCount).foreach(threadIdx => {
        val iterationStart = threadIdx * itemsPerIteration
        val iterationEnd = if (threadIdx == threadCount - 1) totalItemCount else iterationStart + itemsPerIteration
        futures(threadIdx) = ConcurrencyParams.submit(new Runnable() {
            def run() {
              (iterationStart until iterationEnd).foreach(rawIdx => {
                val newValue = cellFunction(rawIdx)
                result.setQuick(rawIdx, newValue)
              })
            }
        })
      })
      ConcurrencyParams.waitForCompletion(futures)
    }
    else {
      logger.finest("Processing sequentially (" + totalItemCount + ")...")

      (0 until totalItemCount).foreach(rawIdx => {
        val newValue = cellFunction(rawIdx)
        result.setQuick(rawIdx, newValue)
      })
    }
  }

  def processCells[T](result: Matrix2D[T], cellFunction: Function3[Int, Int, T, T]) {
    val xItemCount = result.rows
    val yItemCount = result.columns
    val parallelAllowed = canReadInParallel(result) && canWriteInParallel(result)
    if (parallelAllowed && calculateIndexesInParallel(xItemCount, yItemCount)) {
      logger.finest("Processing in parallel (" + xItemCount + "x" + yItemCount + ")...")

      val totalItemCount = xItemCount*yItemCount

      val threadCount = math.min(ConcurrencyParams.getNumberOfThreads, totalItemCount)
      val futures = new Array[Future[_]](threadCount)
      // This rounds down items-per-iteration.  It leaves any extra items in the last iteration.
      val itemsPerIteration = totalItemCount / threadCount

      (0 until threadCount).foreach(threadIdx => {
        val iterationStart = threadIdx * itemsPerIteration
        val iterationEnd = if (threadIdx == threadCount - 1) totalItemCount else iterationStart + itemsPerIteration
        futures(threadIdx) = ConcurrencyParams.submit(new Runnable() {
            def run() {
              (iterationStart until iterationEnd).foreach(rawIdx => {
                val xIndex = rawIdx / yItemCount
                val yIndex = rawIdx % yItemCount
                val oldValue = result.getQuick(xIndex, yIndex)
                val newValue = cellFunction(xIndex, yIndex, oldValue)
                if (oldValue != newValue)
                  result.setQuick(xIndex, yIndex, newValue)
              })
            }
        })
      })
      ConcurrencyParams.waitForCompletion(futures)
    }
    else {
      logger.finest("Processing sequentially (" + xItemCount + "x" + yItemCount + ")...")

      (0 until xItemCount).foreach(xIndex => {
        (0 until yItemCount).foreach(yIndex => {
          val oldValue = result.getQuick(xIndex, yIndex)
          val newValue = cellFunction(xIndex, yIndex, oldValue)
          if (oldValue != newValue)
            result.setQuick(xIndex, yIndex, newValue)
        })
      })
    }
  }

  def processCells[T](result: Matrix1D[T], cellFunction: Function1[T, T]) {
    val totalItemCount = result.size.toInt

    val parallelAllowed = canWriteInParallel(result)
    if (parallelAllowed && calculateIndexesInParallel(totalItemCount)) {
      logger.finest("Processing in parallel (" + totalItemCount + ")...")

      val threadCount = math.min(ConcurrencyParams.getNumberOfThreads, totalItemCount)
      val futures = new Array[Future[_]](threadCount)
      // This rounds down items-per-iteration.  It leaves any extra items in the last iteration.
      val itemsPerIteration = totalItemCount / threadCount

      (0 until threadCount).foreach(threadIdx => {
        val iterationStart = threadIdx * itemsPerIteration
        val iterationEnd = if (threadIdx == threadCount - 1) totalItemCount else iterationStart + itemsPerIteration
        futures(threadIdx) = ConcurrencyParams.submit(new Runnable() {
            def run() {
              (iterationStart until iterationEnd).foreach(rawIdx => {
                val oldValue = result.getQuick(rawIdx)
                val newValue = cellFunction(oldValue)
                result.setQuick(rawIdx, newValue)
              })
            }
        })
      })
      ConcurrencyParams.waitForCompletion(futures)
    }
    else {
      logger.finest("Processing sequentially (" + totalItemCount + ")...")

      (0 until totalItemCount).foreach(rawIdx => {
        val oldValue = result.getQuick(rawIdx)
        val newValue = cellFunction(oldValue)
        result.setQuick(rawIdx, newValue)
      })
    }
  }

  def processCells[T](result: Matrix2D[T], cellFunction: Function1[T, T]) {
    val xItemCount = result.rows
    val yItemCount = result.columns
    val parallelAllowed = canReadInParallel(result) && canWriteInParallel(result)
    if (parallelAllowed && calculateIndexesInParallel(xItemCount, yItemCount)) {
      logger.finest("Processing in parallel (" + xItemCount + "x" + yItemCount + ")...")

      val totalItemCount = xItemCount*yItemCount

      val threadCount = math.min(ConcurrencyParams.getNumberOfThreads, totalItemCount)
      val futures = new Array[Future[_]](threadCount)
      // This rounds down items-per-iteration.  It leaves any extra items in the last iteration.
      val itemsPerIteration = totalItemCount / threadCount

      (0 until threadCount).foreach(threadIdx => {
        val iterationStart = threadIdx * itemsPerIteration
        val iterationEnd = if (threadIdx == threadCount - 1) totalItemCount else iterationStart + itemsPerIteration
        futures(threadIdx) = ConcurrencyParams.submit(new Runnable() {
            def run() {
              (iterationStart until iterationEnd).foreach(rawIdx => {
                val xIndex = rawIdx / yItemCount
                val yIndex = rawIdx % yItemCount
                val oldValue = result.getQuick(xIndex, yIndex)
                val newValue = cellFunction(oldValue)
                if (oldValue != newValue)
                  result.setQuick(xIndex, yIndex, newValue)
              })
            }
        })
      })
      ConcurrencyParams.waitForCompletion(futures)
    }
    else {
      logger.finest("Processing sequentially (" + xItemCount + "x" + yItemCount + ")...")

      (0 until xItemCount).foreach(xIndex => {
        (0 until yItemCount).foreach(yIndex => {
          val oldValue = result.getQuick(xIndex, yIndex)
          val newValue = cellFunction(oldValue)
          if (oldValue != newValue)
            result.setQuick(xIndex, yIndex, newValue)
        })
      })
    }
  }

  def processCells[T](X: Matrix2D[T], cellProcedure: IntIntProcedure) {
    processCells(X.rows, X.columns, cellProcedure, parallelAllowed = canReadInParallel(X) && canWriteInParallel(X))
  }

  def processCells[T](X: Matrix1D[T], cellProcedure: IntProcedure) {
    processCells(X.size.toInt, cellProcedure, parallelAllowed = canReadInParallel(X) && canWriteInParallel(X))
  }

  def processCells(xItemCount: Int, yItemCount: Int, cellProcedure: IntIntProcedure, parallelAllowed: Boolean) {
    if (parallelAllowed && calculateIndexesInParallel(xItemCount, yItemCount)) {
      logger.finest("Processing in parallel (" + xItemCount + "x" + yItemCount + ")...")

      val totalItemCount = xItemCount*yItemCount

      val threadCount = math.min(ConcurrencyParams.getNumberOfThreads, totalItemCount)
      val futures = new Array[Future[_]](threadCount)
      // This rounds down items-per-iteration.  It leaves any extra items in the last iteration.
      val itemsPerIteration = totalItemCount / threadCount

      (0 until threadCount).foreach(threadIdx => {
        val iterationStart = threadIdx * itemsPerIteration
        val iterationEnd = if (threadIdx == threadCount - 1) totalItemCount else iterationStart + itemsPerIteration
        futures(threadIdx) = ConcurrencyParams.submit(new Runnable() {
            def run() {
              (iterationStart until iterationEnd).foreach(rawIdx => {
                val xIndex = rawIdx / yItemCount
                val yIndex = rawIdx % yItemCount
                cellProcedure(xIndex, yIndex)
              })
            }
        })
      })
      ConcurrencyParams.waitForCompletion(futures)
    }
    else {
      logger.finest("Processing sequentially (" + xItemCount + "x" + yItemCount + ")...")

      (0 until xItemCount).foreach(xIndex => {
        (0 until yItemCount).foreach(yIndex => {
          cellProcedure(xIndex, yIndex)
        })
      })
    }
  }

  def processCells(itemCount: Int, cellProcedure: IntProcedure, parallelAllowed: Boolean) {
    if (parallelAllowed && calculateIndexesInParallel(itemCount)) {
      logger.finest("Processing in parallel (" + itemCount + ")...")

      val totalItemCount = itemCount

      val threadCount = math.min(ConcurrencyParams.getNumberOfThreads, totalItemCount)
      val futures = new Array[Future[_]](threadCount)
      // This rounds down items-per-iteration.  It leaves any extra items in the last iteration.
      val itemsPerIteration = totalItemCount / threadCount

      (0 until threadCount).foreach(threadIdx => {
        val iterationStart = threadIdx * itemsPerIteration
        val iterationEnd = if (threadIdx == threadCount - 1) totalItemCount else iterationStart + itemsPerIteration
        futures(threadIdx) = ConcurrencyParams.submit(new Runnable() {
            def run() {
              (iterationStart until iterationEnd).foreach(rawIdx => {
                cellProcedure(rawIdx)
              })
            }
        })
      })
      ConcurrencyParams.waitForCompletion(futures)
    }
    else {
      logger.finest("Processing sequentially (" + itemCount + ")...")

      (0 until itemCount).foreach(rawIndex => {
        cellProcedure(rawIndex)
      })
    }
  }

  def aggregateCells(m: DoubleMatrix1D, aggr: DoubleDoubleFunction, cellFunction: DoubleFunction, cond: DoubleProcedure): Double = {
    val totalItemCount = m.size.toInt
    if (calculateIndexesInParallel(totalItemCount) && canReadInParallel(m)) {
      logger.finest("Processing in parallel (" + totalItemCount + ")...")

      val threadCount = math.min(ConcurrencyParams.getNumberOfThreads, totalItemCount)
      val futures = new Array[Future[_]](threadCount)
      // This rounds down items-per-iteration.  It leaves any extra items in the last iteration.
      val itemsPerIteration = totalItemCount / threadCount

      (0 until threadCount).foreach(threadIdx => {
        val iterationStart = threadIdx * itemsPerIteration
        val iterationEnd = if (threadIdx == threadCount - 1) totalItemCount else iterationStart + itemsPerIteration
        futures(threadIdx) = ConcurrencyParams.submit(new Callable[Double]() {
          def call(): Double = {
            var aggrValue = Double.NaN
            var doAggregation = false
            (iterationStart until iterationEnd).foreach(rawIdx => {
              val value = m.getQuick(rawIdx)
              if (cond == null || cond.apply(value)) {
                val cellValue = cellFunction(value)
                if (doAggregation)
                  aggrValue = aggr.apply(aggrValue, cellValue)
                else {
                  aggrValue = cellValue
                  doAggregation = true
                }
              }
            })
            aggrValue
          }
        })
      })
      ConcurrencyParams.waitForCompletion(futures, aggr)
    }
    else {
      logger.finest("Processing sequentially (" + totalItemCount + ")...")

      var aggrValue = Double.NaN
      var doAggregation = false
      for (idx <- 0 until totalItemCount) {
        val value = m.getQuick(idx)
        if (cond == null || cond.apply(value)) {
          val cellValue = cellFunction(value)
          if (doAggregation)
            aggrValue = aggr.apply(aggrValue, cellValue)
          else {
            aggrValue = cellValue
            doAggregation = true
          }
        }
      }
      aggrValue
    }
  }

  def aggregateCells(m: DoubleMatrix1D, aggr: DoubleDoubleFunction, cellFunction: IntDoubleFunction, cond: DoubleProcedure): Double = {
    val totalItemCount = m.size.toInt
    if (calculateIndexesInParallel(totalItemCount) && canReadInParallel(m)) {
      logger.finest("Processing in parallel (" + totalItemCount + ")...")

      val threadCount = math.min(ConcurrencyParams.getNumberOfThreads, totalItemCount)
      val futures = new Array[Future[_]](threadCount)
      // This rounds down items-per-iteration.  It leaves any extra items in the last iteration.
      val itemsPerIteration = totalItemCount / threadCount

      (0 until threadCount).foreach(threadIdx => {
        val iterationStart = threadIdx * itemsPerIteration
        val iterationEnd = if (threadIdx == threadCount - 1) totalItemCount else iterationStart + itemsPerIteration
        futures(threadIdx) = ConcurrencyParams.submit(new Callable[Double]() {
          def call(): Double = {
            var aggrValue = Double.NaN
            var doAggregation = false
            (iterationStart until iterationEnd).foreach(rawIdx => {
              val value = m.getQuick(rawIdx)
              if (cond == null || cond.apply(value)) {
                val cellValue = cellFunction(rawIdx, value)
                if (doAggregation)
                  aggrValue = aggr.apply(aggrValue, cellValue)
                else {
                  aggrValue = cellValue
                  doAggregation = true
                }
              }
            })
            aggrValue
          }
        })
      })
      ConcurrencyParams.waitForCompletion(futures, aggr)
    }
    else {
      logger.finest("Processing sequentially (" + totalItemCount + ")...")

      var aggrValue = Double.NaN
      var doAggregation = false
      for (idx <- 0 until totalItemCount) {
        val value = m.getQuick(idx)
        if (cond == null || cond.apply(value)) {
          val cellValue = cellFunction(idx, value)
          if (doAggregation)
            aggrValue = aggr.apply(aggrValue, cellValue)
          else {
            aggrValue = cellValue
            doAggregation = true
          }
        }
      }
      aggrValue
    }
  }

  def aggregateCells(m: DoubleMatrix2D, aggr: DoubleDoubleFunction, cellFunction: DoubleFunction, cond: DoubleProcedure): Double = {
    val xItemCount = m.rows
    val yItemCount = m.columns
    val totalItemCount = xItemCount*yItemCount
    if (calculateIndexesInParallel(totalItemCount) && canReadInParallel(m)) {
      logger.finest("Processing in parallel (" + m.rows + "x" + m.columns + ")...")

      val threadCount = math.min(ConcurrencyParams.getNumberOfThreads, totalItemCount)
      val futures = new Array[Future[_]](threadCount)
      // This rounds down items-per-iteration.  It leaves any extra items in the last iteration.
      val itemsPerIteration = totalItemCount / threadCount

      (0 until threadCount).foreach(threadIdx => {
        val iterationStart = threadIdx * itemsPerIteration
        val iterationEnd = if (threadIdx == threadCount - 1) totalItemCount else iterationStart + itemsPerIteration
        futures(threadIdx) = ConcurrencyParams.submit(new Callable[Double]() {
          def call(): Double = {
            var aggrValue = Double.NaN
            var doAggregation = false
            (iterationStart until iterationEnd).foreach(rawIdx => {
              val xIndex = rawIdx / yItemCount
              val yIndex = rawIdx % yItemCount
              val value = m.getQuick(xIndex, yIndex)
              if (cond == null || cond.apply(value)) {
                val cellValue = cellFunction(value)
                if (doAggregation)
                  aggrValue = aggr.apply(aggrValue, cellValue)
                else {
                  aggrValue = cellValue
                  doAggregation = true
                }
              }
            })
            aggrValue
          }
        })
      })
      ConcurrencyParams.waitForCompletion(futures, aggr)
    }
    else {
      logger.finest("Processing sequentially (" + xItemCount + "x" + yItemCount + ")...")

      var aggrValue = Double.NaN
      var doAggregation = false
      for (r <- 0 until xItemCount) {
          for (c <- 0 until yItemCount) {
            val value = m.getQuick(r, c)
            if (cond == null || cond.apply(value)) {
              val cellValue = cellFunction(value)
              if (doAggregation)
                aggrValue = aggr.apply(aggrValue, cellValue)
              else {
                aggrValue = cellValue
                doAggregation = true
              }
            }
          }
      }
      aggrValue
    }
  }

  def aggregateCells(m: DoubleMatrix2D, aggr: DoubleDoubleFunction, cellFunction: IntIntDoubleFunction, cond: DoubleProcedure): Double = {
    val xItemCount = m.rows
    val yItemCount = m.columns
    val totalItemCount = xItemCount*yItemCount
    if (calculateIndexesInParallel(totalItemCount) && canReadInParallel(m)) {
      logger.finest("Processing in parallel (" + m.rows + "x" + m.columns + ")...")

      val threadCount = math.min(ConcurrencyParams.getNumberOfThreads, totalItemCount)
      val futures = new Array[Future[_]](threadCount)
      // This rounds down items-per-iteration.  It leaves any extra items in the last iteration.
      val itemsPerIteration = totalItemCount / threadCount

      (0 until threadCount).foreach(threadIdx => {
        val iterationStart = threadIdx * itemsPerIteration
        val iterationEnd = if (threadIdx == threadCount - 1) totalItemCount else iterationStart + itemsPerIteration
        futures(threadIdx) = ConcurrencyParams.submit(new Callable[Double]() {
          def call(): Double = {
            var aggrValue = Double.NaN
            var doAggregation = false
            (iterationStart until iterationEnd).foreach(rawIdx => {
              val xIndex = rawIdx / yItemCount
              val yIndex = rawIdx % yItemCount
              val value = m.getQuick(xIndex, yIndex)
              if (cond == null || cond.apply(value)) {
                val cellValue = cellFunction(xIndex, yIndex, value)
                if (doAggregation)
                  aggrValue = aggr.apply(aggrValue, cellValue)
                else {
                  aggrValue = cellValue
                  doAggregation = true
                }
              }
            })
            aggrValue
          }
        })
      })
      ConcurrencyParams.waitForCompletion(futures, aggr)
    }
    else {
      logger.finest("Processing sequentially (" + xItemCount + "x" + yItemCount + ")...")

      var aggrValue = Double.NaN
      var doAggregation = false
      for (r <- 0 until xItemCount) {
          for (c <- 0 until yItemCount) {
            val value = m.getQuick(r, c)
            if (cond == null || cond.apply(value)) {
              val cellValue = cellFunction(r, c, value)
              if (doAggregation)
                aggrValue = aggr.apply(aggrValue, cellValue)
              else {
                aggrValue = cellValue
                doAggregation = true
              }
            }
          }
      }
      aggrValue
    }
  }
}

object MatrixProcessor {
  val singleton = new MatrixProcessor()
}

