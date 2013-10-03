package cern.colt.matrix

import edu.emory.mathcs.utils.ConcurrencyUtils
import scala.collection.mutable
import java.util.concurrent.{Future, Callable}
import java.util.logging.Logger
import cern.colt.matrix.impl.{SparseCCMatrix2D, SparseHashMatrix1D, SparseRCMatrix2D, SparseHashMatrix2D}

/**
 */
object ConcurrencyParams {

  private val logger = Logger.getLogger(ConcurrencyParams.getClass.getCanonicalName)

  ConcurrencyUtils.setNumberOfThreads(1)    // Force it to always be 1

  // Use strings because some classes are not visible everywhere
  private var nonParallelReadMatrixTypes = new mutable.HashSet[String]()
  private var nonParallelWriteMatrixTypes = new mutable.HashSet[String]()

  // These classes have synchronized set/get methods.  If they're accessed in parallel the
  // synchronization slows everything to a crawl.  So make it so these classes always are processed
  // sequentially.
  nonParallelWriteMatrixTypes.add(classOf[SparseHashMatrix2D[_]].getName)
  nonParallelWriteMatrixTypes.add(classOf[SparseRCMatrix2D[_]].getName)
  nonParallelWriteMatrixTypes.add(classOf[SparseHashMatrix1D[_]].getName)
  nonParallelWriteMatrixTypes.add(classOf[SparseCCMatrix2D[_]].getName)
  // TODO: What about the WrapperDouble* or Delegate* classes?  They could enclose one of the above classes.
  // TODO: It's really only writes that need to be synchronized above.  However, the getQuick() methods
  // are also synchronized, so parallel access can still grind to a halt.

  private var numThreads: Int = ConcurrencyUtils.getNumberOfProcessors - 2       // Two processors to allow for GC

  /**
   * Sets the number of threads
   *
   * @param n  Number of threads
   */
  def setNumberOfThreads(n: Int) {
      if (n < 1)
          throw new IllegalArgumentException("n must be greater or equal 1")
    numThreads = n
  }

  /**
   * Returns the current number of threads.
   *
   * @return the current number of threads.
   */
  def getNumberOfThreads: Int = numThreads


  def addNonParallelReadMatrixClass(c: Class[_]) {
    nonParallelReadMatrixTypes.add(c.getName)
  }

  def addNonParallelWriteMatrixClass(c: Class[_]) {
    nonParallelWriteMatrixTypes.add(c.getName)
  }

  private var threadsBeginN_RowsTimesColumns: Int = 1000

  def getThreadsBeginN_RowsTimesColumns: Int = threadsBeginN_RowsTimesColumns

  def setThreadsBeginN_RowsTimesColumns(value: Int) {
    threadsBeginN_RowsTimesColumns = value
  }

  private var threadsBeginN_RowsOrColumns: Int = 1000

  def getThreadsBeginN_RowsOrColumns: Int = threadsBeginN_RowsOrColumns

  def setThreadsBeginN_RowsOrColumns(value: Int) {
    threadsBeginN_RowsOrColumns = value
  }

  def canReadInParallel(origClass: Class[_]): Boolean = {
    var matrixClass = origClass
    while (matrixClass != null && matrixClass != classOf[Object]) {
      if ( nonParallelReadMatrixTypes.contains(matrixClass.getName) ) {
        if (matrixClass != origClass)
          logger.finest("Non-parallel-reads matrix class: " + origClass.getName + " (" + matrixClass.getName + ")")
        else
          logger.finest("Non-parallel-reads matrix class: " + origClass.getName)
        return false
      }
      matrixClass = matrixClass.getSuperclass
    }
    logger.finest("Parallel-reads matrix class: " + origClass.getName)
    true
  }

  def canReadInParallel(x: Matrix[_]): Boolean = {
    canReadInParallel( x.getClass )
  }

  def canWriteInParallel(origClass: Class[_]): Boolean = {
    if ( ! canReadInParallel(origClass) )
      return false

    var matrixClass = origClass
    while (matrixClass != null && matrixClass != classOf[Object]) {
      if ( nonParallelWriteMatrixTypes.contains(matrixClass.getName) ) {
        if (matrixClass != origClass)
          logger.finest("Non-parallel-writes matrix class: " + origClass.getName + " (" + matrixClass.getName + ")")
        else
          logger.finest("Non-parallel-writes matrix class: " + origClass.getName)
        return false
      }
      matrixClass = matrixClass.getSuperclass
    }
    logger.finest("Parallel-writes matrix class: " + origClass.getName)
    true
  }

  def canWriteInParallel(x: Matrix[_]): Boolean = {
    canWriteInParallel( x.getClass )
  }

  def calculateRowsInParallel(X: Matrix2D[_]): Boolean = {
    getNumberOfThreads > 1 && X.rows >= ConcurrencyParams.getThreadsBeginN_RowsOrColumns && canReadInParallel(X)
  }

  def calculateRowsInParallel(X: Matrix2D[_], Y: Matrix2D[_]): Boolean = {
    getNumberOfThreads > 1 && X.rows*Y.rows >= ConcurrencyParams.getThreadsBeginN_RowsTimesColumns &&
      canReadInParallel(X) && canReadInParallel(Y)
  }

  def calculateInParallel(X: Int, Y: Int): Boolean = {
    getNumberOfThreads > 1 && X*Y >= ConcurrencyParams.getThreadsBeginN_RowsTimesColumns
  }

  def calculateInParallel(X: Int): Boolean = {
    getNumberOfThreads > 1 && X >= ConcurrencyParams.getThreadsBeginN_RowsOrColumns
  }

  def calculateColumnsInParallel(X: Matrix2D[_]): Boolean = {
    getNumberOfThreads > 1 && X.columns >= ConcurrencyParams.getThreadsBeginN_RowsOrColumns
  }

  def calculateColumnsInParallel(X: Matrix2D[_], Y: Matrix2D[_]): Boolean = {
    getNumberOfThreads > 1 && X.columns*Y.columns >= ConcurrencyParams.getThreadsBeginN_RowsTimesColumns
  }

  /**
   * Submits a value-returning task for execution and returns a Future
   * representing the pending results of the task.
   *
   * @param task
   *            task for execution
   * @return a handle to the task submitted for execution
   */
  def submit[T](task: Callable[T]): Future[T] = {
    ConcurrencyUtils.submit(task)
  }

  /**
   * Submits a Runnable task for execution and returns a Future representing
   * that task.
   *
   * @param task
   *            task for execution
   * @return a handle to the task submitted for execution
   */
  def submit(task: Runnable): Future[_] = {
    ConcurrencyUtils.submit(task)
  }

  /**
   * Waits for all threads to complete computation.
   *
   * @param futures
   *            handles to running threads
   */
  def waitForCompletion(futures: Array[Future[_]]) {
    ConcurrencyUtils.waitForCompletion(futures)
  }

  /**
   * Waits for all threads to complete computation and aggregates the result.
   *
   * @param futures
   *            handles to running threads
   * @param aggr
   *            an aggregation function
   * @return the result of aggregation
   */
  def waitForCompletion(futures: Array[Future[_]], aggr: Function2[Double, Double, Double]): Double = {
    ConcurrencyUtils.waitForCompletion(futures, aggr)
  }

}
