package cern.colt.matrix

import edu.emory.mathcs.utils.ConcurrencyUtils
import scala.collection.mutable
import java.util.concurrent._
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

  private var THREAD_POOL: ExecutorService = Executors.newCachedThreadPool(new CustomThreadFactory(new CustomExceptionHandler()))

  private class CustomExceptionHandler extends Thread.UncaughtExceptionHandler {

    def uncaughtException(t: Thread, e: Throwable) {
      e.printStackTrace()
    }
  }

  object CustomThreadFactory {

    private val defaultFactory = Executors.defaultThreadFactory()
  }

  private class CustomThreadFactory(val handler: Thread.UncaughtExceptionHandler)
      extends ThreadFactory {

    def newThread(r: Runnable): Thread = {
      val t = CustomThreadFactory.defaultFactory.newThread(r)
      t.setUncaughtExceptionHandler(handler)
      t.setDaemon(true)
      t
    }
  }

  /**
   * Shutdowns the thread pool.
   */
  def shutdown() {
    THREAD_POOL.shutdown()
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
    if (THREAD_POOL.isShutdown || THREAD_POOL.isTerminated) {
      THREAD_POOL = Executors.newCachedThreadPool(new CustomThreadFactory(new CustomExceptionHandler()))
    }
    THREAD_POOL.submit(task)
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
    if (THREAD_POOL.isShutdown || THREAD_POOL.isTerminated) {
      THREAD_POOL = Executors.newCachedThreadPool(new CustomThreadFactory(new CustomExceptionHandler()))
    }
    THREAD_POOL.submit(task)
  }

  /**
   * Waits for all threads to complete computation.
   *
   * @param futures
   *            handles to running threads
   */
  def waitForCompletion(futures: Array[Future[_]]) {
    val size = futures.length
    try {
      for (j <- 0 until size) {
        futures(j).get
      }
    } catch {
      case ex: ExecutionException => ex.printStackTrace()
      case e: InterruptedException => e.printStackTrace()
    }
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
  def waitForCompletion[T: Manifest: Numeric](futures: Array[Future[T]], aggr: Function2[T, T, T]): T = {
    val size = futures.length
    val results = Array.ofDim[T](size)
    var a: T = implicitly[Numeric[T]].zero
    try {
      for (j <- 0 until size) {
        results(j) = futures(j).get
      }
      a = results(0)
      for (j <- 1 until size) {
        a = aggr.apply(a, results(j))
      }
    } catch {
      case ex: ExecutionException => ex.printStackTrace()
      case e: InterruptedException => e.printStackTrace()
    }
    a
  }

}
