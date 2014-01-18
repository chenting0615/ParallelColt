package edu.emory.mathcs.utils

object ConcurrencyUtils {

  private var NTHREADS: Int = getNumberOfProcessors

  private var THREADS_BEGIN_N_1D_FFT_2THREADS: Int = 8192

  private var THREADS_BEGIN_N_1D_FFT_4THREADS: Int = 65536

  private var THREADS_BEGIN_N_1D: Int = 32768

  private var THREADS_BEGIN_N_2D: Int = 65536

  private var THREADS_BEGIN_N_3D: Int = 65536

  /**
   * Causes the currently executing thread to sleep (temporarily cease
   * execution) for the specified number of milliseconds.
   *
   * @param millis Milliseconds to sleep
   */
  def sleep(millis: Long) {
    try {
      Thread.sleep(millis)
    } catch {
      case e: InterruptedException => e.printStackTrace()
    }
  }
  /**
   * Returns the number of available processors
   *
   * @return number of available processors
   */
  def getNumberOfProcessors: Int = {
    Runtime.getRuntime.availableProcessors()
  }

  /**
   * Returns the current number of threads.
   *
   * @return the current number of threads.
   */
  def getNumberOfThreads: Int = NTHREADS

  /**
   * Returns the minimal size of 1D data for which threads are used.
   *
   * @return the minimal size of 1D data for which threads are used
   */
  def getThreadsBeginN_1D: Int = THREADS_BEGIN_N_1D

  /**
   * Returns the minimal size of 1D data for which two threads are used.
   *
   * @return the minimal size of 1D data for which two threads are used
   */
  def getThreadsBeginN_1D_FFT_2Threads: Int = THREADS_BEGIN_N_1D_FFT_2THREADS

  /**
   * Returns the minimal size of 1D data for which four threads are used.
   *
   * @return the minimal size of 1D data for which four threads are used
   */
  def getThreadsBeginN_1D_FFT_4Threads: Int = THREADS_BEGIN_N_1D_FFT_4THREADS

  /**
   * Returns the minimal size of 2D data for which threads are used.
   *
   * @return the minimal size of 2D data for which threads are used
   */
  def getThreadsBeginN_2D: Int = THREADS_BEGIN_N_2D

  /**
   * Returns the minimal size of 3D data for which threads are used.
   *
   * @return the minimal size of 3D data for which threads are used
   */
  def getThreadsBeginN_3D: Int = THREADS_BEGIN_N_3D

  /**
   * Sets the minimal size of 1D data for which two threads are used.
   *
   * @param n
   *            the minimal size of 1D data for which two threads are used
   */
  def setThreadsBeginN_1D_FFT_2Threads(n: Int) {
    THREADS_BEGIN_N_1D_FFT_2THREADS = if (n < 512) 512 else n
  }

  /**
   * Sets the minimal size of 1D data for which four threads are used.
   *
   * @param n
   *            the minimal size of 1D data for which four threads are used
   */
  def setThreadsBeginN_1D_FFT_4Threads(n: Int) {
    THREADS_BEGIN_N_1D_FFT_4THREADS = if (n < 512) 512 else n
  }

  /**
   * Sets the minimal size of 1D data for which threads are used.
   *
   * @param n
   *            the minimal size of 1D data for which threads are used
   */
  def setThreadsBeginN_1D(n: Int) {
    THREADS_BEGIN_N_1D = n
  }

  /**
   * Sets the minimal size of 2D data for which threads are used.
   *
   * @param n
   *            the minimal size of 2D data for which threads are used
   */
  def setThreadsBeginN_2D(n: Int) {
    THREADS_BEGIN_N_2D = n
  }

  /**
   * Sets the minimal size of 3D data for which threads are used.
   *
   * @param n
   *            the minimal size of 3D data for which threads are used
   */
  def setThreadsBeginN_3D(n: Int) {
    THREADS_BEGIN_N_3D = n
  }

  /**
   * Resets the minimal size of 1D data for which two and four threads are
   * used.
   */
  def resetThreadsBeginN_FFT() {
    THREADS_BEGIN_N_1D_FFT_2THREADS = 8192
    THREADS_BEGIN_N_1D_FFT_4THREADS = 65536
  }

  /**
   * Resets the minimal size of 1D, 2D and 3D data for which threads are used.
   */
  def resetThreadsBeginN() {
    THREADS_BEGIN_N_1D = 32768
    THREADS_BEGIN_N_2D = 65536
    THREADS_BEGIN_N_3D = 65536
  }

  /**
   * Sets the number of threads
   *
   * @param n Number of threads
   */
  def setNumberOfThreads(n: Int) {
    if (n < 1) throw new IllegalArgumentException("n must be greater or equal 1")
    NTHREADS = n
  }

  /**
   * Returns the closest power of two greater than or equal to x.
   * @return the closest power of two greater than or equal to x
   */
  def nextPow2(xP: Int): Int = {
    var x = xP
    if (x < 1) throw new IllegalArgumentException("x must be greater or equal 1")
    if ((x & (x - 1)) == 0) {
      return x
    }
    x |= (x >>> 1)
    x |= (x >>> 2)
    x |= (x >>> 4)
    x |= (x >>> 8)
    x |= (x >>> 16)
    x |= (x >>> 32)
    x + 1
  }

  def extendDimension(x: Int): Int = {
    if (x < 1) throw new IllegalArgumentException("x must be greater or equal 1")
    val nextExp = nextExp2(x)
    val nextPow = nextExp + 1
    val extDim = Math.round(Math.pow(2.0, nextPow.toDouble)).toInt
    extDim
  }

  def nextExp2(n: Int): Int = {
    val e = Math.log(n.toDouble) / Math.log(2.0)
    var p = Math.ceil(e).toInt
    val f = n / Math.pow(2.0, p.toDouble)
    if (f == 0.5) {
      p = p - 1
    }
    p
  }

  /**
   * Returns the closest power of two less than or equal to x
   * @return the closest power of two less then or equal to x
   */
  def prevPow2(x: Int): Int = {
    if (x < 1) throw new IllegalArgumentException("x must be greater or equal 1")
    Math.pow(2, Math.floor(Math.log(x) / Math.log(2))).toInt
  }

  /**
   * Checks if n is a power-of-two number
   * @return true if n is power of 2
   */
  def isPowerOf2(n: Int): Boolean = {
    if (n <= 0) false else (n & (n - 1)) == 0
  }
}

/**
 * Concurrency utilities.
 *
 * @author Piotr Wendykier (piotr.wendykier@gmail.com)
 */
class ConcurrencyUtils {


}
