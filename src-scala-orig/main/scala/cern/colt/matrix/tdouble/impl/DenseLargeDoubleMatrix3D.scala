package cern.colt.matrix.tdouble.impl

import java.util.concurrent.Future
import cern.colt.matrix.tdcomplex.impl.DenseLargeDComplexMatrix3D
import cern.colt.matrix.tdouble.DoubleMatrix3D
import edu.emory.mathcs.jtransforms.dct.DoubleDCT_2D
import edu.emory.mathcs.jtransforms.dct.DoubleDCT_3D
import edu.emory.mathcs.jtransforms.dht.DoubleDHT_2D
import edu.emory.mathcs.jtransforms.dht.DoubleDHT_3D
import edu.emory.mathcs.jtransforms.dst.DoubleDST_2D
import edu.emory.mathcs.jtransforms.dst.DoubleDST_3D
import edu.emory.mathcs.jtransforms.fft.DoubleFFT_2D
import edu.emory.mathcs.jtransforms.fft.DoubleFFT_3D
import edu.emory.mathcs.utils.ConcurrencyUtils
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Dense 3-d matrix holding <tt>double</tt> elements. First see the <a
 * href="package-summary.html">package summary</a> and javadoc <a
 * href="package-tree.html">tree view</a> to get the broad picture.
 * <p>
 * <b>Implementation:</b>
 * <p>
 * This data structure allows to store more than 2^31 elements. Internally holds
 * one three-dimensional array, elements[slices][rows][columns]. Note that this
 * implementation is not synchronized.
 * <p>
 * <b>Time complexity:</b>
 * <p>
 * <tt>O(1)</tt> (i.e. constant time) for the basic operations <tt>get</tt>,
 * <tt>getQuick</tt>, <tt>set</tt>, <tt>setQuick</tt> and <tt>size</tt>.
 *
 * @author Piotr Wendykier (piotr.wendykier@gmail.com)
 *
 */
@SerialVersionUID(1L)
class DenseLargeDoubleMatrix3D(slices: Int, rows: Int, columns: Int) extends WrapperDoubleMatrix3D(null) {

  var elements: Array[Array[Array[Double]]] = new Array[Array[Array[Double]]](slices, rows, columns)

  private var fft3: DoubleFFT_3D = _

  private var dct3: DoubleDCT_3D = _

  private var dst3: DoubleDST_3D = _

  private var dht3: DoubleDHT_3D = _

  private var fft2Slices: DoubleFFT_2D = _

  private var dct2Slices: DoubleDCT_2D = _

  private var dst2Slices: DoubleDST_2D = _

  private var dht2Slices: DoubleDHT_2D = _

  try {
    setUp(slices, rows, columns)
  } catch {
    case exc: IllegalArgumentException => if ("matrix too large" != exc.getMessage) throw exc
  }

  /**
   * Computes the 3D discrete cosine transform (DCT-II) of this matrix.
   *
   * @param scale
   *            if true then scaling is performed
   *
   */
  def dct3(scale: Boolean) {
    val oldNthreads = ConcurrencyUtils.getNumberOfThreads
    ConcurrencyUtils.setNumberOfThreads(ConcurrencyUtils.nextPow2(oldNthreads))
    if (dct3 == null) {
      dct3 = new DoubleDCT_3D(slices, rows, columns)
    }
    dct3.forward(elements, scale)
    ConcurrencyUtils.setNumberOfThreads(oldNthreads)
  }

  /**
   * Computes the 2D discrete cosine transform (DCT-II) of each slice of this
   * matrix.
   *
   * @param scale
   *            if true then scaling is performed
   *
   */
  def dct2Slices(scale: Boolean) {
    if (dct2Slices == null) {
      dct2Slices = new DoubleDCT_2D(rows, columns)
    }
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_3D)) {
      ConcurrencyUtils.setThreadsBeginN_2D(Integer.MAX_VALUE)
      ConcurrencyUtils.setThreadsBeginN_1D_FFT_2Threads(Integer.MAX_VALUE)
      ConcurrencyUtils.setThreadsBeginN_1D_FFT_4Threads(Integer.MAX_VALUE)
      nthreads = Math.min(nthreads, slices)
      val futures = Array.ofDim[Future](nthreads)
      val k = slices / nthreads
      for (j <- 0 until nthreads) {
        val firstSlice = j * k
        val lastSlice = if ((j == nthreads - 1)) slices else firstSlice + k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            for (s <- firstSlice until lastSlice) {
              dct2Slices.forward(elements(s), scale)
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
      ConcurrencyUtils.resetThreadsBeginN()
      ConcurrencyUtils.resetThreadsBeginN_FFT()
    } else {
      for (s <- 0 until slices) {
        dct2Slices.forward(elements(s), scale)
      }
    }
  }

  /**
   * Computes the 3D discrete Hartley transform (DHT) of this matrix.
   */
  def dht3() {
    val oldNthreads = ConcurrencyUtils.getNumberOfThreads
    ConcurrencyUtils.setNumberOfThreads(ConcurrencyUtils.nextPow2(oldNthreads))
    if (dht3 == null) {
      dht3 = new DoubleDHT_3D(slices, rows, columns)
    }
    dht3.forward(elements)
    ConcurrencyUtils.setNumberOfThreads(oldNthreads)
  }

  /**
   * Computes the 2D discrete Hartley transform (DHT) of each slice of this
   * matrix.
   *
   */
  def dht2Slices() {
    if (dht2Slices == null) {
      dht2Slices = new DoubleDHT_2D(rows, columns)
    }
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_3D)) {
      ConcurrencyUtils.setThreadsBeginN_2D(Integer.MAX_VALUE)
      ConcurrencyUtils.setThreadsBeginN_1D_FFT_2Threads(Integer.MAX_VALUE)
      ConcurrencyUtils.setThreadsBeginN_1D_FFT_4Threads(Integer.MAX_VALUE)
      nthreads = Math.min(nthreads, slices)
      val futures = Array.ofDim[Future](nthreads)
      val k = slices / nthreads
      for (j <- 0 until nthreads) {
        val firstSlice = j * k
        val lastSlice = if ((j == nthreads - 1)) slices else firstSlice + k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            for (s <- firstSlice until lastSlice) {
              dht2Slices.forward(elements(s))
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
      ConcurrencyUtils.resetThreadsBeginN()
      ConcurrencyUtils.resetThreadsBeginN_FFT()
    } else {
      for (s <- 0 until slices) {
        dht2Slices.forward(elements(s))
      }
    }
  }

  /**
   * Computes the 3D discrete sine transform (DST-II) of this matrix.
   *
   * @param scale
   *            if true then scaling is performed
   */
  def dst3(scale: Boolean) {
    val oldNthreads = ConcurrencyUtils.getNumberOfThreads
    ConcurrencyUtils.setNumberOfThreads(ConcurrencyUtils.nextPow2(oldNthreads))
    if (dst3 == null) {
      dst3 = new DoubleDST_3D(slices, rows, columns)
    }
    dst3.forward(elements, scale)
    ConcurrencyUtils.setNumberOfThreads(oldNthreads)
  }

  /**
   * Computes the 2D discrete sine transform (DST-II) of each slice of this
   * matrix.
   *
   * @param scale
   *            if true then scaling is performed
   */
  def dst2Slices(scale: Boolean) {
    if (dst2Slices == null) {
      dst2Slices = new DoubleDST_2D(rows, columns)
    }
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_3D)) {
      ConcurrencyUtils.setThreadsBeginN_2D(Integer.MAX_VALUE)
      ConcurrencyUtils.setThreadsBeginN_1D_FFT_2Threads(Integer.MAX_VALUE)
      ConcurrencyUtils.setThreadsBeginN_1D_FFT_4Threads(Integer.MAX_VALUE)
      nthreads = Math.min(nthreads, slices)
      val futures = Array.ofDim[Future](nthreads)
      val k = slices / nthreads
      for (j <- 0 until nthreads) {
        val firstSlice = j * k
        val lastSlice = if ((j == nthreads - 1)) slices else firstSlice + k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            for (s <- firstSlice until lastSlice) {
              dst2Slices.forward(elements(s), scale)
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
      ConcurrencyUtils.resetThreadsBeginN()
      ConcurrencyUtils.resetThreadsBeginN_FFT()
    } else {
      for (s <- 0 until slices) {
        dst2Slices.forward(elements(s), scale)
      }
    }
  }

  /**
   * Computes the 3D discrete Fourier transform (DFT) of this matrix. The
   * physical layout of the output data is as follows:
   *
   * <pre>
   * this[k1][k2][2*k3] = Re[k1][k2][k3]
   *                 = Re[(n1-k1)%n1][(n2-k2)%n2][n3-k3],
   * this[k1][k2][2*k3+1] = Im[k1][k2][k3]
   *                   = -Im[(n1-k1)%n1][(n2-k2)%n2][n3-k3],
   *     0&lt;=k1&lt;n1, 0&lt;=k2&lt;n2, 0&lt;k3&lt;n3/2,
   * this[k1][k2][0] = Re[k1][k2][0]
   *              = Re[(n1-k1)%n1][n2-k2][0],
   * this[k1][k2][1] = Im[k1][k2][0]
   *              = -Im[(n1-k1)%n1][n2-k2][0],
   * this[k1][n2-k2][1] = Re[(n1-k1)%n1][k2][n3/2]
   *                 = Re[k1][n2-k2][n3/2],
   * this[k1][n2-k2][0] = -Im[(n1-k1)%n1][k2][n3/2]
   *                 = Im[k1][n2-k2][n3/2],
   *     0&lt;=k1&lt;n1, 0&lt;k2&lt;n2/2,
   * this[k1][0][0] = Re[k1][0][0]
   *             = Re[n1-k1][0][0],
   * this[k1][0][1] = Im[k1][0][0]
   *             = -Im[n1-k1][0][0],
   * this[k1][n2/2][0] = Re[k1][n2/2][0]
   *                = Re[n1-k1][n2/2][0],
   * this[k1][n2/2][1] = Im[k1][n2/2][0]
   *                = -Im[n1-k1][n2/2][0],
   * this[n1-k1][0][1] = Re[k1][0][n3/2]
   *                = Re[n1-k1][0][n3/2],
   * this[n1-k1][0][0] = -Im[k1][0][n3/2]
   *                = Im[n1-k1][0][n3/2],
   * this[n1-k1][n2/2][1] = Re[k1][n2/2][n3/2]
   *                   = Re[n1-k1][n2/2][n3/2],
   * this[n1-k1][n2/2][0] = -Im[k1][n2/2][n3/2]
   *                   = Im[n1-k1][n2/2][n3/2],
   *     0&lt;k1&lt;n1/2,
   * this[0][0][0] = Re[0][0][0],
   * this[0][0][1] = Re[0][0][n3/2],
   * this[0][n2/2][0] = Re[0][n2/2][0],
   * this[0][n2/2][1] = Re[0][n2/2][n3/2],
   * this[n1/2][0][0] = Re[n1/2][0][0],
   * this[n1/2][0][1] = Re[n1/2][0][n3/2],
   * this[n1/2][n2/2][0] = Re[n1/2][n2/2][0],
   * this[n1/2][n2/2][1] = Re[n1/2][n2/2][n3/2]
   * </pre>
   *
   *
   * This method computes only half of the elements of the real transform. The
   * other half satisfies the symmetry condition. If you want the full real
   * forward transform, use <code>getFft3</code>. To get back the original
   * data, use <code>ifft3</code>.
   *
   * @throws IllegalArgumentException
   *             if the slice size or the row size or the column size of this
   *             matrix is not a power of 2 number.
   */
  def fft3() {
    val oldNthreads = ConcurrencyUtils.getNumberOfThreads
    ConcurrencyUtils.setNumberOfThreads(ConcurrencyUtils.nextPow2(oldNthreads))
    if (fft3 == null) {
      fft3 = new DoubleFFT_3D(slices, rows, columns)
    }
    fft3.realForward(elements)
    ConcurrencyUtils.setNumberOfThreads(oldNthreads)
  }

  /**
   * Returns new complex matrix which is the 2D discrete Fourier transform
   * (DFT) of each slice of this matrix.
   *
   * @return the 2D discrete Fourier transform (DFT) of each slice of this
   *         matrix.
   *
   */
  def getFft2Slices(): DenseLargeDComplexMatrix3D = {
    if (fft2Slices == null) {
      fft2Slices = new DoubleFFT_2D(rows, columns)
    }
    val C = new DenseLargeDComplexMatrix3D(slices, rows, columns)
    val cElems = C.elements()
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_3D)) {
      ConcurrencyUtils.setThreadsBeginN_2D(Integer.MAX_VALUE)
      ConcurrencyUtils.setThreadsBeginN_1D_FFT_2Threads(Integer.MAX_VALUE)
      ConcurrencyUtils.setThreadsBeginN_1D_FFT_4Threads(Integer.MAX_VALUE)
      nthreads = Math.min(nthreads, slices)
      val futures = Array.ofDim[Future](nthreads)
      val k = slices / nthreads
      for (j <- 0 until nthreads) {
        val firstSlice = j * k
        val lastSlice = if ((j == nthreads - 1)) slices else firstSlice + k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            for (s <- firstSlice until lastSlice) {
              for (r <- 0 until rows) {
                System.arraycopy(elements(s)(r), 0, cElems(s)(r), 0, columns)
              }
              fft2Slices.realForwardFull(cElems(s))
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
      ConcurrencyUtils.resetThreadsBeginN()
      ConcurrencyUtils.resetThreadsBeginN_FFT()
    } else {
      for (s <- 0 until slices) {
        for (r <- 0 until rows) {
          System.arraycopy(elements(s)(r), 0, cElems(s)(r), 0, columns)
        }
        fft2Slices.realForwardFull(cElems(s))
      }
    }
    C
  }

  /**
   * Returns new complex matrix which is the 3D discrete Fourier transform
   * (DFT) of this matrix.
   *
   * @return the 3D discrete Fourier transform (DFT) of this matrix.
   */
  def getFft3(): DenseLargeDComplexMatrix3D = {
    val oldNthreads = ConcurrencyUtils.getNumberOfThreads
    ConcurrencyUtils.setNumberOfThreads(ConcurrencyUtils.nextPow2(oldNthreads))
    val C = new DenseLargeDComplexMatrix3D(slices, rows, columns)
    val cElems = (C).elements()
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_3D)) {
      nthreads = Math.min(nthreads, slices)
      val futures = Array.ofDim[Future](nthreads)
      val k = slices / nthreads
      for (j <- 0 until nthreads) {
        val startslice = j * k
        var stopslice: Int = 0
        stopslice = if (j == nthreads - 1) slices else startslice + k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            for (s <- startslice until stopslice; r <- 0 until rows) {
              System.arraycopy(elements(s)(r), 0, cElems(s)(r), 0, columns)
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
    } else {
      for (s <- 0 until slices; r <- 0 until rows) {
        System.arraycopy(elements(s)(r), 0, cElems(s)(r), 0, columns)
      }
    }
    if (fft3 == null) {
      fft3 = new DoubleFFT_3D(slices, rows, columns)
    }
    fft3.realForwardFull(cElems)
    ConcurrencyUtils.setNumberOfThreads(oldNthreads)
    C
  }

  /**
   * Returns new complex matrix which is the 2D inverse of the discrete
   * Fourier transform (IDFT) of each slice of this matrix.
   *
   * @param scale
   *            if true then scaling is performed
   *
   * @return the 2D inverse of the discrete Fourier transform (IDFT) of each
   *         slice of this matrix.
   *
   */
  def getIfft2Slices(scale: Boolean): DenseLargeDComplexMatrix3D = {
    if (fft2Slices == null) {
      fft2Slices = new DoubleFFT_2D(rows, columns)
    }
    val C = new DenseLargeDComplexMatrix3D(slices, rows, columns)
    val cElems = C.elements()
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_3D)) {
      ConcurrencyUtils.setThreadsBeginN_2D(Integer.MAX_VALUE)
      ConcurrencyUtils.setThreadsBeginN_1D_FFT_2Threads(Integer.MAX_VALUE)
      ConcurrencyUtils.setThreadsBeginN_1D_FFT_4Threads(Integer.MAX_VALUE)
      nthreads = Math.min(nthreads, slices)
      val futures = Array.ofDim[Future](nthreads)
      val k = slices / nthreads
      for (j <- 0 until nthreads) {
        val firstSlice = j * k
        val lastSlice = if ((j == nthreads - 1)) slices else firstSlice + k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            for (s <- firstSlice until lastSlice) {
              for (r <- 0 until rows) {
                System.arraycopy(elements(s)(r), 0, cElems(s)(r), 0, columns)
              }
              fft2Slices.realInverseFull(cElems(s), scale)
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
      ConcurrencyUtils.resetThreadsBeginN()
      ConcurrencyUtils.resetThreadsBeginN_FFT()
    } else {
      for (s <- 0 until slices) {
        for (r <- 0 until rows) {
          System.arraycopy(elements(s)(r), 0, cElems(s)(r), 0, columns)
        }
        fft2Slices.realInverseFull(cElems(s), scale)
      }
    }
    C
  }

  /**
   * Returns new complex matrix which is the 3D inverse of the discrete
   * Fourier transform (IDFT) of this matrix.
   *
   * @param scale
   *            if true then scaling is performed
   *
   * @return the 3D inverse of the discrete Fourier transform (IDFT) of this
   *         matrix.
   *
   */
  def getIfft3(scale: Boolean): DenseLargeDComplexMatrix3D = {
    val oldNthreads = ConcurrencyUtils.getNumberOfThreads
    ConcurrencyUtils.setNumberOfThreads(ConcurrencyUtils.nextPow2(oldNthreads))
    val C = new DenseLargeDComplexMatrix3D(slices, rows, columns)
    val cElems = (C).elements()
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_3D)) {
      nthreads = Math.min(nthreads, slices)
      val futures = Array.ofDim[Future](nthreads)
      val k = slices / nthreads
      for (j <- 0 until nthreads) {
        val startslice = j * k
        var stopslice: Int = 0
        stopslice = if (j == nthreads - 1) slices else startslice + k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            for (s <- startslice until stopslice; r <- 0 until rows) {
              System.arraycopy(elements(s)(r), 0, cElems(s)(r), 0, columns)
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
    } else {
      for (s <- 0 until slices; r <- 0 until rows) {
        System.arraycopy(elements(s)(r), 0, cElems(s)(r), 0, columns)
      }
    }
    if (fft3 == null) {
      fft3 = new DoubleFFT_3D(slices, rows, columns)
    }
    fft3.realInverseFull(cElems, scale)
    ConcurrencyUtils.setNumberOfThreads(oldNthreads)
    C
  }

  def getQuick(slice: Int, row: Int, column: Int): Double = elements(slice)(row)(column)

  /**
   * Computes the 2D inverse of the discrete cosine transform (DCT-III) of
   * each slice of this matrix.
   *
   * @param scale
   *            if true then scaling is performed
   *
   */
  def idct2Slices(scale: Boolean) {
    if (dct2Slices == null) {
      dct2Slices = new DoubleDCT_2D(rows, columns)
    }
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_3D)) {
      ConcurrencyUtils.setThreadsBeginN_2D(Integer.MAX_VALUE)
      ConcurrencyUtils.setThreadsBeginN_1D_FFT_2Threads(Integer.MAX_VALUE)
      ConcurrencyUtils.setThreadsBeginN_1D_FFT_4Threads(Integer.MAX_VALUE)
      nthreads = Math.min(nthreads, slices)
      val futures = Array.ofDim[Future](nthreads)
      val k = slices / nthreads
      for (j <- 0 until nthreads) {
        val firstSlice = j * k
        val lastSlice = if ((j == nthreads - 1)) slices else firstSlice + k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            for (s <- firstSlice until lastSlice) {
              dct2Slices.inverse(elements(s), scale)
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
      ConcurrencyUtils.resetThreadsBeginN()
      ConcurrencyUtils.resetThreadsBeginN_FFT()
    } else {
      for (s <- 0 until slices) {
        dct2Slices.inverse(elements(s), scale)
      }
    }
  }

  /**
   * Computes the 3D inverse of the discrete Hartley transform (IDHT) of this
   * matrix.
   *
   * @param scale
   *            if true then scaling is performed
   *
   * @throws IllegalArgumentException
   *             if the slice size or the row size or the column size of this
   *             matrix is not a power of 2 number.
   *
   */
  def idht3(scale: Boolean) {
    val oldNthreads = ConcurrencyUtils.getNumberOfThreads
    ConcurrencyUtils.setNumberOfThreads(ConcurrencyUtils.nextPow2(oldNthreads))
    if (dht3 == null) {
      dht3 = new DoubleDHT_3D(slices, rows, columns)
    }
    dht3.inverse(elements, scale)
    ConcurrencyUtils.setNumberOfThreads(oldNthreads)
  }

  /**
   * Computes the 2D inverse of the discrete Hartley transform (IDHT) of each
   * slice of this matrix.
   *
   * @param scale
   *            if true then scaling is performed
   *
   * @throws IllegalArgumentException
   *             if the slice size or the row size or the column size of this
   *             matrix is not a power of 2 number.
   *
   */
  def idht2Slices(scale: Boolean) {
    if (dht2Slices == null) {
      dht2Slices = new DoubleDHT_2D(rows, columns)
    }
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_3D)) {
      ConcurrencyUtils.setThreadsBeginN_2D(Integer.MAX_VALUE)
      ConcurrencyUtils.setThreadsBeginN_1D_FFT_2Threads(Integer.MAX_VALUE)
      ConcurrencyUtils.setThreadsBeginN_1D_FFT_4Threads(Integer.MAX_VALUE)
      nthreads = Math.min(nthreads, slices)
      val futures = Array.ofDim[Future](nthreads)
      val k = slices / nthreads
      for (j <- 0 until nthreads) {
        val firstSlice = j * k
        val lastSlice = if ((j == nthreads - 1)) slices else firstSlice + k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            for (s <- firstSlice until lastSlice) {
              dht2Slices.inverse(elements(s), scale)
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
      ConcurrencyUtils.resetThreadsBeginN()
      ConcurrencyUtils.resetThreadsBeginN_FFT()
    } else {
      for (s <- 0 until slices) {
        dht2Slices.inverse(elements(s), scale)
      }
    }
  }

  /**
   * Computes the 3D inverse of the discrete cosine transform (DCT-III) of
   * this matrix.
   *
   * @param scale
   *            if true then scaling is performed
   *
   */
  def idct3(scale: Boolean) {
    val oldNthreads = ConcurrencyUtils.getNumberOfThreads
    ConcurrencyUtils.setNumberOfThreads(ConcurrencyUtils.nextPow2(oldNthreads))
    if (dct3 == null) {
      dct3 = new DoubleDCT_3D(slices, rows, columns)
    }
    dct3.inverse(elements, scale)
    ConcurrencyUtils.setNumberOfThreads(oldNthreads)
  }

  /**
   * Computes the 2D inverse of the discrete sine transform (DST-III) of each
   * slice of this matrix.
   *
   * @param scale
   *            if true then scaling is performed
   *
   */
  def idst2Slices(scale: Boolean) {
    if (dst2Slices == null) {
      dst2Slices = new DoubleDST_2D(rows, columns)
    }
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_3D)) {
      ConcurrencyUtils.setThreadsBeginN_2D(Integer.MAX_VALUE)
      ConcurrencyUtils.setThreadsBeginN_1D_FFT_2Threads(Integer.MAX_VALUE)
      ConcurrencyUtils.setThreadsBeginN_1D_FFT_4Threads(Integer.MAX_VALUE)
      nthreads = Math.min(nthreads, slices)
      val futures = Array.ofDim[Future](nthreads)
      val k = slices / nthreads
      for (j <- 0 until nthreads) {
        val firstSlice = j * k
        val lastSlice = if ((j == nthreads - 1)) slices else firstSlice + k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            for (s <- firstSlice until lastSlice) {
              dst2Slices.inverse(elements(s), scale)
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
      ConcurrencyUtils.resetThreadsBeginN()
      ConcurrencyUtils.resetThreadsBeginN_FFT()
    } else {
      for (s <- 0 until slices) {
        dst2Slices.inverse(elements(s), scale)
      }
    }
  }

  /**
   * Computes the 3D inverse of the discrete sine transform (DST-III) of this
   * matrix.
   *
   * @param scale
   *            if true then scaling is performed
   *
   */
  def idst3(scale: Boolean) {
    val oldNthreads = ConcurrencyUtils.getNumberOfThreads
    ConcurrencyUtils.setNumberOfThreads(ConcurrencyUtils.nextPow2(oldNthreads))
    if (dst3 == null) {
      dst3 = new DoubleDST_3D(slices, rows, columns)
    }
    dst3.inverse(elements, scale)
    ConcurrencyUtils.setNumberOfThreads(oldNthreads)
  }

  /**
   * Computes the 3D inverse of the discrete Fourier transform (IDFT) of this
   * matrix. The physical layout of the input data has to be as follows:
   *
   * <pre>
   * this[k1][k2][2*k3] = Re[k1][k2][k3]
   *                 = Re[(n1-k1)%n1][(n2-k2)%n2][n3-k3],
   * this[k1][k2][2*k3+1] = Im[k1][k2][k3]
   *                   = -Im[(n1-k1)%n1][(n2-k2)%n2][n3-k3],
   *     0&lt;=k1&lt;n1, 0&lt;=k2&lt;n2, 0&lt;k3&lt;n3/2,
   * this[k1][k2][0] = Re[k1][k2][0]
   *              = Re[(n1-k1)%n1][n2-k2][0],
   * this[k1][k2][1] = Im[k1][k2][0]
   *              = -Im[(n1-k1)%n1][n2-k2][0],
   * this[k1][n2-k2][1] = Re[(n1-k1)%n1][k2][n3/2]
   *                 = Re[k1][n2-k2][n3/2],
   * this[k1][n2-k2][0] = -Im[(n1-k1)%n1][k2][n3/2]
   *                 = Im[k1][n2-k2][n3/2],
   *     0&lt;=k1&lt;n1, 0&lt;k2&lt;n2/2,
   * this[k1][0][0] = Re[k1][0][0]
   *             = Re[n1-k1][0][0],
   * this[k1][0][1] = Im[k1][0][0]
   *             = -Im[n1-k1][0][0],
   * this[k1][n2/2][0] = Re[k1][n2/2][0]
   *                = Re[n1-k1][n2/2][0],
   * this[k1][n2/2][1] = Im[k1][n2/2][0]
   *                = -Im[n1-k1][n2/2][0],
   * this[n1-k1][0][1] = Re[k1][0][n3/2]
   *                = Re[n1-k1][0][n3/2],
   * this[n1-k1][0][0] = -Im[k1][0][n3/2]
   *                = Im[n1-k1][0][n3/2],
   * this[n1-k1][n2/2][1] = Re[k1][n2/2][n3/2]
   *                   = Re[n1-k1][n2/2][n3/2],
   * this[n1-k1][n2/2][0] = -Im[k1][n2/2][n3/2]
   *                   = Im[n1-k1][n2/2][n3/2],
   *     0&lt;k1&lt;n1/2,
   * this[0][0][0] = Re[0][0][0],
   * this[0][0][1] = Re[0][0][n3/2],
   * this[0][n2/2][0] = Re[0][n2/2][0],
   * this[0][n2/2][1] = Re[0][n2/2][n3/2],
   * this[n1/2][0][0] = Re[n1/2][0][0],
   * this[n1/2][0][1] = Re[n1/2][0][n3/2],
   * this[n1/2][n2/2][0] = Re[n1/2][n2/2][0],
   * this[n1/2][n2/2][1] = Re[n1/2][n2/2][n3/2]
   * </pre>
   *
   * This method computes only half of the elements of the real transform. The
   * other half satisfies the symmetry condition. If you want the full real
   * inverse transform, use <code>getIfft3</code>.
   *
   * @param scale
   *            if true then scaling is performed
   *
   * @throws IllegalArgumentException
   *             if the slice size or the row size or the column size of this
   *             matrix is not a power of 2 number.
   */
  def ifft3(scale: Boolean) {
    val oldNthreads = ConcurrencyUtils.getNumberOfThreads
    ConcurrencyUtils.setNumberOfThreads(ConcurrencyUtils.nextPow2(oldNthreads))
    if (fft3 == null) {
      fft3 = new DoubleFFT_3D(slices, rows, columns)
    }
    fft3.realInverse(elements, scale)
    ConcurrencyUtils.setNumberOfThreads(oldNthreads)
  }

  def setQuick(slice: Int,
      row: Int,
      column: Int,
      value: Double) {
    elements(slice)(row)(column) = value
  }

  protected def getStorageMatrix(): DoubleMatrix3D = this

  def like(slices: Int, rows: Int, columns: Int): DoubleMatrix3D = {
    new DenseLargeDoubleMatrix3D(slices, rows, columns)
  }
}
