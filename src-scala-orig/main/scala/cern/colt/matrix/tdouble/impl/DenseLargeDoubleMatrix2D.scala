package cern.colt.matrix.tdouble.impl

import java.util.concurrent.Future
import cern.colt.matrix.tdcomplex.impl.DenseLargeDComplexMatrix2D
import cern.colt.matrix.tdouble.StrideMatrix1D
import cern.colt.matrix.tdouble.StrideMatrix2D
import edu.emory.mathcs.jtransforms.dct.DoubleDCT_1D
import edu.emory.mathcs.jtransforms.dct.DoubleDCT_2D
import edu.emory.mathcs.jtransforms.dht.DoubleDHT_1D
import edu.emory.mathcs.jtransforms.dht.DoubleDHT_2D
import edu.emory.mathcs.jtransforms.dst.DoubleDST_1D
import edu.emory.mathcs.jtransforms.dst.DoubleDST_2D
import edu.emory.mathcs.jtransforms.fft.DoubleFFT_1D
import edu.emory.mathcs.jtransforms.fft.DoubleFFT_2D
import edu.emory.mathcs.utils.ConcurrencyUtils
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Dense 2-d matrix holding <tt>double</tt> elements. First see the <a
 * href="package-summary.html">package summary</a> and javadoc <a
 * href="package-tree.html">tree view</a> to get the broad picture.
 * <p>
 * <b>Implementation:</b>
 * <p>
 * This data structure allows to store more than 2^31 elements. Internally holds
 * one two-dimensional array, elements[rows][columns]. Note that this
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
class DenseLargeDoubleMatrix2D(rows: Int, columns: Int) extends WrapperMatrix2D(null) {

  var elements: Array[Array[Double]] = new Array[Array[Double]](rows, columns)

  private var fft2: DoubleFFT_2D = _

  private var dct2: DoubleDCT_2D = _

  private var dst2: DoubleDST_2D = _

  private var dht2: DoubleDHT_2D = _

  private var fftRows: DoubleFFT_1D = _

  private var fftColumns: DoubleFFT_1D = _

  private var dctRows: DoubleDCT_1D = _

  private var dctColumns: DoubleDCT_1D = _

  private var dstRows: DoubleDST_1D = _

  private var dstColumns: DoubleDST_1D = _

  private var dhtRows: DoubleDHT_1D = _

  private var dhtColumns: DoubleDHT_1D = _

  try {
    setUp(rows, columns)
  } catch {
    case exc: IllegalArgumentException => if ("matrix too large" != exc.getMessage) throw exc
  }

  content = this

  /**
   * Computes the 2D discrete cosine transform (DCT-II) of this matrix.
   *
   * @param scale
   *            if true then scaling is performed
   *
   */
  def dct2(scale: Boolean) {
    val oldNthreads = ConcurrencyUtils.getNumberOfThreads
    ConcurrencyUtils.setNumberOfThreads(ConcurrencyUtils.nextPow2(oldNthreads))
    if (dct2 == null) {
      dct2 = new DoubleDCT_2D(rows, columns)
    }
    dct2.forward(elements, scale)
    ConcurrencyUtils.setNumberOfThreads(oldNthreads)
  }

  /**
   * Computes the discrete cosine transform (DCT-II) of each column of this
   * matrix.
   *
   * @param scale
   *            if true then scaling is performed
   *
   */
  def dctColumns(scale: Boolean) {
    if (dctColumns == null) {
      dctColumns = new DoubleDCT_1D(rows)
    }
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_2D)) {
      ConcurrencyUtils.setThreadsBeginN_1D_FFT_2Threads(Integer.MAX_VALUE)
      ConcurrencyUtils.setThreadsBeginN_1D_FFT_4Threads(Integer.MAX_VALUE)
      nthreads = Math.min(nthreads, columns)
      val futures = Array.ofDim[Future](nthreads)
      val k = columns / nthreads
      for (j <- 0 until nthreads) {
        val firstCol = j * k
        val lastCol = if ((j == nthreads - 1)) columns else firstCol + k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            var column: Array[Double] = null
            for (c <- firstCol until lastCol) {
              column = viewColumn(c).copy().elements().asInstanceOf[Array[Double]]
              dctColumns.forward(column, scale)
              viewColumn(c).assign(column)
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
      ConcurrencyUtils.resetThreadsBeginN_FFT()
    } else {
      var column: Array[Double] = null
      for (c <- 0 until columns) {
        column = viewColumn(c).copy().elements().asInstanceOf[Array[Double]]
        dctColumns.forward(column, scale)
        viewColumn(c).assign(column)
      }
    }
  }

  /**
   * Computes the discrete cosine transform (DCT-II) of each row of this
   * matrix.
   *
   * @param scale
   *            if true then scaling is performed
   *
   */
  def dctRows(scale: Boolean) {
    if (dctRows == null) {
      dctRows = new DoubleDCT_1D(columns)
    }
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_2D)) {
      ConcurrencyUtils.setThreadsBeginN_1D_FFT_2Threads(Integer.MAX_VALUE)
      ConcurrencyUtils.setThreadsBeginN_1D_FFT_4Threads(Integer.MAX_VALUE)
      nthreads = Math.min(nthreads, rows)
      val futures = Array.ofDim[Future](nthreads)
      val k = rows / nthreads
      for (j <- 0 until nthreads) {
        val firstRow = j * k
        val lastRow = if ((j == nthreads - 1)) rows else firstRow + k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            for (r <- firstRow until lastRow) {
              dctRows.forward(elements(r), scale)
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
      ConcurrencyUtils.resetThreadsBeginN_FFT()
    } else {
      for (r <- 0 until rows) {
        dctRows.forward(elements(r), scale)
      }
    }
  }

  /**
   * Computes the 2D discrete Hartley transform (DHT) of this matrix.
   *
   */
  def dht2() {
    val oldNthreads = ConcurrencyUtils.getNumberOfThreads
    ConcurrencyUtils.setNumberOfThreads(ConcurrencyUtils.nextPow2(oldNthreads))
    if (dht2 == null) {
      dht2 = new DoubleDHT_2D(rows, columns)
    }
    dht2.forward(elements)
    ConcurrencyUtils.setNumberOfThreads(oldNthreads)
  }

  /**
   * Computes the discrete Hartley transform (DHT) of each column of this
   * matrix.
   *
   */
  def dhtColumns() {
    if (dhtColumns == null) {
      dhtColumns = new DoubleDHT_1D(rows)
    }
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_2D)) {
      ConcurrencyUtils.setThreadsBeginN_1D_FFT_2Threads(Integer.MAX_VALUE)
      ConcurrencyUtils.setThreadsBeginN_1D_FFT_4Threads(Integer.MAX_VALUE)
      nthreads = Math.min(nthreads, columns)
      val futures = Array.ofDim[Future](nthreads)
      val k = columns / nthreads
      for (j <- 0 until nthreads) {
        val firstCol = j * k
        val lastCol = if ((j == nthreads - 1)) columns else firstCol + k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            var column: Array[Double] = null
            for (c <- firstCol until lastCol) {
              column = viewColumn(c).copy().elements().asInstanceOf[Array[Double]]
              dhtColumns.forward(column)
              viewColumn(c).assign(column)
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
      ConcurrencyUtils.resetThreadsBeginN_FFT()
    } else {
      var column: Array[Double] = null
      for (c <- 0 until columns) {
        column = viewColumn(c).copy().elements().asInstanceOf[Array[Double]]
        dhtColumns.forward(column)
        viewColumn(c).assign(column)
      }
    }
  }

  /**
   * Computes the discrete Hartley transform (DHT) of each row of this matrix.
   *
   */
  def dhtRows() {
    if (dhtRows == null) {
      dhtRows = new DoubleDHT_1D(columns)
    }
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_2D)) {
      ConcurrencyUtils.setThreadsBeginN_1D_FFT_2Threads(Integer.MAX_VALUE)
      ConcurrencyUtils.setThreadsBeginN_1D_FFT_4Threads(Integer.MAX_VALUE)
      nthreads = Math.min(nthreads, rows)
      val futures = Array.ofDim[Future](nthreads)
      val k = rows / nthreads
      for (j <- 0 until nthreads) {
        val firstRow = j * k
        val lastRow = if ((j == nthreads - 1)) rows else firstRow + k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            for (r <- firstRow until lastRow) {
              dhtRows.forward(elements(r))
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
      ConcurrencyUtils.resetThreadsBeginN_FFT()
    } else {
      for (r <- 0 until rows) {
        dhtRows.forward(elements(r))
      }
    }
  }

  /**
   * Computes the 2D discrete sine transform (DST-II) of this matrix.
   *
   * @param scale
   *            if true then scaling is performed
   *
   */
  def dst2(scale: Boolean) {
    val oldNthreads = ConcurrencyUtils.getNumberOfThreads
    ConcurrencyUtils.setNumberOfThreads(ConcurrencyUtils.nextPow2(oldNthreads))
    if (dst2 == null) {
      dst2 = new DoubleDST_2D(rows, columns)
    }
    dst2.forward(elements, scale)
    ConcurrencyUtils.setNumberOfThreads(oldNthreads)
  }

  /**
   * Computes the discrete sine transform (DST-II) of each column of this
   * matrix.
   *
   * @param scale
   *            if true then scaling is performed
   *
   */
  def dstColumns(scale: Boolean) {
    if (dstColumns == null) {
      dstColumns = new DoubleDST_1D(rows)
    }
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_2D)) {
      ConcurrencyUtils.setThreadsBeginN_1D_FFT_2Threads(Integer.MAX_VALUE)
      ConcurrencyUtils.setThreadsBeginN_1D_FFT_4Threads(Integer.MAX_VALUE)
      nthreads = Math.min(nthreads, columns)
      val futures = Array.ofDim[Future](nthreads)
      val k = columns / nthreads
      for (j <- 0 until nthreads) {
        val firstCol = j * k
        val lastCol = if ((j == nthreads - 1)) columns else firstCol + k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            var column: Array[Double] = null
            for (c <- firstCol until lastCol) {
              column = viewColumn(c).copy().elements().asInstanceOf[Array[Double]]
              dstColumns.forward(column, scale)
              viewColumn(c).assign(column)
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
      ConcurrencyUtils.resetThreadsBeginN_FFT()
    } else {
      var column: Array[Double] = null
      for (c <- 0 until columns) {
        column = viewColumn(c).copy().elements().asInstanceOf[Array[Double]]
        dstColumns.forward(column, scale)
        viewColumn(c).assign(column)
      }
    }
  }

  /**
   * Computes the discrete sine transform (DST-II) of each row of this matrix.
   *
   * @param scale
   *            if true then scaling is performed
   *
   */
  def dstRows(scale: Boolean) {
    if (dstRows == null) {
      dstRows = new DoubleDST_1D(columns)
    }
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_2D)) {
      ConcurrencyUtils.setThreadsBeginN_1D_FFT_2Threads(Integer.MAX_VALUE)
      ConcurrencyUtils.setThreadsBeginN_1D_FFT_4Threads(Integer.MAX_VALUE)
      nthreads = Math.min(nthreads, rows)
      val futures = Array.ofDim[Future](nthreads)
      val k = rows / nthreads
      for (j <- 0 until nthreads) {
        val firstRow = j * k
        val lastRow = if ((j == nthreads - 1)) rows else firstRow + k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            for (r <- firstRow until lastRow) {
              dstRows.forward(elements(r), scale)
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
      ConcurrencyUtils.resetThreadsBeginN_FFT()
    } else {
      for (r <- 0 until rows) {
        dstRows.forward(elements(r), scale)
      }
    }
  }

  /**
   * Computes the 2D discrete Fourier transform (DFT) of this matrix. The
   * physical layout of the output data is as follows:
   *
   * <pre>
   * this[k1][2*k2] = Re[k1][k2] = Re[rows-k1][columns-k2],
   * this[k1][2*k2+1] = Im[k1][k2] = -Im[rows-k1][columns-k2],
   *       0&lt;k1&lt;rows, 0&lt;k2&lt;columns/2,
   * this[0][2*k2] = Re[0][k2] = Re[0][columns-k2],
   * this[0][2*k2+1] = Im[0][k2] = -Im[0][columns-k2],
   *       0&lt;k2&lt;columns/2,
   * this[k1][0] = Re[k1][0] = Re[rows-k1][0],
   * this[k1][1] = Im[k1][0] = -Im[rows-k1][0],
   * this[rows-k1][1] = Re[k1][columns/2] = Re[rows-k1][columns/2],
   * this[rows-k1][0] = -Im[k1][columns/2] = Im[rows-k1][columns/2],
   *       0&lt;k1&lt;rows/2,
   * this[0][0] = Re[0][0],
   * this[0][1] = Re[0][columns/2],
   * this[rows/2][0] = Re[rows/2][0],
   * this[rows/2][1] = Re[rows/2][columns/2]
   * </pre>
   *
   * This method computes only half of the elements of the real transform. The
   * other half satisfies the symmetry condition. If you want the full real
   * forward transform, use <code>getFft2</code>. To get back the original
   * data, use <code>ifft2</code>.
   *
   * @throws IllegalArgumentException
   *             if the row size or the column size of this matrix is not a
   *             power of 2 number.
   *
   */
  def fft2() {
    val oldNthreads = ConcurrencyUtils.getNumberOfThreads
    ConcurrencyUtils.setNumberOfThreads(ConcurrencyUtils.nextPow2(oldNthreads))
    if (fft2 == null) {
      fft2 = new DoubleFFT_2D(rows, columns)
    }
    fft2.realForward(elements)
    ConcurrencyUtils.setNumberOfThreads(oldNthreads)
  }

  /**
   * Returns new complex matrix which is the 2D discrete Fourier transform
   * (DFT) of this matrix.
   *
   * @return the 2D discrete Fourier transform (DFT) of this matrix.
   *
   */
  def getFft2(): DenseLargeDComplexMatrix2D = {
    val oldNthreads = ConcurrencyUtils.getNumberOfThreads
    ConcurrencyUtils.setNumberOfThreads(ConcurrencyUtils.nextPow2(oldNthreads))
    if (fft2 == null) {
      fft2 = new DoubleFFT_2D(rows, columns)
    }
    val C = new DenseLargeDComplexMatrix2D(rows, columns)
    val elementsC = (C).elements()
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_2D)) {
      nthreads = Math.min(nthreads, columns)
      val futures = Array.ofDim[Future](nthreads)
      val k = rows / nthreads
      for (j <- 0 until nthreads) {
        val firstRow = j * k
        val lastRow = if ((j == nthreads - 1)) rows else firstRow + k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            for (r <- firstRow until lastRow) {
              System.arraycopy(elements(r), 0, elementsC(r), 0, columns)
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
    } else {
      for (r <- 0 until rows) {
        System.arraycopy(elements(r), 0, elementsC(r), 0, columns)
      }
    }
    fft2.realForwardFull(elementsC)
    ConcurrencyUtils.setNumberOfThreads(oldNthreads)
    C
  }

  /**
   * Returns new complex matrix which is the discrete Fourier transform (DFT)
   * of each column of this matrix.
   *
   * @return the discrete Fourier transform (DFT) of each column of this
   *         matrix.
   */
  def getFftColumns(): DenseLargeDComplexMatrix2D = {
    if (fftColumns == null) {
      fftColumns = new DoubleFFT_1D(rows)
    }
    val oldNthreads = ConcurrencyUtils.getNumberOfThreads
    ConcurrencyUtils.setNumberOfThreads(ConcurrencyUtils.nextPow2(oldNthreads))
    val C = new DenseLargeDComplexMatrix2D(rows, columns)
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_2D)) {
      ConcurrencyUtils.setThreadsBeginN_1D_FFT_2Threads(Integer.MAX_VALUE)
      ConcurrencyUtils.setThreadsBeginN_1D_FFT_4Threads(Integer.MAX_VALUE)
      nthreads = Math.min(nthreads, columns)
      val futures = Array.ofDim[Future](nthreads)
      val k = columns / nthreads
      for (j <- 0 until nthreads) {
        val firstCol = j * k
        val lastColumn = if ((j == nthreads - 1)) columns else firstCol + k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            for (c <- firstCol until lastColumn) {
              var column = Array.ofDim[Double](2 * rows)
              System.arraycopy(viewColumn(c).copy().elements(), 0, column, 0, rows)
              fftColumns.realForwardFull(column)
              C.viewColumn(c).assign(column)
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
      ConcurrencyUtils.resetThreadsBeginN_FFT()
    } else {
      for (c <- 0 until columns) {
        val column = Array.ofDim[Double](2 * rows)
        System.arraycopy(viewColumn(c).copy().elements(), 0, column, 0, rows)
        fftColumns.realForwardFull(column)
        C.viewColumn(c).assign(column)
      }
    }
    ConcurrencyUtils.setNumberOfThreads(oldNthreads)
    C
  }

  /**
   * Returns new complex matrix which is the discrete Fourier transform (DFT)
   * of each row of this matrix.
   *
   * @return the discrete Fourier transform (DFT) of each row of this matrix.
   */
  def getFftRows(): DenseLargeDComplexMatrix2D = {
    if (fftRows == null) {
      fftRows = new DoubleFFT_1D(columns)
    }
    val oldNthreads = ConcurrencyUtils.getNumberOfThreads
    ConcurrencyUtils.setNumberOfThreads(ConcurrencyUtils.nextPow2(oldNthreads))
    val C = new DenseLargeDComplexMatrix2D(rows, columns)
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_2D)) {
      ConcurrencyUtils.setThreadsBeginN_1D_FFT_2Threads(Integer.MAX_VALUE)
      ConcurrencyUtils.setThreadsBeginN_1D_FFT_4Threads(Integer.MAX_VALUE)
      nthreads = Math.min(nthreads, rows)
      val futures = Array.ofDim[Future](nthreads)
      val k = rows / nthreads
      for (j <- 0 until nthreads) {
        val firstRow = j * k
        val lastRow = if ((j == nthreads - 1)) rows else firstRow + k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            for (r <- firstRow until lastRow) {
              var row = Array.ofDim[Double](2 * columns)
              System.arraycopy(elements(r), 0, row, 0, columns)
              fftRows.realForwardFull(row)
              C.viewRow(r).assign(row)
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
      ConcurrencyUtils.resetThreadsBeginN_FFT()
    } else {
      for (r <- 0 until rows) {
        val row = Array.ofDim[Double](2 * columns)
        System.arraycopy(elements(r), 0, row, 0, columns)
        fftRows.realForwardFull(row)
        C.viewRow(r).assign(row)
      }
    }
    ConcurrencyUtils.setNumberOfThreads(oldNthreads)
    C
  }

  /**
   * Returns new complex matrix which is the 2D inverse of the discrete
   * Fourier transform (IDFT) of this matrix.
   *
   * @return the 2D inverse of the discrete Fourier transform (IDFT) of this
   *         matrix.
   */
  def getIfft2(scale: Boolean): DenseLargeDComplexMatrix2D = {
    val oldNthreads = ConcurrencyUtils.getNumberOfThreads
    ConcurrencyUtils.setNumberOfThreads(ConcurrencyUtils.nextPow2(oldNthreads))
    val C = new DenseLargeDComplexMatrix2D(rows, columns)
    val elementsC = (C).elements()
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_2D)) {
      nthreads = Math.min(nthreads, columns)
      val futures = Array.ofDim[Future](nthreads)
      val k = rows / nthreads
      for (j <- 0 until nthreads) {
        val firstRow = j * k
        val lastRow = if ((j == nthreads - 1)) rows else firstRow + k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            for (r <- firstRow until lastRow) {
              System.arraycopy(elements(r), 0, elementsC(r), 0, columns)
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
    } else {
      for (r <- 0 until rows) {
        System.arraycopy(elements(r), 0, elementsC(r), 0, columns)
      }
    }
    if (fft2 == null) {
      fft2 = new DoubleFFT_2D(rows, columns)
    }
    fft2.realInverseFull(elementsC, scale)
    ConcurrencyUtils.setNumberOfThreads(oldNthreads)
    C
  }

  /**
   * Returns new complex matrix which is the inverse of the discrete Fourier
   * transform (IDFT) of each column of this matrix.
   *
   * @return the inverse of the discrete Fourier transform (IDFT) of each
   *         column of this matrix.
   */
  def getIfftColumns(scale: Boolean): DenseLargeDComplexMatrix2D = {
    if (fftColumns == null) {
      fftColumns = new DoubleFFT_1D(rows)
    }
    val oldNthreads = ConcurrencyUtils.getNumberOfThreads
    ConcurrencyUtils.setNumberOfThreads(ConcurrencyUtils.nextPow2(oldNthreads))
    val C = new DenseLargeDComplexMatrix2D(rows, columns)
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_2D)) {
      ConcurrencyUtils.setThreadsBeginN_1D_FFT_2Threads(Integer.MAX_VALUE)
      ConcurrencyUtils.setThreadsBeginN_1D_FFT_4Threads(Integer.MAX_VALUE)
      nthreads = Math.min(nthreads, columns)
      val futures = Array.ofDim[Future](nthreads)
      val k = columns / nthreads
      for (j <- 0 until nthreads) {
        val firstColumn = j * k
        val lastColumn = if ((j == nthreads - 1)) columns else firstColumn + k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            for (c <- firstColumn until lastColumn) {
              var column = Array.ofDim[Double](2 * rows)
              System.arraycopy(viewColumn(c).copy().elements(), 0, column, 0, rows)
              fftColumns.realInverseFull(column, scale)
              C.viewColumn(c).assign(column)
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
      ConcurrencyUtils.resetThreadsBeginN_FFT()
    } else {
      for (c <- 0 until columns) {
        val column = Array.ofDim[Double](2 * rows)
        System.arraycopy(viewColumn(c).copy().elements(), 0, column, 0, rows)
        fftColumns.realInverseFull(column, scale)
        C.viewColumn(c).assign(column)
      }
    }
    ConcurrencyUtils.setNumberOfThreads(oldNthreads)
    C
  }

  /**
   * Returns new complex matrix which is the inverse of the discrete Fourier
   * transform (IDFT) of each row of this matrix.
   *
   * @return the inverse of the discrete Fourier transform (IDFT) of each row
   *         of this matrix.
   */
  def getIfftRows(scale: Boolean): DenseLargeDComplexMatrix2D = {
    if (fftRows == null) {
      fftRows = new DoubleFFT_1D(columns)
    }
    val oldNthreads = ConcurrencyUtils.getNumberOfThreads
    ConcurrencyUtils.setNumberOfThreads(ConcurrencyUtils.nextPow2(oldNthreads))
    val C = new DenseLargeDComplexMatrix2D(rows, columns)
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_2D)) {
      ConcurrencyUtils.setThreadsBeginN_1D_FFT_2Threads(Integer.MAX_VALUE)
      ConcurrencyUtils.setThreadsBeginN_1D_FFT_4Threads(Integer.MAX_VALUE)
      nthreads = Math.min(nthreads, rows)
      val futures = Array.ofDim[Future](nthreads)
      val k = rows / nthreads
      for (j <- 0 until nthreads) {
        val firstRow = j * k
        val lastRow = if ((j == nthreads - 1)) rows else firstRow + k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            for (r <- firstRow until lastRow) {
              var row = Array.ofDim[Double](2 * columns)
              System.arraycopy(elements(r), 0, row, 0, columns)
              fftRows.realInverseFull(row, scale)
              C.viewRow(r).assign(row)
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
      ConcurrencyUtils.resetThreadsBeginN_FFT()
    } else {
      for (r <- 0 until rows) {
        val row = Array.ofDim[Double](2 * columns)
        System.arraycopy(elements(r), 0, row, 0, columns)
        fftRows.realInverseFull(row, scale)
        C.viewRow(r).assign(row)
      }
    }
    ConcurrencyUtils.setNumberOfThreads(oldNthreads)
    C
  }

  def getQuick(row: Int, column: Int): Double = elements(row)(column)

  /**
   * Computes the 2D inverse of the discrete cosine transform (DCT-III) of
   * this matrix.
   *
   * @param scale
   *            if true then scaling is performed
   *
   */
  def idct2(scale: Boolean) {
    val oldNthreads = ConcurrencyUtils.getNumberOfThreads
    ConcurrencyUtils.setNumberOfThreads(ConcurrencyUtils.nextPow2(oldNthreads))
    if (dct2 == null) {
      dct2 = new DoubleDCT_2D(rows, columns)
    }
    dct2.inverse(elements, scale)
    ConcurrencyUtils.setNumberOfThreads(oldNthreads)
  }

  /**
   * Computes the inverse of the discrete cosine transform (DCT-III) of each
   * column of this matrix.
   *
   * @param scale
   *            if true then scaling is performed
   *
   */
  def idctColumns(scale: Boolean) {
    if (dctColumns == null) {
      dctColumns = new DoubleDCT_1D(rows)
    }
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_2D)) {
      ConcurrencyUtils.setThreadsBeginN_1D_FFT_2Threads(Integer.MAX_VALUE)
      ConcurrencyUtils.setThreadsBeginN_1D_FFT_4Threads(Integer.MAX_VALUE)
      nthreads = Math.min(nthreads, columns)
      val futures = Array.ofDim[Future](nthreads)
      val k = columns / nthreads
      for (j <- 0 until nthreads) {
        val firstColumn = j * k
        val lastColumn = if ((j == nthreads - 1)) columns else firstColumn + k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            var column: Array[Double] = null
            for (c <- firstColumn until lastColumn) {
              column = viewColumn(c).copy().elements().asInstanceOf[Array[Double]]
              dctColumns.inverse(column, scale)
              viewColumn(c).assign(column)
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
      ConcurrencyUtils.resetThreadsBeginN_FFT()
    } else {
      var column: Array[Double] = null
      for (c <- 0 until columns) {
        column = viewColumn(c).copy().elements().asInstanceOf[Array[Double]]
        dctColumns.inverse(column, scale)
        viewColumn(c).assign(column)
      }
    }
  }

  /**
   * Computes the inverse of the discrete cosine transform (DCT-III) of each
   * row of this matrix.
   *
   * @param scale
   *            if true then scaling is performed
   *
   */
  def idctRows(scale: Boolean) {
    if (dctRows == null) {
      dctRows = new DoubleDCT_1D(columns)
    }
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_2D)) {
      ConcurrencyUtils.setThreadsBeginN_1D_FFT_2Threads(Integer.MAX_VALUE)
      ConcurrencyUtils.setThreadsBeginN_1D_FFT_4Threads(Integer.MAX_VALUE)
      nthreads = Math.min(nthreads, rows)
      val futures = Array.ofDim[Future](nthreads)
      val k = rows / nthreads
      for (j <- 0 until nthreads) {
        val firstRow = j * k
        val lastRow = if ((j == nthreads - 1)) rows else firstRow + k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            for (r <- firstRow until lastRow) {
              dctRows.inverse(elements(r), scale)
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
      ConcurrencyUtils.resetThreadsBeginN_FFT()
    } else {
      for (r <- 0 until rows) {
        dctRows.inverse(elements(r), scale)
      }
    }
  }

  /**
   * Computes the 2D inverse of the discrete Hartley transform (IDHT) of this
   * matrix.
   *
   * @param scale
   *            if true then scaling is performed
   *
   */
  def idht2(scale: Boolean) {
    val oldNthreads = ConcurrencyUtils.getNumberOfThreads
    ConcurrencyUtils.setNumberOfThreads(ConcurrencyUtils.nextPow2(oldNthreads))
    if (dht2 == null) {
      dht2 = new DoubleDHT_2D(rows, columns)
    }
    dht2.inverse(elements, scale)
    ConcurrencyUtils.setNumberOfThreads(oldNthreads)
  }

  /**
   * Computes the inverse of the discrete Hartley transform (IDHT) of each
   * column of this matrix.
   *
   * @param scale
   *            if true then scaling is performed
   *
   */
  def idhtColumns(scale: Boolean) {
    if (dhtColumns == null) {
      dhtColumns = new DoubleDHT_1D(rows)
    }
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_2D)) {
      ConcurrencyUtils.setThreadsBeginN_1D_FFT_2Threads(Integer.MAX_VALUE)
      ConcurrencyUtils.setThreadsBeginN_1D_FFT_4Threads(Integer.MAX_VALUE)
      nthreads = Math.min(nthreads, columns)
      val futures = Array.ofDim[Future](nthreads)
      val k = columns / nthreads
      for (j <- 0 until nthreads) {
        val firstColumn = j * k
        val lastColumn = if ((j == nthreads - 1)) columns else firstColumn + k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            var column: Array[Double] = null
            for (c <- firstColumn until lastColumn) {
              column = viewColumn(c).copy().elements().asInstanceOf[Array[Double]]
              dhtColumns.inverse(column, scale)
              viewColumn(c).assign(column)
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
      ConcurrencyUtils.resetThreadsBeginN_FFT()
    } else {
      var column: Array[Double] = null
      for (c <- 0 until columns) {
        column = viewColumn(c).copy().elements().asInstanceOf[Array[Double]]
        dhtColumns.inverse(column, scale)
        viewColumn(c).assign(column)
      }
    }
  }

  /**
   * Computes the inverse of the discrete Hartley transform (IDHT) of each row
   * of this matrix.
   *
   * @param scale
   *            if true then scaling is performed
   *
   */
  def idhtRows(scale: Boolean) {
    if (dhtRows == null) {
      dhtRows = new DoubleDHT_1D(columns)
    }
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_2D)) {
      ConcurrencyUtils.setThreadsBeginN_1D_FFT_2Threads(Integer.MAX_VALUE)
      ConcurrencyUtils.setThreadsBeginN_1D_FFT_4Threads(Integer.MAX_VALUE)
      nthreads = Math.min(nthreads, rows)
      val futures = Array.ofDim[Future](nthreads)
      val k = rows / nthreads
      for (j <- 0 until nthreads) {
        val firstRow = j * k
        val lastRow = if ((j == nthreads - 1)) rows else firstRow + k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            for (r <- firstRow until lastRow) {
              dhtRows.inverse(elements(r), scale)
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
      ConcurrencyUtils.resetThreadsBeginN_FFT()
    } else {
      for (r <- 0 until rows) {
        dhtRows.inverse(elements(r), scale)
      }
    }
  }

  /**
   * Computes the 2D inverse of the discrete sine transform (DST-III) of this
   * matrix.
   *
   * @param scale
   *            if true then scaling is performed
   *
   */
  def idst2(scale: Boolean) {
    val oldNthreads = ConcurrencyUtils.getNumberOfThreads
    ConcurrencyUtils.setNumberOfThreads(ConcurrencyUtils.nextPow2(oldNthreads))
    if (dst2 == null) {
      dst2 = new DoubleDST_2D(rows, columns)
    }
    dst2.inverse(elements, scale)
    ConcurrencyUtils.setNumberOfThreads(oldNthreads)
  }

  /**
   * Computes the inverse of the discrete sine transform (DST-III) of each
   * column of this matrix.
   *
   * @param scale
   *            if true then scaling is performed
   *
   */
  def idstColumns(scale: Boolean) {
    if (dstColumns == null) {
      dstColumns = new DoubleDST_1D(rows)
    }
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_2D)) {
      ConcurrencyUtils.setThreadsBeginN_1D_FFT_2Threads(Integer.MAX_VALUE)
      ConcurrencyUtils.setThreadsBeginN_1D_FFT_4Threads(Integer.MAX_VALUE)
      nthreads = Math.min(nthreads, columns)
      val futures = Array.ofDim[Future](nthreads)
      val k = columns / nthreads
      for (j <- 0 until nthreads) {
        val firstColumn = j * k
        val lastColumn = if ((j == nthreads - 1)) columns else firstColumn + k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            var column: Array[Double] = null
            for (c <- firstColumn until lastColumn) {
              column = viewColumn(c).copy().elements().asInstanceOf[Array[Double]]
              dstColumns.inverse(column, scale)
              viewColumn(c).assign(column)
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
      ConcurrencyUtils.resetThreadsBeginN_FFT()
    } else {
      var column: Array[Double] = null
      for (c <- 0 until columns) {
        column = viewColumn(c).copy().elements().asInstanceOf[Array[Double]]
        dstColumns.inverse(column, scale)
        viewColumn(c).assign(column)
      }
    }
  }

  /**
   * Computes the inverse of the discrete sine transform (DST-III) of each row
   * of this matrix.
   *
   * @param scale
   *            if true then scaling is performed
   *
   */
  def idstRows(scale: Boolean) {
    if (dstRows == null) {
      dstRows = new DoubleDST_1D(columns)
    }
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_2D)) {
      ConcurrencyUtils.setThreadsBeginN_1D_FFT_2Threads(Integer.MAX_VALUE)
      ConcurrencyUtils.setThreadsBeginN_1D_FFT_4Threads(Integer.MAX_VALUE)
      nthreads = Math.min(nthreads, rows)
      val futures = Array.ofDim[Future](nthreads)
      val k = rows / nthreads
      for (j <- 0 until nthreads) {
        val firstRow = j * k
        val lastRow = if ((j == nthreads - 1)) rows else firstRow + k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            for (r <- firstRow until lastRow) {
              dstRows.inverse(elements(r), scale)
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
      ConcurrencyUtils.resetThreadsBeginN_FFT()
    } else {
      for (r <- 0 until rows) {
        dstRows.inverse(elements(r), scale)
      }
    }
  }

  /**
   * Computes the 2D inverse of the discrete Fourier transform (IDFT) of this
   * matrix. The physical layout of the input data has to be as follows:
   *
   * <pre>
   * this[k1][2*k2] = Re[k1][k2] = Re[rows-k1][columns-k2],
   * this[k1][2*k2+1] = Im[k1][k2] = -Im[rows-k1][columns-k2],
   *       0&lt;k1&lt;rows, 0&lt;k2&lt;columns/2,
   * this[0][2*k2] = Re[0][k2] = Re[0][columns-k2],
   * this[0][2*k2+1] = Im[0][k2] = -Im[0][columns-k2],
   *       0&lt;k2&lt;columns/2,
   * this[k1][0] = Re[k1][0] = Re[rows-k1][0],
   * this[k1][1] = Im[k1][0] = -Im[rows-k1][0],
   * this[rows-k1][1] = Re[k1][columns/2] = Re[rows-k1][columns/2],
   * this[rows-k1][0] = -Im[k1][columns/2] = Im[rows-k1][columns/2],
   *       0&lt;k1&lt;rows/2,
   * this[0][0] = Re[0][0],
   * this[0][1] = Re[0][columns/2],
   * this[rows/2][0] = Re[rows/2][0],
   * this[rows/2][1] = Re[rows/2][columns/2]
   * </pre>
   *
   * This method computes only half of the elements of the real transform. The
   * other half satisfies the symmetry condition. If you want the full real
   * inverse transform, use <code>getIfft2</code>.
   *
   * @throws IllegalArgumentException
   *             if the row size or the column size of this matrix is not a
   *             power of 2 number.
   *
   * @param scale
   *            if true then scaling is performed
   *
   */
  def ifft2(scale: Boolean) {
    val oldNthreads = ConcurrencyUtils.getNumberOfThreads
    ConcurrencyUtils.setNumberOfThreads(ConcurrencyUtils.nextPow2(oldNthreads))
    if (fft2 == null) {
      fft2 = new DoubleFFT_2D(rows, columns)
    }
    fft2.realInverse(elements, scale)
    ConcurrencyUtils.setNumberOfThreads(oldNthreads)
  }

  def setQuick(row: Int, column: Int, value: Double) {
    elements(row)(column) = value
  }

  protected def getStorageMatrix(): StrideMatrix2D = this

  def like(rows: Int, columns: Int): StrideMatrix2D = {
    new DenseLargeDoubleMatrix2D(rows, columns)
  }

  def like1D(size: Int): StrideMatrix1D = new DenseMatrix1D(size)
}
