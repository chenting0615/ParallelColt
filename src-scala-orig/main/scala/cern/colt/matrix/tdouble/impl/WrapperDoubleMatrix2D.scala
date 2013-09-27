package cern.colt.matrix.tdouble.impl

import cern.colt.list.tdouble.DoubleArrayList
import cern.colt.list.tint.IntArrayList
import cern.colt.matrix.tdcomplex.impl.DenseLargeDComplexMatrix2D
import cern.colt.matrix.tdouble.StrideMatrix1D
import cern.colt.matrix.tdouble.StrideMatrix2D
import edu.emory.mathcs.utils.ConcurrencyUtils
import java.util.concurrent.Future
//remove if not needed
import scala.collection.JavaConversions._

/**
 * 2-d matrix holding <tt>double</tt> elements; either a view wrapping another
 * matrix or a matrix whose views are wrappers.
 *
 * @author wolfgang.hoschek@cern.ch
 * @version 1.0, 04/14/2000
 *
 * @author Piotr Wendykier (piotr.wendykier@gmail.com)
 */
@SerialVersionUID(1L)
class WrapperDoubleMatrix2D(newContent: StrideMatrix2D) extends StrideMatrix2D {

  protected var content: StrideMatrix2D = newContent

  if (newContent != null) try {
    setUp(newContent.rows(), newContent.columns())
  } catch {
    case exc: IllegalArgumentException => if ("matrix too large" != exc.getMessage) throw exc
  }

  def assign(values: Array[Double]): StrideMatrix2D = {
    if (content.isInstanceOf[DiagonalDoubleMatrix2D]) {
      val dlength = content.asInstanceOf[DiagonalDoubleMatrix2D].dlength
      val elems = content.asInstanceOf[DiagonalDoubleMatrix2D].elements
      if (values.length != dlength) throw new IllegalArgumentException("Must have same length: length=" + values.length + " dlength=" +
        dlength)
      var nthreads = ConcurrencyUtils.getNumberOfThreads
      if ((nthreads > 1) && (dlength >= ConcurrencyUtils.getThreadsBeginN_2D)) {
        nthreads = Math.min(nthreads, dlength)
        val futures = Array.ofDim[Future](nthreads)
        val k = dlength / nthreads
        for (j <- 0 until nthreads) {
          val firstIdx = j * k
          val lastIdx = if ((j == nthreads - 1)) dlength else firstIdx + k
          futures(j) = ConcurrencyUtils.submit(new Runnable() {

            def run() {
              for (i <- firstIdx until lastIdx) {
                elems(i) = values(i)
              }
            }
          })
        }
        ConcurrencyUtils.waitForCompletion(futures)
      } else {
        for (i <- 0 until dlength) {
          elems(i) = values(i)
        }
      }
      this
    } else {
      super.assign(values)
    }
  }

  def assign(values: Array[Float]): StrideMatrix2D = {
    if (content.isInstanceOf[DiagonalDoubleMatrix2D]) {
      val dlength = content.asInstanceOf[DiagonalDoubleMatrix2D].dlength
      val elems = content.asInstanceOf[DiagonalDoubleMatrix2D].elements
      if (values.length != dlength) throw new IllegalArgumentException("Must have same length: length=" + values.length + " dlength=" +
        dlength)
      var nthreads = ConcurrencyUtils.getNumberOfThreads
      if ((nthreads > 1) && (dlength >= ConcurrencyUtils.getThreadsBeginN_2D)) {
        nthreads = Math.min(nthreads, dlength)
        val futures = Array.ofDim[Future](nthreads)
        val k = dlength / nthreads
        for (j <- 0 until nthreads) {
          val firstIdx = j * k
          val lastIdx = if ((j == nthreads - 1)) dlength else firstIdx + k
          futures(j) = ConcurrencyUtils.submit(new Runnable() {

            def run() {
              for (i <- firstIdx until lastIdx) {
                elems(i) = values(i)
              }
            }
          })
        }
        ConcurrencyUtils.waitForCompletion(futures)
      } else {
        for (i <- 0 until dlength) {
          elems(i) = values(i)
        }
      }
      this
    } else {
      super.assign(values)
    }
  }

  def assign(y: StrideMatrix2D, function: cern.colt.function.tdouble.DoubleDoubleFunction): StrideMatrix2D = {
    checkShape(y)
    if (y.isInstanceOf[WrapperMatrix2D]) {
      val rowList = new IntArrayList()
      val columnList = new IntArrayList()
      val valueList = new DoubleArrayList()
      y.getNonZeros(rowList, columnList, valueList)
      assign(y, function, rowList, columnList)
    } else {
      super.assign(y, function)
    }
    this
  }

  def elements(): AnyRef = content.elements()

  def getQuick(row: Int, column: Int): Double = {
    synchronized {
      content.getQuick(row, column)
    }
  }

  override def equals(value: Double): Boolean = {
    if (content.isInstanceOf[DiagonalDoubleMatrix2D]) {
      val epsilon = cern.colt.matrix.tdouble.algo.DoubleProperty.DEFAULT
        .tolerance()
      val elements = content.elements().asInstanceOf[Array[Double]]
      for (r <- 0 until elements.length) {
        val x = elements(r)
        var diff = Math.abs(value - x)
        if ((diff != diff) && ((value != value && x != x) || value == x)) diff = 0
        if (!(diff <= epsilon)) {
          return false
        }
      }
      true
    } else {
      super == value
    }
  }

  override def equals(obj: Any): Boolean = {
    if (content.isInstanceOf[DiagonalDoubleMatrix2D] && obj.isInstanceOf[DiagonalDoubleMatrix2D]) {
      val epsilon = cern.colt.matrix.tdouble.algo.DoubleProperty.DEFAULT
        .tolerance()
      if (this == obj) return true
      val A = content.asInstanceOf[DiagonalDoubleMatrix2D]
      val B = obj.asInstanceOf[DiagonalDoubleMatrix2D]
      if (A.columns() != B.columns() || A.rows() != B.rows() || A.diagonalIndex() != B.diagonalIndex() ||
        A.diagonalLength() != B.diagonalLength()) return false
      val AElements = A.elements()
      val BElements = B.elements()
      for (r <- 0 until AElements.length) {
        val x = AElements(r)
        val value = BElements(r)
        var diff = Math.abs(value - x)
        if ((diff != diff) && ((value != value && x != x) || value == x)) diff = 0
        if (!(diff <= epsilon)) {
          return false
        }
      }
      true
    } else {
      super == obj
    }
  }

  def like(rows: Int, columns: Int): StrideMatrix2D = content.like(rows, columns)

  def like1D(size: Int): StrideMatrix1D = content.like1D(size)

  /**
   * Computes the 2D discrete cosine transform (DCT-II) of this matrix.
   *
   * @param scale
   *            if true then scaling is performed
   *
   */
  def dct2(scale: Boolean) {
    if (content.isInstanceOf[DenseLargeDoubleMatrix2D]) {
      if (this.isNoView) {
        content.asInstanceOf[DenseLargeDoubleMatrix2D].dct2(scale)
      } else {
        val copy = copy().asInstanceOf[DenseLargeDoubleMatrix2D]
        copy.dct2(scale)
        assign(copy)
      }
    } else {
      throw new IllegalArgumentException("This method is not supported")
    }
  }

  /**
   * Computes the discrete cosine transform (DCT-II) of each column of this
   * matrix.
   *
   * @param scale
   *            if true then scaling is performed
   */
  def dctColumns(scale: Boolean) {
    if (content.isInstanceOf[DenseLargeDoubleMatrix2D]) {
      if (this.isNoView) {
        content.asInstanceOf[DenseLargeDoubleMatrix2D].dctColumns(scale)
      } else {
        val copy = copy().asInstanceOf[DenseLargeDoubleMatrix2D]
        copy.dctColumns(scale)
        assign(copy)
      }
    } else {
      throw new IllegalArgumentException("This method is not supported")
    }
  }

  /**
   * Computes the discrete cosine transform (DCT-II) of each row of this
   * matrix.
   *
   * @param scale
   *            if true then scaling is performed
   */
  def dctRows(scale: Boolean) {
    if (content.isInstanceOf[DenseLargeDoubleMatrix2D]) {
      if (this.isNoView) {
        content.asInstanceOf[DenseLargeDoubleMatrix2D].dctRows(scale)
      } else {
        val copy = copy().asInstanceOf[DenseLargeDoubleMatrix2D]
        copy.dctRows(scale)
        assign(copy)
      }
    } else {
      throw new IllegalArgumentException("This method is not supported")
    }
  }

  /**
   * Computes the 2D discrete sine transform (DST-II) of this matrix.
   *
   * @param scale
   *            if true then scaling is performed
   */
  def dst2(scale: Boolean) {
    if (content.isInstanceOf[DenseLargeDoubleMatrix2D]) {
      if (this.isNoView) {
        content.asInstanceOf[DenseLargeDoubleMatrix2D].dst2(scale)
      } else {
        val copy = copy().asInstanceOf[DenseLargeDoubleMatrix2D]
        copy.dst2(scale)
        assign(copy)
      }
    } else {
      throw new IllegalArgumentException("This method is not supported")
    }
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
    if (content.isInstanceOf[DenseLargeDoubleMatrix2D]) {
      if (this.isNoView) {
        content.asInstanceOf[DenseLargeDoubleMatrix2D].dstColumns(scale)
      } else {
        val copy = copy().asInstanceOf[DenseLargeDoubleMatrix2D]
        copy.dstColumns(scale)
        assign(copy)
      }
    } else {
      throw new IllegalArgumentException("This method is not supported")
    }
  }

  /**
   * Computes the discrete sine transform (DST-II) of each row of this matrix.
   *
   * @param scale
   *            if true then scaling is performed
   */
  def dstRows(scale: Boolean) {
    if (content.isInstanceOf[DenseLargeDoubleMatrix2D]) {
      if (this.isNoView) {
        content.asInstanceOf[DenseLargeDoubleMatrix2D].dstRows(scale)
      } else {
        val copy = copy().asInstanceOf[DenseLargeDoubleMatrix2D]
        copy.dstRows(scale)
        assign(copy)
      }
    } else {
      throw new IllegalArgumentException("This method is not supported")
    }
  }

  /**
   * Computes the 2D discrete Hartley transform (DHT) of this matrix.
   */
  def dht2() {
    if (content.isInstanceOf[DenseLargeDoubleMatrix2D]) {
      if (this.isNoView) {
        content.asInstanceOf[DenseLargeDoubleMatrix2D].dht2()
      } else {
        val copy = copy().asInstanceOf[DenseLargeDoubleMatrix2D]
        copy.dht2()
        assign(copy)
      }
    } else {
      throw new IllegalArgumentException("This method is not supported")
    }
  }

  /**
   * Computes the discrete Hertley transform (DHT) of each column of this
   * matrix.
   */
  def dhtColumns() {
    if (content.isInstanceOf[DenseLargeDoubleMatrix2D]) {
      if (this.isNoView) {
        content.asInstanceOf[DenseLargeDoubleMatrix2D].dhtColumns()
      } else {
        val copy = copy().asInstanceOf[DenseLargeDoubleMatrix2D]
        copy.dhtColumns()
        assign(copy)
      }
    } else {
      throw new IllegalArgumentException("This method is not supported")
    }
  }

  /**
   * Computes the discrete Hertley transform (DHT) of each row of this matrix.
   */
  def dhtRows() {
    if (content.isInstanceOf[DenseLargeDoubleMatrix2D]) {
      if (this.isNoView) {
        content.asInstanceOf[DenseLargeDoubleMatrix2D].dhtRows()
      } else {
        val copy = copy().asInstanceOf[DenseLargeDoubleMatrix2D]
        copy.dhtRows()
        assign(copy)
      }
    } else {
      throw new IllegalArgumentException("This method is not supported")
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
    if (content.isInstanceOf[DenseLargeDoubleMatrix2D]) {
      if (this.isNoView) {
        content.asInstanceOf[DenseLargeDoubleMatrix2D].fft2()
      } else {
        val copy = copy().asInstanceOf[DenseLargeDoubleMatrix2D]
        copy.fft2()
        assign(copy)
      }
    } else {
      throw new IllegalArgumentException("This method is not supported")
    }
  }

  /**
   * Returns new complex matrix which is the 2D discrete Fourier transform
   * (DFT) of this matrix.
   *
   * @return the 2D discrete Fourier transform (DFT) of this matrix.
   *
   */
  def getFft2(): DenseLargeDComplexMatrix2D = {
    if (content.isInstanceOf[DenseLargeDoubleMatrix2D]) {
      if (this.isNoView) {
        content.asInstanceOf[DenseLargeDoubleMatrix2D].getFft2
      } else {
        copy().asInstanceOf[DenseLargeDoubleMatrix2D].getFft2
      }
    } else {
      throw new IllegalArgumentException("This method is not supported")
    }
  }

  /**
   * Returns new complex matrix which is the 2D inverse of the discrete
   * Fourier transform (IDFT) of this matrix.
   *
   * @return the 2D inverse of the discrete Fourier transform (IDFT) of this
   *         matrix.
   */
  def getIfft2(scale: Boolean): DenseLargeDComplexMatrix2D = {
    if (content.isInstanceOf[DenseLargeDoubleMatrix2D]) {
      if (this.isNoView) {
        content.asInstanceOf[DenseLargeDoubleMatrix2D].getIfft2(scale)
      } else {
        copy().asInstanceOf[DenseLargeDoubleMatrix2D].getIfft2(scale)
      }
    } else {
      throw new IllegalArgumentException("This method is not supported")
    }
  }

  /**
   * Returns new complex matrix which is the discrete Fourier transform (DFT)
   * of each column of this matrix.
   *
   * @return the discrete Fourier transform (DFT) of each column of this
   *         matrix.
   */
  def getFftColumns(): DenseLargeDComplexMatrix2D = {
    if (content.isInstanceOf[DenseLargeDoubleMatrix2D]) {
      if (this.isNoView) {
        content.asInstanceOf[DenseLargeDoubleMatrix2D].getFftColumns
      } else {
        copy().asInstanceOf[DenseLargeDoubleMatrix2D].getFftColumns
      }
    } else {
      throw new IllegalArgumentException("This method is not supported")
    }
  }

  /**
   * Returns new complex matrix which is the discrete Fourier transform (DFT)
   * of each row of this matrix.
   *
   * @return the discrete Fourier transform (DFT) of each row of this matrix.
   */
  def getFftRows(): DenseLargeDComplexMatrix2D = {
    if (content.isInstanceOf[DenseLargeDoubleMatrix2D]) {
      if (this.isNoView) {
        content.asInstanceOf[DenseLargeDoubleMatrix2D].getFftRows
      } else {
        copy().asInstanceOf[DenseLargeDoubleMatrix2D].getFftRows
      }
    } else {
      throw new IllegalArgumentException("This method is not supported")
    }
  }

  /**
   * Returns new complex matrix which is the inverse of the discrete Fourier
   * transform (IDFT) of each column of this matrix.
   *
   * @return the inverse of the discrete Fourier transform (IDFT) of each
   *         column of this matrix.
   */
  def getIfftColumns(scale: Boolean): DenseLargeDComplexMatrix2D = {
    if (content.isInstanceOf[DenseLargeDoubleMatrix2D]) {
      if (this.isNoView) {
        content.asInstanceOf[DenseLargeDoubleMatrix2D].getIfftColumns(scale)
      } else {
        copy().asInstanceOf[DenseLargeDoubleMatrix2D].getIfftColumns(scale)
      }
    } else {
      throw new IllegalArgumentException("This method is not supported")
    }
  }

  /**
   * Returns new complex matrix which is the inverse of the discrete Fourier
   * transform (IDFT) of each row of this matrix.
   *
   * @return the inverse of the discrete Fourier transform (IDFT) of each row
   *         of this matrix.
   */
  def getIfftRows(scale: Boolean): DenseLargeDComplexMatrix2D = {
    if (this.isNoView) {
      content.asInstanceOf[DenseLargeDoubleMatrix2D].getIfftRows(scale)
    } else {
      copy().asInstanceOf[DenseLargeDoubleMatrix2D].getIfftRows(scale)
    }
  }

  /**
   * Computes the 2D inverse of the discrete cosine transform (DCT-III) of
   * this matrix.
   *
   * @param scale
   *            if true then scaling is performed
   */
  def idct2(scale: Boolean) {
    if (content.isInstanceOf[DenseLargeDoubleMatrix2D]) {
      if (this.isNoView) {
        content.asInstanceOf[DenseLargeDoubleMatrix2D].idct2(scale)
      } else {
        val copy = copy().asInstanceOf[DenseLargeDoubleMatrix2D]
        copy.idct2(scale)
        assign(copy)
      }
    } else {
      throw new IllegalArgumentException("This method is not supported")
    }
  }

  /**
   * Computes the inverse of the discrete cosine transform (DCT-III) of each
   * column of this matrix.
   *
   * @param scale
   *            if true then scaling is performed
   */
  def idctColumns(scale: Boolean) {
    if (content.isInstanceOf[DenseLargeDoubleMatrix2D]) {
      if (this.isNoView) {
        content.asInstanceOf[DenseLargeDoubleMatrix2D].idctColumns(scale)
      } else {
        val copy = copy().asInstanceOf[DenseLargeDoubleMatrix2D]
        copy.idctColumns(scale)
        assign(copy)
      }
    } else {
      throw new IllegalArgumentException("This method is not supported")
    }
  }

  /**
   * Computes the inverse of the discrete cosine transform (DCT-III) of each
   * row of this matrix.
   *
   * @param scale
   *            if true then scaling is performed
   */
  def idctRows(scale: Boolean) {
    if (content.isInstanceOf[DenseLargeDoubleMatrix2D]) {
      if (this.isNoView) {
        content.asInstanceOf[DenseLargeDoubleMatrix2D].idctRows(scale)
      } else {
        val copy = copy().asInstanceOf[DenseLargeDoubleMatrix2D]
        copy.idctRows(scale)
        assign(copy)
      }
    } else {
      throw new IllegalArgumentException("This method is not supported")
    }
  }

  /**
   * Computes the 2D inverse of the discrete size transform (DST-III) of this
   * matrix.
   *
   * @param scale
   *            if true then scaling is performed
   */
  def idst2(scale: Boolean) {
    if (content.isInstanceOf[DenseLargeDoubleMatrix2D]) {
      if (this.isNoView) {
        content.asInstanceOf[DenseLargeDoubleMatrix2D].idst2(scale)
      } else {
        val copy = copy().asInstanceOf[DenseLargeDoubleMatrix2D]
        copy.idst2(scale)
        assign(copy)
      }
    } else {
      throw new IllegalArgumentException("This method is not supported")
    }
  }

  /**
   * Computes the inverse of the discrete sine transform (DST-III) of each
   * column of this matrix.
   *
   * @param scale
   *            if true then scaling is performed
   */
  def idstColumns(scale: Boolean) {
    if (content.isInstanceOf[DenseLargeDoubleMatrix2D]) {
      if (this.isNoView) {
        content.asInstanceOf[DenseLargeDoubleMatrix2D].idstColumns(scale)
      } else {
        val copy = copy().asInstanceOf[DenseLargeDoubleMatrix2D]
        copy.idstColumns(scale)
        assign(copy)
      }
    } else {
      throw new IllegalArgumentException("This method is not supported")
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
    if (content.isInstanceOf[DenseLargeDoubleMatrix2D]) {
      if (this.isNoView) {
        content.asInstanceOf[DenseLargeDoubleMatrix2D].idstRows(scale)
      } else {
        val copy = copy().asInstanceOf[DenseLargeDoubleMatrix2D]
        copy.idstRows(scale)
        assign(copy)
      }
    } else {
      throw new IllegalArgumentException("This method is not supported")
    }
  }

  /**
   * Computes the 2D inverse of the discrete Hartley transform (DHT) of this
   * matrix.
   *
   * @param scale
   *            if true then scaling is performed
   */
  def idht2(scale: Boolean) {
    if (content.isInstanceOf[DenseLargeDoubleMatrix2D]) {
      if (this.isNoView) {
        content.asInstanceOf[DenseLargeDoubleMatrix2D].idht2(scale)
      } else {
        val copy = copy().asInstanceOf[DenseLargeDoubleMatrix2D]
        copy.idht2(scale)
        assign(copy)
      }
    } else {
      throw new IllegalArgumentException("This method is not supported")
    }
  }

  /**
   * Computes the inverse of the discrete Hartley transform (DHT) of each
   * column of this matrix.
   *
   * @param scale
   *            if true then scaling is performed
   */
  def idhtColumns(scale: Boolean) {
    if (content.isInstanceOf[DenseLargeDoubleMatrix2D]) {
      if (this.isNoView) {
        content.asInstanceOf[DenseLargeDoubleMatrix2D].idhtColumns(scale)
      } else {
        val copy = copy().asInstanceOf[DenseLargeDoubleMatrix2D]
        copy.idhtColumns(scale)
        assign(copy)
      }
    } else {
      throw new IllegalArgumentException("This method is not supported")
    }
  }

  /**
   * Computes the inverse of the discrete Hartley transform (DHT) of each row
   * of this matrix.
   *
   * @param scale
   *            if true then scaling is performed
   *
   */
  def idhtRows(scale: Boolean) {
    if (content.isInstanceOf[DenseLargeDoubleMatrix2D]) {
      if (this.isNoView) {
        content.asInstanceOf[DenseLargeDoubleMatrix2D].idhtRows(scale)
      } else {
        val copy = copy().asInstanceOf[DenseLargeDoubleMatrix2D]
        copy.idhtRows(scale)
        assign(copy)
      }
    } else {
      throw new IllegalArgumentException("This method is not supported")
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
    if (content.isInstanceOf[DenseLargeDoubleMatrix2D]) {
      if (this.isNoView) {
        content.asInstanceOf[DenseLargeDoubleMatrix2D].ifft2(scale)
      } else {
        val copy = copy().asInstanceOf[DenseLargeDoubleMatrix2D]
        copy.ifft2(scale)
        assign(copy)
      }
    } else {
      throw new IllegalArgumentException("This method is not supported")
    }
  }

  def setQuick(row: Int, column: Int, value: Double) {
    synchronized {
      content.setQuick(row, column, value)
    }
  }

  def vectorize(): StrideMatrix1D = {
    val v = new DenseMatrix1D(size.toInt)
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_2D)) {
      nthreads = Math.min(nthreads, columns)
      val futures = Array.ofDim[Future](nthreads)
      val k = columns / nthreads
      for (j <- 0 until nthreads) {
        val firstCol = j * k
        val lastCol = if ((j == nthreads - 1)) columns else firstCol + k
        val firstidx = j * k * rows
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            var idx = firstidx
            for (c <- firstCol until lastCol; r <- 0 until rows) {
              v.setQuick(idx += 1, getQuick(r, c))
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
    } else {
      var idx = 0
      for (c <- 0 until columns; r <- 0 until rows) {
        v.setQuick(idx += 1, getQuick(r, c))
      }
    }
    v
  }

  def viewColumn(column: Int): StrideMatrix1D = viewDice().viewRow(column)

  def viewColumnFlip(): StrideMatrix2D = {
    if (columns == 0) return this
    val view = new WrapperMatrix2D(this) {

      private val serialVersionUID = 1L

      def getQuick(row: Int, column: Int): Double = {
        synchronized {
          return content.getQuick(row, columns - 1 - column)
        }
      }

      def setQuick(row: Int, column: Int, value: Double) {
        synchronized {
          content.setQuick(row, columns - 1 - column, value)
        }
      }

      def get(row: Int, column: Int): Double = {
        synchronized {
          return content.get(row, columns - 1 - column)
        }
      }

      def set(row: Int, column: Int, value: Double) {
        synchronized {
          content.set(row, columns - 1 - column, value)
        }
      }
    }
    view.isNoView = false
    view
  }

  def viewDice(): StrideMatrix2D = {
    val view = new WrapperMatrix2D(this) {

      private val serialVersionUID = 1L

      def getQuick(row: Int, column: Int): Double = {
        synchronized {
          return content.getQuick(column, row)
        }
      }

      def setQuick(row: Int, column: Int, value: Double) {
        synchronized {
          content.setQuick(column, row, value)
        }
      }

      def get(row: Int, column: Int): Double = {
        synchronized {
          return content.get(column, row)
        }
      }

      def set(row: Int, column: Int, value: Double) {
        synchronized {
          content.set(column, row, value)
        }
      }
    }
    view.rows = columns
    view.columns = rows
    view.isNoView = false
    view
  }

  def viewPart(row: Int,
      column: Int,
      height: Int,
      width: Int): StrideMatrix2D = {
    checkBox(row, column, height, width)
    val view = new WrapperMatrix2D(this) {

      private val serialVersionUID = 1L

      def getQuick(i: Int, j: Int): Double = {
        synchronized {
          return content.getQuick(row + i, column + j)
        }
      }

      def setQuick(i: Int, j: Int, value: Double) {
        synchronized {
          content.setQuick(row + i, column + j, value)
        }
      }

      def get(i: Int, j: Int): Double = {
        synchronized {
          return content.get(row + i, column + j)
        }
      }

      def set(i: Int, j: Int, value: Double) {
        synchronized {
          content.set(row + i, column + j, value)
        }
      }
    }
    view.rows = height
    view.columns = width
    view.isNoView = false
    view
  }

  def viewRow(row: Int): StrideMatrix1D = {
    checkRow(row)
    new DelegateDoubleMatrix1D(this, row)
  }

  def viewRowFlip(): StrideMatrix2D = {
    if (rows == 0) return this
    val view = new WrapperMatrix2D(this) {

      private val serialVersionUID = 1L

      def getQuick(row: Int, column: Int): Double = {
        synchronized {
          return content.getQuick(rows - 1 - row, column)
        }
      }

      def setQuick(row: Int, column: Int, value: Double) {
        synchronized {
          content.setQuick(rows - 1 - row, column, value)
        }
      }

      def get(row: Int, column: Int): Double = {
        synchronized {
          return content.get(rows - 1 - row, column)
        }
      }

      def set(row: Int, column: Int, value: Double) {
        synchronized {
          content.set(rows - 1 - row, column, value)
        }
      }
    }
    view.isNoView = false
    view
  }

  def viewSelection(rowIndexes: Array[Int], columnIndexes: Array[Int]): StrideMatrix2D = {
    if (rowIndexes == null) {
      rowIndexes = Array.ofDim[Int](rows)
      var i = rows
      while (i >= 0) rowIndexes(i) = i
    }
    if (columnIndexes == null) {
      columnIndexes = Array.ofDim[Int](columns)
      var i = columns
      while (i >= 0) columnIndexes(i) = i
    }
    checkRowIndexes(rowIndexes)
    checkColumnIndexes(columnIndexes)
    val rix = rowIndexes
    val cix = columnIndexes
    val view = new WrapperMatrix2D(this) {

      private val serialVersionUID = 1L

      def getQuick(i: Int, j: Int): Double = {
        synchronized {
          return content.getQuick(rix(i), cix(j))
        }
      }

      def setQuick(i: Int, j: Int, value: Double) {
        synchronized {
          content.setQuick(rix(i), cix(j), value)
        }
      }

      def get(i: Int, j: Int): Double = {
        synchronized {
          return content.get(rix(i), cix(j))
        }
      }

      def set(i: Int, j: Int, value: Double) {
        synchronized {
          content.set(rix(i), cix(j), value)
        }
      }
    }
    view.rows = rowIndexes.length
    view.columns = columnIndexes.length
    view.isNoView = false
    view
  }

  def viewStrides(_rowStride: Int, _columnStride: Int): StrideMatrix2D = {
    if (_rowStride <= 0 || _columnStride <= 0) throw new IndexOutOfBoundsException("illegal stride")
    val view = new WrapperMatrix2D(this) {

      private val serialVersionUID = 1L

      def getQuick(row: Int, column: Int): Double = {
        synchronized {
          return content.getQuick(_rowStride * row, _columnStride * column)
        }
      }

      def setQuick(row: Int, column: Int, value: Double) {
        synchronized {
          content.setQuick(_rowStride * row, _columnStride * column, value)
        }
      }

      def get(row: Int, column: Int): Double = {
        synchronized {
          return content.get(_rowStride * row, _columnStride * column)
        }
      }

      def set(row: Int, column: Int, value: Double) {
        synchronized {
          content.set(_rowStride * row, _columnStride * column, value)
        }
      }
    }
    if (rows != 0) view.rows = (rows - 1) / _rowStride + 1
    if (columns != 0) view.columns = (columns - 1) / _columnStride + 1
    view.isNoView = false
    view
  }

  protected def getStorageMatrix(): StrideMatrix2D = content

  protected def like1D(size: Int, offset: Int, stride: Int): StrideMatrix1D = throw new InternalError()

  protected def viewSelectionLike(rowOffsets: Array[Int], columnOffsets: Array[Int]): StrideMatrix2D = {
    throw new InternalError()
  }
}
