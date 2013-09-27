package cern.colt.matrix.tdouble.impl

import cern.colt.matrix.tdcomplex.impl.DenseLargeDComplexMatrix3D
import cern.colt.matrix.tdouble.StrideMatrix1D
import cern.colt.matrix.tdouble.StrideMatrix2D
import cern.colt.matrix.tdouble.DoubleMatrix3D
//remove if not needed
import scala.collection.JavaConversions._

/**
 * 3-d matrix holding <tt>double</tt> elements; either a view wrapping another
 * matrix or a matrix whose views are wrappers.
 *
 *
 * @author Piotr Wendykier (piotr.wendykier@gmail.com)
 */
@SerialVersionUID(1L)
class WrapperDoubleMatrix3D(newContent: DoubleMatrix3D) extends DoubleMatrix3D {

  protected var content: DoubleMatrix3D = newContent

  if (newContent != null) try {
    setUp(newContent.slices(), newContent.rows(), newContent.columns())
  } catch {
    case exc: IllegalArgumentException => if ("matrix too large" != exc.getMessage) throw exc
  }

  def elements(): AnyRef = content.elements()

  /**
   * Computes the 3D discrete cosine transform (DCT-II) of this matrix.
   *
   * @param scale
   *            if true then scaling is performed
   *
   */
  def dct3(scale: Boolean) {
    if (content.isInstanceOf[DenseLargeDoubleMatrix3D]) {
      if (this.isNoView == true) {
        content.asInstanceOf[DenseLargeDoubleMatrix3D].dct3(scale)
      } else {
        val copy = copy().asInstanceOf[DenseLargeDoubleMatrix3D]
        copy.dct3(scale)
        assign(copy)
      }
    } else {
      throw new IllegalArgumentException("This method is not supported")
    }
  }

  /**
   * Computes the 2D discrete cosine transform (DCT-II) of each slice of this
   * matrix.
   *
   * @param scale
   *            if true then scaling is performed
   */
  def dct2Slices(scale: Boolean) {
    if (content.isInstanceOf[DenseLargeDoubleMatrix3D]) {
      if (this.isNoView == true) {
        content.asInstanceOf[DenseLargeDoubleMatrix3D].dct2Slices(scale)
      } else {
        val copy = copy().asInstanceOf[DenseLargeDoubleMatrix3D]
        copy.dct2Slices(scale)
        assign(copy)
      }
    } else {
      throw new IllegalArgumentException("This method is not supported")
    }
  }

  /**
   * Computes the 3D discrete sine transform (DST-II) of this matrix.
   *
   * @param scale
   *            if true then scaling is performed
   */
  def dst3(scale: Boolean) {
    if (content.isInstanceOf[DenseLargeDoubleMatrix3D]) {
      if (this.isNoView == true) {
        content.asInstanceOf[DenseLargeDoubleMatrix3D].dst3(scale)
      } else {
        val copy = copy().asInstanceOf[DenseLargeDoubleMatrix3D]
        copy.dst3(scale)
        assign(copy)
      }
    } else {
      throw new IllegalArgumentException("This method is not supported")
    }
  }

  /**
   * Computes the 2D discrete sine transform (DST-II) of each slice of this
   * matrix.
   *
   * @param scale
   *            if true then scaling is performed
   *
   */
  def dst2Slices(scale: Boolean) {
    if (content.isInstanceOf[DenseLargeDoubleMatrix3D]) {
      if (this.isNoView == true) {
        content.asInstanceOf[DenseLargeDoubleMatrix3D].dst2Slices(scale)
      } else {
        val copy = copy().asInstanceOf[DenseLargeDoubleMatrix3D]
        copy.dst2Slices(scale)
        assign(copy)
      }
    } else {
      throw new IllegalArgumentException("This method is not supported")
    }
  }

  /**
   * Computes the 3D discrete Hartley transform (DHT) of this matrix.
   */
  def dht3() {
    if (content.isInstanceOf[DenseLargeDoubleMatrix3D]) {
      if (this.isNoView == true) {
        content.asInstanceOf[DenseLargeDoubleMatrix3D].dht3()
      } else {
        val copy = copy().asInstanceOf[DenseLargeDoubleMatrix3D]
        copy.dht3()
        assign(copy)
      }
    } else {
      throw new IllegalArgumentException("This method is not supported")
    }
  }

  /**
   * Computes the 2D discrete Hertley transform (DHT) of each column of this
   * matrix.
   */
  def dht2Slices() {
    if (content.isInstanceOf[DenseLargeDoubleMatrix3D]) {
      if (this.isNoView == true) {
        content.asInstanceOf[DenseLargeDoubleMatrix3D].dht2Slices()
      } else {
        val copy = copy().asInstanceOf[DenseLargeDoubleMatrix3D]
        copy.dht2Slices()
        assign(copy)
      }
    } else {
      throw new IllegalArgumentException("This method is not supported")
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
    if (content.isInstanceOf[DenseLargeDoubleMatrix3D]) {
      if (this.isNoView == true) {
        content.asInstanceOf[DenseLargeDoubleMatrix3D].fft3()
      } else {
        val copy = copy().asInstanceOf[DenseLargeDoubleMatrix3D]
        copy.fft3()
        assign(copy)
      }
    } else {
      throw new IllegalArgumentException("This method is not supported")
    }
  }

  /**
   * Returns new complex matrix which is the 3D discrete Fourier transform
   * (DFT) of this matrix.
   *
   * @return the 3D discrete Fourier transform (DFT) of this matrix.
   *
   */
  def getFft3(): DenseLargeDComplexMatrix3D = {
    if (content.isInstanceOf[DenseLargeDoubleMatrix3D]) {
      if (this.isNoView == true) {
        content.asInstanceOf[DenseLargeDoubleMatrix3D].getFft3
      } else {
        copy().asInstanceOf[DenseLargeDoubleMatrix3D].getFft3
      }
    } else {
      throw new IllegalArgumentException("This method is not supported")
    }
  }

  /**
   * Returns new complex matrix which is the 3D inverse of the discrete
   * Fourier transform (IDFT) of this matrix.
   *
   * @return the 3D inverse of the discrete Fourier transform (IDFT) of this
   *         matrix.
   */
  def getIfft3(scale: Boolean): DenseLargeDComplexMatrix3D = {
    if (content.isInstanceOf[DenseLargeDoubleMatrix3D]) {
      if (this.isNoView == true) {
        content.asInstanceOf[DenseLargeDoubleMatrix3D].getIfft3(scale)
      } else {
        copy().asInstanceOf[DenseLargeDoubleMatrix3D].getIfft3(scale)
      }
    } else {
      throw new IllegalArgumentException("This method is not supported")
    }
  }

  /**
   * Returns new complex matrix which is the 2D discrete Fourier transform
   * (DFT) of each slice of this matrix.
   *
   * @return the 2D discrete Fourier transform (DFT) of each slice of this
   *         matrix.
   */
  def getFft2Slices(): DenseLargeDComplexMatrix3D = {
    if (content.isInstanceOf[DenseLargeDoubleMatrix3D]) {
      if (this.isNoView == true) {
        content.asInstanceOf[DenseLargeDoubleMatrix3D].getFft2Slices
      } else {
        copy().asInstanceOf[DenseLargeDoubleMatrix3D].getFft2Slices
      }
    } else {
      throw new IllegalArgumentException("This method is not supported")
    }
  }

  /**
   * Returns new complex matrix which is the 2D inverse of the discrete
   * Fourier transform (IDFT) of each slice of this matrix.
   *
   * @return the 2D inverse of the discrete Fourier transform (IDFT) of each
   *         slice of this matrix.
   */
  def getIfft2Slices(scale: Boolean): DenseLargeDComplexMatrix3D = {
    if (content.isInstanceOf[DenseLargeDoubleMatrix3D]) {
      if (this.isNoView == true) {
        content.asInstanceOf[DenseLargeDoubleMatrix3D].getIfft2Slices(scale)
      } else {
        copy().asInstanceOf[DenseLargeDoubleMatrix3D].getIfft2Slices(scale)
      }
    } else {
      throw new IllegalArgumentException("This method is not supported")
    }
  }

  /**
   * Computes the 3D inverse of the discrete cosine transform (DCT-III) of
   * this matrix.
   *
   * @param scale
   *            if true then scaling is performed
   */
  def idct3(scale: Boolean) {
    if (content.isInstanceOf[DenseLargeDoubleMatrix3D]) {
      if (this.isNoView == true) {
        content.asInstanceOf[DenseLargeDoubleMatrix3D].idct3(scale)
      } else {
        val copy = copy().asInstanceOf[DenseLargeDoubleMatrix3D]
        copy.idct3(scale)
        assign(copy)
      }
    } else {
      throw new IllegalArgumentException("This method is not supported")
    }
  }

  /**
   * Computes the 2D inverse of the discrete cosine transform (DCT-III) of
   * each slice of this matrix.
   *
   * @param scale
   *            if true then scaling is performed
   */
  def idct2Slices(scale: Boolean) {
    if (content.isInstanceOf[DenseLargeDoubleMatrix3D]) {
      if (this.isNoView == true) {
        content.asInstanceOf[DenseLargeDoubleMatrix3D].idct2Slices(scale)
      } else {
        val copy = copy().asInstanceOf[DenseLargeDoubleMatrix3D]
        copy.idct2Slices(scale)
        assign(copy)
      }
    } else {
      throw new IllegalArgumentException("This method is not supported")
    }
  }

  /**
   * Computes the 3D inverse of the discrete size transform (DST-III) of this
   * matrix.
   *
   * @param scale
   *            if true then scaling is performed
   */
  def idst3(scale: Boolean) {
    if (content.isInstanceOf[DenseLargeDoubleMatrix3D]) {
      if (this.isNoView == true) {
        content.asInstanceOf[DenseLargeDoubleMatrix3D].idst3(scale)
      } else {
        val copy = copy().asInstanceOf[DenseLargeDoubleMatrix3D]
        copy.idst3(scale)
        assign(copy)
      }
    } else {
      throw new IllegalArgumentException("This method is not supported")
    }
  }

  /**
   * Computes the 2D inverse of the discrete sine transform (DST-III) of each
   * slice of this matrix.
   *
   * @param scale
   *            if true then scaling is performed
   */
  def idst2Slices(scale: Boolean) {
    if (content.isInstanceOf[DenseLargeDoubleMatrix3D]) {
      if (this.isNoView == true) {
        content.asInstanceOf[DenseLargeDoubleMatrix3D].idst2Slices(scale)
      } else {
        val copy = copy().asInstanceOf[DenseLargeDoubleMatrix3D]
        copy.idst2Slices(scale)
        assign(copy)
      }
    } else {
      throw new IllegalArgumentException("This method is not supported")
    }
  }

  /**
   * Computes the 3D inverse of the discrete Hartley transform (DHT) of this
   * matrix.
   *
   * @param scale
   *            if true then scaling is performed
   */
  def idht3(scale: Boolean) {
    if (content.isInstanceOf[DenseLargeDoubleMatrix3D]) {
      if (this.isNoView == true) {
        content.asInstanceOf[DenseLargeDoubleMatrix3D].idht3(scale)
      } else {
        val copy = copy().asInstanceOf[DenseLargeDoubleMatrix3D]
        copy.idht3(scale)
        assign(copy)
      }
    } else {
      throw new IllegalArgumentException("This method is not supported")
    }
  }

  /**
   * Computes the 2D inverse of the discrete Hartley transform (DHT) of each
   * slice of this matrix.
   *
   * @param scale
   *            if true then scaling is performed
   */
  def idht2Slices(scale: Boolean) {
    if (content.isInstanceOf[DenseLargeDoubleMatrix3D]) {
      if (this.isNoView == true) {
        content.asInstanceOf[DenseLargeDoubleMatrix3D].idht2Slices(scale)
      } else {
        val copy = copy().asInstanceOf[DenseLargeDoubleMatrix3D]
        copy.idht2Slices(scale)
        assign(copy)
      }
    } else {
      throw new IllegalArgumentException("This method is not supported")
    }
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
    if (content.isInstanceOf[DenseLargeDoubleMatrix3D]) {
      if (this.isNoView == true) {
        content.asInstanceOf[DenseLargeDoubleMatrix3D].ifft3(scale)
      } else {
        val copy = copy().asInstanceOf[DenseLargeDoubleMatrix3D]
        copy.ifft3(scale)
        assign(copy)
      }
    } else {
      throw new IllegalArgumentException("This method is not supported")
    }
  }

  def getQuick(slice: Int, row: Int, column: Int): Double = {
    synchronized {
      content.getQuick(slice, row, column)
    }
  }

  def like(slices: Int, rows: Int, columns: Int): DoubleMatrix3D = content.like(slices, rows, columns)

  def setQuick(slice: Int,
      row: Int,
      column: Int,
      value: Double) {
    synchronized {
      content.setQuick(slice, row, column, value)
    }
  }

  def vectorize(): StrideMatrix1D = {
    val v = new DenseMatrix1D(size.toInt)
    val length = rows * columns
    for (s <- 0 until slices) {
      v.viewPart(s * length, length).assign(viewSlice(s).vectorize())
    }
    v
  }

  def viewColumn(column: Int): StrideMatrix2D = {
    checkColumn(column)
    new DelegateDoubleMatrix2D(this, 2, column)
  }

  def viewColumnFlip(): DoubleMatrix3D = {
    if (columns == 0) return this
    val view = new WrapperDoubleMatrix3D(this) {

      private val serialVersionUID = 1L

      def getQuick(slice: Int, row: Int, column: Int): Double = {
        synchronized {
          return content.getQuick(slice, row, columns - 1 - column)
        }
      }

      def setQuick(slice: Int,
          row: Int,
          column: Int,
          value: Double) {
        synchronized {
          content.setQuick(slice, row, columns - 1 - column, value)
        }
      }

      def get(slice: Int, row: Int, column: Int): Double = {
        synchronized {
          return content.get(slice, row, columns - 1 - column)
        }
      }

      def set(slice: Int,
          row: Int,
          column: Int,
          value: Double) {
        synchronized {
          content.set(slice, row, columns - 1 - column, value)
        }
      }
    }
    view.isNoView = false
    view
  }

  def viewSlice(slice: Int): StrideMatrix2D = {
    checkSlice(slice)
    new DelegateDoubleMatrix2D(this, 0, slice)
  }

  def viewSliceFlip(): DoubleMatrix3D = {
    if (slices == 0) return this
    val view = new WrapperDoubleMatrix3D(this) {

      private val serialVersionUID = 1L

      def getQuick(slice: Int, row: Int, column: Int): Double = {
        synchronized {
          return content.getQuick(slices - 1 - slice, row, column)
        }
      }

      def setQuick(slice: Int,
          row: Int,
          column: Int,
          value: Double) {
        synchronized {
          content.setQuick(slices - 1 - slice, row, column, value)
        }
      }

      def get(slice: Int, row: Int, column: Int): Double = {
        synchronized {
          return content.get(slices - 1 - slice, row, column)
        }
      }

      def set(slice: Int,
          row: Int,
          column: Int,
          value: Double) {
        synchronized {
          content.set(slices - 1 - slice, row, column, value)
        }
      }
    }
    view.isNoView = false
    view
  }

  def viewDice(axis0: Int, axis1: Int, axis2: Int): DoubleMatrix3D = {
    val d = 3
    if (axis0 < 0 || axis0 >= d || axis1 < 0 || axis1 >= d || axis2 < 0 ||
      axis2 >= d ||
      axis0 == axis1 ||
      axis0 == axis2 ||
      axis1 == axis2) {
      throw new IllegalArgumentException("Illegal Axes: " + axis0 + ", " + axis1 + ", " + axis2)
    }
    var view: WrapperDoubleMatrix3D = null
    if (axis0 == 0 && axis1 == 1 && axis2 == 2) {
      view = new WrapperDoubleMatrix3D(this)
    } else if (axis0 == 1 && axis1 == 0 && axis2 == 2) {
      view = new WrapperDoubleMatrix3D(this) {

        /**
         *
         */
        private val serialVersionUID = 1L

        def getQuick(slice: Int, row: Int, column: Int): Double = {
          synchronized {
            return content.getQuick(row, slice, column)
          }
        }

        def setQuick(slice: Int,
            row: Int,
            column: Int,
            value: Double) {
          synchronized {
            content.setQuick(row, slice, column, value)
          }
        }

        def get(slice: Int, row: Int, column: Int): Double = {
          synchronized {
            return content.get(row, slice, column)
          }
        }

        def set(slice: Int,
            row: Int,
            column: Int,
            value: Double) {
          synchronized {
            content.set(row, slice, column, value)
          }
        }
      }
    } else if (axis0 == 1 && axis1 == 2 && axis2 == 0) {
      view = new WrapperDoubleMatrix3D(this) {

        /**
         *
         */
        private val serialVersionUID = 1L

        def getQuick(slice: Int, row: Int, column: Int): Double = {
          synchronized {
            return content.getQuick(row, column, slice)
          }
        }

        def setQuick(slice: Int,
            row: Int,
            column: Int,
            value: Double) {
          synchronized {
            content.setQuick(row, column, slice, value)
          }
        }

        def get(slice: Int, row: Int, column: Int): Double = {
          synchronized {
            return content.get(row, column, slice)
          }
        }

        def set(slice: Int,
            row: Int,
            column: Int,
            value: Double) {
          synchronized {
            content.set(row, column, slice, value)
          }
        }
      }
    } else if (axis0 == 2 && axis1 == 1 && axis2 == 0) {
      view = new WrapperDoubleMatrix3D(this) {

        /**
         *
         */
        private val serialVersionUID = 1L

        def getQuick(slice: Int, row: Int, column: Int): Double = {
          synchronized {
            return content.getQuick(column, row, slice)
          }
        }

        def setQuick(slice: Int,
            row: Int,
            column: Int,
            value: Double) {
          synchronized {
            content.setQuick(column, row, slice, value)
          }
        }

        def get(slice: Int, row: Int, column: Int): Double = {
          synchronized {
            return content.get(column, row, slice)
          }
        }

        def set(slice: Int,
            row: Int,
            column: Int,
            value: Double) {
          synchronized {
            content.set(column, row, slice, value)
          }
        }
      }
    } else if (axis0 == 2 && axis1 == 0 && axis2 == 1) {
      view = new WrapperDoubleMatrix3D(this) {

        /**
         *
         */
        private val serialVersionUID = 1L

        def getQuick(slice: Int, row: Int, column: Int): Double = {
          synchronized {
            return content.getQuick(column, slice, row)
          }
        }

        def setQuick(slice: Int,
            row: Int,
            column: Int,
            value: Double) {
          synchronized {
            content.setQuick(column, slice, row, value)
          }
        }

        def get(slice: Int, row: Int, column: Int): Double = {
          synchronized {
            return content.get(column, slice, row)
          }
        }

        def set(slice: Int,
            row: Int,
            column: Int,
            value: Double) {
          synchronized {
            content.set(column, slice, row, value)
          }
        }
      }
    }
    val shape = shape()
    view.slices = shape(axis0)
    view.rows = shape(axis1)
    view.columns = shape(axis2)
    view.isNoView = false
    view
  }

  def viewPart(slice: Int,
      row: Int,
      column: Int,
      depth: Int,
      height: Int,
      width: Int): DoubleMatrix3D = {
    checkBox(slice, row, column, depth, height, width)
    val view = new WrapperDoubleMatrix3D(this) {

      private val serialVersionUID = 1L

      def getQuick(i: Int, j: Int, k: Int): Double = {
        synchronized {
          return content.getQuick(slice + i, row + j, column + k)
        }
      }

      def setQuick(i: Int,
          j: Int,
          k: Int,
          value: Double) {
        synchronized {
          content.setQuick(slice + i, row + j, column + k, value)
        }
      }

      def get(i: Int, j: Int, k: Int): Double = {
        synchronized {
          return content.get(slice + i, row + j, column + k)
        }
      }

      def set(i: Int,
          j: Int,
          k: Int,
          value: Double) {
        synchronized {
          content.set(slice + i, row + j, column + k, value)
        }
      }
    }
    view.slices = depth
    view.rows = height
    view.columns = width
    view.isNoView = false
    view
  }

  def viewRow(row: Int): StrideMatrix2D = {
    checkRow(row)
    new DelegateDoubleMatrix2D(this, 1, row)
  }

  def viewRowFlip(): DoubleMatrix3D = {
    if (rows == 0) return this
    val view = new WrapperDoubleMatrix3D(this) {

      private val serialVersionUID = 1L

      def getQuick(slice: Int, row: Int, column: Int): Double = {
        synchronized {
          return content.getQuick(slice, rows - 1 - row, column)
        }
      }

      def setQuick(slice: Int,
          row: Int,
          column: Int,
          value: Double) {
        synchronized {
          content.setQuick(slice, rows - 1 - row, column, value)
        }
      }

      def get(slice: Int, row: Int, column: Int): Double = {
        synchronized {
          return content.get(slice, rows - 1 - row, column)
        }
      }

      def set(slice: Int,
          row: Int,
          column: Int,
          value: Double) {
        synchronized {
          content.set(slice, rows - 1 - row, column, value)
        }
      }
    }
    view.isNoView = false
    view
  }

  def viewSelection(sliceIndexes: Array[Int], rowIndexes: Array[Int], columnIndexes: Array[Int]): DoubleMatrix3D = {
    if (sliceIndexes == null) {
      sliceIndexes = Array.ofDim[Int](slices)
      var i = slices
      while (i >= 0) sliceIndexes(i) = i
    }
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
    checkSliceIndexes(sliceIndexes)
    checkRowIndexes(rowIndexes)
    checkColumnIndexes(columnIndexes)
    val six = sliceIndexes
    val rix = rowIndexes
    val cix = columnIndexes
    val view = new WrapperDoubleMatrix3D(this) {

      private val serialVersionUID = 1L

      def getQuick(i: Int, j: Int, k: Int): Double = {
        synchronized {
          return content.getQuick(six(i), rix(j), cix(k))
        }
      }

      def setQuick(i: Int,
          j: Int,
          k: Int,
          value: Double) {
        synchronized {
          content.setQuick(six(i), rix(j), cix(k), value)
        }
      }

      def get(i: Int, j: Int, k: Int): Double = {
        synchronized {
          return content.get(six(i), rix(j), cix(k))
        }
      }

      def set(i: Int,
          j: Int,
          k: Int,
          value: Double) {
        synchronized {
          content.set(six(i), rix(j), cix(k), value)
        }
      }
    }
    view.slices = sliceIndexes.length
    view.rows = rowIndexes.length
    view.columns = columnIndexes.length
    view.isNoView = false
    view
  }

  def viewStrides(_sliceStride: Int, _rowStride: Int, _columnStride: Int): DoubleMatrix3D = {
    if (_sliceStride <= 0 || _rowStride <= 0 || _columnStride <= 0) throw new IndexOutOfBoundsException("illegal stride")
    val view = new WrapperDoubleMatrix3D(this) {

      private val serialVersionUID = 1L

      def getQuick(slice: Int, row: Int, column: Int): Double = {
        synchronized {
          return content.getQuick(_sliceStride * slice, _rowStride * row, _columnStride * column)
        }
      }

      def setQuick(slice: Int,
          row: Int,
          column: Int,
          value: Double) {
        synchronized {
          content.setQuick(_sliceStride * slice, _rowStride * row, _columnStride * column, value)
        }
      }

      def get(slice: Int, row: Int, column: Int): Double = {
        synchronized {
          return content.get(_sliceStride * slice, _rowStride * row, _columnStride * column)
        }
      }

      def set(slice: Int,
          row: Int,
          column: Int,
          value: Double) {
        synchronized {
          content.set(_sliceStride * slice, _rowStride * row, _columnStride * column, value)
        }
      }
    }
    if (slices != 0) view.slices = (slices - 1) / _sliceStride + 1
    if (rows != 0) view.rows = (rows - 1) / _rowStride + 1
    if (columns != 0) view.columns = (columns - 1) / _columnStride + 1
    view.isNoView = false
    view
  }

  protected def getStorageMatrix(): DoubleMatrix3D = content

  def like2D(rows: Int, columns: Int): StrideMatrix2D = throw new InternalError()

  protected def like2D(rows: Int,
      columns: Int,
      rowZero: Int,
      columnZero: Int,
      rowStride: Int,
      columnStride: Int): StrideMatrix2D = throw new InternalError()

  protected def viewSelectionLike(sliceOffsets: Array[Int], rowOffsets: Array[Int], columnOffsets: Array[Int]): DoubleMatrix3D = {
    throw new InternalError()
  }
}
