package cern.colt.matrix.tdouble.algo

import java.util.concurrent.Future
import cern.colt.matrix.tdouble.StrideMatrix1D
import cern.colt.matrix.tdouble.StrideMatrix2D
import cern.jet.math.tdouble.DoubleFunctions
import edu.emory.mathcs.utils.ConcurrencyUtils
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Parallel implementation of the Basic Linear Algebra System for symmetric
 * multi processing boxes. In all cases, no or only marginal speedup is seen for
 * small problem sizes; they are detected and the sequential algorithm is used.
 *
 *
 * @author wolfgang.hoschek@cern.ch
 * @version 0.9, 16/04/2000
 *
 * @author Piotr Wendykier (piotr.wendykier@gmail.com)
 */
class SmpDoubleBlas extends DoubleBlas {

  def assign(A: StrideMatrix2D, function: cern.colt.function.tdouble.Function1) {
    A.assign(function)
  }

  def assign(A: StrideMatrix2D, B: StrideMatrix2D, function: cern.colt.function.tdouble.DoubleDoubleFunction) {
    A.assign(B, function)
  }

  def dasum(x: StrideMatrix1D): Double = {
    x.aggregate(DoubleFunctions.plus, DoubleFunctions.abs)
  }

  def daxpy(alpha: Double, x: StrideMatrix1D, y: StrideMatrix1D) {
    y.assign(x, DoubleFunctions.plusMultSecond(alpha))
  }

  def daxpy(alpha: Double, A: StrideMatrix2D, B: StrideMatrix2D) {
    B.assign(A, DoubleFunctions.plusMultSecond(alpha))
  }

  def dcopy(x: StrideMatrix1D, y: StrideMatrix1D) {
    y.assign(x)
  }

  def dcopy(A: StrideMatrix2D, B: StrideMatrix2D) {
    B.assign(A)
  }

  def ddot(x: StrideMatrix1D, y: StrideMatrix1D): Double = x.zDotProduct(y)

  def dgemm(transposeA: Boolean,
      transposeB: Boolean,
      alpha: Double,
      A: StrideMatrix2D,
      B: StrideMatrix2D,
      beta: Double,
      C: StrideMatrix2D) {
    A.zMult(B, C, alpha, beta, transposeA, transposeB)
  }

  def dgemv(transposeA: Boolean,
      alpha: Double,
      A: StrideMatrix2D,
      x: StrideMatrix1D,
      beta: Double,
      y: StrideMatrix1D) {
    A.zMult(x, y, alpha, beta, transposeA)
  }

  def dger(alpha: Double,
      x: StrideMatrix1D,
      y: StrideMatrix1D,
      A: StrideMatrix2D) {
    val fun = cern.jet.math.tdouble.DoublePlusMultSecond.plusMult(0)
    val rows = A.rows()
    for (i <- 0 until rows) {
      fun.multiplicator = alpha * x.getQuick(i)
      A.viewRow(i).assign(y, fun)
    }
  }

  def dnrm2(x: StrideMatrix1D): Double = DenseDoubleAlgebra.DEFAULT.norm2(x)

  def drot(x: StrideMatrix1D,
      y: StrideMatrix1D,
      c: Double,
      s: Double) {
    x.checkSize(y)
    val tmp = x.copy()
    x.assign(DoubleFunctions.mult(c))
    x.assign(y, DoubleFunctions.plusMultSecond(s))
    y.assign(DoubleFunctions.mult(c))
    y.assign(tmp, DoubleFunctions.minusMult(s))
  }

  def drotg(a: Double, b: Double, rotvec: Array[Double]) {
    var c: Double = 0.0
    var s: Double = 0.0
    var roe: Double = 0.0
    var scale: Double = 0.0
    var r: Double = 0.0
    var z: Double = 0.0
    var ra: Double = 0.0
    var rb: Double = 0.0
    roe = b
    if (Math.abs(a) > Math.abs(b)) roe = a
    scale = Math.abs(a) + Math.abs(b)
    if (scale != 0.0) {
      ra = a / scale
      rb = b / scale
      r = scale * Math.sqrt(ra * ra + rb * rb)
      r = sign(1.0, roe) * r
      c = a / r
      s = b / r
      z = 1.0
      if (Math.abs(a) > Math.abs(b)) z = s
      if ((Math.abs(b) >= Math.abs(a)) && (c != 0.0)) z = 1.0 / c
    } else {
      c = 1.0
      s = 0.0
      r = 0.0
      z = 0.0
    }
    a = r
    b = z
    rotvec(0) = a
    rotvec(1) = b
    rotvec(2) = c
    rotvec(3) = s
  }

  def dscal(alpha: Double, x: StrideMatrix1D) {
    x.assign(DoubleFunctions.mult(alpha))
  }

  def dscal(alpha: Double, A: StrideMatrix2D) {
    A.assign(DoubleFunctions.mult(alpha))
  }

  def dswap(x: StrideMatrix1D, y: StrideMatrix1D) {
    y.swap(x)
  }

  def dswap(A: StrideMatrix2D, B: StrideMatrix2D) {
    A.checkShape(B)
    val rows = A.rows()
    for (i <- 0 until rows) A.viewRow(i).swap(B.viewRow(i))
  }

  def dsymv(isUpperTriangular: Boolean,
      alpha: Double,
      A: StrideMatrix2D,
      x: StrideMatrix1D,
      beta: Double,
      y: StrideMatrix1D) {
    var A_loc: StrideMatrix2D = null
    A_loc = if (isUpperTriangular) A.viewDice() else A
    DoubleProperty.DEFAULT.checkSquare(A_loc)
    val size = A_loc.rows()
    if (size != x.size || size != y.size) {
      throw new IllegalArgumentException(A_loc.toShapeString() + ", " + x.toShapeString() + ", " +
        y.toShapeString())
    }
    val tmp = x.like()
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_2D)) {
      nthreads = Math.min(nthreads, size)
      val futures = Array.ofDim[Future](nthreads)
      val k = size / nthreads
      for (j <- 0 until nthreads) {
        val firstIdx = j * k
        val lastIdx = if ((j == nthreads - 1)) size else firstIdx + k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            for (i <- firstIdx until lastIdx) {
              var sum = 0
              var j = 0
              while (j <= i) {
                sum += A_loc.getQuick(i, j) * x.getQuick(j)
                j += 1
              }
              for (j <- i + 1 until lastIdx) {
                sum += A_loc.getQuick(j, i) * x.getQuick(j)
              }
              tmp.setQuick(i, alpha * sum + beta * y.getQuick(i))
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
    } else {
      for (i <- 0 until size) {
        var sum = 0
        var j = 0
        while (j <= i) {
          sum += A_loc.getQuick(i, j) * x.getQuick(j)
          j += 1
        }
        for (j <- i + 1 until size) {
          sum += A_loc.getQuick(j, i) * x.getQuick(j)
        }
        tmp.setQuick(i, alpha * sum + beta * y.getQuick(i))
      }
    }
    y.assign(tmp)
  }

  def dtrmv(isUpperTriangular: Boolean,
      transposeA: Boolean,
      isUnitTriangular: Boolean,
      A: StrideMatrix2D,
      x: StrideMatrix1D) {
    var A_loc: StrideMatrix2D = null
    var isUpperTriangular_loc: Boolean = false
    if (transposeA) {
      A_loc = A.viewDice()
      isUpperTriangular_loc = !isUpperTriangular
    } else {
      A_loc = A
      isUpperTriangular_loc = isUpperTriangular
    }
    DoubleProperty.DEFAULT.checkSquare(A_loc)
    val size = A_loc.rows()
    if (size != x.size) {
      throw new IllegalArgumentException(A_loc.toShapeString() + ", " + x.toShapeString())
    }
    val b = x.like()
    val y = x.like()
    if (isUnitTriangular) {
      y.assign(1)
    } else {
      for (i <- 0 until size) {
        y.setQuick(i, A_loc.getQuick(i, i))
      }
    }
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_2D)) {
      nthreads = Math.min(nthreads, size)
      val futures = Array.ofDim[Future](nthreads)
      val k = size / nthreads
      for (j <- 0 until nthreads) {
        val firstIdx = j * k
        val lastIdx = if ((j == nthreads - 1)) size else firstIdx + k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            for (i <- firstIdx until lastIdx) {
              var sum = 0
              if (!isUpperTriangular_loc) {
                for (j <- 0 until i) {
                  sum += A_loc.getQuick(i, j) * x.getQuick(j)
                }
                sum += y.getQuick(i) * x.getQuick(i)
              } else {
                sum += y.getQuick(i) * x.getQuick(i)
                for (j <- i + 1 until lastIdx) {
                  sum += A_loc.getQuick(i, j) * x.getQuick(j)
                }
              }
              b.setQuick(i, sum)
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
    } else {
      for (i <- 0 until size) {
        var sum = 0
        if (!isUpperTriangular_loc) {
          for (j <- 0 until i) {
            sum += A_loc.getQuick(i, j) * x.getQuick(j)
          }
          sum += y.getQuick(i) * x.getQuick(i)
        } else {
          sum += y.getQuick(i) * x.getQuick(i)
          for (j <- i + 1 until size) {
            sum += A_loc.getQuick(i, j) * x.getQuick(j)
          }
        }
        b.setQuick(i, sum)
      }
    }
    x.assign(b)
  }

  def idamax(x: StrideMatrix1D): Int = {
    val x_abs = x.copy()
    x_abs.assign(DoubleFunctions.abs)
    val maxAndLoc = x_abs.getMaxLocation
    maxAndLoc(1).toInt
  }

  /**
   * Implements the FORTRAN sign (not sin) function. See the code for details.
   *
   * @param a
   *            a
   * @param b
   *            b
   */
  private def sign(a: Double, b: Double): Double = {
    if (b < 0.0) {
      -Math.abs(a)
    } else {
      Math.abs(a)
    }
  }
}
