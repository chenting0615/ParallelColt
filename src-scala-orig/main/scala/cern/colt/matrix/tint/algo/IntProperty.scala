package cern.colt.matrix.tint.algo

import java.util.concurrent.Callable
import java.util.concurrent.ExecutionException
import java.util.concurrent.Future
import cern.colt.matrix.AbstractFormatter
import cern.colt.matrix.tint.IntMatrix1D
import cern.colt.matrix.tint.IntMatrix2D
import cern.colt.matrix.tint.IntMatrix3D
import cern.jet.math.tint.IntFunctions
import edu.emory.mathcs.utils.ConcurrencyUtils
import IntProperty._
//remove if not needed
import scala.collection.JavaConversions._

object IntProperty {

  /**
   * The default Property object;.
   */
  val DEFAULT = new IntProperty()

  /**
   * Returns a String with <tt>length</tt> blanks.
   */
  protected def blanks(length: Int): String = {
    if (length < 0) length = 0
    val buf = new StringBuffer(length)
    for (k <- 0 until length) {
      buf.append(' ')
    }
    buf.toString
  }

  /**
   */
  protected def get(list: cern.colt.list.tobject.ObjectArrayList, index: Int): String = list.get(index).asInstanceOf[String]
}

/**
 * Tests matrices for linear algebraic properties (equality, tridiagonality,
 * symmetry, singularity, etc).
 * <p>
 *
 * Note that this implementation is not synchronized.
 * <p>
 * Here are some example properties
 * <table border="1" cellspacing="0">
 * <tr align="left" valign="top">
 * <td valign="middle" align="left"><tt>matrix</tt></td>
 * <td> <tt>4&nbsp;x&nbsp;4&nbsp;<br>
 0&nbsp;0&nbsp;0&nbsp;0<br>
 0&nbsp;0&nbsp;0&nbsp;0<br>
 0&nbsp;0&nbsp;0&nbsp;0<br>
 0&nbsp;0&nbsp;0&nbsp;0 </tt></td>
 * <td><tt>4&nbsp;x&nbsp;4<br>
 1&nbsp;0&nbsp;0&nbsp;0<br>
 0&nbsp;0&nbsp;0&nbsp;0<br>
 0&nbsp;0&nbsp;0&nbsp;0<br>
 0&nbsp;0&nbsp;0&nbsp;1 </tt></td>
 * <td><tt>4&nbsp;x&nbsp;4<br>
 1&nbsp;1&nbsp;0&nbsp;0<br>
 1&nbsp;1&nbsp;1&nbsp;0<br>
 0&nbsp;1&nbsp;1&nbsp;1<br>
 0&nbsp;0&nbsp;1&nbsp;1 </tt></td>
 * <td><tt> 4&nbsp;x&nbsp;4<br>
 0&nbsp;1&nbsp;1&nbsp;1<br>
 0&nbsp;1&nbsp;1&nbsp;1<br>
 0&nbsp;0&nbsp;0&nbsp;1<br>
 0&nbsp;0&nbsp;0&nbsp;1 </tt></td>
 * <td><tt> 4&nbsp;x&nbsp;4<br>
 0&nbsp;0&nbsp;0&nbsp;0<br>
 1&nbsp;1&nbsp;0&nbsp;0<br>
 1&nbsp;1&nbsp;0&nbsp;0<br>
 1&nbsp;1&nbsp;1&nbsp;1 </tt></td>
 * <td><tt>4&nbsp;x&nbsp;4<br>
 1&nbsp;1&nbsp;0&nbsp;0<br>
 0&nbsp;1&nbsp;1&nbsp;0<br>
 0&nbsp;1&nbsp;0&nbsp;1<br>
 1&nbsp;0&nbsp;1&nbsp;1 </tt><tt> </tt></td>
 * <td><tt>4&nbsp;x&nbsp;4<br>
 1&nbsp;1&nbsp;1&nbsp;0<br>
 0&nbsp;1&nbsp;0&nbsp;0<br>
 1&nbsp;1&nbsp;0&nbsp;1<br>
 0&nbsp;0&nbsp;1&nbsp;1 </tt></td>
 * </tr>
 * <tr align="center" valign="middle">
 * <td><tt>upperBandwidth</tt></td>
 * <td><div align="center"><tt>0</tt></div></td>
 * <td><div align="center"><tt>0</tt></div></td>
 * <td><div align="center"><tt>1</tt></div></td>
 * <td><tt>3</tt></td>
 * <td align="center" valign="middle"><tt>0</tt></td>
 * <td align="center" valign="middle"><div align="center"><tt>1</tt></div></td>
 * <td align="center" valign="middle"><div align="center"><tt>2</tt></div></td>
 * </tr>
 * <tr align="center" valign="middle">
 * <td><tt>lowerBandwidth</tt></td>
 * <td><div align="center"><tt>0</tt></div></td>
 * <td><div align="center"><tt>0</tt></div></td>
 * <td><div align="center"><tt>1</tt></div></td>
 * <td><tt>0</tt></td>
 * <td align="center" valign="middle"><tt>3</tt></td>
 * <td align="center" valign="middle"><div align="center"><tt>3</tt></div></td>
 * <td align="center" valign="middle"><div align="center"><tt>2</tt></div></td>
 * </tr>
 * <tr align="center" valign="middle">
 * <td><tt>semiBandwidth</tt></td>
 * <td><div align="center"><tt>1</tt></div></td>
 * <td><div align="center"><tt>1</tt></div></td>
 * <td><div align="center"><tt>2</tt></div></td>
 * <td><tt>4</tt></td>
 * <td align="center" valign="middle"><tt>4</tt></td>
 * <td align="center" valign="middle"><div align="center"><tt>4</tt></div></td>
 * <td align="center" valign="middle"><div align="center"><tt>3</tt></div></td>
 * </tr>
 * <tr align="center" valign="middle">
 * <td><tt>description</tt></td>
 * <td><div align="center"><tt>zero</tt></div></td>
 * <td><div align="center"><tt>diagonal</tt></div></td>
 * <td><div align="center"><tt>tridiagonal</tt></div></td>
 * <td><tt>upper triangular</tt></td>
 * <td align="center" valign="middle"><tt>lower triangular</tt></td>
 * <td align="center" valign="middle"><div align="center"><tt>unstructured</tt>
 * </div></td>
 * <td align="center" valign="middle"><div align="center"><tt>unstructured</tt>
 * </div></td>
 * </tr>
 * </table>
 *
 * @author Piotr Wendykier (piotr.wendykier@gmail.com)
 */
@SerialVersionUID(1L)
class IntProperty extends cern.colt.PersistentObject {

  /**
   * Checks whether the given matrix <tt>A</tt> is <i>rectangular</i>.
   *
   * @throws IllegalArgumentException
   *             if <tt>A.rows() < A.columns()</tt>.
   */
  def checkRectangular(A: IntMatrix2D) {
    if (A.rows() < A.columns()) {
      throw new IllegalArgumentException("Matrix must be rectangular: " + AbstractFormatter.shape(A))
    }
  }

  /**
   * Checks whether the given matrix <tt>A</tt> is <i>square</i>.
   *
   * @throws IllegalArgumentException
   *             if <tt>A.rows() != A.columns()</tt>.
   */
  def checkSquare(A: IntMatrix2D) {
    if (A.rows() != A.columns()) throw new IllegalArgumentException("Matrix must be square: " + AbstractFormatter.shape(A))
  }

  /**
   * Returns the matrix's fraction of non-zero cells;
   * <tt>A.cardinality() / A.size()</tt>.
   */
  def density(A: IntMatrix2D): Int = A.cardinality() / A.size.toInt

  /**
   * Returns whether all cells of the given matrix <tt>A</tt> are equal to the
   * given value. The result is <tt>true</tt> if and only if
   * <tt>A != null</tt> and <tt>! (Math.abs(value - A[i]) > tolerance())</tt>
   * holds for all coordinates.
   *
   * @param A
   *            the first matrix to compare.
   * @param value
   *            the value to compare against.
   * @return <tt>true</tt> if the matrix is equal to the value; <tt>false</tt>
   *         otherwise.
   */
  def equals(A: IntMatrix1D, value: Int): Boolean = {
    if (A == null) return false
    val size = A.size.toInt
    var result = false
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_1D)) {
      nthreads = Math.min(nthreads, size)
      val futures = Array.ofDim[Future](nthreads)
      val results = Array.ofDim[Boolean](nthreads)
      val k = size / nthreads
      for (j <- 0 until nthreads) {
        val firstIdx = j * k
        val lastIdx = if ((j == nthreads - 1)) size else firstIdx + k
        futures(j) = ConcurrencyUtils.submit(new Callable[Boolean]() {

          def call(): java.lang.Boolean = {
            (firstIdx until lastIdx).find(!(A.getQuick(_) == value))
              .map(false)
              .getOrElse(true)
          }
        })
      }
      try {
        for (j <- 0 until nthreads) {
          results(j) = futures(j).get.asInstanceOf[java.lang.Boolean]
        }
        result = results(0).booleanValue()
        for (j <- 1 until nthreads) {
          result = result && results(j).booleanValue()
        }
      } catch {
        case ex: ExecutionException => ex.printStackTrace()
        case e: InterruptedException => e.printStackTrace()
      }
      result
    } else {
      (0 until size).find(!(A.getQuick(_) == value)).map(false)
        .getOrElse(true)
    }
  }

  /**
   * Returns whether both given matrices <tt>A</tt> and <tt>B</tt> are equal.
   * The result is <tt>true</tt> if <tt>A==B</tt>. Otherwise, the result is
   * <tt>true</tt> if and only if both arguments are <tt>!= null</tt>, have
   * the same size and <tt>! (Math.abs(A[i] - B[i]) > tolerance())</tt> holds
   * for all indexes.
   *
   * @param A
   *            the first matrix to compare.
   * @param B
   *            the second matrix to compare.
   * @return <tt>true</tt> if both matrices are equal; <tt>false</tt>
   *         otherwise.
   */
  def equals(A: IntMatrix1D, B: IntMatrix1D): Boolean = {
    if (A == B) return true
    if (!(A != null && B != null)) return false
    val size = A.size.toInt
    if (size != B.size) return false
    var result = false
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size >= ConcurrencyUtils.getThreadsBeginN_1D)) {
      nthreads = Math.min(nthreads, size)
      val futures = Array.ofDim[Future](nthreads)
      val results = Array.ofDim[Boolean](nthreads)
      val k = size / nthreads
      for (j <- 0 until nthreads) {
        val firstIdx = j * k
        val lastIdx = if ((j == nthreads - 1)) size else firstIdx + k
        futures(j) = ConcurrencyUtils.submit(new Callable[Boolean]() {

          def call(): java.lang.Boolean = {
            (firstIdx until lastIdx).find(!(A.getQuick(_) == B.getQuick(_)))
              .map(false)
              .getOrElse(true)
          }
        })
      }
      try {
        for (j <- 0 until nthreads) {
          results(j) = futures(j).get.asInstanceOf[java.lang.Boolean]
        }
        result = results(0).booleanValue()
        for (j <- 1 until nthreads) {
          result = result && results(j).booleanValue()
        }
      } catch {
        case ex: ExecutionException => ex.printStackTrace()
        case e: InterruptedException => e.printStackTrace()
      }
      result
    } else {
      (0 until size).find(!(A.getQuick(_) == B.getQuick(_)))
        .map(false)
        .getOrElse(true)
    }
  }

  /**
   * Returns whether all cells of the given matrix <tt>A</tt> are equal to the
   * given value. The result is <tt>true</tt> if and only if
   * <tt>A != null</tt> and
   * <tt>! (Math.abs(value - A[row,col]) > tolerance())</tt> holds for all
   * coordinates.
   *
   * @param A
   *            the first matrix to compare.
   * @param value
   *            the value to compare against.
   * @return <tt>true</tt> if the matrix is equal to the value; <tt>false</tt>
   *         otherwise.
   */
  def equals(A: IntMatrix2D, value: Int): Boolean = {
    if (A == null) return false
    val rows = A.rows()
    val columns = A.columns()
    var result = false
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (A.size >= ConcurrencyUtils.getThreadsBeginN_2D)) {
      nthreads = Math.min(nthreads, A.rows())
      val futures = Array.ofDim[Future](nthreads)
      val results = Array.ofDim[Boolean](nthreads)
      val k = A.rows() / nthreads
      for (j <- 0 until nthreads) {
        val firstRow = j * k
        val lastRow = if ((j == nthreads - 1)) A.rows() else firstRow + k
        futures(j) = ConcurrencyUtils.submit(new Callable[Boolean]() {

          def call(): java.lang.Boolean = {
            for (r <- firstRow until lastRow; c <- 0 until columns if !(A.getQuick(r, c) == value)) return false
            return true
          }
        })
      }
      try {
        for (j <- 0 until nthreads) {
          results(j) = futures(j).get.asInstanceOf[java.lang.Boolean]
        }
        result = results(0).booleanValue()
        for (j <- 1 until nthreads) {
          result = result && results(j).booleanValue()
        }
      } catch {
        case ex: ExecutionException => ex.printStackTrace()
        case e: InterruptedException => e.printStackTrace()
      }
      result
    } else {
      for (r <- 0 until rows; c <- 0 until columns if !(A.getQuick(r, c) == value)) return false
      true
    }
  }

  /**
   * Returns whether both given matrices <tt>A</tt> and <tt>B</tt> are equal.
   * The result is <tt>true</tt> if <tt>A==B</tt>. Otherwise, the result is
   * <tt>true</tt> if and only if both arguments are <tt>!= null</tt>, have
   * the same number of columns and rows and
   * <tt>! (Math.abs(A[row,col] - B[row,col]) > tolerance())</tt> holds for
   * all coordinates.
   *
   * @param A
   *            the first matrix to compare.
   * @param B
   *            the second matrix to compare.
   * @return <tt>true</tt> if both matrices are equal; <tt>false</tt>
   *         otherwise.
   */
  def equals(A: IntMatrix2D, B: IntMatrix2D): Boolean = {
    if (A == B) return true
    if (!(A != null && B != null)) return false
    val rows = A.rows()
    val columns = A.columns()
    if (columns != B.columns() || rows != B.rows()) return false
    var result = false
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (A.size >= ConcurrencyUtils.getThreadsBeginN_2D)) {
      nthreads = Math.min(nthreads, A.rows())
      val futures = Array.ofDim[Future](nthreads)
      val results = Array.ofDim[Boolean](nthreads)
      val k = A.rows() / nthreads
      for (j <- 0 until nthreads) {
        val firstRow = j * k
        val lastRow = if ((j == nthreads - 1)) A.rows() else firstRow + k
        futures(j) = ConcurrencyUtils.submit(new Callable[Boolean]() {

          def call(): java.lang.Boolean = {
            for (r <- firstRow until lastRow; c <- 0 until columns if !(A.getQuick(r, c) == B.getQuick(r, 
              c))) return false
            return true
          }
        })
      }
      try {
        for (j <- 0 until nthreads) {
          results(j) = futures(j).get.asInstanceOf[java.lang.Boolean]
        }
        result = results(0).booleanValue()
        for (j <- 1 until nthreads) {
          result = result && results(j).booleanValue()
        }
      } catch {
        case ex: ExecutionException => ex.printStackTrace()
        case e: InterruptedException => e.printStackTrace()
      }
      result
    } else {
      for (r <- 0 until rows; c <- 0 until columns if !(A.getQuick(r, c) == B.getQuick(r, c))) return false
      true
    }
  }

  /**
   * Returns whether all cells of the given matrix <tt>A</tt> are equal to the
   * given value. The result is <tt>true</tt> if and only if
   * <tt>A != null</tt> and
   * <tt>! (Math.abs(value - A[slice,row,col]) > tolerance())</tt> holds for
   * all coordinates.
   *
   * @param A
   *            the first matrix to compare.
   * @param value
   *            the value to compare against.
   * @return <tt>true</tt> if the matrix is equal to the value; <tt>false</tt>
   *         otherwise.
   */
  def equals(A: IntMatrix3D, value: Int): Boolean = {
    if (A == null) return false
    val slices = A.slices()
    val rows = A.rows()
    val columns = A.columns()
    var result = false
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (A.size >= ConcurrencyUtils.getThreadsBeginN_3D)) {
      nthreads = Math.min(nthreads, slices)
      val futures = Array.ofDim[Future](nthreads)
      val results = Array.ofDim[Boolean](nthreads)
      val k = slices / nthreads
      for (j <- 0 until nthreads) {
        val startslice = j * k
        var stopslice: Int = 0
        stopslice = if (j == nthreads - 1) slices else startslice + k
        futures(j) = ConcurrencyUtils.submit(new Callable[Boolean]() {

          def call(): java.lang.Boolean = {
            for (s <- startslice until stopslice; r <- 0 until rows; c <- 0 until columns if !(A.getQuick(s, 
              r, c) == value)) return false
            return true
          }
        })
      }
      try {
        for (j <- 0 until nthreads) {
          results(j) = futures(j).get.asInstanceOf[java.lang.Boolean]
        }
        result = results(0).booleanValue()
        for (j <- 1 until nthreads) {
          result = result && results(j).booleanValue()
        }
      } catch {
        case ex: ExecutionException => ex.printStackTrace()
        case e: InterruptedException => e.printStackTrace()
      }
      result
    } else {
      for (s <- 0 until slices; r <- 0 until rows; c <- 0 until columns if !(A.getQuick(s, r, c) == value)) return false
      true
    }
  }

  /**
   * Returns whether both given matrices <tt>A</tt> and <tt>B</tt> are equal.
   * The result is <tt>true</tt> if <tt>A==B</tt>. Otherwise, the result is
   * <tt>true</tt> if and only if both arguments are <tt>!= null</tt>, have
   * the same number of columns, rows and slices, and
   * <tt>! (Math.abs(A[slice,row,col] - B[slice,row,col]) > tolerance())</tt>
   * holds for all coordinates.
   *
   * @param A
   *            the first matrix to compare.
   * @param B
   *            the second matrix to compare.
   * @return <tt>true</tt> if both matrices are equal; <tt>false</tt>
   *         otherwise.
   */
  def equals(A: IntMatrix3D, B: IntMatrix3D): Boolean = {
    if (A == B) return true
    if (!(A != null && B != null)) return false
    val slices = A.slices()
    val rows = A.rows()
    val columns = A.columns()
    if (columns != B.columns() || rows != B.rows() || slices != B.slices()) return false
    var result = false
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (A.size >= ConcurrencyUtils.getThreadsBeginN_3D)) {
      nthreads = Math.min(nthreads, slices)
      val futures = Array.ofDim[Future](nthreads)
      val results = Array.ofDim[Boolean](nthreads)
      val k = slices / nthreads
      for (j <- 0 until nthreads) {
        val startslice = j * k
        var stopslice: Int = 0
        stopslice = if (j == nthreads - 1) slices else startslice + k
        futures(j) = ConcurrencyUtils.submit(new Callable[Boolean]() {

          def call(): java.lang.Boolean = {
            for (s <- startslice until stopslice; r <- 0 until rows; c <- 0 until columns if !(A.getQuick(s, 
              r, c) == B.getQuick(s, r, c))) return false
            return true
          }
        })
      }
      try {
        for (j <- 0 until nthreads) {
          results(j) = futures(j).get.asInstanceOf[java.lang.Boolean]
        }
        result = results(0).booleanValue()
        for (j <- 1 until nthreads) {
          result = result && results(j).booleanValue()
        }
      } catch {
        case ex: ExecutionException => ex.printStackTrace()
        case e: InterruptedException => e.printStackTrace()
      }
      result
    } else {
      for (s <- 0 until slices; r <- 0 until rows; c <- 0 until columns if !(A.getQuick(s, r, c) == B.getQuick(s, 
        r, c))) return false
      true
    }
  }

  /**
   * Modifies the given matrix square matrix <tt>A</tt> such that it is
   * diagonally dominant by row and column, hence non-singular, hence
   * invertible. For testing purposes only.
   *
   * @param A
   *            the square matrix to modify.
   * @throws IllegalArgumentException
   *             if <tt>!isSquare(A)</tt>.
   */
  def generateNonSingular(A: IntMatrix2D) {
    checkSquare(A)
    val F = cern.jet.math.tint.IntFunctions.intFunctions
    val min = Math.min(A.rows(), A.columns())
    var i = min
    while (i >= 0) {
      A.setQuick(i, i, 0)
    }
    var i = min
    while (i >= 0) {
      val rowSum = A.viewRow(i).aggregate(IntFunctions.plus, IntFunctions.abs)
      val colSum = A.viewColumn(i).aggregate(IntFunctions.plus, IntFunctions.abs)
      A.setQuick(i, i, Math.max(rowSum, colSum) + i + 1)
    }
  }

  /**
   * A matrix <tt>A</tt> is <i>diagonal</i> if <tt>A[i,j] == 0</tt> whenever
   * <tt>i != j</tt>. Matrix may but need not be square.
   */
  def isDiagonal(A: IntMatrix2D): Boolean = {
    val rows = A.rows()
    val columns = A.columns()
    var row = rows
    while (row >= 0) {
      var column = columns
      while (column >= 0) {
        if (row != column && A.getQuick(row, column) != 0) return false
      }
    }
    true
  }

  /**
   * A matrix <tt>A</tt> is <i>diagonally dominant by column</i> if the
   * absolute value of each diagonal element is larger than the sum of the
   * absolute values of the off-diagonal elements in the corresponding column.
   *
   * <tt>returns true if for all i: abs(A[i,i]) &gt; Sum(abs(A[j,i])); j != i.</tt>
   * Matrix may but need not be square.
   * <p>
   * Note: Ignores tolerance.
   */
  def isDiagonallyDominantByColumn(A: IntMatrix2D): Boolean = {
    val F = cern.jet.math.tint.IntFunctions.intFunctions
    val min = Math.min(A.rows(), A.columns())
    var i = min
    while (i >= 0) {
      var diag = Math.abs(A.getQuick(i, i))
      diag += diag
      if (diag <= 
        A.viewColumn(i).aggregate(IntFunctions.plus, IntFunctions.abs)) return false
    }
    true
  }

  /**
   * A matrix <tt>A</tt> is <i>diagonally dominant by row</i> if the absolute
   * value of each diagonal element is larger than the sum of the absolute
   * values of the off-diagonal elements in the corresponding row.
   * <tt>returns true if for all i: abs(A[i,i]) &gt; Sum(abs(A[i,j])); j != i.</tt>
   * Matrix may but need not be square.
   * <p>
   * Note: Ignores tolerance.
   */
  def isDiagonallyDominantByRow(A: IntMatrix2D): Boolean = {
    val F = cern.jet.math.tint.IntFunctions.intFunctions
    val min = Math.min(A.rows(), A.columns())
    var i = min
    while (i >= 0) {
      var diag = Math.abs(A.getQuick(i, i))
      diag += diag
      if (diag <= 
        A.viewRow(i).aggregate(IntFunctions.plus, IntFunctions.abs)) return false
    }
    true
  }

  /**
   * A matrix <tt>A</tt> is an <i>identity</i> matrix if <tt>A[i,i] == 1</tt>
   * and all other cells are zero. Matrix may but need not be square.
   */
  def isIdentity(A: IntMatrix2D): Boolean = {
    val rows = A.rows()
    val columns = A.columns()
    var row = rows
    while (row >= 0) {
      var column = columns
      while (column >= 0) {
        val v = A.getQuick(row, column)
        if (row == column) {
          if (v != 1) return false
        } else if (v != 0) return false
      }
    }
    true
  }

  /**
   * A matrix <tt>A</tt> is <i>lower bidiagonal</i> if <tt>A[i,j]==0</tt>
   * unless <tt>i==j || i==j+1</tt>. Matrix may but need not be square.
   */
  def isLowerBidiagonal(A: IntMatrix2D): Boolean = {
    val rows = A.rows()
    val columns = A.columns()
    var row = rows
    while (row >= 0) {
      var column = columns
      while (column >= 0) {
        if (!(row == column || row == column + 1)) {
          if (A.getQuick(row, column) != 0) return false
        }
      }
    }
    true
  }

  /**
   * A matrix <tt>A</tt> is <i>lower triangular</i> if <tt>A[i,j]==0</tt>
   * whenever <tt>i &lt; j</tt>. Matrix may but need not be square.
   */
  def isLowerTriangular(A: IntMatrix2D): Boolean = {
    val rows = A.rows()
    val columns = A.columns()
    var column = columns
    while (column >= 0) {
      var row = Math.min(column, rows)
      while (row >= 0) {
        if (A.getQuick(row, column) != 0) return false
      }
    }
    true
  }

  /**
   * A matrix <tt>A</tt> is <i>non-negative</i> if <tt>A[i,j] &gt;= 0</tt>
   * holds for all cells.
   * <p>
   * Note: Ignores tolerance.
   */
  def isNonNegative(A: IntMatrix2D): Boolean = {
    val rows = A.rows()
    val columns = A.columns()
    var row = rows
    while (row >= 0) {
      var column = columns
      while (column >= 0) {
        if (!(A.getQuick(row, column) >= 0)) return false
      }
    }
    true
  }

  /**
   * A square matrix <tt>A</tt> is <i>orthogonal</i> if
   * <tt>A*transpose(A) = I</tt>.
   *
   * @throws IllegalArgumentException
   *             if <tt>!isSquare(A)</tt>.
   */
  def isOrthogonal(A: IntMatrix2D): Boolean = {
    checkSquare(A)
    ==(A.zMult(A, null, 1, 0, false, true), cern.colt.matrix.tint.IntFactory2D.dense.identity(A.rows()))
  }

  /**
   * A matrix <tt>A</tt> is <i>positive</i> if <tt>A[i,j] &gt; 0</tt> holds
   * for all cells.
   * <p>
   * Note: Ignores tolerance.
   */
  def isPositive(A: IntMatrix2D): Boolean = {
    val rows = A.rows()
    val columns = A.columns()
    var row = rows
    while (row >= 0) {
      var column = columns
      while (column >= 0) {
        if (!(A.getQuick(row, column) > 0)) return false
      }
    }
    true
  }

  /**
   * A square matrix <tt>A</tt> is <i>skew-symmetric</i> if
   * <tt>A = -transpose(A)</tt>, that is <tt>A[i,j] == -A[j,i]</tt>.
   *
   * @throws IllegalArgumentException
   *             if <tt>!isSquare(A)</tt>.
   */
  def isSkewSymmetric(A: IntMatrix2D): Boolean = {
    checkSquare(A)
    val rows = A.rows()
    val columns = A.columns()
    var row = rows
    while (row >= 0) {
      var column = rows
      while (column >= 0) {
        if (A.getQuick(row, column) != -A.getQuick(column, row)) return false
      }
    }
    true
  }

  /**
   * A matrix <tt>A</tt> is <i>square</i> if it has the same number of rows
   * and columns.
   */
  def isSquare(A: IntMatrix2D): Boolean = A.rows() == A.columns()

  /**
   * A matrix <tt>A</tt> is <i>strictly lower triangular</i> if
   * <tt>A[i,j]==0</tt> whenever <tt>i &lt;= j</tt>. Matrix may but need not
   * be square.
   */
  def isStrictlyLowerTriangular(A: IntMatrix2D): Boolean = {
    val rows = A.rows()
    val columns = A.columns()
    var column = columns
    while (column >= 0) {
      var row = Math.min(rows, column + 1)
      while (row >= 0) {
        if (A.getQuick(row, column) != 0) return false
      }
    }
    true
  }

  /**
   * A matrix <tt>A</tt> is <i>strictly triangular</i> if it is triangular and
   * its diagonal elements all equal 0. Matrix may but need not be square.
   */
  def isStrictlyTriangular(A: IntMatrix2D): Boolean = {
    if (!isTriangular(A)) return false
    var i = Math.min(A.rows(), A.columns())
    while (i >= 0) {
      if (A.getQuick(i, i) != 0) return false
    }
    true
  }

  /**
   * A matrix <tt>A</tt> is <i>strictly upper triangular</i> if
   * <tt>A[i,j]==0</tt> whenever <tt>i &gt;= j</tt>. Matrix may but need not
   * be square.
   */
  def isStrictlyUpperTriangular(A: IntMatrix2D): Boolean = {
    val rows = A.rows()
    val columns = A.columns()
    var column = columns
    while (column >= 0) {
      var row = rows
      while (row >= column) {
        if (A.getQuick(row, column) != 0) return false
      }
    }
    true
  }

  /**
   * A matrix <tt>A</tt> is <i>symmetric</i> if <tt>A = tranpose(A)</tt>, that
   * is <tt>A[i,j] == A[j,i]</tt>.
   *
   * @throws IllegalArgumentException
   *             if <tt>!isSquare(A)</tt>.
   */
  def isSymmetric(A: IntMatrix2D): Boolean = {
    checkSquare(A)
    ==(A, A.viewDice())
  }

  /**
   * A matrix <tt>A</tt> is <i>triangular</i> iff it is either upper or lower
   * triangular. Matrix may but need not be square.
   */
  def isTriangular(A: IntMatrix2D): Boolean = {
    isLowerTriangular(A) || isUpperTriangular(A)
  }

  /**
   * A matrix <tt>A</tt> is <i>tridiagonal</i> if <tt>A[i,j]==0</tt> whenever
   * <tt>Math.abs(i-j) > 1</tt>. Matrix may but need not be square.
   */
  def isTridiagonal(A: IntMatrix2D): Boolean = {
    val rows = A.rows()
    val columns = A.columns()
    var row = rows
    while (row >= 0) {
      var column = columns
      while (column >= 0) {
        if (Math.abs(row - column) > 1) {
          if (A.getQuick(row, column) != 0) return false
        }
      }
    }
    true
  }

  /**
   * A matrix <tt>A</tt> is <i>unit triangular</i> if it is triangular and its
   * diagonal elements all equal 1. Matrix may but need not be square.
   */
  def isUnitTriangular(A: IntMatrix2D): Boolean = {
    if (!isTriangular(A)) return false
    var i = Math.min(A.rows(), A.columns())
    while (i >= 0) {
      if (A.getQuick(i, i) != 1) return false
    }
    true
  }

  /**
   * A matrix <tt>A</tt> is <i>upper bidiagonal</i> if <tt>A[i,j]==0</tt>
   * unless <tt>i==j || i==j-1</tt>. Matrix may but need not be square.
   */
  def isUpperBidiagonal(A: IntMatrix2D): Boolean = {
    val rows = A.rows()
    val columns = A.columns()
    var row = rows
    while (row >= 0) {
      var column = columns
      while (column >= 0) {
        if (!(row == column || row == column - 1)) {
          if (A.getQuick(row, column) != 0) return false
        }
      }
    }
    true
  }

  /**
   * A matrix <tt>A</tt> is <i>upper triangular</i> if <tt>A[i,j]==0</tt>
   * whenever <tt>i &gt; j</tt>. Matrix may but need not be square.
   */
  def isUpperTriangular(A: IntMatrix2D): Boolean = {
    val rows = A.rows()
    val columns = A.columns()
    var column = columns
    while (column >= 0) {
      var row = rows
      while (row > column) {
        if (A.getQuick(row, column) != 0) return false
      }
    }
    true
  }

  /**
   * A matrix <tt>A</tt> is <i>zero</i> if all its cells are zero.
   */
  def isZero(A: IntMatrix2D): Boolean = ==(A, 0)

  /**
   * The <i>lower bandwidth</i> of a square matrix <tt>A</tt> is the maximum
   * <tt>i-j</tt> for which <tt>A[i,j]</tt> is nonzero and <tt>i &gt; j</tt>.
   * A <i>banded</i> matrix has a "band" about the diagonal. Diagonal,
   * tridiagonal and triangular matrices are special cases.
   *
   * @param A
   *            the square matrix to analyze.
   * @return the lower bandwith.
   * @throws IllegalArgumentException
   *             if <tt>!isSquare(A)</tt>.
   * @see #semiBandwidth(IntMatrix2D)
   * @see #upperBandwidth(IntMatrix2D)
   */
  def lowerBandwidth(A: IntMatrix2D): Int = {
    checkSquare(A)
    val rows = A.rows()
    var k = rows
    while (k >= 0) {
      var i = rows - k
      while (i >= 0) {
        val j = i + k
        if (A.getQuick(j, i) != 0) return k
      }
    }
    0
  }

  /**
   * Returns the <i>semi-bandwidth</i> of the given square matrix <tt>A</tt>.
   * A <i>banded</i> matrix has a "band" about the diagonal. It is a matrix
   * with all cells equal to zero, with the possible exception of the cells
   * along the diagonal line, the <tt>k</tt> diagonal lines above the
   * diagonal, and the <tt>k</tt> diagonal lines below the diagonal. The
   * <i>semi-bandwith l</i> is the number <tt>k+1</tt>. The <i>bandwidth p</i>
   * is the number <tt>2*k + 1</tt>. For example, a tridiagonal matrix
   * corresponds to <tt>k=1, l=2, p=3</tt>, a diagonal or zero matrix
   * corresponds to <tt>k=0, l=1, p=1</tt>,
   * <p>
   * The <i>upper bandwidth</i> is the maximum <tt>j-i</tt> for which
   * <tt>A[i,j]</tt> is nonzero and <tt>j &gt; i</tt>. The <i>lower
   * bandwidth</i> is the maximum <tt>i-j</tt> for which <tt>A[i,j]</tt> is
   * nonzero and <tt>i &gt; j</tt>. Diagonal, tridiagonal and triangular
   * matrices are special cases.
   * <p>
   * Examples:
   * <table border="1" cellspacing="0">
   * <tr align="left" valign="top">
   * <td valign="middle" align="left"><tt>matrix</tt></td>
   * <td> <tt>4&nbsp;x&nbsp;4&nbsp;<br>
   0&nbsp;0&nbsp;0&nbsp;0<br>
   0&nbsp;0&nbsp;0&nbsp;0<br>
   0&nbsp;0&nbsp;0&nbsp;0<br>
   0&nbsp;0&nbsp;0&nbsp;0 </tt></td>
   * <td><tt>4&nbsp;x&nbsp;4<br>
   1&nbsp;0&nbsp;0&nbsp;0<br>
   0&nbsp;0&nbsp;0&nbsp;0<br>
   0&nbsp;0&nbsp;0&nbsp;0<br>
   0&nbsp;0&nbsp;0&nbsp;1 </tt></td>
   * <td><tt>4&nbsp;x&nbsp;4<br>
   1&nbsp;1&nbsp;0&nbsp;0<br>
   1&nbsp;1&nbsp;1&nbsp;0<br>
   0&nbsp;1&nbsp;1&nbsp;1<br>
   0&nbsp;0&nbsp;1&nbsp;1 </tt></td>
   * <td><tt> 4&nbsp;x&nbsp;4<br>
   0&nbsp;1&nbsp;1&nbsp;1<br>
   0&nbsp;1&nbsp;1&nbsp;1<br>
   0&nbsp;0&nbsp;0&nbsp;1<br>
   0&nbsp;0&nbsp;0&nbsp;1 </tt></td>
   * <td><tt> 4&nbsp;x&nbsp;4<br>
   0&nbsp;0&nbsp;0&nbsp;0<br>
   1&nbsp;1&nbsp;0&nbsp;0<br>
   1&nbsp;1&nbsp;0&nbsp;0<br>
   1&nbsp;1&nbsp;1&nbsp;1 </tt></td>
   * <td><tt>4&nbsp;x&nbsp;4<br>
   1&nbsp;1&nbsp;0&nbsp;0<br>
   0&nbsp;1&nbsp;1&nbsp;0<br>
   0&nbsp;1&nbsp;0&nbsp;1<br>
   1&nbsp;0&nbsp;1&nbsp;1 </tt><tt> </tt></td>
   * <td><tt>4&nbsp;x&nbsp;4<br>
   1&nbsp;1&nbsp;1&nbsp;0<br>
   0&nbsp;1&nbsp;0&nbsp;0<br>
   1&nbsp;1&nbsp;0&nbsp;1<br>
   0&nbsp;0&nbsp;1&nbsp;1 </tt></td>
   * </tr>
   * <tr align="center" valign="middle">
   * <td><tt>upperBandwidth</tt></td>
   * <td><div align="center"><tt>0</tt></div></td>
   * <td><div align="center"><tt>0</tt></div></td>
   * <td><div align="center"><tt>1</tt></div></td>
   * <td><tt>3</tt></td>
   * <td align="center" valign="middle"><tt>0</tt></td>
   * <td align="center" valign="middle"><div align="center"><tt>1</tt></div></td>
   * <td align="center" valign="middle"><div align="center"><tt>2</tt></div></td>
   * </tr>
   * <tr align="center" valign="middle">
   * <td><tt>lowerBandwidth</tt></td>
   * <td><div align="center"><tt>0</tt></div></td>
   * <td><div align="center"><tt>0</tt></div></td>
   * <td><div align="center"><tt>1</tt></div></td>
   * <td><tt>0</tt></td>
   * <td align="center" valign="middle"><tt>3</tt></td>
   * <td align="center" valign="middle"><div align="center"><tt>3</tt></div></td>
   * <td align="center" valign="middle"><div align="center"><tt>2</tt></div></td>
   * </tr>
   * <tr align="center" valign="middle">
   * <td><tt>semiBandwidth</tt></td>
   * <td><div align="center"><tt>1</tt></div></td>
   * <td><div align="center"><tt>1</tt></div></td>
   * <td><div align="center"><tt>2</tt></div></td>
   * <td><tt>4</tt></td>
   * <td align="center" valign="middle"><tt>4</tt></td>
   * <td align="center" valign="middle"><div align="center"><tt>4</tt></div></td>
   * <td align="center" valign="middle"><div align="center"><tt>3</tt></div></td>
   * </tr>
   * <tr align="center" valign="middle">
   * <td><tt>description</tt></td>
   * <td><div align="center"><tt>zero</tt></div></td>
   * <td><div align="center"><tt>diagonal</tt></div></td>
   * <td><div align="center"><tt>tridiagonal</tt></div></td>
   * <td><tt>upper triangular</tt></td>
   * <td align="center" valign="middle"><tt>lower triangular</tt></td>
   * <td align="center" valign="middle"><div align="center">
   * <tt>unstructured</tt></div></td>
   * <td align="center" valign="middle"><div align="center">
   * <tt>unstructured</tt></div></td>
   * </tr>
   * </table>
   *
   * @param A
   *            the square matrix to analyze.
   * @return the semi-bandwith <tt>l</tt>.
   * @throws IllegalArgumentException
   *             if <tt>!isSquare(A)</tt>.
   * @see #lowerBandwidth(IntMatrix2D)
   * @see #upperBandwidth(IntMatrix2D)
   */
  def semiBandwidth(A: IntMatrix2D): Int = {
    checkSquare(A)
    val rows = A.rows()
    var k = rows
    while (k >= 0) {
      var i = rows - k
      while (i >= 0) {
        val j = i + k
        if (A.getQuick(j, i) != 0) return k + 1
        if (A.getQuick(i, j) != 0) return k + 1
      }
    }
    1
  }

  /**
   * Returns summary information about the given matrix <tt>A</tt>. That is a
   * String with (propertyName, propertyValue) pairs. Useful for debugging or
   * to quickly get the rough picture of a matrix. For example,
   *
   * <pre>
   *   density                      : 0.9
   *   isDiagonal                   : false
   *   isDiagonallyDominantByRow    : false
   *   isDiagonallyDominantByColumn : false
   *   isIdentity                   : false
   *   isLowerBidiagonal            : false
   *   isLowerTriangular            : false
   *   isNonNegative                : true
   *   isOrthogonal                 : Illegal operation or error: Matrix must be square.
   *   isPositive                   : true
   *   isSingular                   : Illegal operation or error: Matrix must be square.
   *   isSkewSymmetric              : Illegal operation or error: Matrix must be square.
   *   isSquare                     : false
   *   isStrictlyLowerTriangular    : false
   *   isStrictlyTriangular         : false
   *   isStrictlyUpperTriangular    : false
   *   isSymmetric                  : Illegal operation or error: Matrix must be square.
   *   isTriangular                 : false
   *   isTridiagonal                : false
   *   isUnitTriangular             : false
   *   isUpperBidiagonal            : false
   *   isUpperTriangular            : false
   *   isZero                       : false
   *   lowerBandwidth               : Illegal operation or error: Matrix must be square.
   *   semiBandwidth                : Illegal operation or error: Matrix must be square.
   *   upperBandwidth               : Illegal operation or error: Matrix must be square.
   *
   * </pre>
   */
  def toString(A: IntMatrix2D): String = {
    val names = new cern.colt.list.tobject.ObjectArrayList()
    val values = new cern.colt.list.tobject.ObjectArrayList()
    val unknown = "Illegal operation or error: "
    names.add("density")
    try {
      values.add(String.valueOf(density(A)))
    } catch {
      case exc: IllegalArgumentException => values.add(unknown + exc.getMessage)
    }
    names.add("isDiagonal")
    try {
      values.add(String.valueOf(isDiagonal(A)))
    } catch {
      case exc: IllegalArgumentException => values.add(unknown + exc.getMessage)
    }
    names.add("isDiagonallyDominantByRow")
    try {
      values.add(String.valueOf(isDiagonallyDominantByRow(A)))
    } catch {
      case exc: IllegalArgumentException => values.add(unknown + exc.getMessage)
    }
    names.add("isDiagonallyDominantByColumn")
    try {
      values.add(String.valueOf(isDiagonallyDominantByColumn(A)))
    } catch {
      case exc: IllegalArgumentException => values.add(unknown + exc.getMessage)
    }
    names.add("isIdentity")
    try {
      values.add(String.valueOf(isIdentity(A)))
    } catch {
      case exc: IllegalArgumentException => values.add(unknown + exc.getMessage)
    }
    names.add("isLowerBidiagonal")
    try {
      values.add(String.valueOf(isLowerBidiagonal(A)))
    } catch {
      case exc: IllegalArgumentException => values.add(unknown + exc.getMessage)
    }
    names.add("isLowerTriangular")
    try {
      values.add(String.valueOf(isLowerTriangular(A)))
    } catch {
      case exc: IllegalArgumentException => values.add(unknown + exc.getMessage)
    }
    names.add("isNonNegative")
    try {
      values.add(String.valueOf(isNonNegative(A)))
    } catch {
      case exc: IllegalArgumentException => values.add(unknown + exc.getMessage)
    }
    names.add("isOrthogonal")
    try {
      values.add(String.valueOf(isOrthogonal(A)))
    } catch {
      case exc: IllegalArgumentException => values.add(unknown + exc.getMessage)
    }
    names.add("isPositive")
    try {
      values.add(String.valueOf(isPositive(A)))
    } catch {
      case exc: IllegalArgumentException => values.add(unknown + exc.getMessage)
    }
    names.add("isSkewSymmetric")
    try {
      values.add(String.valueOf(isSkewSymmetric(A)))
    } catch {
      case exc: IllegalArgumentException => values.add(unknown + exc.getMessage)
    }
    names.add("isSquare")
    try {
      values.add(String.valueOf(isSquare(A)))
    } catch {
      case exc: IllegalArgumentException => values.add(unknown + exc.getMessage)
    }
    names.add("isStrictlyLowerTriangular")
    try {
      values.add(String.valueOf(isStrictlyLowerTriangular(A)))
    } catch {
      case exc: IllegalArgumentException => values.add(unknown + exc.getMessage)
    }
    names.add("isStrictlyTriangular")
    try {
      values.add(String.valueOf(isStrictlyTriangular(A)))
    } catch {
      case exc: IllegalArgumentException => values.add(unknown + exc.getMessage)
    }
    names.add("isStrictlyUpperTriangular")
    try {
      values.add(String.valueOf(isStrictlyUpperTriangular(A)))
    } catch {
      case exc: IllegalArgumentException => values.add(unknown + exc.getMessage)
    }
    names.add("isSymmetric")
    try {
      values.add(String.valueOf(isSymmetric(A)))
    } catch {
      case exc: IllegalArgumentException => values.add(unknown + exc.getMessage)
    }
    names.add("isTriangular")
    try {
      values.add(String.valueOf(isTriangular(A)))
    } catch {
      case exc: IllegalArgumentException => values.add(unknown + exc.getMessage)
    }
    names.add("isTridiagonal")
    try {
      values.add(String.valueOf(isTridiagonal(A)))
    } catch {
      case exc: IllegalArgumentException => values.add(unknown + exc.getMessage)
    }
    names.add("isUnitTriangular")
    try {
      values.add(String.valueOf(isUnitTriangular(A)))
    } catch {
      case exc: IllegalArgumentException => values.add(unknown + exc.getMessage)
    }
    names.add("isUpperBidiagonal")
    try {
      values.add(String.valueOf(isUpperBidiagonal(A)))
    } catch {
      case exc: IllegalArgumentException => values.add(unknown + exc.getMessage)
    }
    names.add("isUpperTriangular")
    try {
      values.add(String.valueOf(isUpperTriangular(A)))
    } catch {
      case exc: IllegalArgumentException => values.add(unknown + exc.getMessage)
    }
    names.add("isZero")
    try {
      values.add(String.valueOf(isZero(A)))
    } catch {
      case exc: IllegalArgumentException => values.add(unknown + exc.getMessage)
    }
    names.add("lowerBandwidth")
    try {
      values.add(String.valueOf(lowerBandwidth(A)))
    } catch {
      case exc: IllegalArgumentException => values.add(unknown + exc.getMessage)
    }
    names.add("semiBandwidth")
    try {
      values.add(String.valueOf(semiBandwidth(A)))
    } catch {
      case exc: IllegalArgumentException => values.add(unknown + exc.getMessage)
    }
    names.add("upperBandwidth")
    try {
      values.add(String.valueOf(upperBandwidth(A)))
    } catch {
      case exc: IllegalArgumentException => values.add(unknown + exc.getMessage)
    }
    val comp = new cern.colt.function.tint.IntComparator() {

      def compare(a: Int, b: Int): Int = {
        return get(names, a).compareTo(get(names, b))
      }
    }
    val swapper = new cern.colt.Swapper() {

      def swap(a: Int, b: Int) {
        var tmp: AnyRef = null
        tmp = names.get(a)
        names.set(a, names.get(b))
        names.set(b, tmp)
        tmp = values.get(a)
        values.set(a, values.get(b))
        values.set(b, tmp)
      }
    }
    cern.colt.GenericSorting.quickSort(0, names.size, comp, swapper)
    var maxLength = 0
    for (i <- 0 until names.size) {
      val length = names.get(i).asInstanceOf[String].length
      maxLength = Math.max(length, maxLength)
    }
    val buf = new StringBuffer()
    for (i <- 0 until names.size) {
      val name = names.get(i).asInstanceOf[String]
      buf.append(name)
      buf.append(blanks(maxLength - name.length))
      buf.append(" : ")
      buf.append(values.get(i))
      if (i < names.size - 1) buf.append('\n')
    }
    buf.toString
  }

  /**
   * The <i>upper bandwidth</i> of a square matrix <tt>A</tt> is the maximum
   * <tt>j-i</tt> for which <tt>A[i,j]</tt> is nonzero and <tt>j &gt; i</tt>.
   * A <i>banded</i> matrix has a "band" about the diagonal. Diagonal,
   * tridiagonal and triangular matrices are special cases.
   *
   * @param A
   *            the square matrix to analyze.
   * @return the upper bandwith.
   * @throws IllegalArgumentException
   *             if <tt>!isSquare(A)</tt>.
   * @see #semiBandwidth(IntMatrix2D)
   * @see #lowerBandwidth(IntMatrix2D)
   */
  def upperBandwidth(A: IntMatrix2D): Int = {
    checkSquare(A)
    val rows = A.rows()
    var k = rows
    while (k >= 0) {
      var i = rows - k
      while (i >= 0) {
        val j = i + k
        if (A.getQuick(i, j) != 0) return k
      }
    }
    0
  }
}
