package cern.colt.matrix.tdouble.algo

import java.util.concurrent.Callable
import java.util.concurrent.ExecutionException
import java.util.concurrent.Future
import cern.colt.list.tdouble.DoubleArrayList
import cern.colt.list.tint.IntArrayList
import cern.colt.matrix.Norm
import cern.colt.matrix.tdouble.DoubleFactory2D
import cern.colt.matrix.tdouble.StrideMatrix1D
import cern.colt.matrix.tdouble.StrideMatrix2D
import cern.colt.matrix.tdouble.DoubleMatrix3D
import cern.colt.matrix.tdouble.algo.decomposition.DenseDoubleCholeskyDecomposition
import cern.colt.matrix.tdouble.algo.decomposition.DenseDoubleEigenvalueDecomposition
import cern.colt.matrix.tdouble.algo.decomposition.DenseDoubleLUDecomposition
import cern.colt.matrix.tdouble.algo.decomposition.DenseDoubleQRDecomposition
import cern.colt.matrix.tdouble.algo.decomposition.DenseDoubleSingularValueDecomposition
import cern.colt.matrix.tdouble.impl.DenseMatrix1D
import cern.colt.matrix.tdouble.impl.DenseMatrix2D
import cern.colt.matrix.tdouble.impl.DenseDoubleMatrix3D
import cern.colt.matrix.tdouble.impl.SparseCCDoubleMatrix2D
import cern.colt.matrix.tdouble.impl.SparseRCDoubleMatrix2D
import cern.colt.matrix.tint.IntMatrix1D
import cern.colt.matrix.tint.IntMatrix2D
import cern.colt.matrix.tint.impl.DenseIntMatrix1D
import cern.colt.matrix.tint.impl.DenseIntMatrix2D
import cern.jet.math.tdouble.DoubleFunctions
import cern.jet.math.tint.IntFunctions
import edu.emory.mathcs.utils.ConcurrencyUtils
import DenseDoubleAlgebra._
//remove if not needed
import scala.collection.JavaConversions._

object DenseDoubleAlgebra {

  /**
   * A default Algebra object; has {@link DoubleProperty#DEFAULT} attached for
   * tolerance. Allows ommiting to construct an Algebra object time and again.
   *
   * Note that this Algebra object is immutable. Any attempt to assign a new
   * Property object to it (via method <tt>setProperty</tt>), or to alter the
   * tolerance of its property object (via
   * <tt>property().setTolerance(...)</tt>) will throw an exception.
   */
  val DEFAULT = new DenseDoubleAlgebra()

  /**
   * A default Algebra object; has {@link DoubleProperty#ZERO} attached for
   * tolerance. Allows ommiting to construct an Algebra object time and again.
   *
   * Note that this Algebra object is immutable. Any attempt to assign a new
   * Property object to it (via method <tt>setProperty</tt>), or to alter the
   * tolerance of its property object (via
   * <tt>property().setTolerance(...)</tt>) will throw an exception.
   */
  val ZERO = new DenseDoubleAlgebra()

  DEFAULT.property = DoubleProperty.DEFAULT

  ZERO.property = DoubleProperty.ZERO

  /**
   * Returns sqrt(a^2 + b^2) without under/overflow.
   */
  def hypot(a: Double, b: Double): Double = {
    var r: Double = 0.0
    if (Math.abs(a) > Math.abs(b)) {
      r = b / a
      r = Math.abs(a) * Math.sqrt(1 + r * r)
    } else if (b != 0) {
      r = a / b
      r = Math.abs(b) * Math.sqrt(1 + r * r)
    } else {
      r = 0.0
    }
    r
  }

  /**
   * Returns sqrt(a^2 + b^2) without under/overflow.
   */
  def hypotFunction(): cern.colt.function.tdouble.DoubleDoubleFunction = {
    new cern.colt.function.tdouble.DoubleDoubleFunction() {

      def apply(a: Double, b: Double): Double = hypot(a, b)
    }
  }
}

/**
 * Linear algebraic matrix operations operating on dense matrices.
 *
 * @author wolfgang.hoschek@cern.ch
 * @version 1.0, 09/24/99
 *
 * @author Piotr Wendykier (piotr.wendykier@gmail.com)
 */
@SerialVersionUID(1L)
class DenseDoubleAlgebra(tolerance: Double) extends cern.colt.PersistentObject {

  /**
   * The property object attached to this instance.
   */
  protected var property: DoubleProperty = _

  setProperty(new DoubleProperty(tolerance))

  /**
   * Constructs a new instance with an equality tolerance given by
   * <tt>Property.DEFAULT.tolerance()</tt>.
   */
  def this() {
    this(DoubleProperty.DEFAULT.tolerance())
  }

  /**
   * Constructs and returns the cholesky-decomposition of the given matrix.
   */
  def chol(matrix: StrideMatrix2D): DenseDoubleCholeskyDecomposition = {
    new DenseDoubleCholeskyDecomposition(matrix)
  }

  /**
   * Returns a copy of the receiver. The attached property object is also
   * copied. Hence, the property object of the copy is mutable.
   *
   * @return a copy of the receiver.
   */
  def clone(): AnyRef = {
    new DenseDoubleAlgebra(property.tolerance())
  }

  /**
   * Returns the condition of matrix <tt>A</tt>, which is the ratio of largest
   * to smallest singular value.
   */
  def cond(A: StrideMatrix2D): Double = svd(A).cond()

  /**
   * Returns the determinant of matrix <tt>A</tt>.
   *
   * @return the determinant.
   */
  def det(A: StrideMatrix2D): Double = lu(A).det()

  /**
   * Constructs and returns the Eigenvalue-decomposition of the given matrix.
   */
  def eig(matrix: StrideMatrix2D): DenseDoubleEigenvalueDecomposition = {
    new DenseDoubleEigenvalueDecomposition(matrix)
  }

  /**
   * Returns the inverse or pseudo-inverse of matrix <tt>A</tt>.
   *
   * @return a new independent matrix; inverse(matrix) if the matrix is
   *         square, pseudoinverse otherwise.
   */
  def inverse(A: StrideMatrix2D): StrideMatrix2D = {
    if (property.isSquare(A) && property.isDiagonal(A)) {
      val inv = A.copy()
      var isNonSingular = true
      var i = inv.rows()
      while (i >= 0) {
        val v = inv.getQuick(i, i)
        isNonSingular &= (v != 0)
        inv.setQuick(i, i, 1 / v)
      }
      if (!isNonSingular) throw new IllegalArgumentException("A is singular.")
      return inv
    }
    solve(A, DoubleFactory2D.dense.identity(A.rows()))
  }

  /**
   * Constructs and returns the LU-decomposition of the given matrix.
   */
  def lu(matrix: StrideMatrix2D): DenseDoubleLUDecomposition = new DenseDoubleLUDecomposition(matrix)

  /**
   * Computes the Kronecker product of two real matrices.
   *
   * @param x
   * @param y
   * @return the Kronecker product of two real matrices
   */
  def kron(x: StrideMatrix1D, y: StrideMatrix1D): StrideMatrix1D = {
    val size_x = x.size.toInt
    val size_y = y.size.toInt
    val C = new DenseMatrix1D(size_x * size_y)
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (size_x >= ConcurrencyUtils.getThreadsBeginN_1D)) {
      ConcurrencyUtils.setThreadsBeginN_1D(Integer.MAX_VALUE)
      nthreads = Math.min(nthreads, size_x)
      val futures = Array.ofDim[Future](nthreads)
      val k = size_x / nthreads
      for (j <- 0 until nthreads) {
        val firstIdx = j * k
        val lastIdx = if ((j == nthreads - 1)) size_x else firstIdx + k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            for (i <- firstIdx until lastIdx) {
              C.viewPart(i * size_y, size_y).assign(y, DoubleFunctions.multSecond(x.getQuick(i)))
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
      ConcurrencyUtils.resetThreadsBeginN()
    } else {
      for (i <- 0 until size_x) {
        C.viewPart(i * size_y, size_y).assign(y, DoubleFunctions.multSecond(x.getQuick(i)))
      }
    }
    C
  }

  /**
   * Computes the Kronecker product of two real matrices.
   *
   * @param X
   * @param Y
   * @return the Kronecker product of two real matrices
   */
  def kron(X: StrideMatrix2D, Y: StrideMatrix2D): StrideMatrix2D = {
    val rows_x = X.rows()
    val columns_x = X.columns()
    val rows_y = Y.rows()
    val columns_y = Y.columns()
    if ((X.getClass.getName.indexOf("Dense", 0) != -1 && Y.getClass.getName.indexOf("Dense", 0) != -1)) {
      val C = new DenseMatrix2D(rows_x * rows_y, columns_x * columns_y)
      var nthreads = ConcurrencyUtils.getNumberOfThreads
      if ((nthreads > 1) && (X.size >= ConcurrencyUtils.getThreadsBeginN_2D)) {
        ConcurrencyUtils.setThreadsBeginN_1D(Integer.MAX_VALUE)
        nthreads = Math.min(nthreads, rows_x)
        val futures = Array.ofDim[Future](nthreads)
        val k = rows_x / nthreads
        for (j <- 0 until nthreads) {
          val firstRow = j * k
          val lastRow = if ((j == nthreads - 1)) rows_x else firstRow + k
          futures(j) = ConcurrencyUtils.submit(new Runnable() {

            def run() {
              for (r <- firstRow until lastRow; c <- 0 until columns_x) {
                C.viewPart(r * rows_y, c * columns_y, rows_y, columns_y)
                  .assign(Y, DoubleFunctions.multSecond(X.getQuick(r, c)))
              }
            }
          })
        }
        ConcurrencyUtils.waitForCompletion(futures)
        ConcurrencyUtils.resetThreadsBeginN()
      } else {
        for (r <- 0 until rows_x; c <- 0 until columns_x) {
          C.viewPart(r * rows_y, c * columns_y, rows_y, columns_y)
            .assign(Y, DoubleFunctions.multSecond(X.getQuick(r, c)))
        }
      }
      C
    } else {
      val iaList = new IntArrayList()
      val jaList = new IntArrayList()
      val saList = new DoubleArrayList()
      val ibList = new IntArrayList()
      val jbList = new IntArrayList()
      val sbList = new DoubleArrayList()
      X.getNonZeros(iaList, jaList, saList)
      Y.getNonZeros(ibList, jbList, sbList)
      iaList.trimToSize()
      jaList.trimToSize()
      saList.trimToSize()
      ibList.trimToSize()
      jbList.trimToSize()
      sbList.trimToSize()
      val ia = new DenseIntMatrix1D(iaList.elements())
      val ja = new DenseIntMatrix1D(jaList.elements())
      val sa = new DenseMatrix1D(saList.elements())
      val ib = new DenseIntMatrix1D(ibList.elements())
      val jb = new DenseIntMatrix1D(jbList.elements())
      val sb = new DenseMatrix1D(sbList.elements())
      ia.assign(IntFunctions.mult(rows_y))
      val ik = new DenseIntMatrix2D(sbList.size, ia.size.toInt)
      for (i <- 0 until sbList.size) {
        ik.viewRow(i).assign(ia).assign(IntFunctions.plus(ib.getQuick(i)))
      }
      ja.assign(IntFunctions.mult(columns_y))
      val jk = new DenseIntMatrix2D(sbList.size, ja.size.toInt)
      for (i <- 0 until sbList.size) {
        jk.viewRow(i).assign(ja).assign(IntFunctions.plus(jb.getQuick(i)))
      }
      val sk = multOuter(sa, sb, null)
      if (X.isInstanceOf[SparseCCDoubleMatrix2D] || Y.isInstanceOf[SparseCCDoubleMatrix2D]) {
        new SparseCCDoubleMatrix2D(rows_x * rows_y, columns_x * columns_y, ik.vectorize().elements().asInstanceOf[Array[Int]],
          jk.vectorize().elements().asInstanceOf[Array[Int]], sk.viewDice().vectorize().elements().asInstanceOf[Array[Double]],
          false, false, false)
      } else {
        new SparseRCDoubleMatrix2D(rows_x * rows_y, columns_x * columns_y, ik.vectorize().elements().asInstanceOf[Array[Int]],
          jk.vectorize().elements().asInstanceOf[Array[Int]], sk.viewDice().vectorize().elements().asInstanceOf[Array[Double]],
          false, false, false)
      }
    }
  }

  /**
   * Inner product of two vectors; <tt>Sum(x[i] * y[i])</tt>. Also known as
   * dot product. <br>
   * Equivalent to <tt>x.zDotProduct(y)</tt>.
   *
   * @param x
   *            the first source vector.
   * @param y
   *            the second source matrix.
   * @return the inner product.
   *
   * @throws IllegalArgumentException
   *             if <tt>x.size() != y.size()</tt>.
   */
  def mult(x: StrideMatrix1D, y: StrideMatrix1D): Double = x.zDotProduct(y)

  /**
   * Linear algebraic matrix-vector multiplication; <tt>z = A * y</tt>.
   * <tt>z[i] = Sum(A[i,j] * y[j]), i=0..A.rows()-1, j=0..y.size()-1</tt>.
   *
   * @param A
   *            the source matrix.
   * @param y
   *            the source vector.
   * @return <tt>z</tt>; a new vector with <tt>z.size()==A.rows()</tt>.
   *
   * @throws IllegalArgumentException
   *             if <tt>A.columns() != y.size()</tt>.
   */
  def mult(A: StrideMatrix2D, y: StrideMatrix1D): StrideMatrix1D = A.zMult(y, null)

  /**
   * Linear algebraic matrix-matrix multiplication; <tt>C = A x B</tt>.
   * <tt>C[i,j] = Sum(A[i,k] * B[k,j]), k=0..n-1</tt>. <br>
   * Matrix shapes: <tt>A(m x n), B(n x p), C(m x p)</tt>.
   *
   * @param A
   *            the first source matrix.
   * @param B
   *            the second source matrix.
   * @return <tt>C</tt>; a new matrix holding the results, with
   *         <tt>C.rows()=A.rows(), C.columns()==B.columns()</tt>.
   *
   * @throws IllegalArgumentException
   *             if <tt>B.rows() != A.columns()</tt>.
   */
  def mult(A: StrideMatrix2D, B: StrideMatrix2D): StrideMatrix2D = A.zMult(B, null)

  /**
   * Outer product of two vectors; Sets <tt>A[i,j] = x[i] * y[j]</tt>.
   *
   * @param x
   *            the first source vector.
   * @param y
   *            the second source vector.
   * @param A
   *            the matrix to hold the results. Set this parameter to
   *            <tt>null</tt> to indicate that a new result matrix shall be
   *            constructed.
   * @return A (for convenience only).
   * @throws IllegalArgumentException
   *             if <tt>A.rows() != x.size() || A.columns() != y.size()</tt>.
   */
  def multOuter(x: StrideMatrix1D, y: StrideMatrix1D, A: StrideMatrix2D): StrideMatrix2D = {
    val rows = x.size.toInt
    val columns = y.size.toInt
    var AA: StrideMatrix2D = null
    AA = if (A == null) x.like2D(rows, columns) else A
    if (AA.rows() != rows || AA.columns() != columns) throw new IllegalArgumentException()
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (rows >= ConcurrencyUtils.getThreadsBeginN_1D)) {
      ConcurrencyUtils.setThreadsBeginN_1D(Integer.MAX_VALUE)
      ConcurrencyUtils.setThreadsBeginN_1D(Integer.MAX_VALUE)
      nthreads = Math.min(nthreads, rows)
      val futures = Array.ofDim[Future](nthreads)
      val k = rows / nthreads
      for (j <- 0 until nthreads) {
        val firstRow = j * k
        val lastRow = if ((j == nthreads - 1)) rows else firstRow + k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            for (r <- firstRow until lastRow) {
              AA.viewRow(r).assign(y)
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
      ConcurrencyUtils.resetThreadsBeginN()
    } else {
      var r = rows
      while (r >= 0) {
        AA.viewRow(r).assign(y)
      }
    }
    if ((nthreads > 1) && (columns >= ConcurrencyUtils.getThreadsBeginN_1D)) {
      ConcurrencyUtils.setThreadsBeginN_1D(Integer.MAX_VALUE)
      ConcurrencyUtils.setThreadsBeginN_1D(Integer.MAX_VALUE)
      nthreads = Math.min(nthreads, columns)
      val futures = Array.ofDim[Future](nthreads)
      val k = columns / nthreads
      for (j <- 0 until nthreads) {
        val firstColumn = j * k
        val lastColumn = if ((j == nthreads - 1)) columns else firstColumn + k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            for (c <- firstColumn until lastColumn) {
              AA.viewColumn(c).assign(x, DoubleFunctions.mult)
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
      ConcurrencyUtils.resetThreadsBeginN()
    } else {
      var c = columns
      while (c >= 0) {
        AA.viewColumn(c).assign(x, DoubleFunctions.mult)
      }
    }
    AA
  }

  /**
   * Returns the one-norm of vector <tt>x</tt>, which is
   * <tt>Sum(abs(x[i]))</tt>.
   */
  def norm1(x: StrideMatrix1D): Double = {
    if (x.size == 0) return 0
    x.aggregate(cern.jet.math.tdouble.DoubleFunctions.plus, cern.jet.math.tdouble.DoubleFunctions.abs)
  }

  /**
   * Returns the one-norm of matrix <tt>A</tt>, which is the maximum absolute
   * column sum.
   */
  def norm1(A: StrideMatrix2D): Double = {
    var max = 0
    var column = A.columns()
    while (column >= 0) {
      max = Math.max(max, norm1(A.viewColumn(column)))
    }
    max
  }

  /**
   * Returns the two-norm (aka <i>euclidean norm</i>) of vector <tt>x</tt>;
   * equivalent to <tt>Sqrt(mult(x,x))</tt>.
   */
  def norm2(x: StrideMatrix1D): Double = Math.sqrt(x.zDotProduct(x))

  /**
   * Returns the two-norm (aka <i>euclidean norm</i>) of vector
   * <tt>X.vectorize()</tt>;
   */
  def vectorNorm2(X: StrideMatrix2D): Double = {
    if (X.isView == true || !(X.isInstanceOf[DenseMatrix2D])) {
      val rows = X.rows()
      val columns = X.columns()
      var sum = 0
      var nthreads = ConcurrencyUtils.getNumberOfThreads
      if ((nthreads > 1) &&
        (rows * columns >= ConcurrencyUtils.getThreadsBeginN_2D)) {
        nthreads = Math.min(nthreads, rows)
        val futures = Array.ofDim[Future](nthreads)
        var result: java.lang.Double = null
        val k = rows / nthreads
        for (j <- 0 until nthreads) {
          val firstRow = j * k
          val lastRow = if ((j == nthreads - 1)) rows else firstRow + k
          futures(j) = ConcurrencyUtils.submit(new Callable[Double]() {

            def call(): java.lang.Double = {
              var sum = 0
              var elem: Double = 0.0
              for (r <- firstRow until lastRow; c <- 0 until columns) {
                elem = X.getQuick(r, c)
                sum += (elem * elem)
              }
              return sum
            }
          })
        }
        try {
          for (j <- 0 until nthreads) {
            result = futures(j).get.asInstanceOf[java.lang.Double]
            sum += result
          }
        } catch {
          case ex: ExecutionException => ex.printStackTrace()
          case e: InterruptedException => e.printStackTrace()
        }
      } else {
        var elem: Double = 0.0
        for (r <- 0 until rows; c <- 0 until columns) {
          elem = X.getQuick(r, c)
          sum += (elem * elem)
        }
      }
      Math.sqrt(sum)
    } else {
      val elems = X.asInstanceOf[DenseMatrix2D].elements()
      var sum = 0
      var nthreads = ConcurrencyUtils.getNumberOfThreads
      if ((nthreads > 1) &&
        (elems.length >= ConcurrencyUtils.getThreadsBeginN_2D)) {
        nthreads = Math.min(nthreads, elems.length)
        val futures = Array.ofDim[Future](nthreads)
        var result: java.lang.Double = null
        val k = elems.length / nthreads
        for (j <- 0 until nthreads) {
          val firstIdx = j * k
          val lastIdx = if ((j == nthreads - 1)) elems.length else firstIdx + k
          futures(j) = ConcurrencyUtils.submit(new Callable[Double]() {

            def call(): java.lang.Double = {
              var sum = 0
              for (l <- firstIdx until lastIdx) {
                sum += (elems(l) * elems(l))
              }
              return sum
            }
          })
        }
        try {
          for (j <- 0 until nthreads) {
            result = futures(j).get.asInstanceOf[java.lang.Double]
            sum += result
          }
        } catch {
          case ex: ExecutionException => ex.printStackTrace()
          case e: InterruptedException => e.printStackTrace()
        }
      } else {
        for (l <- 0 until elems.length) {
          sum += (elems(l) * elems(l))
        }
      }
      Math.sqrt(sum)
    }
  }

  /**
   * Returns the two-norm (aka <i>euclidean norm</i>) of vector
   * <tt>X.vectorize()</tt>;
   */
  def vectorNorm2(X: DoubleMatrix3D): Double = {
    if (X.isView == true || !(X.isInstanceOf[DenseDoubleMatrix3D])) {
      val slices = X.slices()
      val rows = X.rows()
      val columns = X.columns()
      var sum = 0
      var nthreads = ConcurrencyUtils.getNumberOfThreads
      if ((nthreads > 1) &&
        (rows * columns >= ConcurrencyUtils.getThreadsBeginN_2D)) {
        nthreads = Math.min(nthreads, slices)
        val futures = Array.ofDim[Future](nthreads)
        var result: java.lang.Double = null
        val k = slices / nthreads
        for (j <- 0 until nthreads) {
          val firstSlice = j * k
          val lastSlice = if ((j == nthreads - 1)) slices else firstSlice + k
          futures(j) = ConcurrencyUtils.submit(new Callable[Double]() {

            def call(): java.lang.Double = {
              var sum = 0
              var elem: Double = 0.0
              for (s <- firstSlice until lastSlice; r <- 0 until rows; c <- 0 until columns) {
                elem = X.getQuick(s, r, c)
                sum += (elem * elem)
              }
              return sum
            }
          })
        }
        try {
          for (j <- 0 until nthreads) {
            result = futures(j).get.asInstanceOf[java.lang.Double]
            sum += result
          }
        } catch {
          case ex: ExecutionException => ex.printStackTrace()
          case e: InterruptedException => e.printStackTrace()
        }
      } else {
        var elem: Double = 0.0
        for (s <- 0 until slices; r <- 0 until rows; c <- 0 until columns) {
          elem = X.getQuick(s, r, c)
          sum += (elem * elem)
        }
      }
      Math.sqrt(sum)
    } else {
      val elems = X.asInstanceOf[DenseDoubleMatrix3D].elements()
      var sum = 0
      var nthreads = ConcurrencyUtils.getNumberOfThreads
      if ((nthreads > 1) &&
        (elems.length >= ConcurrencyUtils.getThreadsBeginN_2D)) {
        nthreads = Math.min(nthreads, elems.length)
        val futures = Array.ofDim[Future](nthreads)
        var result: java.lang.Double = null
        val k = elems.length / nthreads
        for (j <- 0 until nthreads) {
          val firstIdx = j * k
          val lastIdx = if ((j == nthreads - 1)) elems.length else firstIdx + k
          futures(j) = ConcurrencyUtils.submit(new Callable[Double]() {

            def call(): java.lang.Double = {
              var sum = 0
              for (l <- firstIdx until lastIdx) {
                sum += (elems(l) * elems(l))
              }
              return sum
            }
          })
        }
        try {
          for (j <- 0 until nthreads) {
            result = futures(j).get.asInstanceOf[java.lang.Double]
            sum += result
          }
        } catch {
          case ex: ExecutionException => ex.printStackTrace()
          case e: InterruptedException => e.printStackTrace()
        }
      } else {
        for (l <- 0 until elems.length) {
          sum += (elems(l) * elems(l))
        }
      }
      Math.sqrt(sum)
    }
  }

  def norm(A: StrideMatrix2D, `type`: Norm): Double = `type` match {
    case Frobenius => DEFAULT.normF(A)
    case Infinity => DEFAULT.normInfinity(A)
    case One => DEFAULT.norm1(A)
    case Two => DEFAULT.norm2(A)
    case _ => 0
  }

  def norm(x: StrideMatrix1D, `type`: Norm): Double = `type` match {
    case Frobenius => DEFAULT.normF(x)
    case Infinity => DEFAULT.normInfinity(x)
    case One => DEFAULT.norm1(x)
    case Two => DEFAULT.norm2(x)
    case _ => 0
  }

  /**
   * Returns the two-norm of matrix <tt>A</tt>, which is the maximum singular
   * value; obtained from SVD.
   */
  def norm2(A: StrideMatrix2D): Double = svd(A).norm2()

  /**
   * Returns the Frobenius norm of matrix <tt>A</tt>, which is
   * <tt>Sqrt(Sum(A[i,j]<sup>2</sup>))</tt>.
   */
  def normF(A: StrideMatrix2D): Double = {
    if (A.size == 0) return 0
    A.aggregate(hypotFunction(), cern.jet.math.tdouble.DoubleFunctions.identity)
  }

  /**
   * Returns the Frobenius norm of matrix <tt>A</tt>, which is
   * <tt>Sqrt(Sum(A[i]<sup>2</sup>))</tt>.
   */
  def normF(A: StrideMatrix1D): Double = {
    if (A.size == 0) return 0
    A.aggregate(hypotFunction(), cern.jet.math.tdouble.DoubleFunctions.identity)
  }

  /**
   * Returns the infinity norm of vector <tt>x</tt>, which is
   * <tt>Max(abs(x[i]))</tt>.
   */
  def normInfinity(x: StrideMatrix1D): Double = {
    if (x.size == 0) return 0
    x.aggregate(cern.jet.math.tdouble.DoubleFunctions.max, cern.jet.math.tdouble.DoubleFunctions.abs)
  }

  /**
   * Returns the infinity norm of matrix <tt>A</tt>, which is the maximum
   * absolute row sum.
   */
  def normInfinity(A: StrideMatrix2D): Double = {
    var max = 0
    var row = A.rows()
    while (row >= 0) {
      max = Math.max(max, norm1(A.viewRow(row)))
    }
    max
  }

  /**
   * Modifies the given vector <tt>A</tt> such that it is permuted as
   * specified; Useful for pivoting. Cell <tt>A[i]</tt> will go into cell
   * <tt>A[indexes[i]]</tt>.
   * <p>
   * <b>Example:</b>
   *
   * <pre>
   * 	 Reordering
   * 	 [A,B,C,D,E] with indexes [0,4,2,3,1] yields
   * 	 [A,E,C,D,B]
   * 	 In other words A[0]&lt;--A[0], A[1]&lt;--A[4], A[2]&lt;--A[2], A[3]&lt;--A[3], A[4]&lt;--A[1].
   *
   * 	 Reordering
   * 	 [A,B,C,D,E] with indexes [0,4,1,2,3] yields
   * 	 [A,E,B,C,D]
   * 	 In other words A[0]&lt;--A[0], A[1]&lt;--A[4], A[2]&lt;--A[1], A[3]&lt;--A[2], A[4]&lt;--A[3].
   *
   * </pre>
   *
   * @param A
   *            the vector to permute.
   * @param indexes
   *            the permutation indexes, must satisfy
   *            <tt>indexes.length==A.size() && indexes[i] >= 0 && indexes[i] < A.size()</tt>
   *            ;
   * @param work
   *            the working storage, must satisfy
   *            <tt>work.length >= A.size()</tt>; set <tt>work==null</tt> if
   *            you don't care about performance.
   * @return the modified <tt>A</tt> (for convenience only).
   * @throws IndexOutOfBoundsException
   *             if <tt>indexes.length != A.size()</tt>.
   */
  def permute(A: StrideMatrix1D, indexes: Array[Int], work: Array[Double]): StrideMatrix1D = {
    val size = A.size.toInt
    if (indexes.length != size) throw new IndexOutOfBoundsException("invalid permutation")
    if (work == null || size > work.length) {
      work = A.toArray()
    } else {
      A.toArray(work)
    }
    var i = size
    while (i >= 0) A.setQuick(i, work(indexes(i)))
    A
  }

  /**
   * Constructs and returns a new row and column permuted <i>selection
   * view</i> of matrix <tt>A</tt>; equivalent to
   * {@link DoubleMatrix2D#viewSelection(int[],int[])}. The returned matrix is
   * backed by this matrix, so changes in the returned matrix are reflected in
   * this matrix, and vice-versa. Use idioms like
   * <tt>result = permute(...).copy()</tt> to generate an independent sub
   * matrix.
   *
   * @return the new permuted selection view.
   */
  def permute(A: StrideMatrix2D, rowIndexes: Array[Int], columnIndexes: Array[Int]): StrideMatrix2D = {
    A.viewSelection(rowIndexes, columnIndexes)
  }

  /**
   * Modifies the given matrix <tt>A</tt> such that it's columns are permuted
   * as specified; Useful for pivoting. Column <tt>A[i]</tt> will go into
   * column <tt>A[indexes[i]]</tt>. Equivalent to
   * <tt>permuteRows(transpose(A), indexes, work)</tt>.
   *
   * @param A
   *            the matrix to permute.
   * @param indexes
   *            the permutation indexes, must satisfy
   *            <tt>indexes.length==A.columns() && indexes[i] >= 0 && indexes[i] < A.columns()</tt>
   *            ;
   * @param work
   *            the working storage, must satisfy
   *            <tt>work.length >= A.columns()</tt>; set <tt>work==null</tt>
   *            if you don't care about performance.
   * @return the modified <tt>A</tt> (for convenience only).
   * @throws IndexOutOfBoundsException
   *             if <tt>indexes.length != A.columns()</tt>.
   */
  def permuteColumns(A: StrideMatrix2D, indexes: Array[Int], work: Array[Int]): StrideMatrix2D = {
    permuteRows(A.viewDice(), indexes, work)
  }

  /**
   * Modifies the given matrix <tt>A</tt> such that it's rows are permuted as
   * specified; Useful for pivoting. Row <tt>A[i]</tt> will go into row
   * <tt>A[indexes[i]]</tt>.
   * <p>
   * <b>Example:</b>
   *
   * <pre>
   * 	 Reordering
   * 	 [A,B,C,D,E] with indexes [0,4,2,3,1] yields
   * 	 [A,E,C,D,B]
   * 	 In other words A[0]&lt;--A[0], A[1]&lt;--A[4], A[2]&lt;--A[2], A[3]&lt;--A[3], A[4]&lt;--A[1].
   *
   * 	 Reordering
   * 	 [A,B,C,D,E] with indexes [0,4,1,2,3] yields
   * 	 [A,E,B,C,D]
   * 	 In other words A[0]&lt;--A[0], A[1]&lt;--A[4], A[2]&lt;--A[1], A[3]&lt;--A[2], A[4]&lt;--A[3].
   *
   * </pre>
   *
   * @param A
   *            the matrix to permute.
   * @param indexes
   *            the permutation indexes, must satisfy
   *            <tt>indexes.length==A.rows() && indexes[i] >= 0 && indexes[i] < A.rows()</tt>
   *            ;
   * @param work
   *            the working storage, must satisfy
   *            <tt>work.length >= A.rows()</tt>; set <tt>work==null</tt> if
   *            you don't care about performance.
   * @return the modified <tt>A</tt> (for convenience only).
   * @throws IndexOutOfBoundsException
   *             if <tt>indexes.length != A.rows()</tt>.
   */
  def permuteRows(A: StrideMatrix2D, indexes: Array[Int], work: Array[Int]): StrideMatrix2D = {
    val size = A.rows()
    if (indexes.length != size) throw new IndexOutOfBoundsException("invalid permutation")
    val columns = A.columns()
    if (columns < size / 10) {
      val doubleWork = Array.ofDim[Double](size)
      var j = A.columns()
      while (j >= 0) permute(A.viewColumn(j), indexes, doubleWork)
      return A
    }
    val swapper = new cern.colt.Swapper() {

      def swap(a: Int, b: Int) {
        A.viewRow(a).swap(A.viewRow(b))
      }
    }
    cern.colt.GenericPermuting.permute(indexes, swapper, work, null)
    A
  }

  /**
   * Linear algebraic matrix power;
   * <tt>B = A<sup>k</sup> <==> B = A*A*...*A</tt>.
   * <ul>
   * <li><tt>p &gt;= 1: B = A*A*...*A</tt>.</li>
   * <li><tt>p == 0: B = identity matrix</tt>.</li>
   * <li><tt>p &lt;  0: B = pow(inverse(A),-p)</tt>.</li>
   * </ul>
   * Implementation: Based on logarithms of 2, memory usage minimized.
   *
   * @param A
   *            the source matrix; must be square; stays unaffected by this
   *            operation.
   * @param p
   *            the exponent, can be any number.
   * @return <tt>B</tt>, a newly constructed result matrix;
   *         storage-independent of <tt>A</tt>.
   *
   * @throws IllegalArgumentException
   *             if <tt>!property().isSquare(A)</tt>.
   */
  def pow(A: StrideMatrix2D, p: Int): StrideMatrix2D = {
    val blas = new SmpDoubleBlas()
    DoubleProperty.DEFAULT.checkSquare(A)
    if (p < 0) {
      A = inverse(A)
      p = -p
    }
    if (p == 0) return DoubleFactory2D.dense.identity(A.rows())
    var T = A.like()
    if (p == 1) return T.assign(A)
    if (p == 2) {
      blas.dgemm(false, false, 1, A, A, 0, T)
      return T
    }
    val k = cern.colt.matrix.tbit.QuickBitVector.mostSignificantBit(p)
    var i = 0
    while (i <= k && (p & (1 << i)) == 0) {
      blas.dgemm(false, false, 1, A, A, 0, T)
      val swap = A
      A = T
      T = swap
      i += 1
    }
    var B = A.copy()
    i += 1
    while (i <= k) {
      blas.dgemm(false, false, 1, A, A, 0, T)
      var swap = A
      A = T
      T = swap
      if ((p & (1 << i)) != 0) {
        blas.dgemm(false, false, 1, B, A, 0, T)
        swap = B
        B = T
        T = swap
      }
      i += 1
    }
    B
  }

  /**
   * Returns the property object attached to this Algebra, defining tolerance.
   *
   * @return the Property object.
   * @see #setProperty(DoubleProperty)
   */
  def property(): DoubleProperty = property

  /**
   * Constructs and returns the QR-decomposition of the given matrix.
   */
  def qr(matrix: StrideMatrix2D): DenseDoubleQRDecomposition = new DenseDoubleQRDecomposition(matrix)

  /**
   * Returns the effective numerical rank of matrix <tt>A</tt>, obtained from
   * Singular Value Decomposition.
   */
  def rank(A: StrideMatrix2D): Int = svd(A).rank()

  /**
   * Attaches the given property object to this Algebra, defining tolerance.
   *
   * @param property
   *            the Property object to be attached.
   * @throws UnsupportedOperationException
   *             if <tt>this==DEFAULT && property!=this.property()</tt> - The
   *             DEFAULT Algebra object is immutable.
   * @throws UnsupportedOperationException
   *             if <tt>this==ZERO && property!=this.property()</tt> - The
   *             ZERO Algebra object is immutable.
   * @see #property
   */
  def setProperty(property: DoubleProperty) {
    if (this == DEFAULT && property != this.property) throw new IllegalArgumentException("Attempted to modify immutable object.")
    if (this == ZERO && property != this.property) throw new IllegalArgumentException("Attempted to modify immutable object.")
    this.property = property
  }

  /**
   * Solves the upper triangular system U*x=b;
   *
   * @param U
   *            upper triangular matrix
   * @param b
   *            right-hand side
   * @return x, a new independent matrix;
   */
  def backwardSolve(U: StrideMatrix2D, b: StrideMatrix1D): StrideMatrix1D = {
    val rows = U.rows()
    val x = b.like()
    x.setQuick(rows - 1, b.getQuick(rows - 1) / U.getQuick(rows - 1, rows - 1))
    var sum: Double = 0.0
    var r = rows - 2
    while (r >= 0) {
      sum = U.viewRow(r).zDotProduct(x)
      x.setQuick(r, (b.getQuick(r) - sum) / U.getQuick(r, r))
      r -= 1
    }
    x
  }

  /**
   * Solves the lower triangular system U*x=b;
   *
   * @param L
   *            lower triangular matrix
   * @param b
   *            right-hand side
   * @return x, a new independent matrix;
   */
  def forwardSolve(L: StrideMatrix2D, b: StrideMatrix1D): StrideMatrix1D = {
    val rows = L.rows()
    val x = b.like()
    var sum: Double = 0.0
    x.setQuick(0, b.getQuick(0) / L.getQuick(0, 0))
    for (r <- 1 until rows) {
      sum = L.viewRow(r).zDotProduct(x)
      x.setQuick(r, (b.getQuick(r) - sum) / L.getQuick(r, r))
    }
    x
  }

  /**
   * Solves A*x = b.
   *
   * @return x; a new independent matrix; solution if A is square, least
   *         squares solution otherwise.
   */
  def solve(A: StrideMatrix2D, b: StrideMatrix1D): StrideMatrix1D = {
    if (A.rows() == A.columns()) {
      lu(A).solve(b)
    } else {
      val x = b.copy()
      qr(A).solve(x)
      x.viewPart(0, A.columns()).copy()
    }
  }

  /**
   * Solves A*X = B.
   *
   * @return X; a new independent matrix; solution if A is square, least
   *         squares solution otherwise.
   */
  def solve(A: StrideMatrix2D, B: StrideMatrix2D): StrideMatrix2D = {
    if (A.rows() == A.columns()) {
      lu(A).solve(B)
    } else {
      val X = B.copy()
      qr(A).solve(X)
      X.viewPart(0, 0, A.columns(), B.columns()).copy()
    }
  }

  /**
   * Solves X*A = B, which is also A'*X' = B'.
   *
   * @return X; a new independent matrix; solution if A is square, least
   *         squares solution otherwise.
   */
  def solveTranspose(A: StrideMatrix2D, B: StrideMatrix2D): StrideMatrix2D = solve(transpose(A), transpose(B))

  /**
   * Copies the columns of the indicated rows into a new sub matrix.
   *
   * <tt>sub[0..rowIndexes.length-1,0..columnTo-columnFrom] = A[rowIndexes(:),columnFrom..columnTo]</tt>
   * ; The returned matrix is <i>not backed</i> by this matrix, so changes in
   * the returned matrix are <i>not reflected</i> in this matrix, and
   * vice-versa.
   *
   * @param A
   *            the source matrix to copy from.
   * @param rowIndexes
   *            the indexes of the rows to copy. May be unsorted.
   * @param columnFrom
   *            the index of the first column to copy (inclusive).
   * @param columnTo
   *            the index of the last column to copy (inclusive).
   * @return a new sub matrix; with
   *         <tt>sub.rows()==rowIndexes.length; sub.columns()==columnTo-columnFrom+1</tt>
   *         .
   * @throws IndexOutOfBoundsException
   *             if
   *
   *             <tt>columnFrom<0 || columnTo-columnFrom+1<0 || columnTo+1>matrix.columns() || for any row=rowIndexes[i]: row < 0 || row >= matrix.rows()</tt>
   *             .
   */
  def subMatrix(A: StrideMatrix2D,
      rowIndexes: Array[Int],
      columnFrom: Int,
      columnTo: Int): StrideMatrix2D = {
    val width = columnTo - columnFrom + 1
    val rows = A.rows()
    A = A.viewPart(0, columnFrom, rows, width)
    val sub = A.like(rowIndexes.length, width)
    var r = rowIndexes.length
    while (r >= 0) {
      val row = rowIndexes(r)
      if (row < 0 || row >= rows) throw new IndexOutOfBoundsException("Illegal Index")
      sub.viewRow(r).assign(A.viewRow(row))
    }
    sub
  }

  /**
   * Copies the rows of the indicated columns into a new sub matrix.
   *
   * <tt>sub[0..rowTo-rowFrom,0..columnIndexes.length-1] = A[rowFrom..rowTo,columnIndexes(:)]</tt>
   * ; The returned matrix is <i>not backed</i> by this matrix, so changes in
   * the returned matrix are <i>not reflected</i> in this matrix, and
   * vice-versa.
   *
   * @param A
   *            the source matrix to copy from.
   * @param rowFrom
   *            the index of the first row to copy (inclusive).
   * @param rowTo
   *            the index of the last row to copy (inclusive).
   * @param columnIndexes
   *            the indexes of the columns to copy. May be unsorted.
   * @return a new sub matrix; with
   *         <tt>sub.rows()==rowTo-rowFrom+1; sub.columns()==columnIndexes.length</tt>
   *         .
   * @throws IndexOutOfBoundsException
   *             if
   *
   *             <tt>rowFrom<0 || rowTo-rowFrom+1<0 || rowTo+1>matrix.rows() || for any col=columnIndexes[i]: col < 0 || col >= matrix.columns()</tt>
   *             .
   */
  def subMatrix(A: StrideMatrix2D,
      rowFrom: Int,
      rowTo: Int,
      columnIndexes: Array[Int]): StrideMatrix2D = {
    if (rowTo - rowFrom >= A.rows()) throw new IndexOutOfBoundsException("Too many rows")
    val height = rowTo - rowFrom + 1
    val columns = A.columns()
    A = A.viewPart(rowFrom, 0, height, columns)
    val sub = A.like(height, columnIndexes.length)
    var c = columnIndexes.length
    while (c >= 0) {
      val column = columnIndexes(c)
      if (column < 0 || column >= columns) throw new IndexOutOfBoundsException("Illegal Index")
      sub.viewColumn(c).assign(A.viewColumn(column))
    }
    sub
  }

  /**
   * Constructs and returns a new <i>sub-range view</i> which is the sub
   * matrix <tt>A[fromRow..toRow,fromColumn..toColumn]</tt>. The returned
   * matrix is backed by this matrix, so changes in the returned matrix are
   * reflected in this matrix, and vice-versa. Use idioms like
   * <tt>result = subMatrix(...).copy()</tt> to generate an independent sub
   * matrix.
   *
   * @param A
   *            the source matrix.
   * @param fromRow
   *            The index of the first row (inclusive).
   * @param toRow
   *            The index of the last row (inclusive).
   * @param fromColumn
   *            The index of the first column (inclusive).
   * @param toColumn
   *            The index of the last column (inclusive).
   * @return a new sub-range view.
   * @throws IndexOutOfBoundsException
   *             if
   *
   *             <tt>fromColumn<0 || toColumn-fromColumn+1<0 || toColumn>=A.columns() || fromRow<0 || toRow-fromRow+1<0 || toRow>=A.rows()</tt>
   */
  def subMatrix(A: StrideMatrix2D,
      fromRow: Int,
      toRow: Int,
      fromColumn: Int,
      toColumn: Int): StrideMatrix2D = {
    A.viewPart(fromRow, fromColumn, toRow - fromRow + 1, toColumn - fromColumn + 1)
  }

  /**
   * Constructs and returns the SingularValue-decomposition of the given
   * matrix.
   */
  def svd(matrix: StrideMatrix2D): DenseDoubleSingularValueDecomposition = {
    new DenseDoubleSingularValueDecomposition(matrix, true, true)
  }

  /**
   * Returns a String with (propertyName, propertyValue) pairs. Useful for
   * debugging or to quickly get the rough picture. For example,
   *
   * <pre>
   * 	 cond          : 14.073264490042144
   * 	 det           : Illegal operation or error: Matrix must be square.
   * 	 norm1         : 0.9620244354009628
   * 	 norm2         : 3.0
   * 	 normF         : 1.304841791648992
   * 	 normInfinity  : 1.5406551198102534
   * 	 rank          : 3
   * 	 trace         : 0
   *
   * </pre>
   */
  def toString(matrix: StrideMatrix2D): String = {
    val names = new cern.colt.list.tobject.ObjectArrayList()
    val values = new cern.colt.list.tobject.ObjectArrayList()
    val unknown = "Illegal operation or error: "
    names.add("cond")
    try {
      values.add(String.valueOf(cond(matrix)))
    } catch {
      case exc: IllegalArgumentException => values.add(unknown + exc.getMessage)
    }
    names.add("det")
    try {
      values.add(String.valueOf(det(matrix)))
    } catch {
      case exc: IllegalArgumentException => values.add(unknown + exc.getMessage)
    }
    names.add("norm1")
    try {
      values.add(String.valueOf(norm1(matrix)))
    } catch {
      case exc: IllegalArgumentException => values.add(unknown + exc.getMessage)
    }
    names.add("norm2")
    try {
      values.add(String.valueOf(norm2(matrix)))
    } catch {
      case exc: IllegalArgumentException => values.add(unknown + exc.getMessage)
    }
    names.add("normF")
    try {
      values.add(String.valueOf(normF(matrix)))
    } catch {
      case exc: IllegalArgumentException => values.add(unknown + exc.getMessage)
    }
    names.add("normInfinity")
    try {
      values.add(String.valueOf(normInfinity(matrix)))
    } catch {
      case exc: IllegalArgumentException => values.add(unknown + exc.getMessage)
    }
    names.add("rank")
    try {
      values.add(String.valueOf(rank(matrix)))
    } catch {
      case exc: IllegalArgumentException => values.add(unknown + exc.getMessage)
    }
    names.add("trace")
    try {
      values.add(String.valueOf(trace(matrix)))
    } catch {
      case exc: IllegalArgumentException => values.add(unknown + exc.getMessage)
    }
    val comp = new cern.colt.function.tint.IntComparator() {

      def compare(a: Int, b: Int): Int = {
        return DoubleProperty.get(names, a).compareTo(DoubleProperty.get(names, b))
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
      buf.append(DoubleProperty.blanks(maxLength - name.length))
      buf.append(" : ")
      buf.append(values.get(i))
      if (i < names.size - 1) buf.append('\n')
    }
    buf.toString
  }

  /**
   * Returns the results of <tt>toString(A)</tt> and additionally the results
   * of all sorts of decompositions applied to the given matrix. Useful for
   * debugging or to quickly get the rough picture. For example,
   *
   * <pre>
   * 	 A = 3 x 3 matrix
   * 	 249  66  68
   * 	 104 214 108
   * 	 144 146 293
   *
   * 	 cond         : 3.931600417472078
   * 	 det          : 9638870.0
   * 	 norm1        : 497.0
   * 	 norm2        : 473.34508217011404
   * 	 normF        : 516.873292016525
   * 	 normInfinity : 583.0
   * 	 rank         : 3
   * 	 trace        : 756.0
   *
   * 	 density                      : 1.0
   * 	 isDiagonal                   : false
   * 	 isDiagonallyDominantByColumn : true
   * 	 isDiagonallyDominantByRow    : true
   * 	 isIdentity                   : false
   * 	 isLowerBidiagonal            : false
   * 	 isLowerTriangular            : false
   * 	 isNonNegative                : true
   * 	 isOrthogonal                 : false
   * 	 isPositive                   : true
   * 	 isSingular                   : false
   * 	 isSkewSymmetric              : false
   * 	 isSquare                     : true
   * 	 isStrictlyLowerTriangular    : false
   * 	 isStrictlyTriangular         : false
   * 	 isStrictlyUpperTriangular    : false
   * 	 isSymmetric                  : false
   * 	 isTriangular                 : false
   * 	 isTridiagonal                : false
   * 	 isUnitTriangular             : false
   * 	 isUpperBidiagonal            : false
   * 	 isUpperTriangular            : false
   * 	 isZero                       : false
   * 	 lowerBandwidth               : 2
   * 	 semiBandwidth                : 3
   * 	 upperBandwidth               : 2
   *
   * 	 -----------------------------------------------------------------------------
   * 	 LUDecompositionQuick(A) --&gt; isNonSingular(A), det(A), pivot, L, U, inverse(A)
   * 	 -----------------------------------------------------------------------------
   * 	 isNonSingular = true
   * 	 det = 9638870.0
   * 	 pivot = [0, 1, 2]
   *
   * 	 L = 3 x 3 matrix
   * 	 1        0       0
   * 	 0.417671 1       0
   * 	 0.578313 0.57839 1
   *
   * 	 U = 3 x 3 matrix
   * 	 249  66         68
   * 	 0 186.433735  79.598394
   * 	 0   0        207.635819
   *
   * 	 inverse(A) = 3 x 3 matrix
   * 	 0.004869 -0.000976 -0.00077
   * 	 -0.001548  0.006553 -0.002056
   * 	 -0.001622 -0.002786  0.004816
   *
   * 	 -----------------------------------------------------------------
   * 	 QRDecomposition(A) --&gt; hasFullRank(A), H, Q, R, pseudo inverse(A)
   * 	 -----------------------------------------------------------------
   * 	 hasFullRank = true
   *
   * 	 H = 3 x 3 matrix
   * 	 1.814086 0        0
   * 	 0.34002  1.903675 0
   * 	 0.470797 0.428218 2
   *
   * 	 Q = 3 x 3 matrix
   * 	 -0.814086  0.508871  0.279845
   * 	 -0.34002  -0.808296  0.48067
   * 	 -0.470797 -0.296154 -0.831049
   *
   * 	 R = 3 x 3 matrix
   * 	 -305.864349 -195.230337 -230.023539
   * 	 0        -182.628353  467.703164
   * 	 0           0        -309.13388
   *
   * 	 pseudo inverse(A) = 3 x 3 matrix
   * 	 0.006601  0.001998 -0.005912
   * 	 -0.005105  0.000444  0.008506
   * 	 -0.000905 -0.001555  0.002688
   *
   * 	 --------------------------------------------------------------------------
   * 	 CholeskyDecomposition(A) --&gt; isSymmetricPositiveDefinite(A), L, inverse(A)
   * 	 --------------------------------------------------------------------------
   * 	 isSymmetricPositiveDefinite = false
   *
   * 	 L = 3 x 3 matrix
   * 	 15.779734  0         0
   * 	 6.590732 13.059948  0
   * 	 9.125629  6.573948 12.903724
   *
   * 	 inverse(A) = Illegal operation or error: Matrix is not symmetric positive definite.
   *
   * 	 ---------------------------------------------------------------------
   * 	 EigenvalueDecomposition(A) --&gt; D, V, realEigenvalues, imagEigenvalues
   * 	 ---------------------------------------------------------------------
   * 	 realEigenvalues = 1 x 3 matrix
   * 	 462.796507 172.382058 120.821435
   * 	 imagEigenvalues = 1 x 3 matrix
   * 	 0 0 0
   *
   * 	 D = 3 x 3 matrix
   * 	 462.796507   0          0
   * 	 0        172.382058   0
   * 	 0          0        120.821435
   *
   * 	 V = 3 x 3 matrix
   * 	 -0.398877 -0.778282  0.094294
   * 	 -0.500327  0.217793 -0.806319
   * 	 -0.768485  0.66553   0.604862
   *
   * 	 ---------------------------------------------------------------------
   * 	 SingularValueDecomposition(A) --&gt; cond(A), rank(A), norm2(A), U, S, V
   * 	 ---------------------------------------------------------------------
   * 	 cond = 3.931600417472078
   * 	 rank = 3
   * 	 norm2 = 473.34508217011404
   *
   * 	 U = 3 x 3 matrix
   * 	 0.46657  -0.877519  0.110777
   * 	 0.50486   0.161382 -0.847982
   * 	 0.726243  0.45157   0.51832
   *
   * 	 S = 3 x 3 matrix
   * 	 473.345082   0          0
   * 	 0        169.137441   0
   * 	 0          0        120.395013
   *
   * 	 V = 3 x 3 matrix
   * 	 0.577296 -0.808174  0.116546
   * 	 0.517308  0.251562 -0.817991
   * 	 0.631761  0.532513  0.563301
   *
   * </pre>
   */
  def toVerboseString(matrix: StrideMatrix2D): String = {
    val constructionException = "Illegal operation or error upon construction of "
    val buf = new StringBuffer()
    buf.append("A = ")
    buf.append(matrix)
    buf.append("\n\n" + toString matrix)
    buf.append("\n\n" + DoubleProperty.DEFAULT toString matrix)
    var lu: DenseDoubleLUDecomposition = null
    try {
      lu = new DenseDoubleLUDecomposition(matrix)
    } catch {
      case exc: IllegalArgumentException => buf.append("\n\n" + constructionException + " LUDecomposition: " +
        exc.getMessage)
    }
    if (lu != null) buf.append("\n\n" + lu.toString)
    var qr: DenseDoubleQRDecomposition = null
    try {
      qr = new DenseDoubleQRDecomposition(matrix)
    } catch {
      case exc: IllegalArgumentException => buf.append("\n\n" + constructionException + " QRDecomposition: " +
        exc.getMessage)
    }
    if (qr != null) buf.append("\n\n" + qr.toString)
    var chol: DenseDoubleCholeskyDecomposition = null
    try {
      chol = new DenseDoubleCholeskyDecomposition(matrix)
    } catch {
      case exc: IllegalArgumentException => buf.append("\n\n" + constructionException + " CholeskyDecomposition: " +
        exc.getMessage)
    }
    if (chol != null) buf.append("\n\n" + chol.toString)
    var eig: DenseDoubleEigenvalueDecomposition = null
    try {
      eig = new DenseDoubleEigenvalueDecomposition(matrix)
    } catch {
      case exc: IllegalArgumentException => buf.append("\n\n" + constructionException + " EigenvalueDecomposition: " +
        exc.getMessage)
    }
    if (eig != null) buf.append("\n\n" + eig.toString)
    var svd: DenseDoubleSingularValueDecomposition = null
    try {
      svd = new DenseDoubleSingularValueDecomposition(matrix, true, true)
    } catch {
      case exc: IllegalArgumentException => buf.append("\n\n" + constructionException + " SingularValueDecomposition: " +
        exc.getMessage)
    }
    if (svd != null) buf.append("\n\n" + svd.toString)
    buf.toString
  }

  /**
   * Returns the sum of the diagonal elements of matrix <tt>A</tt>;
   * <tt>Sum(A[i,i])</tt>.
   */
  def trace(A: StrideMatrix2D): Double = {
    var sum = 0
    var i = Math.min(A.rows(), A.columns())
    while (i >= 0) {
      sum += A.getQuick(i, i)
    }
    sum
  }

  /**
   * Constructs and returns a new view which is the transposition of the given
   * matrix <tt>A</tt>. Equivalent to {@link DoubleMatrix2D#viewDice
   * A.viewDice()}. This is a zero-copy transposition, taking O(1), i.e.
   * constant time. The returned view is backed by this matrix, so changes in
   * the returned view are reflected in this matrix, and vice-versa. Use
   * idioms like <tt>result = transpose(A).copy()</tt> to generate an
   * independent matrix.
   * <p>
   * <b>Example:</b>
   * <table border="0">
   * <tr nowrap>
   * <td valign="top">2 x 3 matrix: <br>
   * 1, 2, 3<br>
   * 4, 5, 6</td>
   * <td>transpose ==></td>
   * <td valign="top">3 x 2 matrix:<br>
   * 1, 4 <br>
   * 2, 5 <br>
   * 3, 6</td>
   * <td>transpose ==></td>
   * <td valign="top">2 x 3 matrix: <br>
   * 1, 2, 3<br>
   * 4, 5, 6</td>
   * </tr>
   * </table>
   *
   * @return a new transposed view.
   */
  def transpose(A: StrideMatrix2D): StrideMatrix2D = A.viewDice()

  /**
   * Modifies the matrix to be a lower trapezoidal matrix.
   *
   * @return <tt>A</tt> (for convenience only).
   *
   */
  def trapezoidalLower(A: StrideMatrix2D): StrideMatrix2D = {
    val rows = A.rows()
    val columns = A.columns()
    var r = rows
    while (r >= 0) {
      var c = columns
      while (c >= 0) {
        if (r < c) A.setQuick(r, c, 0)
      }
    }
    A
  }

  /**
   * Outer product of two vectors; Returns a matrix with
   * <tt>A[i,j] = x[i] * y[j]</tt>.
   *
   * @param x
   *            the first source vector.
   * @param y
   *            the second source vector.
   * @return the outer product </tt>A</tt>.
   */
  def xmultOuter(x: StrideMatrix1D, y: StrideMatrix1D): StrideMatrix2D = {
    val A = x.like2D(x.size.toInt, y.size.toInt)
    multOuter(x, y, A)
    A
  }

  /**
   * Linear algebraic matrix power;
   * <tt>B = A<sup>k</sup> <==> B = A*A*...*A</tt>.
   *
   * @param A
   *            the source matrix; must be square.
   * @param k
   *            the exponent, can be any number.
   * @return a new result matrix.
   *
   * @throws IllegalArgumentException
   *             if <tt>!Testing.isSquare(A)</tt>.
   */
  def xpowSlow(A: StrideMatrix2D, k: Int): StrideMatrix2D = {
    var result = A.copy()
    for (i <- 0 until k - 1) {
      result = mult(result, A)
    }
    result
  }
}
