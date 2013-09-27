package cern.colt.matrix.tdouble.algo.decomposition

import cern.colt.matrix.tdouble.StrideMatrix1D
import cern.colt.matrix.tdouble.StrideMatrix2D
import cern.colt.matrix.tdouble.algo.DoubleProperty
import cern.colt.matrix.tdouble.impl.DenseColumnDoubleMatrix2D
import cern.colt.matrix.tdouble.impl.DenseMatrix2D
import edu.emory.mathcs.jplasma.tdouble.Dplasma
//remove if not needed
import scala.collection.JavaConversions._

/**
 * For a symmetric, positive definite matrix <tt>A</tt>, the Cholesky
 * decomposition is a lower triangular matrix <tt>L</tt> so that <tt>A = L*L'</tt>; If
 * the matrix is not symmetric positive definite, the IllegalArgumentException
 * is thrown.
 */
@SerialVersionUID(1020)
class DenseDoubleCholeskyDecomposition(A: StrideMatrix2D) extends java.io.Serializable {

  private var Lt: StrideMatrix2D = _

  private var elementsA: Array[Double] = _

  private var columnMatrix: Boolean = false

  /**
   * Row and column dimension (square matrix).
   */
  private var n: Int = A.rows()

  DoubleProperty.DEFAULT.checkSquare(A)

  DoubleProperty.DEFAULT.checkDense(A)

  if (A.isInstanceOf[DenseMatrix2D]) {
    elementsA = A.viewDice().copy().elements().asInstanceOf[Array[Double]]
  } else {
    columnMatrix = true
    elementsA = A.copy().elements().asInstanceOf[Array[Double]]
  }

  Dplasma.plasma_Init(n, n, 1)

  val info = Dplasma.plasma_DPOTRF(Dplasma.PlasmaUpper, n, elementsA, 0, n)

  Dplasma.plasma_Finalize()

  if (info > 0) {
    throw new IllegalArgumentException("Matrix is not symmetric positive definite.")
  }

  if (info < 0) {
    throw new IllegalArgumentException("Error occured while computing Cholesky decomposition: " +
      info)
  }

  /**
   * Returns the triangular factor, <tt>L</tt>.
   *
   * @return <tt>L</tt>
   */
  def getL(): StrideMatrix2D = {
    if (Lt != null) {
      Lt.viewDice().copy()
    } else {
      if (columnMatrix) {
        Lt = new DenseColumnDoubleMatrix2D(n, n)
        val Lelems = Lt.elements().asInstanceOf[Array[Double]]
        var c = n
        while (c >= 0) {
          var r = n
          while (r >= c) {
            Lelems(r * n + c) = elementsA(r * n + c)
          }
        }
      } else {
        Lt = new DenseMatrix2D(n, n)
        val Lelems = Lt.elements().asInstanceOf[Array[Double]]
        var c = n
        while (c >= 0) {
          var r = n
          while (r >= c) {
            Lelems(c * n + r) = elementsA(r * n + c)
          }
        }
      }
      Lt.viewDice().copy()
    }
  }

  def getLtranspose(): StrideMatrix2D = {
    if (Lt != null) {
      Lt
    } else {
      if (columnMatrix) {
        Lt = new DenseColumnDoubleMatrix2D(n, n)
        val Lelems = Lt.elements().asInstanceOf[Array[Double]]
        var c = n
        while (c >= 0) {
          var r = n
          while (r >= c) {
            Lelems(r * n + c) = elementsA(r * n + c)
          }
        }
      } else {
        Lt = new DenseMatrix2D(n, n)
        val Lelems = Lt.elements().asInstanceOf[Array[Double]]
        var c = n
        while (c >= 0) {
          var r = n
          while (r >= c) {
            Lelems(c * n + r) = elementsA(r * n + c)
          }
        }
      }
      Lt
    }
  }

  /**
   * Solves <tt>A*X = B</tt>(in-place). Upon return <tt>B</tt> is overridden
   * with the result <tt>X</tt>.
   *
   * @param B
   *            A Matrix with as many rows as <tt>A</tt> and any number of
   *            columns.
   * @exception IllegalArgumentException
   *                if <tt>B.rows() != A.rows()</tt>.
   */
  def solve(B: StrideMatrix2D) {
    if (B.rows() != n) {
      throw new IllegalArgumentException("B.rows() != A.rows()")
    }
    DoubleProperty.DEFAULT.checkDense(B)
    var elementsX: Array[Double] = null
    elementsX = if (B.isInstanceOf[DenseMatrix2D]) B.viewDice().copy().elements().asInstanceOf[Array[Double]] else if (B.isView) B.copy().elements().asInstanceOf[Array[Double]] else B.elements().asInstanceOf[Array[Double]]
    val nrhs = B.columns()
    Dplasma.plasma_Init(n, n, nrhs)
    val info = Dplasma.plasma_DPOTRS(Dplasma.PlasmaUpper, n, nrhs, elementsA, 0, n, elementsX, 0, n)
    Dplasma.plasma_Finalize()
    if (info != 0) {
      throw new IllegalArgumentException("Error occured while solving the system of equation using Cholesky decomposition: " +
        info)
    }
    if (B.isInstanceOf[DenseMatrix2D]) {
      B.viewDice().assign(elementsX)
    } else {
      if (B.isView) {
        B.assign(elementsX)
      }
    }
  }

  /**
   * Solves <tt>A*x = b</tt>(in-place). Upon return <tt>b</tt> is overridden
   * with the result <tt>x</tt>.
   *
   * @param b
   *            A vector with of size A.rows();
   * @exception IllegalArgumentException
   *                if <tt>b.size() != A.rows()</tt>.
   */
  def solve(b: StrideMatrix1D) {
    if (b.size != n) {
      throw new IllegalArgumentException("b.size() != A.rows()")
    }
    DoubleProperty.DEFAULT.checkDense(b)
    var elementsX: Array[Double] = null
    elementsX = if (b.isView) b.copy().elements().asInstanceOf[Array[Double]] else b.elements().asInstanceOf[Array[Double]]
    Dplasma.plasma_Init(n, n, 1)
    val info = Dplasma.plasma_DPOTRS(Dplasma.PlasmaUpper, n, 1, elementsA, 0, n, elementsX, 0, n)
    Dplasma.plasma_Finalize()
    if (info != 0) {
      throw new IllegalArgumentException("Error occured while solving the system of equation using Cholesky decomposition: " +
        info)
    }
    if (b.isView) {
      b.assign(elementsX)
    }
  }

  /**
   * Returns a String with (propertyName, propertyValue) pairs. Useful for
   * debugging or to quickly get the rough picture. For example,
   *
   * <pre>
   *   rank          : 3
   *   trace         : 0
   *
   * </pre>
   */
  override def toString(): String = {
    val buf = new StringBuffer()
    val unknown = "Illegal operation or error: "
    buf.append("--------------------------------------------------------------------------\n")
    buf.append("CholeskyDecomposition(A) --> L, inverse(A)\n")
    buf.append("--------------------------------------------------------------------------\n")
    buf.append("\nL = ")
    try {
      buf.append(String.valueOf(this.getL))
    } catch {
      case exc: IllegalArgumentException => buf.append(unknown + exc.getMessage)
    }
    buf.append("\n\ninverse(A) = ")
    try {
      val X = cern.colt.matrix.tdouble.DoubleFactory2D.dense.identity(n)
      this.solve(X)
      buf.append(String.valueOf(X))
    } catch {
      case exc: IllegalArgumentException => buf.append(unknown + exc.getMessage)
    }
    buf.toString
  }
}
