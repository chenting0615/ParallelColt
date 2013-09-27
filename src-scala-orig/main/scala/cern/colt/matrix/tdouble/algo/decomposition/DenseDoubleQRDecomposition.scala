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
 * For an <tt>m x n</tt> matrix <tt>A</tt> with <tt>m >= n</tt>, the QR
 * decomposition is an <tt>m x n</tt> orthogonal matrix <tt>Q</tt> and an
 * <tt>n x n</tt> upper triangular matrix <tt>R</tt> so that <tt>A = Q*R</tt>.
 * <P>
 * The QR decompostion always exists, even if the matrix does not have full
 * rank, so the constructor will never fail. The primary use of the QR
 * decomposition is in the least squares solution of nonsquare systems of
 * simultaneous linear equations. This will fail if <tt>isFullRank()</tt>
 * returns <tt>false</tt>.
 *
 * @author Piotr Wendykier (piotr.wendykier@gmail.com)
 */
@SerialVersionUID(1020)
class DenseDoubleQRDecomposition(A: StrideMatrix2D) extends java.io.Serializable {

  /**
   * Array for internal storage of decomposition.
   *
   * @serial internal array storage.
   */
  private var elementsA: Array[Double] = _

  private var T: Array[Double] = Dplasma.plasma_Allocate_T(m, n)

  private var columnMatrix: Boolean = false

  private var R: StrideMatrix2D = _

  private var Q: StrideMatrix2D = _

  /**
   * Row and column dimensions.
   *
   * @serial column dimension.
   * @serial row dimension.
   */
  private var m: Int = A.rows()

  private var n: Int = A.columns()

  DoubleProperty.DEFAULT.checkRectangular(A)

  DoubleProperty.DEFAULT.checkDense(A)

  if (A.isInstanceOf[DenseMatrix2D]) {
    elementsA = A.viewDice().copy().elements().asInstanceOf[Array[Double]]
  } else {
    columnMatrix = true
    elementsA = A.copy().elements().asInstanceOf[Array[Double]]
  }

  val lda = m

  Dplasma.plasma_Init(m, n, 1)

  val info = Dplasma.plasma_DGEQRF(m, n, elementsA, 0, lda, T, 0)

  Dplasma.plasma_Finalize()

  if (info != 0) {
    throw new IllegalArgumentException("Error occured while computing QR decomposition: " + info)
  }

  /**
   * Generates and returns a copy of the orthogonal factor <tt>Q</tt>.
   *
   * @param economySize
   *            if true, then Q is m-by-n, otherwise, Q is m-by-m
   *
   * @return <tt>Q</tt>
   */
  def getQ(economySize: Boolean): StrideMatrix2D = {
    if (Q == null) {
      Dplasma.plasma_Init(m, n, 1)
      Q = new DenseColumnDoubleMatrix2D(m, m)
      val elementsQ = Q.elements().asInstanceOf[Array[Double]]
      for (i <- 0 until m) elementsQ(m * i + i) = 1.0
      val info = Dplasma.plasma_DORMQR(Dplasma.PlasmaLeft, Dplasma.PlasmaNoTrans, m, m, n, elementsA,
        0, m, T, 0, elementsQ, 0, m)
      Dplasma.plasma_Finalize()
      if (info != 0) {
        throw new IllegalArgumentException("Error occured while computing matrix Q: " + info)
      }
      Q = Q.viewDice().copy()
    }
    if (!columnMatrix) {
      if (economySize) {
        Q.viewPart(0, 0, m, n).asInstanceOf[DenseColumnDoubleMatrix2D]
          .getRowMajor
      } else {
        Q.asInstanceOf[DenseColumnDoubleMatrix2D].getRowMajor
      }
    } else {
      if (economySize) {
        Q.viewPart(0, 0, m, n).copy()
      } else {
        Q.copy()
      }
    }
  }

  /**
   * Returns a copy of the upper triangular factor, <tt>R</tt>.
   *
   * @param economySize
   *            if true, then R is n-by-n, otherwise, R is m-by-n
   *
   * @return <tt>R</tt>
   */
  def getR(economySize: Boolean): StrideMatrix2D = {
    if (R == null) {
      R = new DenseColumnDoubleMatrix2D(m, n)
      val elementsR = R.elements().asInstanceOf[Array[Double]]
      for (c <- 0 until n; r <- 0 until m if r <= c) elementsR(c * m + r) = elementsA(c * m + r)
    }
    if (!columnMatrix) {
      if (economySize) {
        R.viewPart(0, 0, n, n).asInstanceOf[DenseColumnDoubleMatrix2D]
          .getRowMajor
      } else {
        R.asInstanceOf[DenseColumnDoubleMatrix2D].getRowMajor
      }
    } else {
      if (economySize) {
        R.viewPart(0, 0, n, n).asInstanceOf[DenseColumnDoubleMatrix2D]
          .copy()
      } else {
        R.copy()
      }
    }
  }

  /**
   * Returns whether the matrix <tt>A</tt> has full rank.
   *
   * @return true if <tt>R</tt>, and hence <tt>A</tt>, has full rank.
   */
  def hasFullRank(): Boolean = {
    (0 until n).find(elementsA(_ * m + _) == 0).map(false)
      .getOrElse(true)
  }

  /**
   * Least squares solution of <tt>A*x = b</tt> (in-place). Upon return
   * <tt>b</tt> is overridden with the result <tt>x</tt>.
   *
   * @param b
   *            right-hand side.
   * @exception IllegalArgumentException
   *                if <tt>b.size() != A.rows()</tt>.
   * @exception IllegalArgumentException
   *                if <tt>!this.hasFullRank()</tt> (<tt>A</tt> is rank
   *                deficient).
   */
  def solve(b: StrideMatrix1D) {
    DoubleProperty.DEFAULT.checkDense(b)
    if (b.size != m) {
      throw new IllegalArgumentException("Matrix row dimensions must agree.")
    }
    if (!this.hasFullRank()) {
      throw new IllegalArgumentException("Matrix is rank deficient.")
    }
    var elementsX: Array[Double] = null
    elementsX = if (b.isView) b.copy().elements().asInstanceOf[Array[Double]] else b.elements().asInstanceOf[Array[Double]]
    Dplasma.plasma_Init(m, n, 1)
    var info = Dplasma.plasma_DORMQR(Dplasma.PlasmaLeft, Dplasma.PlasmaNoTrans, m, 1, n, elementsA, 0,
      m, T, 0, elementsX, 0, m)
    if (info != 0) {
      throw new IllegalArgumentException("Error occured while solving the system of equation using QR decomposition: " +
        info)
    }
    info = Dplasma.plasma_DTRSM(Dplasma.PlasmaLeft, Dplasma.PlasmaUpper, Dplasma.PlasmaNoTrans, Dplasma.PlasmaNonUnit,
      n, 1, elementsA, 0, m, elementsX, 0, m)
    Dplasma.plasma_Finalize()
    if (info != 0) {
      throw new IllegalArgumentException("Error occured while solving the system of equation using QR decomposition: " +
        info)
    }
    if (b.isView) {
      b.assign(elementsX)
    }
  }

  /**
   * Least squares solution of <tt>A*X = B</tt>(in-place). Upon return
   * <tt>B</tt> is overridden with the result <tt>X</tt>.
   *
   * @param B
   *            A matrix with as many rows as <tt>A</tt> and any number of
   *            columns.
   * @exception IllegalArgumentException
   *                if <tt>B.rows() != A.rows()</tt>.
   * @exception IllegalArgumentException
   *                if <tt>!this.hasFullRank()</tt> (<tt>A</tt> is rank
   *                deficient).
   */
  def solve(B: StrideMatrix2D) {
    if (B.rows() != m) {
      throw new IllegalArgumentException("Matrix row dimensions must agree.")
    }
    if (!this.hasFullRank()) {
      throw new IllegalArgumentException("Matrix is rank deficient.")
    }
    DoubleProperty.DEFAULT.checkDense(B)
    var elementsX: Array[Double] = null
    elementsX = if (B.isInstanceOf[DenseMatrix2D]) B.viewDice().copy().elements().asInstanceOf[Array[Double]] else if (B.isView) B.copy().elements().asInstanceOf[Array[Double]] else B.elements().asInstanceOf[Array[Double]]
    val nrhs = B.columns()
    Dplasma.plasma_Init(m, n, nrhs)
    var info = Dplasma.plasma_DORMQR(Dplasma.PlasmaLeft, Dplasma.PlasmaNoTrans, m, nrhs, n, elementsA,
      0, m, T, 0, elementsX, 0, m)
    if (info != 0) {
      throw new IllegalArgumentException("Error occured while solving the system of equation using QR decomposition: " +
        info)
    }
    info = Dplasma.plasma_DTRSM(Dplasma.PlasmaLeft, Dplasma.PlasmaUpper, Dplasma.PlasmaNoTrans, Dplasma.PlasmaNonUnit,
      n, nrhs, elementsA, 0, m, elementsX, 0, m)
    Dplasma.plasma_Finalize()
    if (info != 0) {
      throw new IllegalArgumentException("Error occured while solving the system of equation using QR decomposition: " +
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
    buf.append("-----------------------------------------------------------------\n")
    buf.append("QRDecomposition(A) --> hasFullRank(A), Q, R, pseudo inverse(A)\n")
    buf.append("-----------------------------------------------------------------\n")
    buf.append("hasFullRank = ")
    try {
      buf.append(String.valueOf(this.hasFullRank()))
    } catch {
      case exc: IllegalArgumentException => buf.append(unknown + exc.getMessage)
    }
    buf.append("\n\nQ = ")
    try {
      buf.append(String.valueOf(this.getQ(false)))
    } catch {
      case exc: IllegalArgumentException => buf.append(unknown + exc.getMessage)
    }
    buf.append("\n\nR = ")
    try {
      buf.append(String.valueOf(this.getR(false)))
    } catch {
      case exc: IllegalArgumentException => buf.append(unknown + exc.getMessage)
    }
    buf.append("\n\npseudo inverse(A) = ")
    try {
      val X = cern.colt.matrix.tdouble.DoubleFactory2D.dense.identity(m)
      this.solve(X)
      buf.append(String.valueOf(X))
    } catch {
      case exc: IllegalArgumentException => buf.append(unknown + exc.getMessage)
    }
    buf.toString
  }
}
