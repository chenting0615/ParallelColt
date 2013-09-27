package cern.colt.matrix.tdouble.algo.decomposition

import cern.colt.matrix.tdouble.StrideMatrix1D
import cern.colt.matrix.tdouble.StrideMatrix2D
import cern.colt.matrix.tdouble.algo.DenseDoubleAlgebra
import cern.colt.matrix.tdouble.algo.DoubleProperty
//remove if not needed
import scala.collection.JavaConversions._

/**
 * A low level version of {@link DenseDoubleLUDecomposition}, avoiding
 * unnecessary memory allocation and copying. The input to <tt>decompose</tt>
 * methods is overriden with the result (LU). The input to <tt>solve</tt>
 * methods is overriden with the result (X). In addition to
 * <tt>LUDecomposition</tt>, this class also includes a faster variant of the
 * decomposition, specialized for tridiagonal (and hence also diagonal)
 * matrices, as well as a solver tuned for vectors. Its disadvantage is that it
 * is a bit more difficult to use than <tt>LUDecomposition</tt>. Thus, you may
 * want to disregard this class and come back later, if a need for speed arises.
 * <p>
 * An instance of this class remembers the result of its last decomposition.
 * Usage pattern is as follows: Create an instance of this class, call a
 * decompose method, then retrieve the decompositions, determinant, and/or solve
 * as many equation problems as needed. Once another matrix needs to be
 * LU-decomposed, you need not create a new instance of this class. Start again
 * by calling a decompose method, then retrieve the decomposition and/or solve
 * your equations, and so on. In case a <tt>LU</tt> matrix is already available,
 * call method <tt>setLU</tt> instead of <tt>decompose</tt> and proceed with
 * solving et al.
 * <p>
 * If a matrix shall not be overriden, use <tt>matrix.copy()</tt> and hand the
 * the copy to methods.
 * <p>
 * For an <tt>m x n</tt> matrix <tt>A</tt> with <tt>m >= n</tt>, the LU
 * decomposition is an <tt>m x n</tt> unit lower triangular matrix <tt>L</tt>,
 * an <tt>n x n</tt> upper triangular matrix <tt>U</tt>, and a permutation
 * vector <tt>piv</tt> of length <tt>m</tt> so that <tt>A(piv,:) = L*U</tt>; If
 * <tt>m < n</tt>, then <tt>L</tt> is <tt>m x m</tt> and <tt>U</tt> is
 * <tt>m x n</tt>.
 * <P>
 * The LU decomposition with pivoting always exists, even if the matrix is
 * singular, so the decompose methods will never fail. The primary use of the LU
 * decomposition is in the solution of square systems of simultaneous linear
 * equations. Attempting to solve such a system will throw an exception if
 * <tt>isNonsingular()</tt> returns false.
 * <p>
 */
@SerialVersionUID(1020)
class DenseDoubleLUDecompositionQuick(tolerance: Double) extends java.io.Serializable {

  /**
   * Array for internal storage of decomposition.
   *
   * @serial internal array storage.
   */
  protected var LU: StrideMatrix2D = _

  /**
   * pivot sign.
   *
   * @serial pivot sign.
   */
  protected var pivsign: Int = _

  /**
   * Internal storage of pivot vector.
   *
   * @serial pivot vector.
   */
  protected var piv: Array[Int] = _

  protected var isNonSingular: Boolean = _

  protected var algebra: DenseDoubleAlgebra = new DenseDoubleAlgebra(tolerance)

  @transient protected var workDouble: Array[Double] = _

  @transient protected var work1: Array[Int] = _

  @transient protected var work2: Array[Int] = _

  /**
   * Constructs and returns a new LU Decomposition object with default
   * tolerance <tt>1.0E-9</tt> for singularity detection.
   */
  def this() {
    this(DoubleProperty.DEFAULT.tolerance())
  }

  /**
   * Decomposes matrix <tt>A</tt> into <tt>L</tt> and <tt>U</tt> (in-place).
   * Upon return <tt>A</tt> is overridden with the result <tt>LU</tt>, such
   * that <tt>L*U = A</tt>. Uses a "left-looking", dot-product,
   * Crout/Doolittle algorithm.
   *
   * @param A
   *            any matrix.
   */
  def decompose(A: StrideMatrix2D) {
    val CUT_OFF = 10
    LU = A
    val m = A.rows()
    val n = A.columns()
    if (this.piv == null || this.piv.length != m) this.piv = Array.ofDim[Int](m)
    var i = m
    while (i >= 0) piv(i) = i
    pivsign = 1
    if (m * n == 0) {
      setLU(LU)
      return
    }
    val LUrows = Array.ofDim[StrideMatrix1D](m)
    for (i <- 0 until m) LUrows(i) = LU.viewRow(i)
    val nonZeroIndexes = new cern.colt.list.tint.IntArrayList()
    val LUcolj = LU.viewColumn(0).like()
    val multFunction = cern.jet.math.tdouble.DoubleFunctions.mult(0)
    for (j <- 0 until n) {
      LUcolj.assign(LU.viewColumn(j))
      val maxCardinality = m / CUT_OFF
      LUcolj.getNonZeros(nonZeroIndexes, null, maxCardinality)
      val cardinality = nonZeroIndexes.size
      val sparse = (cardinality < maxCardinality)
      for (i <- 0 until m) {
        val kmax = Math.min(i, j)
        var s: Double = 0.0
        s = if (sparse) LUrows(i).zDotProduct(LUcolj, 0, kmax, nonZeroIndexes) else LUrows(i).zDotProduct(LUcolj,
          0, kmax)
        val before = LUcolj.getQuick(i)
        val after = before - s
        LUcolj.setQuick(i, after)
        LU.setQuick(i, j, after)
        if (sparse) {
          if (before == 0 && after != 0) {
            var pos = nonZeroIndexes.binarySearch(i)
            pos = -pos - 1
            nonZeroIndexes.beforeInsert(pos, i)
          }
          if (before != 0 && after == 0) {
            nonZeroIndexes.remove(nonZeroIndexes.binarySearch(i))
          }
        }
      }
      var p = j
      if (p < m) {
        var max = Math.abs(LUcolj.getQuick(p))
        for (i <- j + 1 until m) {
          val v = Math.abs(LUcolj.getQuick(i))
          if (v > max) {
            p = i
            max = v
          }
        }
      }
      if (p != j) {
        LUrows(p).swap(LUrows(j))
        val k = piv(p)
        piv(p) = piv(j)
        piv(j) = k
        pivsign = -pivsign
      }
      var jj: Double = 0.0
      if (j < m && (jj = LU.getQuick(j, j)) != 0.0) {
        multFunction.multiplicator = 1 / jj
        LU.viewColumn(j).viewPart(j + 1, m - (j + 1)).assign(multFunction)
      }
    }
    setLU(LU)
  }

  /**
   * Decomposes the banded and square matrix <tt>A</tt> into <tt>L</tt> and
   * <tt>U</tt> (in-place). Upon return <tt>A</tt> is overridden with the
   * result <tt>LU</tt>, such that <tt>L*U = A</tt>. Currently supports
   * diagonal and tridiagonal matrices, all other cases fall through to
   * {@link #decompose(DoubleMatrix2D)}.
   *
   * @param semiBandwidth
   *            == 1 --> A is diagonal, == 2 --> A is tridiagonal.
   * @param A
   *            any matrix.
   */
  def decompose(A: StrideMatrix2D, semiBandwidth: Int) {
    if (!algebra.property().isSquare(A) || semiBandwidth < 0 ||
      semiBandwidth > 2) {
      decompose(A)
      return
    }
    LU = A
    val m = A.rows()
    val n = A.columns()
    if (this.piv == null || this.piv.length != m) this.piv = Array.ofDim[Int](m)
    var i = m
    while (i >= 0) piv(i) = i
    pivsign = 1
    if (m * n == 0) {
      setLU(A)
      return
    }
    if (semiBandwidth == 2) {
      if (n > 1) A.setQuick(1, 0, A.getQuick(1, 0) / A.getQuick(0, 0))
      for (i <- 1 until n) {
        val ei = A.getQuick(i, i) - A.getQuick(i, i - 1) * A.getQuick(i - 1, i)
        A.setQuick(i, i, ei)
        if (i < n - 1) A.setQuick(i + 1, i, A.getQuick(i + 1, i) / ei)
      }
    }
    setLU(A)
  }

  /**
   * Returns the determinant, <tt>det(A)</tt>.
   *
   * @exception IllegalArgumentException
   *                if <tt>A.rows() != A.columns()</tt> (Matrix must be
   *                square).
   */
  def det(): Double = {
    val m = m()
    val n = n()
    if (m != n) throw new IllegalArgumentException("Matrix must be square.")
    if (!isNonsingular) return 0
    var det = pivsign
    for (j <- 0 until n) {
      det *= LU.getQuick(j, j)
    }
    det
  }

  /**
   * Returns pivot permutation vector as a one-dimensional double array
   *
   * @return (double) piv
   */
  protected def getDoublePivot(): Array[Double] = {
    val m = m()
    val vals = Array.ofDim[Double](m)
    for (i <- 0 until m) {
      vals(i) = piv(i)
    }
    vals
  }

  /**
   * Returns the lower triangular factor, <tt>L</tt>.
   *
   * @return <tt>L</tt>
   */
  def getL(): StrideMatrix2D = lowerTriangular(LU.copy())

  /**
   * Returns a copy of the combined lower and upper triangular factor,
   * <tt>LU</tt>.
   *
   * @return <tt>LU</tt>
   */
  def getLU(): StrideMatrix2D = LU.copy()

  /**
   * Returns the pivot permutation vector (not a copy of it).
   *
   * @return piv
   */
  def getPivot(): Array[Int] = piv

  /**
   * Returns the upper triangular factor, <tt>U</tt>.
   *
   * @return <tt>U</tt>
   */
  def getU(): StrideMatrix2D = upperTriangular(LU.copy())

  /**
   * Returns whether the matrix is nonsingular (has an inverse).
   *
   * @return true if <tt>U</tt>, and hence <tt>A</tt>, is nonsingular; false
   *         otherwise.
   */
  def isNonsingular(): Boolean = isNonSingular

  /**
   * Returns whether the matrix is nonsingular.
   *
   * @return true if <tt>matrix</tt> is nonsingular; false otherwise.
   */
  protected def isNonsingular(matrix: StrideMatrix2D): Boolean = {
    val m = matrix.rows()
    val n = matrix.columns()
    val epsilon = algebra.property().tolerance()
    var j = Math.min(n, m)
    while (j >= 0) {
      if (Math.abs(matrix.getQuick(j, j)) <= epsilon) return false
    }
    true
  }

  /**
   * Modifies the matrix to be a lower triangular matrix.
   * <p>
   * <b>Examples:</b>
   * <table border="0">
   * <tr nowrap>
   * <td valign="top">3 x 5 matrix:<br>
   * 9, 9, 9, 9, 9<br>
   * 9, 9, 9, 9, 9<br>
   * 9, 9, 9, 9, 9</td>
   * <td align="center">triang.Upper<br>
   * ==></td>
   * <td valign="top">3 x 5 matrix:<br>
   * 9, 9, 9, 9, 9<br>
   * 0, 9, 9, 9, 9<br>
   * 0, 0, 9, 9, 9</td>
   * </tr>
   * <tr nowrap>
   * <td valign="top">5 x 3 matrix:<br>
   * 9, 9, 9<br>
   * 9, 9, 9<br>
   * 9, 9, 9<br>
   * 9, 9, 9<br>
   * 9, 9, 9</td>
   * <td align="center">triang.Upper<br>
   * ==></td>
   * <td valign="top">5 x 3 matrix:<br>
   * 9, 9, 9<br>
   * 0, 9, 9<br>
   * 0, 0, 9<br>
   * 0, 0, 0<br>
   * 0, 0, 0</td>
   * </tr>
   * <tr nowrap>
   * <td valign="top">3 x 5 matrix:<br>
   * 9, 9, 9, 9, 9<br>
   * 9, 9, 9, 9, 9<br>
   * 9, 9, 9, 9, 9</td>
   * <td align="center">triang.Lower<br>
   * ==></td>
   * <td valign="top">3 x 5 matrix:<br>
   * 1, 0, 0, 0, 0<br>
   * 9, 1, 0, 0, 0<br>
   * 9, 9, 1, 0, 0</td>
   * </tr>
   * <tr nowrap>
   * <td valign="top">5 x 3 matrix:<br>
   * 9, 9, 9<br>
   * 9, 9, 9<br>
   * 9, 9, 9<br>
   * 9, 9, 9<br>
   * 9, 9, 9</td>
   * <td align="center">triang.Lower<br>
   * ==></td>
   * <td valign="top">5 x 3 matrix:<br>
   * 1, 0, 0<br>
   * 9, 1, 0<br>
   * 9, 9, 1<br>
   * 9, 9, 9<br>
   * 9, 9, 9</td>
   * </tr>
   * </table>
   *
   * @return <tt>A</tt> (for convenience only).
   * @see #triangulateUpper(DoubleMatrix2D)
   */
  protected def lowerTriangular(A: StrideMatrix2D): StrideMatrix2D = {
    val rows = A.rows()
    val columns = A.columns()
    val min = Math.min(rows, columns)
    var r = min
    while (r >= 0) {
      var c = min
      while (c >= 0) {
        if (r < c) A.setQuick(r, c, 0) else if (r == c) A.setQuick(r, c, 1)
      }
    }
    if (columns > rows) A.viewPart(0, min, rows, columns - min).assign(0)
    A
  }

  /**
   *
   */
  protected def m(): Int = LU.rows()

  /**
   *
   */
  protected def n(): Int = LU.columns()

  /**
   * Sets the combined lower and upper triangular factor, <tt>LU</tt>. The
   * parameter is not checked; make sure it is indeed a proper LU
   * decomposition.
   */
  def setLU(LU: StrideMatrix2D) {
    this.LU = LU
    this.isNonSingular = isNonsingular(LU)
  }

  /**
   * Solves the system of equations <tt>A*X = B</tt> (in-place). Upon return
   * <tt>B</tt> is overridden with the result <tt>X</tt>, such that
   * <tt>L*U*X = B(piv)</tt>.
   *
   * @param B
   *            A vector with <tt>B.size() == A.rows()</tt>.
   * @exception IllegalArgumentException
   *                if </tt>B.size() != A.rows()</tt>.
   * @exception IllegalArgumentException
   *                if A is singular, that is, if <tt>!isNonsingular()</tt>.
   * @exception IllegalArgumentException
   *                if <tt>A.rows() < A.columns()</tt>.
   */
  def solve(B: StrideMatrix1D) {
    algebra.property().checkRectangular(LU)
    val m = m()
    val n = n()
    if (B.size != m) throw new IllegalArgumentException("Matrix dimensions must agree.")
    if (!this.isNonsingular) throw new IllegalArgumentException("Matrix is singular.")
    if (this.workDouble == null || this.workDouble.length < m) this.workDouble = Array.ofDim[Double](m)
    algebra.permute(B, this.piv, this.workDouble)
    if (m * n == 0) return
    for (k <- 0 until n) {
      val f = B.getQuick(k)
      if (f != 0) {
        for (i <- k + 1 until n) {
          val v = LU.getQuick(i, k)
          if (v != 0) B.setQuick(i, B.getQuick(i) - f * v)
        }
      }
    }
    var k = n - 1
    while (k >= 0) {
      B.setQuick(k, B.getQuick(k) / LU.getQuick(k, k))
      val f = B.getQuick(k)
      if (f != 0) {
        for (i <- 0 until k) {
          val v = LU.getQuick(i, k)
          if (v != 0) B.setQuick(i, B.getQuick(i) - f * v)
        }
      }
      k -= 1
    }
  }

  /**
   * Solves the system of equations <tt>A*X = B</tt> (in-place). Upon return
   * <tt>B</tt> is overridden with the result <tt>X</tt>, such that
   * <tt>L*U*X = B(piv,:)</tt>.
   *
   * @param B
   *            A matrix with as many rows as <tt>A</tt> and any number of
   *            columns.
   * @exception IllegalArgumentException
   *                if </tt>B.rows() != A.rows()</tt>.
   * @exception IllegalArgumentException
   *                if A is singular, that is, if <tt>!isNonsingular()</tt>.
   * @exception IllegalArgumentException
   *                if <tt>A.rows() < A.columns()</tt>.
   */
  def solve(B: StrideMatrix2D) {
    val CUT_OFF = 10
    algebra.property().checkRectangular(LU)
    val m = m()
    val n = n()
    if (B.rows() != m) throw new IllegalArgumentException("Matrix row dimensions must agree.")
    if (!this.isNonsingular) throw new IllegalArgumentException("Matrix is singular.")
    if (this.work1 == null || this.work1.length < m) this.work1 = Array.ofDim[Int](m)
    algebra.permuteRows(B, this.piv, this.work1)
    if (m * n == 0) return
    val nx = B.columns()
    val Brows = Array.ofDim[StrideMatrix1D](n)
    for (k <- 0 until n) Brows(k) = B.viewRow(k)
    val div = cern.jet.math.tdouble.DoubleFunctions.div(0)
    val minusMult = cern.jet.math.tdouble.DoublePlusMultSecond.minusMult(0)
    val nonZeroIndexes = new cern.colt.list.tint.IntArrayList()
    var Browk = cern.colt.matrix.tdouble.DoubleFactory1D.dense.make(nx)
    for (k <- 0 until n) {
      Browk.assign(Brows(k))
      val maxCardinality = nx / CUT_OFF
      Browk.getNonZeros(nonZeroIndexes, null, maxCardinality)
      val cardinality = nonZeroIndexes.size
      val sparse = (cardinality < maxCardinality)
      for (i <- k + 1 until n) {
        minusMult.multiplicator = -LU.getQuick(i, k)
        if (minusMult.multiplicator != 0) {
          if (sparse) {
            Brows(i).assign(Browk, minusMult, nonZeroIndexes)
          } else {
            Brows(i).assign(Browk, minusMult)
          }
        }
      }
    }
    var k = n - 1
    while (k >= 0) {
      div.multiplicator = 1 / LU.getQuick(k, k)
      Brows(k).assign(div)
      if (Browk == null) Browk = cern.colt.matrix.tdouble.DoubleFactory1D.dense.make(B.columns())
      Browk.assign(Brows(k))
      val maxCardinality = nx / CUT_OFF
      Browk.getNonZeros(nonZeroIndexes, null, maxCardinality)
      val cardinality = nonZeroIndexes.size
      val sparse = (cardinality < maxCardinality)
      for (i <- 0 until k) {
        minusMult.multiplicator = -LU.getQuick(i, k)
        if (minusMult.multiplicator != 0) {
          if (sparse) {
            Brows(i).assign(Browk, minusMult, nonZeroIndexes)
          } else {
            Brows(i).assign(Browk, minusMult)
          }
        }
      }
      k -= 1
    }
  }

  /**
   * Solves <tt>A*X = B</tt>.
   *
   * @param B
   *            A matrix with as many rows as <tt>A</tt> and any number of
   *            columns.
   * @return <tt>X</tt> so that <tt>L*U*X = B(piv,:)</tt>.
   * @exception IllegalArgumentException
   *                if </tt>B.rows() != A.rows()</tt>.
   * @exception IllegalArgumentException
   *                if A is singular, that is, if
   *                <tt>!this.isNonsingular()</tt>.
   * @exception IllegalArgumentException
   *                if <tt>A.rows() < A.columns()</tt>.
   */
  private def solveOld(B: StrideMatrix2D) {
    algebra.property().checkRectangular(LU)
    val m = m()
    val n = n()
    if (B.rows() != m) throw new IllegalArgumentException("Matrix row dimensions must agree.")
    if (!this.isNonsingular) throw new IllegalArgumentException("Matrix is singular.")
    val nx = B.columns()
    if (this.work1 == null || this.work1.length < m) this.work1 = Array.ofDim[Int](m)
    algebra.permuteRows(B, this.piv, this.work1)
    for (k <- 0 until n; i <- k + 1 until n) {
      val mult = LU.getQuick(i, k)
      if (mult != 0) {
        for (j <- 0 until nx) {
          B.setQuick(i, j, B.getQuick(i, j) - B.getQuick(k, j) * mult)
        }
      }
    }
    var k = n - 1
    while (k >= 0) {
      var mult = 1 / LU.getQuick(k, k)
      if (mult != 1) {
        for (j <- 0 until nx) {
          B.setQuick(k, j, B.getQuick(k, j) * mult)
        }
      }
      for (i <- 0 until k) {
        mult = LU.getQuick(i, k)
        if (mult != 0) {
          for (j <- 0 until nx) {
            B.setQuick(i, j, B.getQuick(i, j) - B.getQuick(k, j) * mult)
          }
        }
      }
      k -= 1
    }
  }

  /**
   * Returns a String with (propertyName, propertyValue) pairs. Useful for
   * debugging or to quickly get the rough picture. For example,
   *
   * <pre>
   * 	 rank          : 3
   * 	 trace         : 0
   *
   * </pre>
   */
  override def toString(): String = {
    val buf = new StringBuffer()
    val unknown = "Illegal operation or error: "
    buf.append("-----------------------------------------------------------------------------\n")
    buf.append("LUDecompositionQuick(A) --> isNonSingular(A), det(A), pivot, L, U, inverse(A)\n")
    buf.append("-----------------------------------------------------------------------------\n")
    buf.append("isNonSingular = ")
    try {
      buf.append(String.valueOf(this.isNonsingular))
    } catch {
      case exc: IllegalArgumentException => buf.append(unknown + exc.getMessage)
    }
    buf.append("\ndet = ")
    try {
      buf.append(String.valueOf(this.det()))
    } catch {
      case exc: IllegalArgumentException => buf.append(unknown + exc.getMessage)
    }
    buf.append("\npivot = ")
    try {
      buf.append(String.valueOf(new cern.colt.list.tint.IntArrayList(this.getPivot)))
    } catch {
      case exc: IllegalArgumentException => buf.append(unknown + exc.getMessage)
    }
    buf.append("\n\nL = ")
    try {
      buf.append(String.valueOf(this.getL))
    } catch {
      case exc: IllegalArgumentException => buf.append(unknown + exc.getMessage)
    }
    buf.append("\n\nU = ")
    try {
      buf.append(String.valueOf(this.getU))
    } catch {
      case exc: IllegalArgumentException => buf.append(unknown + exc.getMessage)
    }
    buf.append("\n\ninverse(A) = ")
    val identity = cern.colt.matrix.tdouble.DoubleFactory2D.dense.identity(LU.rows())
    try {
      this.solve(identity)
      buf.append(String.valueOf(identity))
    } catch {
      case exc: IllegalArgumentException => buf.append(unknown + exc.getMessage)
    }
    buf.toString
  }

  /**
   * Modifies the matrix to be an upper triangular matrix.
   *
   * @return <tt>A</tt> (for convenience only).
   * @see #triangulateLower(DoubleMatrix2D)
   */
  protected def upperTriangular(A: StrideMatrix2D): StrideMatrix2D = {
    val rows = A.rows()
    val columns = A.columns()
    val min = Math.min(rows, columns)
    var r = min
    while (r >= 0) {
      var c = min
      while (c >= 0) {
        if (r > c) A.setQuick(r, c, 0)
      }
    }
    if (columns < rows) A.viewPart(min, 0, rows - min, columns).assign(0)
    A
  }

  /**
   * Returns pivot permutation vector as a one-dimensional double array
   *
   * @return (double) piv
   */
  private def xgetDoublePivot(): Array[Double] = {
    val m = m()
    val vals = Array.ofDim[Double](m)
    for (i <- 0 until m) {
      vals(i) = piv(i)
    }
    vals
  }
}
