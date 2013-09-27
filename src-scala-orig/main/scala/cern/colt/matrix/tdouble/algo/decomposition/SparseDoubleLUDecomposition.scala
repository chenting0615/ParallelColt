package cern.colt.matrix.tdouble.algo.decomposition

import cern.colt.matrix.tdouble.StrideMatrix1D
import cern.colt.matrix.tdouble.StrideMatrix2D
import cern.colt.matrix.tdouble.algo.DoubleProperty
import cern.colt.matrix.tdouble.impl.SparseCCDoubleMatrix2D
import cern.colt.matrix.tdouble.impl.SparseRCDoubleMatrix2D
import edu.emory.mathcs.csparsej.tdouble.Dcs_dmperm
import edu.emory.mathcs.csparsej.tdouble.Dcs_ipvec
import edu.emory.mathcs.csparsej.tdouble.Dcs_lsolve
import edu.emory.mathcs.csparsej.tdouble.Dcs_lu
import edu.emory.mathcs.csparsej.tdouble.Dcs_sqr
import edu.emory.mathcs.csparsej.tdouble.Dcs_usolve
import edu.emory.mathcs.csparsej.tdouble.Dcs_common.Dcs
import edu.emory.mathcs.csparsej.tdouble.Dcs_common.Dcsd
import edu.emory.mathcs.csparsej.tdouble.Dcs_common.Dcsn
import edu.emory.mathcs.csparsej.tdouble.Dcs_common.Dcss
//remove if not needed
import scala.collection.JavaConversions._

/**
 * For a square matrix <tt>A</tt>, the LU decomposition is an unit lower
 * triangular matrix <tt>L</tt>, an upper triangular matrix <tt>U</tt>, and a
 * permutation vector <tt>piv</tt> so that <tt>A(piv,:) = L*U</tt>
 * <P>
 * The LU decomposition with pivoting always exists, even if the matrix is
 * singular. The primary use of the LU decomposition is in the solution of
 * square systems of simultaneous linear equations. This will fail if
 * <tt>isNonsingular()</tt> returns false.
 *
 * @author Piotr Wendykier (piotr.wendykier@gmail.com)
 */
class SparseDoubleLUDecomposition(A: StrideMatrix2D, order: Int, checkIfSingular: Boolean)
    {

  private var S: Dcss = Dcs_sqr.cs_sqr(order, dcs, false)

  private var N: Dcsn = Dcs_lu.cs_lu(dcs, S, 1)

  private var L: StrideMatrix2D = _

  private var U: StrideMatrix2D = _

  private var rcMatrix: Boolean = false

  private var isNonSingular: Boolean = true

  /**
   * Row and column dimension (square matrix).
   */
  private var n: Int = A.rows()

  DoubleProperty.DEFAULT.checkSquare(A)

  DoubleProperty.DEFAULT.checkSparse(A)

  if (order < 0 || order > 3) {
    throw new IllegalArgumentException("order must be a number between 0 and 3")
  }

  var dcs: Dcs = null

  if (A.isInstanceOf[SparseRCDoubleMatrix2D]) {
    rcMatrix = true
    dcs = A.asInstanceOf[SparseRCDoubleMatrix2D].getColumnCompressed
      .elements()
  } else {
    dcs = A.elements().asInstanceOf[Dcs]
  }

  if (S == null) {
    throw new IllegalArgumentException("Exception occured in cs_sqr()")
  }

  if (N == null) {
    throw new IllegalArgumentException("Exception occured in cs_lu()")
  }

  if (checkIfSingular) {
    val D = Dcs_dmperm.cs_dmperm(dcs, 1)
    if (D != null && D.rr(3) < n) {
      isNonSingular = false
    }
  }

  /**
   * Returns the determinant, <tt>det(A)</tt>.
   *
   */
  def det(): Double = {
    if (!isNonsingular) return 0
    var pivsign = 1
    for (i <- 0 until n if N.pinv(i) != i) {
      pivsign = -pivsign
    }
    if (U == null) {
      U = new SparseCCDoubleMatrix2D(N.U)
      if (rcMatrix) {
        U = U.asInstanceOf[SparseCCDoubleMatrix2D].getRowCompressed
      }
    }
    var det = pivsign
    for (j <- 0 until n) {
      det *= U.getQuick(j, j)
    }
    det
  }

  /**
   * Returns the lower triangular factor, <tt>L</tt>.
   *
   * @return <tt>L</tt>
   */
  def getL(): StrideMatrix2D = {
    if (L == null) {
      L = new SparseCCDoubleMatrix2D(N.L)
      if (rcMatrix) {
        L = L.asInstanceOf[SparseCCDoubleMatrix2D].getRowCompressed
      }
    }
    L.copy()
  }

  /**
   * Returns a copy of the pivot permutation vector.
   *
   * @return piv
   */
  def getPivot(): Array[Int] = {
    if (N.pinv == null) return null
    val pinv = Array.ofDim[Int](N.pinv.length)
    System.arraycopy(N.pinv, 0, pinv, 0, pinv.length)
    pinv
  }

  /**
   * Returns the upper triangular factor, <tt>U</tt>.
   *
   * @return <tt>U</tt>
   */
  def getU(): StrideMatrix2D = {
    if (U == null) {
      U = new SparseCCDoubleMatrix2D(N.U)
      if (rcMatrix) {
        U = U.asInstanceOf[SparseCCDoubleMatrix2D].getRowCompressed
      }
    }
    U.copy()
  }

  /**
   * Returns a copy of the symbolic LU analysis object
   *
   * @return symbolic LU analysis
   */
  def getSymbolicAnalysis(): Dcss = {
    val S2 = new Dcss()
    S2.cp = if (S.cp != null) S.cp.clone() else null
    S2.leftmost = if (S.leftmost != null) S.leftmost.clone() else null
    S2.lnz = S.lnz
    S2.m2 = S.m2
    S2.parent = if (S.parent != null) S.parent.clone() else null
    S2.pinv = if (S.pinv != null) S.pinv.clone() else null
    S2.q = if (S.q != null) S.q.clone() else null
    S2.unz = S.unz
    S2
  }

  /**
   * Returns whether the matrix is nonsingular (has an inverse).
   *
   * @return true if <tt>U</tt>, and hence <tt>A</tt>, is nonsingular; false
   *         otherwise.
   */
  def isNonsingular(): Boolean = isNonSingular

  /**
   * Solves <tt>A*x = b</tt>(in-place). Upon return <tt>b</tt> is overridden
   * with the result <tt>x</tt>.
   *
   * @param b
   *            A vector with of size A.rows();
   * @exception IllegalArgumentException
   *                if <tt>b.size() != A.rows()</tt> or if A is singular.
   */
  def solve(b: StrideMatrix1D) {
    if (b.size != n) {
      throw new IllegalArgumentException("b.size() != A.rows()")
    }
    if (!isNonsingular) {
      throw new IllegalArgumentException("A is singular")
    }
    DoubleProperty.DEFAULT.checkDense(b)
    val y = Array.ofDim[Double](n)
    var x: Array[Double] = null
    x = if (b.isView) b.copy().elements().asInstanceOf[Array[Double]] else b.elements().asInstanceOf[Array[Double]]
    Dcs_ipvec.cs_ipvec(N.pinv, x, y, n)
    Dcs_lsolve.cs_lsolve(N.L, y)
    Dcs_usolve.cs_usolve(N.U, y)
    Dcs_ipvec.cs_ipvec(S.q, y, x, n)
    if (b.isView) {
      b.assign(x)
    }
  }
}
