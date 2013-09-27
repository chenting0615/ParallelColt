package cern.colt.matrix.tdouble.algo.decomposition

import cern.colt.matrix.tdouble.StrideMatrix1D
import cern.colt.matrix.tdouble.StrideMatrix2D
import cern.colt.matrix.tdouble.algo.DoubleProperty
import cern.colt.matrix.tdouble.impl.SparseCCDoubleMatrix2D
import cern.colt.matrix.tdouble.impl.SparseRCDoubleMatrix2D
import edu.emory.mathcs.csparsej.tdouble.Dcs_happly
import edu.emory.mathcs.csparsej.tdouble.Dcs_ipvec
import edu.emory.mathcs.csparsej.tdouble.Dcs_pvec
import edu.emory.mathcs.csparsej.tdouble.Dcs_qr
import edu.emory.mathcs.csparsej.tdouble.Dcs_sqr
import edu.emory.mathcs.csparsej.tdouble.Dcs_usolve
import edu.emory.mathcs.csparsej.tdouble.Dcs_utsolve
import edu.emory.mathcs.csparsej.tdouble.Dcs_common.Dcs
import edu.emory.mathcs.csparsej.tdouble.Dcs_common.Dcsn
import edu.emory.mathcs.csparsej.tdouble.Dcs_common.Dcss
//remove if not needed
import scala.collection.JavaConversions._

/**
 * For an <tt>m x n</tt> matrix <tt>A</tt> with <tt>m >= n</tt>, the QR
 * decomposition is an <tt>m x n</tt> orthogonal matrix <tt>Q</tt> and an
 * <tt>n x n</tt> upper triangular matrix <tt>R</tt> so that <tt>A = Q*R</tt>.
 * <P>
 * The QR decompostion always exists, even if the matrix does not have full
 * rank. The primary use of the QR decomposition is in the least squares
 * solution of nonsquare systems of simultaneous linear equations. This will
 * fail if <tt>isFullRank()</tt> returns <tt>false</tt>.
 *
 * @author Piotr Wendykier (piotr.wendykier@gmail.com)
 */
class SparseDoubleQRDecomposition(A: StrideMatrix2D, order: Int) {

  private var S: Dcss = Dcs_sqr.cs_sqr(order, dcs, true)

  private var N: Dcsn = Dcs_qr.cs_qr(dcs, S)

  private var R: StrideMatrix2D = _

  private var V: StrideMatrix2D = _

  private var m: Int = A.rows()

  private var n: Int = A.columns()

  private var rcMatrix: Boolean = false

  DoubleProperty.DEFAULT.checkSparse(A)

  if (order < 0 || order > 3) {
    throw new IllegalArgumentException("order must be a number between 0 and 3")
  }

  var dcs: Dcs = null

  if (A.isInstanceOf[SparseRCDoubleMatrix2D]) {
    rcMatrix = true
    dcs = if (m >= n) A.asInstanceOf[SparseRCDoubleMatrix2D].getColumnCompressed
      .elements() else A.asInstanceOf[SparseRCDoubleMatrix2D].getColumnCompressed
      .getTranspose
      .elements()
  } else {
    dcs = if (m >= n) A.elements().asInstanceOf[Dcs] else A.asInstanceOf[SparseCCDoubleMatrix2D].getTranspose
      .elements()
  }

  if (S == null) {
    throw new IllegalArgumentException("Exception occured in cs_sqr()")
  }

  if (N == null) {
    throw new IllegalArgumentException("Exception occured in cs_qr()")
  }

  /**
   * Returns a copy of the Householder vectors v, from the Householder
   * reflections H = I - beta*v*v'.
   *
   * @return the Householder vectors.
   */
  def getV(): StrideMatrix2D = {
    if (V == null) {
      V = new SparseCCDoubleMatrix2D(N.L)
      if (rcMatrix) {
        V = V.asInstanceOf[SparseCCDoubleMatrix2D].getRowCompressed
      }
    }
    V.copy()
  }

  /**
   * Returns a copy of the beta factors, from the Householder reflections H =
   * I - beta*v*v'.
   *
   * @return the beta factors.
   */
  def getBeta(): Array[Double] = {
    if (N.B == null) {
      return null
    }
    val beta = Array.ofDim[Double](N.B.length)
    System.arraycopy(N.B, 0, beta, 0, N.B.length)
    beta
  }

  /**
   * Returns a copy of the upper triangular factor, <tt>R</tt>.
   *
   * @return <tt>R</tt>
   */
  def getR(): StrideMatrix2D = {
    if (R == null) {
      R = new SparseCCDoubleMatrix2D(N.U)
      if (rcMatrix) {
        R = R.asInstanceOf[SparseCCDoubleMatrix2D].getRowCompressed
      }
    }
    R.copy()
  }

  /**
   * Returns a copy of the symbolic QR analysis object
   *
   * @return symbolic QR analysis
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
   * Returns whether the matrix <tt>A</tt> has full rank.
   *
   * @return true if <tt>R</tt>, and hence <tt>A</tt>, has full rank.
   */
  def hasFullRank(): Boolean = {
    if (R == null) {
      R = new SparseCCDoubleMatrix2D(N.U)
      if (rcMatrix) {
        R = R.asInstanceOf[SparseCCDoubleMatrix2D].getRowCompressed
      }
    }
    val mn = Math.min(m, n)
    for (j <- 0 until mn if R.getQuick(j, j) == 0) return false
    true
  }

  /**
   * Solve a least-squares problem (min ||Ax-b||_2, where A is m-by-n with m
   * >= n) or underdetermined system (Ax=b, where m < n). Upon return
   * <tt>b</tt> is overridden with the result <tt>x</tt>.
   *
   * @param b
   *            right-hand side.
   * @exception IllegalArgumentException
   *                if <tt>b.size() != max(A.rows(), A.columns())</tt>.
   * @exception IllegalArgumentException
   *                if <tt>!this.hasFullRank()</tt> (<tt>A</tt> is rank
   *                deficient).
   */
  def solve(b: StrideMatrix1D) {
    if (b.size != Math.max(m, n)) {
      throw new IllegalArgumentException("The size b must be equal to max(A.rows(), A.columns()).")
    }
    if (!this.hasFullRank()) {
      throw new IllegalArgumentException("Matrix is rank deficient.")
    }
    var x: Array[Double] = null
    x = if (b.isView) b.copy().elements().asInstanceOf[Array[Double]] else b.elements().asInstanceOf[Array[Double]]
    if (m >= n) {
      val y = Array.ofDim[Double](if (S != null) S.m2 else 1)
      Dcs_ipvec.cs_ipvec(S.pinv, x, y, m)
      for (k <- 0 until n) {
        Dcs_happly.cs_happly(N.L, k, N.B(k), y)
      }
      Dcs_usolve.cs_usolve(N.U, y)
      Dcs_ipvec.cs_ipvec(S.q, y, x, n)
    } else {
      val y = Array.ofDim[Double](if (S != null) S.m2 else 1)
      Dcs_pvec.cs_pvec(S.q, x, y, m)
      Dcs_utsolve.cs_utsolve(N.U, y)
      var k = m - 1
      while (k >= 0) {
        Dcs_happly.cs_happly(N.L, k, N.B(k), y)
        k -= 1
      }
      Dcs_pvec.cs_pvec(S.pinv, y, x, n)
    }
  }
}
