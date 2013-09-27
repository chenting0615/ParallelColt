package cern.colt.matrix.tdouble.algo.decomposition

import cern.colt.matrix.tdouble.StrideMatrix1D
import cern.colt.matrix.tdouble.StrideMatrix2D
import cern.colt.matrix.tdouble.algo.DoubleProperty
import cern.colt.matrix.tdouble.impl.SparseCCDoubleMatrix2D
import cern.colt.matrix.tdouble.impl.SparseRCDoubleMatrix2D
import edu.emory.mathcs.csparsej.tdouble.Dcs_chol
import edu.emory.mathcs.csparsej.tdouble.Dcs_ipvec
import edu.emory.mathcs.csparsej.tdouble.Dcs_lsolve
import edu.emory.mathcs.csparsej.tdouble.Dcs_ltsolve
import edu.emory.mathcs.csparsej.tdouble.Dcs_pvec
import edu.emory.mathcs.csparsej.tdouble.Dcs_schol
import edu.emory.mathcs.csparsej.tdouble.Dcs_common.Dcs
import edu.emory.mathcs.csparsej.tdouble.Dcs_common.Dcsn
import edu.emory.mathcs.csparsej.tdouble.Dcs_common.Dcss
//remove if not needed
import scala.collection.JavaConversions._

/**
 * For a symmetric, positive definite matrix <tt>A</tt>, the Cholesky
 * decomposition is a lower triangular matrix <tt>L</tt> so that <tt>A = L*L'</tt>; If
 * the matrix is not symmetric positive definite, the IllegalArgumentException
 * is thrown.
 *
 * @author Piotr Wendykier (piotr.wendykier@gmail.com)
 */
class SparseDoubleCholeskyDecomposition(A: StrideMatrix2D, order: Int) {

  private var S: Dcss = Dcs_schol.cs_schol(order, dcs)

  private var N: Dcsn = Dcs_chol.cs_chol(dcs, S)

  private var L: StrideMatrix2D = _

  private var rcMatrix: Boolean = false

  /**
   * Row and column dimension (square matrix).
   */
  private var n: Int = A.rows()

  DoubleProperty.DEFAULT.checkSquare(A)

  DoubleProperty.DEFAULT.checkSparse(A)

  if (order < 0 || order > 1) {
    throw new IllegalArgumentException("order must be equal 0 or 1")
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
    throw new IllegalArgumentException("Exception occured in cs_schol()")
  }

  if (N == null) {
    throw new IllegalArgumentException("Matrix is not symmetric positive definite")
  }

  /**
   * Returns the triangular factor, <tt>L</tt>.
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
   *
   * Returns the triangular factor, <tt>L'</tt>.
   *
   * @return <tt>L'</tt>
   */
  def getLtranspose(): StrideMatrix2D = {
    if (L == null) {
      L = new SparseCCDoubleMatrix2D(N.L)
      if (rcMatrix) {
        L = L.asInstanceOf[SparseCCDoubleMatrix2D].getRowCompressed
      }
    }
    if (rcMatrix) {
      L.asInstanceOf[SparseRCDoubleMatrix2D].getTranspose
    } else {
      L.asInstanceOf[SparseCCDoubleMatrix2D].getTranspose
    }
  }

  /**
   * Returns a copy of the symbolic Cholesky analysis object
   *
   * @return symbolic Cholesky analysis
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
    val y = Array.ofDim[Double](n)
    var x: Array[Double] = null
    x = if (b.isView) b.copy().elements().asInstanceOf[Array[Double]] else b.elements().asInstanceOf[Array[Double]]
    Dcs_ipvec.cs_ipvec(S.pinv, x, y, n)
    Dcs_lsolve.cs_lsolve(N.L, y)
    Dcs_ltsolve.cs_ltsolve(N.L, y)
    Dcs_pvec.cs_pvec(S.pinv, y, x, n)
    if (b.isView) {
      b.assign(x)
    }
  }
}
