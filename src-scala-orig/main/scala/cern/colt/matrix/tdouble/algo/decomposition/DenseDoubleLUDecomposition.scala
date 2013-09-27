package cern.colt.matrix.tdouble.algo.decomposition

import cern.colt.matrix.tdouble.StrideMatrix1D
import cern.colt.matrix.tdouble.StrideMatrix2D
//remove if not needed
import scala.collection.JavaConversions._

/**
 * For an <tt>m x n</tt> matrix <tt>A</tt> with <tt>m >= n</tt>, the LU
 * decomposition is an <tt>m x n</tt> unit lower triangular matrix <tt>L</tt>,
 * an <tt>n x n</tt> upper triangular matrix <tt>U</tt>, and a permutation
 * vector <tt>piv</tt> of length <tt>m</tt> so that <tt>A(piv,:) = L*U</tt>; If
 * <tt>m < n</tt>, then <tt>L</tt> is <tt>m x m</tt> and <tt>U</tt> is
 * <tt>m x n</tt>.
 * <P>
 * The LU decomposition with pivoting always exists, even if the matrix is
 * singular, so the constructor will never fail. The primary use of the LU
 * decomposition is in the solution of square systems of simultaneous linear
 * equations. This will fail if <tt>isNonsingular()</tt> returns false.
 */
@SerialVersionUID(1020)
class DenseDoubleLUDecomposition(A: StrideMatrix2D) extends java.io.Serializable {

  protected var quick: DenseDoubleLUDecompositionQuick = new DenseDoubleLUDecompositionQuick(0)

  quick.decompose(A.copy())

  /**
   * Returns the determinant, <tt>det(A)</tt>.
   *
   * @exception IllegalArgumentException
   *                Matrix must be square
   */
  def det(): Double = quick.det()

  /**
   * Returns the lower triangular factor, <tt>L</tt>.
   *
   * @return <tt>L</tt>
   */
  def getL(): StrideMatrix2D = quick.getL

  /**
   * Returns a copy of the pivot permutation vector.
   *
   * @return piv
   */
  def getPivot(): Array[Int] = quick.getPivot.clone()

  /**
   * Returns the upper triangular factor, <tt>U</tt>.
   *
   * @return <tt>U</tt>
   */
  def getU(): StrideMatrix2D = quick.getU

  /**
   * Returns whether the matrix is nonsingular (has an inverse).
   *
   * @return true if <tt>U</tt>, and hence <tt>A</tt>, is nonsingular; false
   *         otherwise.
   */
  def isNonsingular(): Boolean = quick.isNonsingular

  /**
   * Solves <tt>A*X = B</tt>.
   *
   * @param B
   *            A matrix with as many rows as <tt>A</tt> and any number of
   *            columns.
   * @return <tt>X</tt> so that <tt>L*U*X = B(piv)</tt>.
   * @exception IllegalArgumentException
   *                if </tt>B.rows() != A.rows()</tt>.
   * @exception IllegalArgumentException
   *                if A is singular, that is, if
   *                <tt>!this.isNonsingular()</tt>.
   * @exception IllegalArgumentException
   *                if <tt>A.rows() < A.columns()</tt>.
   */
  def solve(B: StrideMatrix2D): StrideMatrix2D = {
    val X = B.copy()
    quick.solve(X)
    X
  }

  /**
   * Solves <tt>A*x = b</tt>.
   *
   * @param b
   *            A vector of size <tt>A.rows()</tt>
   * @return <tt>x</tt> so that <tt>L*U*x = b(piv)</tt>.
   * @exception IllegalArgumentException
   *                if </tt>b.size() != A.rows()</tt>.
   * @exception IllegalArgumentException
   *                if A is singular, that is, if
   *                <tt>!this.isNonsingular()</tt>.
   * @exception IllegalArgumentException
   *                if <tt>A.rows() < A.columns()</tt>.
   */
  def solve(b: StrideMatrix1D): StrideMatrix1D = {
    val x = b.copy()
    quick.solve(x)
    x
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
  override def toString(): String = quick.toString
}
