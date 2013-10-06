/*
Copyright (C) 1999 CERN - European Organization for Nuclear Research.
Permission to use, copy, modify, distribute and sell this software and its documentation for any purpose
is hereby granted without fee, provided that the above copyright notice appear in all copies and
that both that copyright notice and this permission notice appear in supporting documentation.
CERN makes no representations about the suitability of this software for any purpose.
It is provided "as is" without expressed or implied warranty.
 */
package cern.colt.matrix.algo

import cern.colt.matrix.{Matrix, Matrix2D}
//import cern.colt.matrix.tdouble.algo.DenseDoubleAlgebra
import cern.colt.matrix.MatrixOperators._
import cern.colt.matrix.impl.DiagonalMatrix2D
import cern.jet.math.tdouble.DoubleFunctions

/**
 * Tests matrices for linear algebraic properties (equality, tridiagonality,
 * symmetry, singularity, etc).
 * <p>
 * Except where explicitly indicated, all methods involving equality tests (
 * <tt>==</tt>) allow for numerical instability, to a degree specified upon
 * instance construction and returned by tolerance(). The public
 * static final variable <tt>DEFAULT</tt> represents a default Property object
 * with a tolerance of <tt>1.0E-9</tt>. The public static final variable
 * <tt>ZERO</tt> represents a Property object with a tolerance of <tt>0.0</tt>.
 * The public static final variable <tt>TWELVE</tt> represents a Property object
 * with a tolerance of <tt>1.0E-12</tt>. As long as you are happy with these
 * tolerances, there is no need to construct Property objects. Simply use idioms
 * like <tt>Property.DEFAULT.equals(A,B)</tt>,
 * <tt>Property.ZERO.equals(A,B)</tt>, <tt>Property.TWELVE.equals(A,B)</tt>.
 * <p>
 * To work with a different tolerance (e.g. <tt>1.0E-15</tt> or <tt>1.0E-5</tt>)
 * use the constructor and/or method setTolerance(double). Note that
 * the public static final Property objects are immutable: Is is not possible to
 * alter their tolerance. Any attempt to do so will throw an Exception.
 * <p>
 * Note that this implementation is not synchronized.
 * <p>
 * Example: <tt>equals(DoubleMatrix2D A, DoubleMatrix2D B)</tt> is defined as
 * follows
 * <table>
 * <td class="PRE">
 *
 * <pre>
 * { some other tests not related to tolerance go here }
 * double epsilon = tolerance();
 * for (int row=rows; --row &gt;= 0;) {
 * for (int column=columns; --column &gt;= 0;) {
 * //if (!(A.getQuick(row,column) == B.getQuick(row,column))) return false;
 * if (Math.abs(A.getQuick(row,column) - B.getQuick(row,column)) &gt; epsilon) return false;
 * }
 * }
 * return true;
 * </pre>
 *
 * </td>
 * </table>
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
 * @author wolfgang.hoschek@cern.ch
 * @version 1.1, 28/May/2000 (fixed strange bugs involving NaN, -inf, inf)
 *
 * @author Piotr Wendykier (piotr.wendykier@gmail.com)
 */
object DoubleProperty {

  /**
   * The default Property object; currently has <tt>tolerance()==1.0E-9</tt>.
   */
  final val DEFAULT: DoubleProperty = new DoubleProperty(1.0E-9)
  /**
   * A Property object with <tt>tolerance()==0.0</tt>.
   */
  final val ZERO: DoubleProperty = new DoubleProperty(0.0)
  /**
   * A Property object with <tt>tolerance()==1.0E-12</tt>.
   */
  final val TWELVE: DoubleProperty = new DoubleProperty(1.0E-12)
}

class DoubleProperty(tolerance_p: Double) extends cern.colt.PersistentObject {

  private var toleranceVar: Double = Math.abs(tolerance_p)

  /**
   * Checks whether the given matrix <tt>A</tt> is <i>rectangular</i>.
   *
   * @throws IllegalArgumentException
     * if <tt>A.rows() < A.columns()</tt>.
   */
  def checkRectangular(A: Matrix2D[_]) {
    if (A.rows < A.columns) {
      throw new IllegalArgumentException("Matrix must be rectangular: " + A.toShapeString)
    }
  }

  /**
   * Checks whether the given matrix <tt>A</tt> is <i>square</i>.
   *
   * @throws IllegalArgumentException
     * if <tt>A.rows() != A.columns()</tt>.
   */
  def checkSquare(A: Matrix2D[_]) {
    if (A.rows != A.columns) {
      throw new IllegalArgumentException("Matrix must be square: " + A.toShapeString)
    }
  }

  def checkDense(A: Matrix[_]) {
    if (A.isSparse) {
      throw new IllegalArgumentException("Matrix must be dense")
    }
  }

  def checkSparse(A: Matrix[_]) {
    if (! A.isSparse) {
      throw new IllegalArgumentException("Matrix must be sparse")
    }
  }

  /**
   * Returns the matrix's fraction of non-zero cells;
   * <tt>A.cardinality() / A.size()</tt>.
   */
  def density(A: Matrix[_]): Double = {
    A.numNonZero / A.size.asInstanceOf[Double]
  }

  /**
   * Returns whether all cells of the given matrix <tt>A</tt> are equal to the
   * given value. The result is <tt>true</tt> if and only if
   * <tt>A != null</tt> and <tt>! (Math.abs(value - A[i]) > tolerance())</tt>
   * holds for all coordinates.
   *
   * @param A
     * the first matrix to compare.
   * @param value
     * the value to compare against.
   * @return <tt>true</tt> if the matrix is equal to the value; <tt>false</tt>
   *         otherwise.
   */
  def equals(A: Matrix[Double], value: Double): Boolean = {
    if (A == null) return false
    A.everyCellEquals(value, toleranceVar)
  }

  /**
   * Returns whether both given matrices <tt>A</tt> and <tt>B</tt> are equal.
   * The result is <tt>true</tt> if <tt>A==B</tt>. Otherwise, the result is
   * <tt>true</tt> if and only if both arguments are <tt>!= null</tt>, have
   * the same size and <tt>! (Math.abs(A[i] - B[i]) > tolerance())</tt> holds
   * for all indexes.
   *
   * @param A
     * the first matrix to compare.
   * @param B
     * the second matrix to compare.
   * @return <tt>true</tt> if both matrices are equal; <tt>false</tt>
   *         otherwise.
   */
  def equals(A: Matrix[Double], B: Matrix[Double]): Boolean = {
    if (A eq B) return true
    if (A == null || B == null) return false
    A.equals(B, toleranceVar)
  }

  /**
   * Modifies the given matrix square matrix <tt>A</tt> such that it is
   * diagonally dominant by row and column, hence non-singular, hence
   * invertible. For testing purposes only.
   *
   * @param A
     * the square matrix to modify.
   * @throws IllegalArgumentException
     * if <tt>!isSquare(A)</tt>.
   */
  def generateNonSingular(A: Matrix2D[Double]) {
    checkSquare(A)
    val min: Int = Math.min(A.rows, A.columns)
    for(i <- 0 until min) {
      A.setQuick(i, i, 0.0)
    }
    for(i <- 0 until min) {
      val rowSum: Double = A.viewRow(i).aggregate(DoubleFunctions.plus, DoubleFunctions.abs)
      val colSum: Double = A.viewColumn(i).aggregate(DoubleFunctions.plus, DoubleFunctions.abs)
      A.setQuick(i, i, Math.max(rowSum, colSum) + i + 1)
    }
  }

  @inline
  private def isNonZero(A: Matrix2D[Double], row: Int, column: Int): Boolean = {
    Math.abs(A.getQuick(row, column)) > toleranceVar
  }

  /**
   * A matrix <tt>A</tt> is <i>diagonal</i> if <tt>A[i,j] == 0</tt> whenever
   * <tt>i != j</tt>. Matrix may but need not be square.
   */
  def isDiagonal(A: Matrix2D[Double]): Boolean = {
    for(row <- 0 until A.rows; column <- 0 until A.columns) {
        if (row != column && isNonZero(A, row, column)) return false
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
  def isDiagonallyDominantByColumn(A: Matrix2D[Double]): Boolean = {
    val min: Int = Math.min(A.rows, A.columns)
    for(i <- 0 until min) {
      var diag = Math.abs(A.getQuick(i, i)) * 2.0
      if (diag <= A.viewColumn(i).aggregate(DoubleFunctions.plus, DoubleFunctions.abs)) return false
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
  def isDiagonallyDominantByRow(A: Matrix2D[Double]): Boolean = {
    val min: Int = Math.min(A.rows, A.columns)
    for(i <- 0 until min) {
      var diag: Double = Math.abs(A.getQuick(i, i)) * 2.0
      if (diag <= A.viewRow(i).aggregate(DoubleFunctions.plus, DoubleFunctions.abs)) return false
    }
    true
  }

  /**
   * A matrix <tt>A</tt> is an <i>identity</i> matrix if <tt>A[i,i] == 1</tt>
   * and all other cells are zero. Matrix may but need not be square.
   */
  def isIdentity(A: Matrix2D[Double]): Boolean = {
    for(row <- 0 until A.rows; column <- 0 until A.columns) {
      val v = A.getQuick(row, column)
      if (row == column) {
        if (Math.abs(1 - v) > toleranceVar) return false
      }
      else if (Math.abs(v) > toleranceVar) return false
    }
    true
  }

  /**
   * A matrix <tt>A</tt> is <i>lower bidiagonal</i> if <tt>A[i,j]==0</tt>
   * unless <tt>i==j || i==j+1</tt>. Matrix may but need not be square.
   */
  def isLowerBidiagonal(A: Matrix2D[Double]): Boolean = {
    for(row <- 0 until A.rows; column <- 0 until A.columns) {
      if (row != column && row != column + 1) {
        if (isNonZero(A, row, column)) return false
      }
    }
    true
  }

  /**
   * A matrix <tt>A</tt> is <i>lower triangular</i> if <tt>A[i,j]==0</tt>
   * whenever <tt>i &lt; j</tt>. Matrix may but need not be square.
   */
  def isLowerTriangular(A: Matrix2D[Double]): Boolean = {
    for(column <- 0 until A.columns; row <- 0 until Math.min(column, A.rows)) {
      if (isNonZero(A, row, column)) return false
    }
    true
  }

  /**
   * A matrix <tt>A</tt> is <i>non-negative</i> if <tt>A[i,j] &gt;= 0</tt>
   * holds for all cells.
   * <p>
   * Note: Ignores tolerance.
   */
  def isNonNegative(A: Matrix2D[Double]): Boolean = {
    for(row <- 0 until A.rows; column <- 0 until A.columns) {
      if (A.getQuick(row, column) < 0.0) return false
    }
    true
  }

  /**
   * A square matrix <tt>A</tt> is <i>orthogonal</i> if
   * <tt>A*transpose(A) = I</tt>.
   *
   * @throws IllegalArgumentException
     * if <tt>!isSquare(A)</tt>.
   */
  def isOrthogonal(A: Matrix2D[Double]): Boolean = {
    checkSquare(A)
    val identity = new DiagonalMatrix2D[Double](A.rows, 1.0)
    equals(A.dot(A, transposeOther=true), identity)
  }

  /**
   * A matrix <tt>A</tt> is <i>positive</i> if <tt>A[i,j] &gt; 0</tt> holds
   * for all cells.
   * <p>
   * Note: Ignores tolerance.
   */
  def isPositive(A: Matrix2D[Double]): Boolean = {
    for(row <- 0 until A.rows; column <- 0 until A.columns) {
      if (A.getQuick(row, column) <= 0.0) return false
    }
    true
  }

  /**
   * A matrix <tt>A</tt> is <i>singular</i> if it has no inverse, that is, iff
   * <tt>det(A)==0</tt>.
   */
/*
  def isSingular(A: Matrix[Double]): Boolean = {
    Math.abs(DenseDoubleAlgebra.DEFAULT.det(A)) <= toleranceVar
  }
*/

  /**
   * A square matrix <tt>A</tt> is <i>skew-symmetric</i> if
   * <tt>A = -transpose(A)</tt>, that is <tt>A[i,j] == -A[j,i]</tt>.
   *
   * @throws IllegalArgumentException
     * if <tt>!isSquare(A)</tt>.
   */
  def isSkewSymmetric(A: Matrix2D[Double]): Boolean = {
    checkSquare(A)
    for(row <- 0 until A.rows; column <- 0 until A.columns) {
      if (Math.abs(A.getQuick(row, column) + A.getQuick(column, row)) > toleranceVar) return false
    }
    true
  }

  /**
   * A matrix <tt>A</tt> is <i>square</i> if it has the same number of rows
   * and columns.
   */
  def isSquare(A: Matrix2D[_]): Boolean = A.rows == A.columns

  /**
   * A matrix <tt>A</tt> is <i>strictly lower triangular</i> if
   * <tt>A[i,j]==0</tt> whenever <tt>i &lt;= j</tt>. Matrix may but need not
   * be square.
   */
  def isStrictlyLowerTriangular(A: Matrix2D[Double]): Boolean = {
    for(column <- 0 until A.columns; row <- 0 until Math.min(A.rows, column + 1)) {
      if (isNonZero(A, row, column)) return false
    }
    true
  }

  /**
   * A matrix <tt>A</tt> is <i>strictly triangular</i> if it is triangular and
   * its diagonal elements all equal 0. Matrix may but need not be square.
   */
  def isStrictlyTriangular(A: Matrix2D[Double]): Boolean = {
    if (! isTriangular(A)) return false
    for(i <- 0 until Math.min(A.rows, A.columns)) {
      if (isNonZero(A, i, i)) return false
    }
    true
  }

  /**
   * A matrix <tt>A</tt> is <i>strictly upper triangular</i> if
   * <tt>A[i,j]==0</tt> whenever <tt>i &gt;= j</tt>. Matrix may but need not
   * be square.
   */
  def isStrictlyUpperTriangular(A: Matrix2D[Double]): Boolean = {
    for(column <- 0 until A.columns; row <- column until A.rows) {
      if (isNonZero(A, row, column)) return false
    }
    true
  }

  /**
   * A matrix <tt>A</tt> is <i>symmetric</i> if <tt>A = tranpose(A)</tt>, that
   * is <tt>A[i,j] == A[j,i]</tt>.
   *
   * @throws IllegalArgumentException
     * if <tt>!isSquare(A)</tt>.
   */
  def isSymmetric(A: Matrix2D[Double]): Boolean = {
    checkSquare(A)
    equals(A, A.viewTranspose())
  }

  /**
   * A matrix <tt>A</tt> is <i>triangular</i> iff it is either upper or lower
   * triangular. Matrix may but need not be square.
   */
  def isTriangular(A: Matrix2D[Double]): Boolean = {
    isLowerTriangular(A) || isUpperTriangular(A)
  }

  /**
   * A matrix <tt>A</tt> is <i>tridiagonal</i> if <tt>A[i,j]==0</tt> whenever
   * <tt>Math.abs(i-j) > 1</tt>. Matrix may but need not be square.
   */
  def isTridiagonal(A: Matrix2D[Double]): Boolean = {
    for(column <- 0 until A.columns; row <- 0 until A.rows) {
      if (Math.abs(row-column) > 1 && isNonZero(A, row, column)) return false
    }
    true
  }

  /**
   * A matrix <tt>A</tt> is <i>unit triangular</i> if it is triangular and its
   * diagonal elements all equal 1. Matrix may but need not be square.
   */
  def isUnitTriangular(A: Matrix2D[Double]): Boolean = {
    if (! isTriangular(A)) return false
    for(i <- 0 until Math.min(A.rows, A.columns)) {
      if (Math.abs(1.0 - A.getQuick(i, i)) > toleranceVar) return false
    }
    true
  }

  /**
   * A matrix <tt>A</tt> is <i>upper bidiagonal</i> if <tt>A[i,j]==0</tt>
   * unless <tt>i==j || i==j-1</tt>. Matrix may but need not be square.
   */
  def isUpperBidiagonal(A: Matrix2D[Double]): Boolean = {
    for(column <- 0 until A.columns; row <- 0 until A.rows) {
      if (row != column && row != column - 1 && isNonZero(A, row, column)) return false
    }
    true
  }

  /**
   * A matrix <tt>A</tt> is <i>upper triangular</i> if <tt>A[i,j]==0</tt>
   * whenever <tt>i &gt; j</tt>. Matrix may but need not be square.
   */
  def isUpperTriangular(A: Matrix2D[Double]): Boolean = {
    for(column <- 0 until A.columns; row <- column+1 until A.rows) {
      if (isNonZero(A, row, column)) return false
    }
    true
  }

  /**
   * A matrix <tt>A</tt> is <i>zero</i> if all its cells are zero.
   */
  def isZero(A: Matrix2D[Double]): Boolean = {
    equals(A, 0.0)
  }

  /**
   * The <i>lower bandwidth</i> of a square matrix <tt>A</tt> is the maximum
   * <tt>i-j</tt> for which <tt>A[i,j]</tt> is nonzero and <tt>i &gt; j</tt>.
   * A <i>banded</i> matrix has a "band" about the diagonal. Diagonal,
   * tridiagonal and triangular matrices are special cases.
   *
   * @param A
     * the square matrix to analyze.
   * @return the lower bandwith.
   * @throws IllegalArgumentException
     * if <tt>!isSquare(A)</tt>.
   * @see #semiBandwidth(DoubleMatrix2D)
   * @see #upperBandwidth(DoubleMatrix2D)
   */
  def lowerBandwidth(A: Matrix2D[Double]): Int = {
    checkSquare(A)
    for(k <- A.rows-1 to 0 by -1) {
      for(i <- A.rows - k - 1 to 0 by -1) {
        if (isNonZero(A, i + k, i)) return k
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
     * the square matrix to analyze.
   * @return the semi-bandwith <tt>l</tt>.
   * @throws IllegalArgumentException
     * if <tt>!isSquare(A)</tt>.
   * @see #lowerBandwidth(DoubleMatrix2D)
   * @see #upperBandwidth(DoubleMatrix2D)
   */
  def semiBandwidth(A: Matrix2D[Double]): Int = {
    checkSquare(A)
    for(k <- A.rows-1 to 0 by -1) {
      for(i <- A.rows - k - 1 to 0 by -1) {
        if (isNonZero(A, i + k, i)) return k+1
        if (isNonZero(A, i, i + k)) return k+1
      }
    }
    1
  }

  /**
   * Sets the tolerance to <tt>Math.abs(newTolerance)</tt>.
   *
   * @throws UnsupportedOperationException
     * if <tt>this==DEFAULT || this==ZERO || this==TWELVE</tt>.
   */
  def setTolerance(newTolerance: Double) {
    if ((this eq DoubleProperty.DEFAULT) || (this eq DoubleProperty.ZERO) || (this eq DoubleProperty.TWELVE))
      throw new IllegalArgumentException("Attempted to modify immutable object.")
    toleranceVar = Math.abs(newTolerance)
  }

  /**
   * Returns the current tolerance.
   */
  def tolerance: Double = {
    toleranceVar
  }

  /**
   * The <i>upper bandwidth</i> of a square matrix <tt>A</tt> is the maximum
   * <tt>j-i</tt> for which <tt>A[i,j]</tt> is nonzero and <tt>j &gt; i</tt>.
   * A <i>banded</i> matrix has a "band" about the diagonal. Diagonal,
   * tridiagonal and triangular matrices are special cases.
   *
   * @param A
     * the square matrix to analyze.
   * @return the upper bandwith.
   * @throws IllegalArgumentException
     * if <tt>!isSquare(A)</tt>.
   * @see #semiBandwidth(DoubleMatrix2D)
   * @see #lowerBandwidth(DoubleMatrix2D)
   */
  def upperBandwidth(A: Matrix2D[Double]): Int = {
    checkSquare(A)
    for(k <- A.rows-1 to 0 by -1) {
      for(i <- A.rows - k - 1 to 0 by -1) {
        if (isNonZero(A, i, i + k)) return k
      }
    }
    0
  }
}
