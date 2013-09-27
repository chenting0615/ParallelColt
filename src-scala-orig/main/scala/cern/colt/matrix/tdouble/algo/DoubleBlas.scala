package cern.colt.matrix.tdouble.algo

import cern.colt.matrix.tdouble.StrideMatrix1D
import cern.colt.matrix.tdouble.StrideMatrix2D
//remove if not needed
import scala.collection.JavaConversions._

/**
 * <p>
 * Subset of the <A HREF="http://netlib2.cs.utk.edu/blas/faq.html">BLAS</A>
 * (Basic Linear Algebra System); High quality "building block" routines for
 * performing basic vector and matrix operations. Because the BLAS are
 * efficient, portable, and widely available, they're commonly used in the
 * development of high quality linear algebra software.
 * <p>
 * Mostly for compatibility with legacy notations. Most operations actually just
 * delegate to the appropriate methods directly defined on matrices and vectors.
 * </p>
 * <p>
 * This class implements the BLAS functions for operations on matrices from the
 * matrix package. It follows the spirit of the <A
 * HREF="http://math.nist.gov/javanumerics/blas.html">Draft Proposal for Java
 * BLAS Interface</A>, by Roldan Pozo of the National Institute of Standards and
 * Technology. Interface definitions are also identical to the Ninja interface.
 * Because the matrix package supports sections, the interface is actually
 * simpler.
 * </p>
 * <p>
 * Currently, the following operations are supported:
 * </p>
 * <ol>
 * <li>BLAS Level 1: Vector-Vector operations</li>
 * <ul>
 * <li>ddot : dot product of two vectors</li>
 * <li>daxpy : scalar times a vector plus a vector</li>
 * <li>drotg : construct a Givens plane rotation</li>
 * <li>drot : apply a plane rotation</li>
 * <li>dcopy : copy vector X into vector Y</li>
 * <li>dswap : interchange vectors X and Y</li>
 * <li>dnrm2 : Euclidean norm of a vector</li>
 * <li>dasum : sum of absolute values of vector components</li>
 * <li>dscal : scale a vector by a scalar</li>
 * <li>idamax: index of element with maximum absolute value</li>
 * </ul>
 * <li>2.BLAS Level 2: Matrix-Vector operations</li>
 * <ul>
 * <li>dgemv : matrix-vector multiply with general matrix</li>
 * <li>dger : rank-1 update on general matrix</li>
 * <li>dsymv : matrix-vector multiply with symmetric matrix</li>
 * <li>dtrmv : matrix-vector multiply with triangular matrix</li>
 * </ul>
 * <li>3.BLAS Level 3: Matrix-Matrix operations
 * <ul>
 * <li>dgemm : matrix-matrix multiply with general matrices</li>
 * </ul>
 * </li>
 * </ol>
 *
 * @author wolfgang.hoschek@cern.ch
 * @version 0.9, 16/04/2000
 */
trait DoubleBlas {

  /**
   * Assigns the result of a function to each cell;
   * <tt>x[row,col] = function(x[row,col])</tt>.
   *
   * @param A
   *            the matrix to modify.
   * @param function
   *            a function object taking as argument the current cell's value.
   * @see cern.jet.math.tdouble.DoubleFunctions
   */
  def assign(A: StrideMatrix2D, function: cern.colt.function.tdouble.Function1): Unit

  /**
   * Assigns the result of a function to each cell;
   * <tt>x[row,col] = function(x[row,col],y[row,col])</tt>.
   *
   * @param x
   *            the matrix to modify.
   * @param y
   *            the secondary matrix to operate on.
   * @param function
   *            a function object taking as first argument the current cell's
   *            value of <tt>this</tt>, and as second argument the current
   *            cell's value of <tt>y</tt>,
   * @throws IllegalArgumentException
   *             if
   *             <tt>x.columns() != y.columns() || x.rows() != y.rows()</tt>
   * @see cern.jet.math.tdouble.DoubleFunctions
   */
  def assign(x: StrideMatrix2D, y: StrideMatrix2D, function: cern.colt.function.tdouble.DoubleDoubleFunction): Unit

  /**
   * Returns the sum of absolute values; <tt>|x[0]| + |x[1]| + ... </tt>. In
   * fact equivalent to
   * <tt>x.aggregate(cern.jet.math.Functions.plus, cern.jet.math.Functions.abs)</tt>
   * .
   *
   * @param x
   *            the first vector.
   */
  def dasum(x: StrideMatrix1D): Double

  /**
   * Combined vector scaling; <tt>y = y + alpha*x</tt>. In fact equivalent to
   * <tt>y.assign(x,cern.jet.math.Functions.plusMult(alpha))</tt>.
   *
   * @param alpha
   *            a scale factor.
   * @param x
   *            the first source vector.
   * @param y
   *            the second source vector, this is also the vector where
   *            results are stored.
   *
   * @throws IllegalArgumentException
   *             <tt>x.size() != y.size()</tt>..
   */
  def daxpy(alpha: Double, x: StrideMatrix1D, y: StrideMatrix1D): Unit

  /**
   * Combined matrix scaling; <tt>B = B + alpha*A</tt>. In fact equivalent to
   * <tt>B.assign(A,cern.jet.math.Functions.plusMult(alpha))</tt>.
   *
   * @param alpha
   *            a scale factor.
   * @param A
   *            the first source matrix.
   * @param B
   *            the second source matrix, this is also the matrix where
   *            results are stored.
   *
   * @throws IllegalArgumentException
   *             if
   *             <tt>A.columns() != B.columns() || A.rows() != B.rows()</tt>.
   */
  def daxpy(alpha: Double, A: StrideMatrix2D, B: StrideMatrix2D): Unit

  /**
   * Vector assignment (copying); <tt>y = x</tt>. In fact equivalent to
   * <tt>y.assign(x)</tt>.
   *
   * @param x
   *            the source vector.
   * @param y
   *            the destination vector.
   *
   * @throws IllegalArgumentException
   *             <tt>x.size() != y.size()</tt>.
   */
  def dcopy(x: StrideMatrix1D, y: StrideMatrix1D): Unit

  /**
   * Matrix assignment (copying); <tt>B = A</tt>. In fact equivalent to
   * <tt>B.assign(A)</tt>.
   *
   * @param A
   *            the source matrix.
   * @param B
   *            the destination matrix.
   *
   * @throws IllegalArgumentException
   *             if
   *             <tt>A.columns() != B.columns() || A.rows() != B.rows()</tt>.
   */
  def dcopy(A: StrideMatrix2D, B: StrideMatrix2D): Unit

  /**
   * Returns the dot product of two vectors x and y, which is
   * <tt>Sum(x[i]*y[i])</tt>. In fact equivalent to <tt>x.zDotProduct(y)</tt>.
   *
   * @param x
   *            the first vector.
   * @param y
   *            the second vector.
   * @return the sum of products.
   *
   * @throws IllegalArgumentException
   *             if <tt>x.size() != y.size()</tt>.
   */
  def ddot(x: StrideMatrix1D, y: StrideMatrix1D): Double

  /**
   * Generalized linear algebraic matrix-matrix multiply;
   * <tt>C = alpha*A*B + beta*C</tt>. In fact equivalent to
   * <tt>A.zMult(B,C,alpha,beta,transposeA,transposeB)</tt>. Note: Matrix
   * shape conformance is checked <i>after</i> potential transpositions.
   *
   * @param transposeA
   *            set this flag to indicate that the multiplication shall be
   *            performed on A'.
   * @param transposeB
   *            set this flag to indicate that the multiplication shall be
   *            performed on B'.
   * @param alpha
   *            a scale factor.
   * @param A
   *            the first source matrix.
   * @param B
   *            the second source matrix.
   * @param beta
   *            a scale factor.
   * @param C
   *            the third source matrix, this is also the matrix where results
   *            are stored.
   *
   * @throws IllegalArgumentException
   *             if <tt>B.rows() != A.columns()</tt>.
   * @throws IllegalArgumentException
   *             if
   *             <tt>C.rows() != A.rows() || C.columns() != B.columns()</tt>.
   * @throws IllegalArgumentException
   *             if <tt>A == C || B == C</tt>.
   */
  def dgemm(transposeA: Boolean,
      transposeB: Boolean,
      alpha: Double,
      A: StrideMatrix2D,
      B: StrideMatrix2D,
      beta: Double,
      C: StrideMatrix2D): Unit

  /**
   * Generalized linear algebraic matrix-vector multiply;
   * <tt>y = alpha*A*x + beta*y</tt>. In fact equivalent to
   * <tt>A.zMult(x,y,alpha,beta,transposeA)</tt>. Note: Matrix shape
   * conformance is checked <i>after</i> potential transpositions.
   *
   * @param transposeA
   *            set this flag to indicate that the multiplication shall be
   *            performed on A'.
   * @param alpha
   *            a scale factor.
   * @param A
   *            the source matrix.
   * @param x
   *            the first source vector.
   * @param beta
   *            a scale factor.
   * @param y
   *            the second source vector, this is also the vector where
   *            results are stored.
   *
   * @throws IllegalArgumentException
   *             <tt>A.columns() != x.size() || A.rows() != y.size())</tt>..
   */
  def dgemv(transposeA: Boolean,
      alpha: Double,
      A: StrideMatrix2D,
      x: StrideMatrix1D,
      beta: Double,
      y: StrideMatrix1D): Unit

  /**
   * Performs a rank 1 update; <tt>A = A + alpha*x*y'</tt>. Example:
   *
   * <pre>
   * 	 A = { {6,5}, {7,6} }, x = {1,2}, y = {3,4}, alpha = 1 --&gt;
   * 	 A = { {9,9}, {13,14} }
   *
   * </pre>
   *
   * @param alpha
   *            a scalar.
   * @param x
   *            an m element vector.
   * @param y
   *            an n element vector.
   * @param A
   *            an m by n matrix.
   */
  def dger(alpha: Double,
      x: StrideMatrix1D,
      y: StrideMatrix1D,
      A: StrideMatrix2D): Unit

  /**
   * Return the 2-norm; <tt>sqrt(x[0]^2 + x[1]^2 + ...)</tt>. In fact
   * equivalent to <tt>Math.sqrt(Algebra.DEFAULT.norm2(x))</tt>.
   *
   * @param x
   *            the vector.
   */
  def dnrm2(x: StrideMatrix1D): Double

  /**
   * Applies a givens plane rotation to (x,y);
   * <tt>x = c*x + s*y; y = c*y - s*x</tt>.
   *
   * @param x
   *            the first vector.
   * @param y
   *            the second vector.
   * @param c
   *            the cosine of the angle of rotation.
   * @param s
   *            the sine of the angle of rotation.
   */
  def drot(x: StrideMatrix1D,
      y: StrideMatrix1D,
      c: Double,
      s: Double): Unit

  /**
   * Constructs a Givens plane rotation for <tt>(a,b)</tt>. Taken from the
   * LINPACK translation from FORTRAN to Java, interface slightly modified. In
   * the LINPACK listing DROTG is attributed to Jack Dongarra
   *
   * @param a
   *            rotational elimination parameter a.
   * @param b
   *            rotational elimination parameter b.
   * @param rotvec
   *            Must be at least of length 4. On output contains the values
   *            <tt>{a,b,c,s}</tt>.
   */
  def drotg(a: Double, b: Double, rotvec: Array[Double]): Unit

  /**
   * Vector scaling; <tt>x = alpha*x</tt>. In fact equivalent to
   * <tt>x.assign(cern.jet.math.Functions.mult(alpha))</tt>.
   *
   * @param alpha
   *            a scale factor.
   * @param x
   *            the first vector.
   */
  def dscal(alpha: Double, x: StrideMatrix1D): Unit

  /**
   * Matrix scaling; <tt>A = alpha*A</tt>. In fact equivalent to
   * <tt>A.assign(cern.jet.math.Functions.mult(alpha))</tt>.
   *
   * @param alpha
   *            a scale factor.
   * @param A
   *            the matrix.
   */
  def dscal(alpha: Double, A: StrideMatrix2D): Unit

  /**
   * Swaps the elements of two vectors; <tt>y <==> x</tt>. In fact equivalent
   * to <tt>y.swap(x)</tt>.
   *
   * @param x
   *            the first vector.
   * @param y
   *            the second vector.
   *
   * @throws IllegalArgumentException
   *             <tt>x.size() != y.size()</tt>.
   */
  def dswap(x: StrideMatrix1D, y: StrideMatrix1D): Unit

  /**
   * Swaps the elements of two matrices; <tt>B <==> A</tt>.
   *
   * @param x
   *            the first matrix.
   * @param y
   *            the second matrix.
   *
   * @throws IllegalArgumentException
   *             if
   *             <tt>A.columns() != B.columns() || A.rows() != B.rows()</tt>.
   */
  def dswap(x: StrideMatrix2D, y: StrideMatrix2D): Unit

  /**
   * Symmetric matrix-vector multiplication; <tt>y = alpha*A*x + beta*y</tt>.
   * Where alpha and beta are scalars, x and y are n element vectors and A is
   * an n by n symmetric matrix. A can be in upper or lower triangular format.
   *
   * @param isUpperTriangular
   *            is A upper triangular or lower triangular part to be used?
   * @param alpha
   *            scaling factor.
   * @param A
   *            the source matrix.
   * @param x
   *            the first source vector.
   * @param beta
   *            scaling factor.
   * @param y
   *            the second vector holding source and destination.
   */
  def dsymv(isUpperTriangular: Boolean,
      alpha: Double,
      A: StrideMatrix2D,
      x: StrideMatrix1D,
      beta: Double,
      y: StrideMatrix1D): Unit

  /**
   * Triangular matrix-vector multiplication; <tt>x = A*x</tt> or <tt>x = A'*x</tt>.
   * Where x is an n element vector and A is an n by n unit, or non-unit,
   * upper or lower triangular matrix.
   *
   * @param isUpperTriangular
   *            is A upper triangular or lower triangular?
   * @param transposeA
   *            set this flag to indicate that the multiplication shall be
   *            performed on A'.
   * @param isUnitTriangular
   *            true --> A is assumed to be unit triangular; false --> A is
   *            not assumed to be unit triangular
   * @param A
   *            the source matrix.
   * @param x
   *            the vector holding source and destination.
   */
  def dtrmv(isUpperTriangular: Boolean,
      transposeA: Boolean,
      isUnitTriangular: Boolean,
      A: StrideMatrix2D,
      x: StrideMatrix1D): Unit

  /**
   * Returns the index of largest absolute value;
   * <tt>i such that |x[i]| == max(|x[0]|,|x[1]|,...).</tt>.
   *
   * @param x
   *            the vector to search through.
   * @return the index of largest absolute value (-1 if x is empty).
   */
  def idamax(x: StrideMatrix1D): Int
}
