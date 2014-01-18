package cern.colt.matrix.algo

import cern.colt.matrix.{Matrix2D, Matrix1D}

trait MultiplyAlgorithm[T] {

  def predicate(A: Matrix1D[T], B: Matrix1D[T]): Boolean = true
  def predicate(A: Matrix2D[T], B: Matrix1D[T], result: Matrix1D[T], transposeA: Boolean): Boolean = true
  def predicate(A: Matrix2D[T], B: Matrix2D[T], result: Matrix2D[T], transposeA: Boolean, transposeB: Boolean): Boolean = true

  /**
   * Linear algebraic matrix-vector multiplication;
   * <tt>result = alpha * A * B</tt>.
   * <tt>result = alpha*Sum(A[i] * B[i]), i=0..A.size()-1</tt>
   * Note: Matrix shape conformance is checked.
   *
   * @param A
   *            the source vector.
   * @param B
   *            the source vector.
   * @return The dot product of the vectors
   *
   * @throws IllegalArgumentException
   *             if <tt>A.size() != y.size()</tt>.
   */
  def dot(A: Matrix1D[T], B: Matrix1D[T]): T

  /**
   * Linear algebraic matrix-vector multiplication;
   * <tt>result = alpha * A * B</tt>.
   * <tt>result = alpha*Sum(A[i] * B[i]), i=start..end-1</tt>
   * Note: Matrix shape conformance is checked.
   *
   * @param A
   *            the source vector.
   * @param B
   *            the source vector.
   * @return The dot product of the vectors
   *
   * @throws IllegalArgumentException
   *             if <tt>A.size() != y.size()</tt>.
   */
  def dot(A: Matrix1D[T], B: Matrix1D[T], start: Int, end: Int): T

  /**
   * Linear algebraic matrix-vector multiplication;
   * <tt>z = alpha * A * y + beta*z</tt>.
   * <tt>z[i] = alpha*Sum(A[i,j] * y[j]) + beta*z[i], i=0..A.rows-1, j=0..y.size()-1</tt>
   * . Where <tt>A == this</tt>. <br>
   * Note: Matrix shape conformance is checked <i>after</i> potential
   * transpositions.
   *
   * @param y
   *            the source vector.
   * @param z
   *            the vector where results are to be stored. Set this parameter
   *            to <tt>null</tt> to indicate that a new result vector shall be
   *            constructed.
   * @return z (for convenience only).
   *
   * @throws IllegalArgumentException
   *             if <tt>A.columns != y.size() || A.rows > z.size())</tt>.
   */
  def multiply(A: Matrix2D[T], y: Matrix1D[T], z: Matrix1D[T], alpha: T, beta: T): Matrix1D[T]

  /**
   * Linear algebraic matrix-matrix multiplication;
   * <tt>C = alpha * A x B + beta*C</tt>.
   * <tt>C[i,j] = alpha*Sum(A[i,k] * B[k,j]) + beta*C[i,j], k=0..n-1</tt>. <br>
   * Matrix shapes: <tt>A(m x n), B(n x p), C(m x p)</tt>. <br>
   * Note: Matrix shape conformance is checked <i>after</i> potential
   * transpositions.
   *
   * @param B
   *            the second source matrix.
   * @param C
   *            the matrix where results are to be stored. Set this parameter
   *            to <tt>null</tt> to indicate that a new result matrix shall be
   *            constructed.
   * @return C (for convenience only).
   *
   * @throws IllegalArgumentException
   *             if <tt>B.rows != A.columns</tt>.
   * @throws IllegalArgumentException
   *             if
   *             <tt>C.rows != A.rows || C.columns != B.columns</tt>.
   * @throws IllegalArgumentException
   *             if <tt>A == C || B == C</tt>.
   */
  def multiply(A: Matrix2D[T], B: Matrix2D[T], C: Matrix2D[T], alpha: T, beta: T): Matrix2D[T]
}

trait MultiplyAlgorithmTranspose[T] extends MultiplyAlgorithm[T] {
  /**
   * Linear algebraic matrix-vector multiplication;
   * <tt>z = alpha * A * y + beta*z</tt>.
   * <tt>z[i] = alpha*Sum(A[i,j] * y[j]) + beta*z[i], i=0..A.rows-1, j=0..y.size()-1</tt>
   * . Where <tt>A == this</tt>. <br>
   * Note: Matrix shape conformance is checked <i>after</i> potential
   * transpositions.
   *
   * @param y
   *            the source vector.
   * @param z
   *            the vector where results are to be stored. Set this parameter
   *            to <tt>null</tt> to indicate that a new result vector shall be
   *            constructed.
   * @return z (for convenience only).
   *
   * @throws IllegalArgumentException
   *             if <tt>A.columns != y.size() || A.rows > z.size())</tt>.
   */
  def multiply(A: Matrix2D[T], y: Matrix1D[T], z: Matrix1D[T], alpha: T, beta: T, transposeA: Boolean): Matrix1D[T]

  /**
   * Linear algebraic matrix-matrix multiplication;
   * <tt>C = alpha * A x B + beta*C</tt>.
   * <tt>C[i,j] = alpha*Sum(A[i,k] * B[k,j]) + beta*C[i,j], k=0..n-1</tt>. <br>
   * Matrix shapes: <tt>A(m x n), B(n x p), C(m x p)</tt>. <br>
   * Note: Matrix shape conformance is checked <i>after</i> potential
   * transpositions.
   *
   * @param B
   *            the second source matrix.
   * @param C
   *            the matrix where results are to be stored. Set this parameter
   *            to <tt>null</tt> to indicate that a new result matrix shall be
   *            constructed.
   * @return C (for convenience only).
   *
   * @throws IllegalArgumentException
   *             if <tt>B.rows != A.columns</tt>.
   * @throws IllegalArgumentException
   *             if
   *             <tt>C.rows != A.rows || C.columns != B.columns</tt>.
   * @throws IllegalArgumentException
   *             if <tt>A == C || B == C</tt>.
   */
  def multiply(A: Matrix2D[T], B: Matrix2D[T], C: Matrix2D[T], alpha: T, beta: T, transposeA: Boolean, transposeB: Boolean): Matrix2D[T]
}

