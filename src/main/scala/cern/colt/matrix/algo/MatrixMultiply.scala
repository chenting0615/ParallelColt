package cern.colt.matrix.algo

import cern.colt.matrix._
import cern.colt.matrix.MatrixChecks._
import scala.collection.mutable.ArrayBuffer
import cern.colt.matrix.MatrixNumeric._

/**
  */
class MatrixMultiply[T: Manifest: MatrixNumeric](algorithms: TraversableOnce[MultiplyAlgorithm[T]], dflt: MultiplyAlgorithm[T]) {

  val numeric = implicitly[MatrixNumeric[T]]

  private val algorithmList = new ArrayBuffer[MultiplyAlgorithm[T]]()
  algorithmList ++= algorithms

  private var defaultAlgorithm: MultiplyAlgorithm[T] = dflt

  def prependAlgorithm(a: MultiplyAlgorithm[T]) {
    algorithmList.insert(0, a)
  }

  def appendAlgorithm(a: MultiplyAlgorithm[T]) {
    algorithmList.append(a)
  }

  def getAlgorithmList = algorithmList

  def setDefaultAlgorithm(a: MultiplyAlgorithm[T]) {
    defaultAlgorithm = a
  }

  def getDefaultAlgorithm = defaultAlgorithm

  /**
   * Linear algebraic matrix-matrix multiplication; <tt>result = A x B</tt>;
   */
  def multiply(A: Matrix2D[T], B: Matrix2D[T]): Matrix2D[T] = {
    multiply2D(A, B, null)
  }

  /**
   * Linear algebraic matrix-matrix multiplication; <tt>result = A x B</tt>;
   */
  def multiply(A: Matrix2D[T], B: Matrix1D[T]): Matrix1D[T] = {
    multiply1D(A, B, null)
  }

  /**
   * Linear algebraic dot-product or vector-vector multiplication;
   * <tt>result = A * B</tt>.
   * <tt>result = Sum(A[i] * B[i]), i=0..A.size()-1</tt>
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
  def dot(A: Matrix1D[T], B: Matrix1D[T]): T = {
    check1DMult(A, B)

    algorithmList.foreach(a => {
      if (a.predicate(A, B))
        a.dot(A, B)
    })
    defaultAlgorithm.dot(A, B)
  }

  /**
   * Linear algebraic dot-product or vector-vector multiplication;
   * <tt>result = A * B</tt>.
   * <tt>result = Sum(A[i] * B[i]), i=start..end-1</tt>
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
  def dot(A: Matrix1D[T], B: Matrix1D[T], start: Int, end: Int): T = {
    A.checkIndex(start)
    A.checkIndex(end-1)
    B.checkIndex(start)
    B.checkIndex(end-1)

    algorithmList.foreach(a => {
      if (a.predicate(A, B))
        a.dot(A, B, start, end)
    })
    defaultAlgorithm.dot(A, B, start, end)
  }

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
  def multiply1D(A: Matrix2D[T], y: Matrix1D[T], z: Matrix1D[T], alpha: T = numeric.one, beta: T = numeric.zero, transposeA: Boolean = false): Matrix1D[T] = {
    check1DMult(A, y, z, transposeA)

    algorithmList.foreach(a => {
      if (a.predicate(A, y, z, transposeA)) {
        if (transposeA) {
          a match {
            case a_t: MultiplyAlgorithmTranspose[T] => return a_t.multiply(A, y, z, alpha, beta, transposeA)
            case _ => {
              val AA = A.viewTranspose()
              return a.multiply(AA, y, z, alpha, beta)
            }
          }
        }
        else
          return a.multiply(A, y, z, alpha, beta)
      }
    })
    if (transposeA) {
      defaultAlgorithm match {
        case a_t: MultiplyAlgorithmTranspose[T] => a_t.multiply(A, y, z, alpha, beta, transposeA)
        case _ => {
          val AA = A.viewTranspose()
          defaultAlgorithm.multiply(AA, y, z, alpha, beta)
        }
      }
    }
    else
      defaultAlgorithm.multiply(A, y, z, alpha, beta)
  }

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
  def multiply2D(A: Matrix2D[T], B: Matrix2D[T], C: Matrix2D[T], alpha: T = numeric.one, beta: T = numeric.zero, transposeA: Boolean = false, transposeB: Boolean = false): Matrix2D[T] = {
    check2DMult(A, B, C, transposeA, transposeB)

    algorithmList.foreach(a => {
      if (a.predicate(A, B, C, transposeA, transposeB)) {
        if (transposeA || transposeB) {
          a match {
            case a_t: MultiplyAlgorithmTranspose[T] => return a_t.multiply(A, B, C, alpha, beta, transposeA, transposeB)
            case _ => {
              val AA = if (transposeA) A.viewTranspose() else A
              val BB = if (transposeB) B.viewTranspose() else B
              return a.multiply(AA, BB, C, alpha, beta)
            }
          }
        }
        else
          return a.multiply(A, B, C, alpha, beta)
      }
    })
    if (transposeA) {
      defaultAlgorithm match {
        case a_t: MultiplyAlgorithmTranspose[T] => a_t.multiply(A, B, C, alpha, beta, transposeA, transposeB)
        case _ => {
          val AA = if (transposeA) A.viewTranspose() else A
          val BB = if (transposeB) B.viewTranspose() else B
          defaultAlgorithm.multiply(AA, BB, C, alpha, beta)
        }
      }
    }
    else
      defaultAlgorithm.multiply(A, B, C, alpha, beta)
  }

  def check1DMult(A: Matrix1D[T], B: Matrix1D[T]) {
    checkSizesEqual(A, B)
  }

  def check1DMult(A: Matrix2D[T], y: Matrix1D[T], result: Matrix1D[T], transposeA: Boolean) {
    if (transposeA) {
      if (result != null)
        checkColumnsEqualsSize(A, result)
      checkRowsEqualsSize(A, y)
    }
    else {
      if (result != null)
        checkRowsEqualsSize(A, result)
      checkColumnsEqualsSize(A, y)
    }
    if (y eq result)
        throw new IllegalArgumentException("Source and destination matrices must not be the same y=" + System.identityHashCode(y) + " and result=" + System.identityHashCode(result))
  }

  def check2DMult(A: Matrix2D[T], B: Matrix2D[T], result: Matrix2D[T], transposeA: Boolean, transposeB: Boolean) {
    if (transposeA && transposeB)
      checkColumnsEqualsRows(B, A)
    else if (transposeA)
      checkRowsEqual(A, B)
    else if (transposeB)
      checkColumnsEqual(A, B)
    else
      checkColumnsEqualsRows(A, B)
    if (result != null) {
      if (transposeA)
        checkColumnsEqualsRows(A, result)
      else
        checkRowsEqual(A, result)
      if (transposeB)
        checkColumnsEqualsRows(result, B)
      else
        checkColumnsEqual(B, result)
    }
    if (A eq result)
        throw new IllegalArgumentException("Source and destination matrices must not be the same A=" + System.identityHashCode(A) + " and result=" + System.identityHashCode(result))
    if (B eq result)
        throw new IllegalArgumentException("Source and destination matrices must not be the same B=" + System.identityHashCode(B) + " and result=" + System.identityHashCode(result))
  }

}

object MatrixMultiply {

  private val multSimpleDbl = new MultiplySimple[Double]()
  private val multForEachDbl = new MultiplyForEachNonZero[Double]()
  private val multIterDbl = new MultiplyNonZeroIterator[Double]()
  implicit object MatrixMultiplyDouble extends MatrixMultiply[Double](Seq(multIterDbl, multForEachDbl, multSimpleDbl), multSimpleDbl) {
  }

  private val multSimpleFlt = new MultiplySimple[Float]()
  private val multForEachFlt = new MultiplyForEachNonZero[Float]()
  private val multIterFlt = new MultiplyNonZeroIterator[Float]()
  implicit object MatrixMultiplyFloat extends MatrixMultiply[Float](Seq(multIterFlt, multForEachFlt, multSimpleFlt), multSimpleFlt) {
  }

  private val multSimpleInt = new MultiplySimple[Int]()
  private val multForEachInt = new MultiplyForEachNonZero[Int]()
  private val multIterInt = new MultiplyNonZeroIterator[Int]()
  implicit object MatrixMultiplyInt extends MatrixMultiply[Int](Seq(multIterInt, multForEachInt, multSimpleInt), multSimpleInt) {
  }

}
