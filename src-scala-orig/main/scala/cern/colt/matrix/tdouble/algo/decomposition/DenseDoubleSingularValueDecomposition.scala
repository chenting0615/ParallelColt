package cern.colt.matrix.tdouble.algo.decomposition

import org.netlib.lapack.LAPACK
import cern.colt.matrix.tdouble.StrideMatrix2D
import cern.colt.matrix.tdouble.algo.DoubleProperty
import cern.colt.matrix.tdouble.impl.DenseColumnDoubleMatrix2D
import cern.colt.matrix.tdouble.impl.DenseMatrix2D
import cern.colt.matrix.tdouble.impl.DiagonalDoubleMatrix2D
import scala.reflect.{BeanProperty, BooleanBeanProperty}
//remove if not needed
import scala.collection.JavaConversions._

/**
 *
 * For an <tt>m x n</tt> matrix <tt>A</tt>, the singular value decomposition is
 * an <tt>m x m</tt> orthogonal matrix <tt>U</tt>, an <tt>m x n</tt> diagonal
 * matrix <tt>S</tt>, and an <tt>n x n</tt> orthogonal matrix <tt>V</tt> so that
 * <tt>A = U*S*V'</tt>.
 * <P>
 * The singular values, <tt>sigma[k] = S[k][k]</tt>, are ordered so that
 * <tt>sigma[0] >= sigma[1] >= ... >= sigma[min(m-1,n-1)]</tt>.
 * <P>
 *
 * This implementation uses the divide-and-conquer algorithm (dgesdd) from
 * LAPACK.
 *
 * @author Piotr Wendykier (piotr.wendykier@gmail.com)
 *
 */
class DenseDoubleSingularValueDecomposition(A: StrideMatrix2D, var wantUV: Boolean, var wantWholeUV: Boolean)
    {

  private var U: StrideMatrix2D = _

  private var V: StrideMatrix2D = _

  private var S: StrideMatrix2D = _

  private var elementsU: Array[Double] = _

  private var elementsVt: Array[Double] = _

  private var elementsS: Array[Double] = new Array[Double](mn)

  @BeanProperty
  var info: org.netlib.util.intW = new org.netlib.util.intW(2)

  private var m: Int = A.rows()

  private var n: Int = A.columns()

  private var mn: Int = Math.min(m, n)

  private var columnMatrix: Boolean = false

  DoubleProperty.DEFAULT.checkDense(A)

  var elementsA: Array[Double] = null

  if (A.isInstanceOf[DenseColumnDoubleMatrix2D]) {
    elementsA = A.copy().elements().asInstanceOf[Array[Double]]
    columnMatrix = true
  } else {
    elementsA = A.viewDice().copy().elements().asInstanceOf[Array[Double]]
  }

  val maxmn = Math.max(m, n)

  var lwork: Int = 0

  var work: Array[Double] = null

  val iwork = Array.ofDim[Int](8 * mn)

  if (wantUV = true) {
    if (wantWholeUV) {
      elementsU = Array.ofDim[Double](m * m)
      elementsVt = Array.ofDim[Double](n * n)
      lwork = 3 * mn * mn + Math.max(maxmn, 4 * mn * mn + 4 * mn) +
        maxmn
      work = Array.ofDim[Double](lwork)
      LAPACK.getInstance.dgesdd("A", m, n, elementsA, m, elementsS, elementsU, m, elementsVt, n, work,
        lwork, iwork, info)
    } else {
      elementsU = Array.ofDim[Double](m * mn)
      elementsVt = Array.ofDim[Double](mn * n)
      lwork = 3 * mn * mn + Math.max(maxmn, 4 * mn * mn + 4 * mn) +
        maxmn
      work = Array.ofDim[Double](lwork)
      LAPACK.getInstance.dgesdd("S", m, n, elementsA, m, elementsS, elementsU, m, elementsVt, mn, work,
        lwork, iwork, info)
    }
  } else {
    lwork = 3 * mn + Math.max(maxmn, 6 * mn) + maxmn
    work = Array.ofDim[Double](lwork)
    LAPACK.getInstance.dgesdd("N", m, n, elementsA, m, elementsS, null, m, null, n, work, lwork, iwork,
      info)
  }

  if (info.`val` != 0) {
    throw new IllegalArgumentException("Error occured while computing SVD decomposition: " +
      info)
  }

  /**
   * Returns the two norm condition number, which is <tt>max(S) / min(S)</tt>.
   */
  def cond(): Double = elementsS(0) / elementsS(mn - 1)

  /**
   * Returns the diagonal matrix of singular values.
   *
   * @return S
   */
  def getS(): StrideMatrix2D = {
    if (S == null) {
      S = if (wantWholeUV == false) new DiagonalDoubleMatrix2D(mn, mn, 0) else new DiagonalDoubleMatrix2D(m,
        n, 0)
      for (i <- 0 until mn) {
        S.setQuick(i, i, elementsS(i))
      }
    }
    S.copy()
  }

  /**
   * Returns the diagonal of <tt>S</tt>, which is a one-dimensional array of
   * singular values
   *
   * @return diagonal of <tt>S</tt>.
   */
  def getSingularValues(): Array[Double] = elementsS

  /**
   * Returns the left singular vectors <tt>U</tt>.
   *
   * @return <tt>U</tt>
   */
  def getU(): StrideMatrix2D = {
    if (wantUV == false) {
      throw new IllegalAccessError("Matrix U was not computed")
    } else {
      if (U == null) {
        U = if (wantWholeUV == false) if (columnMatrix) new DenseColumnDoubleMatrix2D(m, mn).assign(elementsU) else new DenseMatrix2D(mn,
          m).assign(elementsU).viewDice() else if (columnMatrix) new DenseColumnDoubleMatrix2D(m, m).assign(elementsU) else new DenseMatrix2D(m,
          m).assign(elementsU).viewDice()
      }
      U.copy()
    }
  }

  /**
   * Returns the right singular vectors <tt>V</tt>.
   *
   * @return <tt>V</tt>
   */
  def getV(): StrideMatrix2D = {
    if (wantUV == false) {
      throw new IllegalAccessError("Matrix V was not computed")
    } else {
      if (V == null) {
        V = if (wantWholeUV == false) if (columnMatrix) new DenseColumnDoubleMatrix2D(mn, n).assign(elementsVt)
          .viewDice() else new DenseMatrix2D(n, mn).assign(elementsVt) else if (columnMatrix) new DenseColumnDoubleMatrix2D(n,
          n).assign(elementsVt)
          .viewDice() else new DenseMatrix2D(n, n).assign(elementsVt)
      }
      V.copy()
    }
  }

  /**
   * Returns the two norm, which is <tt>max(S)</tt>.
   */
  def norm2(): Double = elementsS(0)

  /**
   * Returns the effective numerical matrix rank, which is the number of
   * nonnegligible singular values.
   */
  def rank(): Int = {
    val eps = Math.pow(2.0, -52.0)
    val tol = Math.max(m, n) * elementsS(0) * eps
    var r = 0
    for (i <- 0 until elementsS.length if elementsS(i) > tol) {
      r += 1
    }
    r
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
    buf.append("---------------------------------------------------------------------\n")
    buf.append("SingularValueDecomposition(A) --> cond(A), rank(A), norm2(A), U, S, V\n")
    buf.append("---------------------------------------------------------------------\n")
    buf.append("cond = ")
    try {
      buf.append(String.valueOf(this.cond()))
    } catch {
      case exc: IllegalArgumentException => buf.append(unknown + exc.getMessage)
    }
    buf.append("\nrank = ")
    try {
      buf.append(String.valueOf(this.rank()))
    } catch {
      case exc: IllegalArgumentException => buf.append(unknown + exc.getMessage)
    }
    buf.append("\nnorm2 = ")
    try {
      buf.append(String.valueOf(this.norm2()))
    } catch {
      case exc: IllegalArgumentException => buf.append(unknown + exc.getMessage)
    }
    buf.append("\n\nU = ")
    try {
      buf.append(String.valueOf(this.getU))
    } catch {
      case exc: IllegalArgumentException => buf.append(unknown + exc.getMessage)
    }
    buf.append("\n\nS = ")
    try {
      buf.append(String.valueOf(this.getS))
    } catch {
      case exc: IllegalArgumentException => buf.append(unknown + exc.getMessage)
    }
    buf.append("\n\nV = ")
    try {
      buf.append(String.valueOf(this.getV))
    } catch {
      case exc: IllegalArgumentException => buf.append(unknown + exc.getMessage)
    }
    buf.toString
  }
}
