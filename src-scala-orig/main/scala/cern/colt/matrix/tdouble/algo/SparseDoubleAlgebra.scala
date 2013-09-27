package cern.colt.matrix.tdouble.algo

import cern.colt.matrix.tdouble.StrideMatrix1D
import cern.colt.matrix.tdouble.StrideMatrix2D
import cern.colt.matrix.tdouble.algo.decomposition.SparseDoubleCholeskyDecomposition
import cern.colt.matrix.tdouble.algo.decomposition.SparseDoubleLUDecomposition
import cern.colt.matrix.tdouble.algo.decomposition.SparseDoubleQRDecomposition
import cern.colt.matrix.tdouble.impl.DenseMatrix1D
import cern.colt.matrix.tdouble.impl.SparseCCDoubleMatrix2D
import cern.colt.matrix.tdouble.impl.SparseRCDoubleMatrix2D
import edu.emory.mathcs.csparsej.tdouble.Dcs_norm
import edu.emory.mathcs.csparsej.tdouble.Dcs_common.Dcs
import SparseDoubleAlgebra._
//remove if not needed
import scala.collection.JavaConversions._

object SparseDoubleAlgebra {

  /**
   * A default Algebra object; has {@link DoubleProperty#DEFAULT} attached for
   * tolerance. Allows ommiting to construct an Algebra object time and again.
   *
   * Note that this Algebra object is immutable. Any attempt to assign a new
   * Property object to it (via method <tt>setProperty</tt>), or to alter the
   * tolerance of its property object (via
   * <tt>property().setTolerance(...)</tt>) will throw an exception.
   */
  val DEFAULT = new SparseDoubleAlgebra()

  /**
   * A default Algebra object; has {@link DoubleProperty#ZERO} attached for
   * tolerance. Allows ommiting to construct an Algebra object time and again.
   *
   * Note that this Algebra object is immutable. Any attempt to assign a new
   * Property object to it (via method <tt>setProperty</tt>), or to alter the
   * tolerance of its property object (via
   * <tt>property().setTolerance(...)</tt>) will throw an exception.
   */
  val ZERO = new SparseDoubleAlgebra()

  DEFAULT.property = DoubleProperty.DEFAULT

  ZERO.property = DoubleProperty.ZERO

  private def normInfinityRC(A: SparseRCDoubleMatrix2D): Double = {
    var p: Int = 0
    var j: Int = 0
    var n: Int = 0
    var Ap: Array[Int] = 0
    var Ax: Array[Double] = 0.0
    var norm = 0
    var s: Double = 0.0
    n = A.rows()
    Ap = A.getRowPointers
    Ax = A.getValues
    j = 0
    while (j < n) {
      s = 0
      p = Ap(j)
      while (p < Ap(j + 1)) {s += Math.abs(Ax(p))p += 1
      }
      norm = Math.max(norm, s)
      j += 1
    }
    (norm)
  }
}

/**
 * Linear algebraic matrix operations operating on sparse matrices.
 *
 * @author Piotr Wendykier (piotr.wendykier@gmail.com)
 */
class SparseDoubleAlgebra(tolerance: Double) {

  /**
   * The property object attached to this instance.
   */
  protected var property: DoubleProperty = _

  setProperty(new DoubleProperty(tolerance))

  /**
   * Constructs a new instance with an equality tolerance given by
   * <tt>Property.DEFAULT.tolerance()</tt>.
   */
  def this() {
    this(DoubleProperty.DEFAULT.tolerance())
  }

  /**
   * Constructs and returns the Cholesky-decomposition of the given matrix.
   *
   * @param matrix
   *            sparse matrix
   * @param order
   *            ordering option (0 or 1); 0: natural ordering, 1: amd(A+A')
   * @return Cholesky-decomposition of the given matrix
   */
  def chol(matrix: StrideMatrix2D, order: Int): SparseDoubleCholeskyDecomposition = {
    new SparseDoubleCholeskyDecomposition(matrix, order)
  }

  /**
   * Returns a copy of the receiver. The attached property object is also
   * copied. Hence, the property object of the copy is mutable.
   *
   * @return a copy of the receiver.
   */
  def clone(): AnyRef = {
    new SparseDoubleAlgebra(property.tolerance())
  }

  /**
   * Returns the determinant of matrix <tt>A</tt>.
   *
   * @param A
   *            sparse matrix
   * @return the determinant of matrix <tt>A</tt>
   */
  def det(A: StrideMatrix2D): Double = lu(A, 0).det()

  /**
   * Constructs and returns the LU-decomposition of the given matrix.
   *
   * @param matrix
   *            sparse matrix
   * @param order
   *            ordering option (0 to 3); 0: natural ordering, 1: amd(A+A'),
   *            2: amd(S'*S), 3: amd(A'*A)
   * @return the LU-decomposition of the given matrix
   */
  def lu(matrix: StrideMatrix2D, order: Int): SparseDoubleLUDecomposition = {
    new SparseDoubleLUDecomposition(matrix, order, true)
  }

  /**
   * Returns the 1-norm of matrix <tt>A</tt>, which is the maximum absolute
   * column sum.
   */
  def norm1(A: StrideMatrix2D): Double = {
    DoubleProperty.DEFAULT.checkSparse(A)
    var norm: Double = 0.0
    norm = if (A.isInstanceOf[SparseCCDoubleMatrix2D]) Dcs_norm.cs_norm(A.elements().asInstanceOf[Dcs]) else Dcs_norm.cs_norm(A.asInstanceOf[SparseRCDoubleMatrix2D].getColumnCompressed
      .elements())
    norm
  }

  /**
   * Returns the infinity norm of matrix <tt>A</tt>, which is the maximum
   * absolute row sum.
   */
  def normInfinity(A: StrideMatrix2D): Double = {
    DoubleProperty.DEFAULT.checkSparse(A)
    var norm: Double = 0.0
    norm = if (A.isInstanceOf[SparseRCDoubleMatrix2D]) normInfinityRC(A.asInstanceOf[SparseRCDoubleMatrix2D]) else normInfinityRC(A.asInstanceOf[SparseCCDoubleMatrix2D].getRowCompressed)
    norm
  }

  /**
   * Returns the property object attached to this Algebra, defining tolerance.
   *
   * @return the Property object.
   * @see #setProperty(DoubleProperty)
   */
  def property(): DoubleProperty = property

  /**
   * Constructs and returns the QR-decomposition of the given matrix.
   *
   * @param matrix
   *            sparse matrix
   * @param order
   *            ordering option (0 to 3); 0: natural ordering, 1: amd(A+A'),
   *            2: amd(S'*S), 3: amd(A'*A)
   * @return the QR-decomposition of the given matrix
   */
  def qr(matrix: StrideMatrix2D, order: Int): SparseDoubleQRDecomposition = {
    new SparseDoubleQRDecomposition(matrix, order)
  }

  /**
   * Attaches the given property object to this Algebra, defining tolerance.
   *
   * @param property
   *            the Property object to be attached.
   * @throws UnsupportedOperationException
   *             if <tt>this==DEFAULT && property!=this.property()</tt> - The
   *             DEFAULT Algebra object is immutable.
   * @throws UnsupportedOperationException
   *             if <tt>this==ZERO && property!=this.property()</tt> - The
   *             ZERO Algebra object is immutable.
   * @see #property
   */
  def setProperty(property: DoubleProperty) {
    if (this == DEFAULT && property != this.property) throw new IllegalArgumentException("Attempted to modify immutable object.")
    if (this == ZERO && property != this.property) throw new IllegalArgumentException("Attempted to modify immutable object.")
    this.property = property
  }

  /**
   * Solves A*x = b.
   *
   * @param A
   *            sparse matrix
   * @param b
   *            right hand side
   * @return x; a new independent matrix; solution if A is square, least
   *         squares solution if A.rows() > A.columns(), underdetermined
   *         system solution if A.rows() < A.columns().
   */
  def solve(A: StrideMatrix2D, b: StrideMatrix1D): StrideMatrix1D = {
    val x = new DenseMatrix1D(Math.max(A.rows(), A.columns()))
    x.viewPart(0, b.size.toInt).assign(b)
    if (A.rows() == A.columns()) {
      lu(A, 0).solve(x)
      x
    } else {
      qr(A, 0).solve(x)
      x.viewPart(0, A.columns()).copy()
    }
  }
}
