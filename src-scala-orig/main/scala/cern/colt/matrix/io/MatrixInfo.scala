package cern.colt.matrix.io

import scala.beans.BooleanBeanProperty

object MatrixField extends Enumeration {

  val Real = new MatrixField()

  val Integer = new MatrixField()

  val Complex = new MatrixField()

  val Pattern = new MatrixField()

  class MatrixField extends Val

  implicit def convertValue(v: Value): MatrixField = v.asInstanceOf[MatrixField]
}

object MatrixSymmetry extends Enumeration {

  val General = new MatrixSymmetry()

  val Symmetric = new MatrixSymmetry()

  val SkewSymmetric = new MatrixSymmetry()

  val Hermitian = new MatrixSymmetry()

  class MatrixSymmetry extends Val

  implicit def convertValue(v: Value): MatrixSymmetry = v.asInstanceOf[MatrixSymmetry]
}

/**
 * Contains information on a matrix in the <a
 * href="http://math.nist.gov/MatrixMarket">Matrix Market</a> exchange format.
 * Supports all valid matrices.
 */
class MatrixInfo(@BooleanBeanProperty var sparse: Boolean, var field: MatrixField.MatrixField, var symmetry: MatrixSymmetry.MatrixSymmetry) {

  validate()

  /**
   * Validates the representation
   */
  private def validate() {
    if (isDense && isPattern) throw new IllegalArgumentException("Matrix cannot be dense with pattern storage")
    if (isReal && isHermitian) throw new IllegalArgumentException("Data cannot be real with hermitian symmetry")
    if (!isComplex && isHermitian) throw new IllegalArgumentException("Data must be complex with hermitian symmetry")
    if (isPattern && isSkewSymmetric) throw new IllegalArgumentException("Storage cannot be pattern and skew symmetrical")
  }

  /**
   * Returns <code>true</code> if the matrix is in coordinate format, else
   * <code>false</code>
   */
  def isCoordinate: Boolean = sparse

  /**
   * Returns <code>true</code> if the matrix is in array format, else
   * <code>false</code>
   */
  def isDense: Boolean = !sparse

  /**
   * Returns <code>true</code> if the matrix is in array format, else
   * <code>false</code>
   */
  def isArray: Boolean = !sparse

  /**
   * Returns <code>true</code> if the matrix stores real numbers, else
   * <code>false</code>
   */
  def isReal: Boolean = field == MatrixField.Real

  /**
   * Returns <code>true</code> if the matrix stores integers, else
   * <code>false</code>
   */
  def isInteger: Boolean = field == MatrixField.Integer

  /**
   * Returns <code>true</code> if the matrix stores complex numbers, else
   * <code>false</code>
   */
  def isComplex: Boolean = field == MatrixField.Complex

  /**
   * Returns <code>true</code> if the matrix does not store any numbers, else
   * <code>false</code>
   */
  def isPattern: Boolean = field == MatrixField.Pattern

  /**
   * Returns <code>true</code> if the matrix form is general, else
   * <code>false</code>
   */
  def isGeneral: Boolean = symmetry == MatrixSymmetry.General

  /**
   * Returns <code>true</code> if the matrix is symmetrical, else
   * <code>false</code>
   */
  def isSymmetric: Boolean = symmetry == MatrixSymmetry.Symmetric

  /**
   * Returns <code>true</code> if the matrix is skew-symmetrical, else
   * <code>false</code>
   */
  def isSkewSymmetric: Boolean = symmetry == MatrixSymmetry.SkewSymmetric

  /**
   * Returns <code>true</code> if the matrix is Hermitian, else
   * <code>false</code>
   */
  def isHermitian: Boolean = symmetry == MatrixSymmetry.Hermitian

  /**
   * Returns a string representation of the specifier. Can be used to provide
   * a header for writing to a file. It is a two-line output, which can look
   * like this:
   *
   * <pre>
   *       %%MatrixMarket matrix coordinate real general
   * </pre>
   */
  override def toString: String = {
    val buf = new StringBuilder()
    buf.append("%%MatrixMarket matrix ")
    if (isSparse) buf.append("coordinate ") else buf.append("array ")
    if (isReal) buf.append("real ") else if (isComplex) buf.append("complex ") else if (isPattern) buf.append("pattern ") else if (isInteger) buf.append("integer ") else throw new IllegalArgumentException("Unknown field specification")
    if (isGeneral) buf.append("general\n") else if (isSymmetric) buf.append("symmetric\n") else if (isSkewSymmetric) buf.append("skew-symmetric\n") else if (isHermitian) buf.append("Hermitian\n") else throw new IllegalArgumentException("Unknown symmetry specification")
    buf.toString()
  }
}
