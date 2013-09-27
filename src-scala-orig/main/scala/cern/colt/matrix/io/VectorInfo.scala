package cern.colt.matrix.io

import scala.beans.BooleanBeanProperty
import cern.colt.matrix.io.VectorField.VectorField

object VectorField extends Enumeration {

  val Real = new VectorField()

  val Integer = new VectorField()

  val Complex = new VectorField()

  val Pattern = new VectorField()

  class VectorField extends Val

  implicit def convertValue(v: Value): VectorField = v.asInstanceOf[VectorField]
}

/**
 * Contains information on a vector in a variant of the <a
 * href="http://math.nist.gov/MatrixMarket">Matrix Market</a> exchange format
 */
class VectorInfo(@BooleanBeanProperty var sparse: Boolean, var field: VectorField)
    {

  validate()

  /**
   * Validates the representation
   */
  private def validate() {
    if (isDense && isPattern) throw new IllegalArgumentException("Vector cannot be dense with pattern storage")
  }

  /**
   * Returns <code>true</code> if the vector is in coordinate format, else
   * <code>false</code>
   */
  def isCoordinate: Boolean = sparse

  /**
   * Returns <code>true</code> if the vector is in array format, else
   * <code>false</code>
   */
  def isDense: Boolean = !sparse

  /**
   * Returns <code>true</code> if the vector is in array format, else
   * <code>false</code>
   */
  def isArray: Boolean = !sparse

  /**
   * Returns <code>true</code> if the vector stores real numbers, else
   * <code>false</code>
   */
  def isReal: Boolean = field == VectorField.Real

  /**
   * Returns <code>true</code> if the vector stores integers, else
   * <code>false</code>
   */
  def isInteger: Boolean = field == VectorField.Integer

  /**
   * Returns <code>true</code> if the vector stores complex numbers, else
   * <code>false</code>
   */
  def isComplex: Boolean = field == VectorField.Complex

  /**
   * Returns <code>true</code> if the vector does not store any numbers, else
   * <code>false</code>
   */
  def isPattern: Boolean = field == VectorField.Pattern

  /**
   * Returns a string representation of the specifier. Can be used to provide
   * a header for writing to a file. It is a two-line output, which can look
   * like this:
   *
   * <pre>
   *      %%MatrixMarket vector coordinate real
   * </pre>
   */
  override def toString: String = {
    val buf = new StringBuilder()
    buf.append("%%MatrixMarket vector ")
    if (isSparse) buf.append("coordinate ") else buf.append("array ")
    if (isReal) buf.append("real\n") else if (isComplex) buf.append("complex\n") else if (isPattern) buf.append("pattern\n") else if (isInteger) buf.append("integer\n") else throw new IllegalArgumentException("Unknown field specification")
    buf.toString()
  }
}
