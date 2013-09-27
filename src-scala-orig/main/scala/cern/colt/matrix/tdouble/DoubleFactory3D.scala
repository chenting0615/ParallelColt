package cern.colt.matrix.tdouble

import cern.colt.matrix.tdouble.impl.DenseDoubleMatrix3D
import cern.colt.matrix.tdouble.impl.SparseDoubleMatrix3D
import cern.jet.math.tdouble.DoubleFunctions
import DoubleFactory3D._
//remove if not needed
import scala.collection.JavaConversions._

object DoubleFactory3D {

  /**
   * A factory producing dense matrices.
   */
  val dense = new DoubleFactory3D()

  /**
   * A factory producing sparse matrices.
   */
  val sparse = new DoubleFactory3D()
}

/**
 * Factory for convenient construction of 3-d matrices holding <tt>double</tt>
 * cells. Use idioms like <tt>DoubleFactory3D.dense.make(4,4,4)</tt> to
 * construct dense matrices, <tt>DoubleFactory3D.sparse.make(4,4,4)</tt> to
 * construct sparse matrices.
 *
 * If the factory is used frequently it might be useful to streamline the
 * notation. For example by aliasing:
 * <table>
 * <td class="PRE">
 *
 * <pre>
 *  DoubleFactory3D F = DoubleFactory3D.dense;
 *  F.make(4,4,4);
 *  F.descending(10,20,5);
 *  F.random(4,4,5);
 *  ...
 * </pre>
 *
 * </td>
 * </table>
 *
 * @author wolfgang.hoschek@cern.ch
 * @version 1.0, 09/24/99
 */
@SerialVersionUID(1L)
class DoubleFactory3D protected () extends cern.colt.PersistentObject {

  /**
   * Constructs a matrix with cells having ascending values. For debugging
   * purposes.
   */
  def ascending(slices: Int, rows: Int, columns: Int): DoubleMatrix3D = {
    descending(slices, rows, columns).assign(DoubleFunctions.chain(DoubleFunctions.neg, DoubleFunctions.minus(slices * rows * columns)))
  }

  /**
   * Constructs a matrix with cells having descending values. For debugging
   * purposes.
   */
  def descending(slices: Int, rows: Int, columns: Int): DoubleMatrix3D = {
    val matrix = make(slices, rows, columns)
    var v = 0
    var slice = slices
    while (slice >= 0) {
      var row = rows
      while (row >= 0) {
        var column = columns
        while (column >= 0) {
          matrix.setQuick(slice, row, column, v += 1)
        }
      }
    }
    matrix
  }

  /**
   * Constructs a matrix with the given cell values. <tt>values</tt> is
   * required to have the form <tt>values[slice][row][column]</tt> and have
   * exactly the same number of slices, rows and columns as the receiver.
   * <p>
   * The values are copied. So subsequent changes in <tt>values</tt> are not
   * reflected in the matrix, and vice-versa.
   *
   * @param values
   *            the values to be filled into the cells.
   * @return <tt>this</tt> (for convenience only).
   * @throws IllegalArgumentException
   *             if
   *             <tt>values.length != slices() || for any 0 &lt;= slice &lt; slices(): values[slice].length != rows()</tt>
   *             .
   * @throws IllegalArgumentException
   *             if
   *             <tt>for any 0 &lt;= column &lt; columns(): values[slice][row].length != columns()</tt>
   *             .
   */
  def make(values: Array[Array[Array[Double]]]): DoubleMatrix3D = {
    if (this == sparse) return new SparseDoubleMatrix3D(values)
    new DenseDoubleMatrix3D(values)
  }

  /**
   * Constructs a matrix with the given shape, each cell initialized with
   * zero.
   */
  def make(slices: Int, rows: Int, columns: Int): DoubleMatrix3D = {
    if (this == sparse) return new SparseDoubleMatrix3D(slices, rows, columns)
    new DenseDoubleMatrix3D(slices, rows, columns)
  }

  /**
   * Constructs a matrix with the given shape, each cell initialized with the
   * given value.
   */
  def make(slices: Int, 
      rows: Int, 
      columns: Int, 
      initialValue: Double): DoubleMatrix3D = {
    make(slices, rows, columns).assign(initialValue)
  }

  /**
   * Constructs a matrix with uniformly distributed values in <tt>(0,1)</tt>
   * (exclusive).
   */
  def random(slices: Int, rows: Int, columns: Int): DoubleMatrix3D = {
    make(slices, rows, columns).assign(cern.jet.math.tdouble.DoubleFunctions.random())
  }
}
