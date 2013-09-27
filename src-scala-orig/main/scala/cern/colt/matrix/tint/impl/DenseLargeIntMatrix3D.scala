package cern.colt.matrix.tint.impl

import cern.colt.matrix.tint.IntMatrix3D
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Dense 3-d matrix holding <tt>int</tt> elements. First see the <a
 * href="package-summary.html">package summary</a> and javadoc <a
 * href="package-tree.html">tree view</a> to get the broad picture.
 * <p>
 * <b>Implementation:</b>
 * <p>
 * This data structure allows to store more than 2^31 elements. Internally holds
 * one three-dimensional array, elements[slices][rows][columns]. Note that this
 * implementation is not synchronized.
 * <p>
 * <b>Time complexity:</b>
 * <p>
 * <tt>O(1)</tt> (i.e. constant time) for the basic operations <tt>get</tt>,
 * <tt>getQuick</tt>, <tt>set</tt>, <tt>setQuick</tt> and <tt>size</tt>.
 *
 * @author Piotr Wendykier (piotr.wendykier@gmail.com)
 *
 */
@SerialVersionUID(1L)
class DenseLargeIntMatrix3D(slices: Int, rows: Int, columns: Int) extends WrapperIntMatrix3D(null) {

  var elements: Array[Array[Array[Int]]] = new Array[Array[Array[Int]]](slices, rows, columns)

  try {
    setUp(slices, rows, columns)
  } catch {
    case exc: IllegalArgumentException => if ("matrix too large" != exc.getMessage) throw exc
  }

  def getQuick(slice: Int, row: Int, column: Int): Int = elements(slice)(row)(column)

  def setQuick(slice: Int,
      row: Int,
      column: Int,
      value: Int) {
    elements(slice)(row)(column) = value
  }

  protected def getStorageMatrix(): IntMatrix3D = this

  def like(slices: Int, rows: Int, columns: Int): IntMatrix3D = {
    new DenseLargeIntMatrix3D(slices, rows, columns)
  }
}
