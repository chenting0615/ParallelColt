package cern.colt.matrix.tint.impl

import cern.colt.matrix.tint.IntMatrix1D
import cern.colt.matrix.tint.IntMatrix2D
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Dense 2-d matrix holding <tt>int</tt> elements. First see the <a
 * href="package-summary.html">package summary</a> and javadoc <a
 * href="package-tree.html">tree view</a> to get the broad picture.
 * <p>
 * <b>Implementation:</b>
 * <p>
 * This data structure allows to store more than 2^31 elements. Internally holds
 * one two-dimensional array, elements[rows][columns]. Note that this
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
class DenseLargeIntMatrix2D(rows: Int, columns: Int) extends WrapperIntMatrix2D(null) {

  var elements: Array[Array[Int]] = new Array[Array[Int]](rows, columns)

  try {
    setUp(rows, columns)
  } catch {
    case exc: IllegalArgumentException => if ("matrix too large" != exc.getMessage) throw exc
  }

  content = this

  def getQuick(row: Int, column: Int): Int = elements(row)(column)

  def setQuick(row: Int, column: Int, value: Int) {
    elements(row)(column) = value
  }

  protected def getStorageMatrix(): IntMatrix2D = this

  def like(rows: Int, columns: Int): IntMatrix2D = {
    new DenseLargeIntMatrix2D(rows, columns)
  }

  def like1D(size: Int): IntMatrix1D = new DenseIntMatrix1D(size)
}
