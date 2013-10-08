package cern.colt.matrix.impl

/**
 * Dense 2-d matrix holding <tt>double</tt> elements. First see the <a
 * href="package-summary.html">package summary</a> and javadoc <a
 * href="package-tree.html">tree view</a> to get the broad picture.
 * <p>
 * <b>Implementation:</b>
 * <p>
 * This data structure allows to store more than 2**31 elements. Internally holds
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
class DenseLargeMatrix2D[@specialized T: Manifest: Numeric](rows: Int, columns: Int) extends RemappedMatrix2D[T] {

  var elementsVar = Array.ofDim[T](rows, columns)

  try {
    setUp(rows, columns)
  } catch {
    case exc: IllegalArgumentException => if ("matrix too large" != exc.getMessage) throw exc
  }

  override def getQuick(row: Int, column: Int): T = {
    elementsVar(row)(column)
  }

  override def setQuick(row: Int, column: Int, value: T) {
    elementsVar(row)(column) = value
  }

  override def like2D(rows: Int, columns: Int) = new DenseLargeMatrix2D[T](rows, columns)

  override def like1D(size: Int): StrideMatrix1D[T] = new DenseMatrix1D[T](size)
}
