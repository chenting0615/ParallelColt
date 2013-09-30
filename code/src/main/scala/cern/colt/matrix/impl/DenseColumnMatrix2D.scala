package cern.colt.matrix.impl

/**
 * Dense 2-d matrix holding <tt>double</tt> elements. First see the <a
 * href="package-summary.html">package summary</a> and javadoc <a
 * href="package-tree.html">tree view</a> to get the broad picture.
 * <p>
 * <b>Implementation:</b>
 * <p>
 * Internally holds one single contigous one-dimensional array, addressed in
 * column major. Note that this implementation is not synchronized.
 * <p>
 * <b>Time complexity:</b>
 * <p>
 * <tt>O(1)</tt> (i.e. constant time) for the basic operations <tt>get</tt>,
 * <tt>getQuick</tt>, <tt>set</tt>, <tt>setQuick</tt> and <tt>size</tt>,
 * <p>
 * Cells are internally addressed in column-major. Applications demanding utmost
 * speed can exploit this fact. Setting/getting values in a loop
 * column-by-column is quicker than row-by-row. Thus
 *
 * <pre>
 * for (int column = 0; column &lt; columns; column++) {
 *     for (int row = 0; row &lt; rows; row++) {
 *         matrix.setQuick(row, column, someValue);
 *     }
 * }
 *
 * </pre>
 *
 * is quicker than
 *
 * <pre>
 * for (int row = 0; row &lt; rows; row++) {
 *     for (int column = 0; column &lt; columns; column++) {
 *         matrix.setQuick(row, column, someValue);
 *     }
 * }
 *
 * </pre>
 *
 * @author Piotr Wendykier (piotr.wendykier@gmail.com)
 *
 */
@specialized
@SerialVersionUID(1L)
class DenseColumnMatrix2D[T: Manifest](rows: Int, columns: Int) extends DenseMatrix2D[T](rows, columns, elements=null, rowZero=0, columnZero=0, rowStride=1, columnStride=rows, isView=false) {

  /**
   * Constructs a matrix with a copy of the given values. <tt>values</tt> is
   * required to have the form <tt>values[row][column]</tt> and have exactly
   * the same number of columns in every row.
   * <p>
   * The values are copied. So subsequent changes in <tt>values</tt> are not
   * reflected in the matrix, and vice-versa.
   *
   * @param values
   *            The values to be filled into the new matrix.
   * @throws IllegalArgumentException
   *             if
   *             <tt>for any 1 &lt;= row &lt; values.length: values[row].length != values[row-1].length</tt>
   *             .
   */
  def this(values: Array[Array[T]]) {
    this(values.length, if (values.length == 0) 0 else values(0).length)
    assign(values)
  }

  override def isRowMajor = false

  /**
   * Returns a new matrix that has the same elements as this matrix, but they
   * are addressed internally in row major. This method creates a new object
   * (not a view), so changes in the returned matrix are NOT reflected in this
   * matrix.
   *
   * @return this matrix with elements addressed internally in row major
   */
  def getRowMajor: DenseMatrix2D[T] = {
    val R = new DenseMatrix2D[T](rows, columns)
    forEachNonZero(new Function3[Int, Int, T, T]() {
      def apply(row: Int, column: Int, value: T) = {
        R.setQuick(row, column, value)
        value
      }
    })
    R
  }

  override def like2D(rows: Int, columns: Int) = new DenseColumnMatrix2D[T](rows, columns)

  override def like1D(size: Int) = new DenseMatrix1D[T](size)
}
