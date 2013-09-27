package cern.colt.matrix.tdouble.impl

import cern.colt.map.tdouble.AbstractLongDoubleMap
import cern.colt.map.tdouble.OpenLongDoubleHashMap
import cern.colt.matrix.tdouble.StrideMatrix1D
import cern.colt.matrix.tdouble.StrideMatrix2D
import cern.colt.matrix.tdouble.DoubleMatrix3D
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Sparse hashed 3-d matrix holding <tt>double</tt> elements. First see the <a
 * href="package-summary.html">package summary</a> and javadoc <a
 * href="package-tree.html">tree view</a> to get the broad picture.
 * <p>
 * <b>Implementation:</b>
 * <p>
 * Note that this implementation is not synchronized. Uses a
 * {@link cern.colt.map.tdouble.OpenIntDoubleHashMap}, which is a compact and
 * performant hashing technique.
 * <p>
 * <b>Memory requirements:</b>
 * <p>
 * Cells that
 * <ul>
 * <li>are never set to non-zero values do not use any memory.
 * <li>switch from zero to non-zero state do use memory.
 * <li>switch back from non-zero to zero state also do use memory. However,
 * their memory is automatically reclaimed from time to time. It can also
 * manually be reclaimed by calling {@link #trimToSize()}.
 * </ul>
 * <p>
 * worst case: <tt>memory [bytes] = (1/minLoadFactor) * nonZeros * 13</tt>. <br>
 * best case: <tt>memory [bytes] = (1/maxLoadFactor) * nonZeros * 13</tt>. <br>
 * Where <tt>nonZeros = cardinality()</tt> is the number of non-zero cells.
 * Thus, a 100 x 100 x 100 matrix with minLoadFactor=0.25 and maxLoadFactor=0.5
 * and 1000000 non-zero cells consumes between 25 MB and 50 MB. The same 100 x
 * 100 x 100 matrix with 1000 non-zero cells consumes between 25 and 50 KB.
 * <p>
 * <b>Time complexity:</b>
 * <p>
 * This class offers <i>expected</i> time complexity <tt>O(1)</tt> (i.e.
 * constant time) for the basic operations <tt>get</tt>, <tt>getQuick</tt>,
 * <tt>set</tt>, <tt>setQuick</tt> and <tt>size</tt> assuming the hash function
 * disperses the elements properly among the buckets. Otherwise, pathological
 * cases, although highly improbable, can occur, degrading performance to
 * <tt>O(N)</tt> in the worst case. As such this sparse class is expected to
 * have no worse time complexity than its dense counterpart
 * {@link DenseDoubleMatrix2D}. However, constant factors are considerably
 * larger.
 * <p>
 * Cells are internally addressed in (in decreasing order of significance):
 * slice major, row major, column major. Applications demanding utmost speed can
 * exploit this fact. Setting/getting values in a loop slice-by-slice,
 * row-by-row, column-by-column is quicker than, for example, column-by-column,
 * row-by-row, slice-by-slice. Thus
 *
 * <pre>
 * for (int slice = 0; slice &lt; slices; slice++) {
 *     for (int row = 0; row &lt; rows; row++) {
 *         for (int column = 0; column &lt; columns; column++) {
 *             matrix.setQuick(slice, row, column, someValue);
 *         }
 *     }
 * }
 * </pre>
 *
 * is quicker than
 *
 * <pre>
 * for (int column = 0; column &lt; columns; column++) {
 *     for (int row = 0; row &lt; rows; row++) {
 *         for (int slice = 0; slice &lt; slices; slice++) {
 *             matrix.setQuick(slice, row, column, someValue);
 *         }
 *     }
 * }
 * </pre>
 *
 * @see cern.colt.map
 * @see cern.colt.map.tdouble.OpenIntDoubleHashMap
 * @author wolfgang.hoschek@cern.ch
 * @version 1.0, 09/24/99
 *
 * @author Piotr Wendykier (piotr.wendykier@gmail.com)
 */
@SerialVersionUID(1L)
class SparseDoubleMatrix3D(slices: Int,
    rows: Int,
    columns: Int,
    initialCapacity: Int,
    minLoadFactor: Double,
    maxLoadFactor: Double) extends DoubleMatrix3D {

  protected var elements: AbstractLongDoubleMap = new OpenLongDoubleHashMap(initialCapacity, minLoadFactor,
    maxLoadFactor)

  try {
    setUp(slices, rows, columns)
  } catch {
    case exc: IllegalArgumentException => if ("matrix too large" != exc.getMessage) throw exc
  }

  /**
   * Constructs a matrix with a copy of the given values. <tt>values</tt> is
   * required to have the form <tt>values[slice][row][column]</tt> and have
   * exactly the same number of rows in in every slice and exactly the same
   * number of columns in in every row.
   * <p>
   * The values are copied. So subsequent changes in <tt>values</tt> are not
   * reflected in the matrix, and vice-versa.
   *
   * @param values
   *            The values to be filled into the new matrix.
   * @throws IllegalArgumentException
   *             if
   *             <tt>for any 1 &lt;= slice &lt; values.length: values[slice].length != values[slice-1].length</tt>
   *             .
   * @throws IllegalArgumentException
   *             if
   *             <tt>for any 1 &lt;= row &lt; values[0].length: values[slice][row].length != values[slice][row-1].length</tt>
   *             .
   */
  def this(values: Array[Array[Array[Double]]]) {
    this(values.length, (if (values.length == 0) 0 else values(0).length), (if (values.length == 0) 0 else if (values(0).length == 0) 0 else values(0)(0).length))
    assign(values)
  }

  /**
   * Constructs a matrix with a given number of slices, rows and columns and
   * default memory usage. All entries are initially <tt>0</tt>.
   *
   * @param slices
   *            the number of slices the matrix shall have.
   * @param rows
   *            the number of rows the matrix shall have.
   * @param columns
   *            the number of columns the matrix shall have.
   * @throws IllegalArgumentException
   *             if <tt>(double)slices*columns*rows > Integer.MAX_VALUE</tt>.
   * @throws IllegalArgumentException
   *             if <tt>slices<0 || rows<0 || columns<0</tt>.
   */
  def this(slices: Int, rows: Int, columns: Int) {
    this(slices, rows, columns, slices * rows * (columns / 1000), 0.2, 0.5)
  }

  /**
   * Constructs a view with the given parameters.
   *
   * @param slices
   *            the number of slices the matrix shall have.
   * @param rows
   *            the number of rows the matrix shall have.
   * @param columns
   *            the number of columns the matrix shall have.
   * @param elements
   *            the cells.
   * @param sliceZero
   *            the position of the first element.
   * @param rowZero
   *            the position of the first element.
   * @param columnZero
   *            the position of the first element.
   * @param sliceStride
   *            the number of elements between two slices, i.e.
   *            <tt>index(k+1,i,j)-index(k,i,j)</tt>.
   * @param rowStride
   *            the number of elements between two rows, i.e.
   *            <tt>index(k,i+1,j)-index(k,i,j)</tt>.
   * @param columnnStride
   *            the number of elements between two columns, i.e.
   *            <tt>index(k,i,j+1)-index(k,i,j)</tt>.
   * @throws IllegalArgumentException
   *             if <tt>(double)slices*columns*rows > Integer.MAX_VALUE</tt>.
   * @throws IllegalArgumentException
   *             if <tt>slices<0 || rows<0 || columns<0</tt>.
   */
  protected def this(slices: Int,
      rows: Int,
      columns: Int,
      elements: AbstractLongDoubleMap,
      sliceZero: Int,
      rowZero: Int,
      columnZero: Int,
      sliceStride: Int,
      rowStride: Int,
      columnStride: Int) {
    this()
    try {
      setUp(slices, rows, columns, sliceZero, rowZero, columnZero, sliceStride, rowStride, columnStride)
    } catch {
      case exc: IllegalArgumentException => if ("matrix too large" != exc.getMessage) throw exc
    }
    this.elements = elements
    this.isNoView = false
  }

  def assign(value: Double): DoubleMatrix3D = {
    if (this.isNoView && value == 0) this.elements.clear() else super.assign(value)
    this
  }

  def cardinality(): Int = {
    if (this.isNoView) this.elements.size else super.cardinality()
  }

  def elements(): AbstractLongDoubleMap = elements

  def ensureCapacity(minCapacity: Int) {
    this.elements.ensureCapacity(minCapacity)
  }

  def getQuick(slice: Int, row: Int, column: Int): Double = {
    synchronized {
      elements.get(sliceZero.toLong + slice.toLong * sliceStride.toLong +
        rowZero.toLong +
        row.toLong * rowStride.toLong +
        columnZero.toLong +
        column.toLong * columnStride.toLong)
    }
  }

  def index(slice: Int, row: Int, column: Int): Long = {
    sliceZero.toLong + slice.toLong * sliceStride.toLong +
      rowZero.toLong +
      row.toLong * rowStride.toLong +
      columnZero.toLong +
      column.toLong * columnStride.toLong
  }

  def like(slices: Int, rows: Int, columns: Int): DoubleMatrix3D = {
    new SparseDoubleMatrix3D(slices, rows, columns)
  }

  def like2D(rows: Int, columns: Int): StrideMatrix2D = new SparseDoubleMatrix2D(rows, columns)

  def setQuick(slice: Int,
      row: Int,
      column: Int,
      value: Double) {
    synchronized {
      val index = sliceZero.toLong + slice.toLong * sliceStride.toLong +
        rowZero.toLong +
        row.toLong * rowStride.toLong +
        columnZero.toLong +
        column.toLong * columnStride.toLong
      if (value == 0) this.elements.removeKey(index) else this.elements.put(index, value)
    }
  }

  override def toString(): String = {
    val builder = new StringBuilder()
    builder.append(slices).append(" x ").append(rows).append(" x ")
      .append(columns)
      .append(" sparse matrix, nnz = ")
      .append(cardinality())
      .append('\n')
    for (s <- 0 until slices; r <- 0 until rows; c <- 0 until columns) {
      val elem = getQuick(s, r, c)
      if (elem != 0) {
        builder.append('(').append(s).append(',').append(r)
          .append(',')
          .append(c)
          .append(')')
          .append('\t')
          .append(elem)
          .append('\n')
      }
    }
    builder.toString
  }

  def trimToSize() {
    this.elements.trimToSize()
  }

  def vectorize(): StrideMatrix1D = {
    val v = new SparseDoubleMatrix1D(size.toInt)
    val length = rows * columns
    for (s <- 0 until slices) {
      v.viewPart(s * length, length).assign(viewSlice(s).vectorize())
    }
    v
  }

  protected def haveSharedCellsRaw(other: DoubleMatrix3D): Boolean = {
    if (other.isInstanceOf[SelectedSparseDoubleMatrix3D]) {
      val otherMatrix = other.asInstanceOf[SelectedSparseDoubleMatrix3D]
      return this.elements == otherMatrix.elements
    } else if (other.isInstanceOf[SparseDoubleMatrix3D]) {
      val otherMatrix = other.asInstanceOf[SparseDoubleMatrix3D]
      return this.elements == otherMatrix.elements
    }
    false
  }

  protected def like2D(rows: Int,
      columns: Int,
      rowZero: Int,
      columnZero: Int,
      rowStride: Int,
      columnStride: Int): StrideMatrix2D = {
    new SparseDoubleMatrix2D(rows, columns, this.elements, rowZero, columnZero, rowStride, columnStride)
  }

  protected def viewSelectionLike(sliceOffsets: Array[Int], rowOffsets: Array[Int], columnOffsets: Array[Int]): DoubleMatrix3D = {
    new SelectedSparseDoubleMatrix3D(this.elements, sliceOffsets, rowOffsets, columnOffsets, 0)
  }
}
