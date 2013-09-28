package cern.colt.matrix.impl

import it.unimi.dsi.fastutil.longs.LongSet
import cern.colt.matrix.{Matrix1D, Matrix2D}
import scala.util.Sorting
//import scala.math.Numeric.Implicits._

/**
 * Sparse hashed 2-d matrix holding <tt>double</tt> elements. First see the <a
 * href="package-summary.html">package summary</a> and javadoc <a
 * href="package-tree.html">tree view</a> to get the broad picture.
 * <p>
 * <b>Implementation:</b>
 * <p>
 * Note that this implementation is not synchronized. Uses a
 * LongDoubleHashMap, which is a compact and
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
 * manually be reclaimed by calling trimToSize().
 * </ul>
 * <p>
 * worst case: <tt>memory [bytes] = (1/minLoadFactor) * nonZeros * 13</tt>. <br>
 * best case: <tt>memory [bytes] = (1/maxLoadFactor) * nonZeros * 13</tt>. <br>
 * Where <tt>nonZeros = cardinality()</tt> is the number of non-zero cells.
 * Thus, a 1000 x 1000 matrix with minLoadFactor=0.25 and maxLoadFactor=0.5 and
 * 1000000 non-zero cells consumes between 25 MB and 50 MB. The same 1000 x 1000
 * matrix with 1000 non-zero cells consumes between 25 and 50 KB.
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
 * DenseMatrix2D. However, constant factors are considerably
 * larger.
 * <p>
 * Cells are internally addressed in row-major. Performance sensitive
 * applications can exploit this fact. Setting values in a loop row-by-row is
 * quicker than column-by-column, because fewer hash collisions occur. Thus
 *
 * <pre>
 * for (int row = 0; row &lt; rows; row++) {
 *     for (int column = 0; column &lt; columns; column++) {
 *         matrix.setQuick(row, column, someValue);
 *     }
 * }
 * </pre>
 *
 * is quicker than
 *
 * <pre>
 * for (int column = 0; column &lt; columns; column++) {
 *     for (int row = 0; row &lt; rows; row++) {
 *         matrix.setQuick(row, column, someValue);
 *     }
 * }
 * </pre>
 *
 * @see cern.colt.map
 * @see cern.colt.map.tdouble.OpenLongDoubleHashMap
 * @author wolfgang.hoschek@cern.ch
 * @version 1.0, 09/24/99
 *
 * @author Piotr Wendykier (piotr.wendykier@gmail.com)
 */
@specialized
@SerialVersionUID(1L)
class SparseHashMatrix2D[T](rows: Int, columns: Int, initialCapacity: Int, loadFactor: Double)(implicit factory: FastUtilLongMap[T], m: Manifest[T]) extends StrideMatrix2D[T] {

  //import n._

  private var elementsVar = factory.createMap(initialCapacity, loadFactor.toFloat)

  try {
    setUp(rows, columns)
  } catch {
    case exc: IllegalArgumentException => if ("matrix too large" != exc.getMessage) throw exc
  }

  /**
   * Constructs a matrix with a given number of rows and columns and default
   * memory usage. All entries are initially <tt>0</tt>.
   *
   * @param rows
   *            the number of rows the matrix shall have.
   * @param columns
   *            the number of columns the matrix shall have.
   * @throws IllegalArgumentException
   *             if
   *             <tt>rows<0 || columns<0 || (double)columns*rows > Integer.MAX_VALUE</tt>
   *             .
   */
  def this(rows: Int, columns: Int)(implicit factory: FastUtilMap[Long, T], m: Manifest[T]) {
    this(rows, columns, rows * columns / 1000, 0.35)
  }

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
  def this(values: Array[Array[T]])(implicit factory: FastUtilMap[Long, T], m: Manifest[T]) {
    this(values.length, if (values.length == 0) 0 else values(0).length)
    assign(values)
  }

  /**
   * Constructs a matrix with a copy of the given indexes and values.
   *
   * @param rows
   *            the number of rows the matrix shall have.
   * @param columns
   *            the number of columns the matrix shall have.
   * @param rowIndexes
   *            row indexes
   * @param columnIndexes
   *            column indexes
   * @param values
   *            numerical values
   */
  def this(rows: Int, columns: Int, rowIndexes: Array[Int], columnIndexes: Array[Int], values: Array[T])(implicit factory: FastUtilMap[Long, T], m: Manifest[T]) {
    this(rows, columns)
    insert(rowIndexes, columnIndexes, values)
  }

  override def assignConstant(value: T) = {
    if (this.isNoView && value == 0)
      this.elementsVar.clear()
    else
      super.assignConstant(value)
    this
  }

  override def assign(source: Matrix2D[T]) = {
    if (source != this) {
      checkShape(source)
      var doFallBackAssign = true
      source match {
        case other: SparseHashMatrix2D[T] => {
          if (this.isNoView && other.isNoView) {
            this.elementsVar.clear()
            this.elementsVar.putAll(other.elementsVar)    //TODO fix and uncomment
            doFallBackAssign = false
          }
        }
      }
      if (doFallBackAssign) {
        elementsVar.clear()
        source.forEachNonZero(new Function3[Int, Int, T, T]() {
          def apply(row: Int, column: Int, value: T) = {
            set(row, column, value)
            value
          }
        })
      }
    }
    this
  }

  override def numNonZero = {
    if (size == elementsVar.size())
      elementsVar.size()
    else
      super.numNonZero
  }

  /**
   * Returns a new matrix that has the same elements as this matrix, but is in
   * a column-compressed form. This method creates a new object (not a view),
   * so changes in the returned matrix are NOT reflected in this matrix.
   *
   * @param sortRowIndexes
   *            if true, then row indexes in column compressed matrix are
   *            sorted
   *
   * @return this matrix in a column-compressed form
   */
  def getColumnCompressed(sortRowIndexes: Boolean): SparseCCMatrix2D[T] = {
    val keys = elementsVar.keySet()
    val nnz = numNonZero.toInt
    val rowIndexes = Array.ofDim[Int](nnz)
    val columnIndexes = Array.ofDim[Int](nnz)
    val values = Array.ofDim[T](nnz)
    val iter = keys.iterator()
    var k = 0
    while(iter.hasNext) {
      val key = iter.next().asInstanceOf[Long]
      rowIndexes(k) = (key / columns).toInt
      columnIndexes(k) = (key % columns).toInt
      values(k) = elementsVar.get(key)
      k += 1
    }
    new SparseCCMatrix2D[T](rows, columns, rowIndexes, columnIndexes, values, false, false, sortRowIndexes)
  }

  /**
   * Returns a new matrix that has the same elements as this matrix, but is in
   * a column-compressed modified form. This method creates a new object (not
   * a view), so changes in the returned matrix are NOT reflected in this
   * matrix.
   *
   * @return this matrix in a column-compressed modified form
   */
  def getColumnCompressedModified: SparseCCMMatrix2D[T] = {
    val A = new SparseCCMMatrix2D[T](rows, columns)
    val keys = elementsVar.keySet().asInstanceOf[LongSet].toLongArray
    Sorting.quickSort(keys)
    for(key <- keys) {
      val row = (key / columns).toInt
      val column = (key % columns).toInt
      A.setQuick(row, column, elementsVar.get(key))
    }
    A
  }

  /**
   * Returns a new matrix that has the same elements as this matrix, but is in
   * a row-compressed form. This method creates a new object (not a view), so
   * changes in the returned matrix are NOT reflected in this matrix.
   *
   * @param sortColumnIndexes
   *            if true, then column indexes in row compressed matrix are
   *            sorted
   *
   * @return this matrix in a row-compressed form
   */
  def getRowCompressed(sortColumnIndexes: Boolean): SparseRCMatrix2D[T] = {
    val nnz = numNonZero.toInt
    val rowIndexes = Array.ofDim[Int](nnz)
    val columnIndexes = Array.ofDim[Int](nnz)
    val values = Array.ofDim[T](nnz)
    val keys = elementsVar.keySet().asInstanceOf[LongSet].toLongArray
    Sorting.quickSort(keys)
    var k = 0
    for (key <- keys) {
      rowIndexes(k) = (key / columns).toInt
      columnIndexes(k) = (key % columns).toInt
      values(k) = elementsVar.get(key)
      k += 1
    }
    new SparseRCMatrix2D[T](rows, columns, rowIndexes, columnIndexes, values, sortColumnIndexes)
  }

  /**
   * Returns a new matrix that has the same elements as this matrix, but is in
   * a row-compressed modified form. This method creates a new object (not a
   * view), so changes in the returned matrix are NOT reflected in this
   * matrix.
   *
   * @return this matrix in a row-compressed modified form
   */
  def getRowCompressedModified: SparseRCMMatrix2D[T] = {
    val A = new SparseRCMMatrix2D[T](rows, columns)
    val keys = elementsVar.keySet().asInstanceOf[LongSet].toLongArray
    Sorting.quickSort(keys)
    for(key <- keys) {
      val row = (key / columns).toInt
      val column = (key % columns).toInt
      A.setQuick(row, column, elementsVar.get(key))
    }
    A
  }

/*
  override def ensureCapacity(minCapacity: Int) {
    this.elements.ensureCapacity(minCapacity)
  }
*/

  override def forEachNonZero(function: Function3[Int, Int, T, T]): StrideMatrix2D[T] = {
    if (this.isNoView) {
      for(key <- elementsVar.keySet().asInstanceOf[LongSet].toLongArray) {
        val row = (key / columns).toInt
        val column = (key % columns).toInt
        val oldValue = elementsVar.get(key)
        val newValue = function.apply(row, column, oldValue)
        if (oldValue != newValue)
          setQuick(row, column, newValue)
      }
    }
    else {
      super.forEachNonZero(function)
    }
    this
  }

  def getQuick(row: Int, column: Int): T = {
    elementsVar.get(toRawIndex(row, column))
  }

  def like2D(rows: Int, columns: Int): StrideMatrix2D[T] = {
    new SparseHashMatrix2D[T](rows, columns)
  }

  def like1D(size: Int): StrideMatrix1D[T] = new SparseMatrix1D[T](size)

  def setQuick(row: Int, column: Int, value: T) {
    synchronized {
      val index = toRawIndex(row, column).toLong
      if (value == 0)
        this.elementsVar.remove(index)
      else
        this.elementsVar.put(index, value)
    }
  }

  override def toString: String = {
    val builder = new StringBuilder()
    builder.append(rows).append(" x ").append(columns).append(" sparse matrix, nnz = ")
      .append(numNonZero)
      .append('\n')
    for (r <- 0 until rows; c <- 0 until columns) {
      val elem = getQuick(r, c)
      if (elem != 0) {
        builder.append('(').append(r).append(',').append(c)
          .append(')')
          .append('\t')
          .append(elem)
          .append('\n')
      }
    }
    builder.toString()
  }

  override def trimToSize() {
//    factory.trim(this.elementsVar)
    this.elementsVar.trim()
  }

  private def insert(rowIndexes: Array[Int], columnIndexes: Array[Int], values: Array[T]) {
    val size = rowIndexes.length
    for (i <- 0 until size) {
      val value = values(i)
      val row = rowIndexes(i)
      val column = columnIndexes(i)
      if (row >= rows || column >= columns) {
        throw new IndexOutOfBoundsException("row: " + row + ", column: " + column + "rows()xcolumns()=" + rows + "x" + columns)
      }
      val index = toRawIndex(row, column).toLong
      if (value != 0) {
        elementsVar.put(index, value)
      }
      else {
        val elem = elementsVar.get(index)
        if (elem != 0) {
          elementsVar.remove(index)
        }
      }
    }
  }

  /**
   * Constructs and returns a new <i>slice view</i> representing the columns
   * of the given row. The returned view is backed by this matrix, so changes
   * in the returned view are reflected in this matrix, and vice-versa. To
   * obtain a slice view on subranges, construct a sub-ranging view (
   * <tt>viewPart(...)</tt>), then apply this method to the sub-range view.
   * <p>
   * <b>Example:</b>
   * <table border="0">
   * <tr nowrap>
   * <td valign="top">2 x 3 matrix: <br>
   * 1, 2, 3<br>
   * 4, 5, 6</td>
   * <td>viewRow(0) ==></td>
   * <td valign="top">Matrix1D of size 3:<br>
   * 1, 2, 3</td>
   * </tr>
   * </table>
   *
   * @param row
   * the row to fix.
   * @return a new slice view.
   * @throws IndexOutOfBoundsException
   *         if <tt>row < 0 || row >= rows()</tt>.
   * @see #viewColumn(int)
   */
  def viewRow(row: Int): Matrix1D[T] = {
    checkRow(row)
    new WrappedRowMatrix1D(this, row)
  }

  /**
   * Constructs and returns a new <i>slice view</i> representing the rows of
   * the given column. The returned view is backed by this matrix, so changes
   * in the returned view are reflected in this matrix, and vice-versa. To
   * obtain a slice view on subranges, construct a sub-ranging view (
   * <tt>viewPart(...)</tt>), then apply this method to the sub-range view.
   * <p>
   * <b>Example:</b>
   * <table border="0">
   * <tr nowrap>
   * <td valign="top">2 x 3 matrix: <br>
   * 1, 2, 3<br>
   * 4, 5, 6</td>
   * <td>viewColumn(0) ==></td>
   * <td valign="top">Matrix1D of size 2:<br>
   * 1, 4</td>
   * </tr>
   * </table>
   *
   * @param column
   * the column to fix.
   * @return a new slice view.
   * @throws IndexOutOfBoundsException
   *         if <tt>column < 0 || column >= columns()</tt>.
   * @see #viewRow(int)
   */
  def viewColumn(column: Int) = {
    checkColumn(column)
    viewTranspose().viewRow(column)
  }
}
