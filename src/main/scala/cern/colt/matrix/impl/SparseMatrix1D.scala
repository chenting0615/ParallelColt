package cern.colt.matrix.impl

import it.unimi.dsi.fastutil.longs.Long2DoubleOpenHashMap
import cern.colt.matrix.Matrix2D

/**
 * Sparse hashed 1-d matrix (aka <i>vector</i>) holding <tt>double</tt>
 * elements. First see the <a href="package-summary.html">package summary</a>
 * and javadoc <a href="package-tree.html">tree view</a> to get the broad
 * picture.
 * <p>
 * <b>Implementation:</b>
 * <p>
 * Note that this implementation is not synchronized. Uses a
 * Long2Double HashMap, which is a compact and
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
 * Thus, a 1000000 matrix with minLoadFactor=0.25 and maxLoadFactor=0.5 and
 * 1000000 non-zero cells consumes between 25 MB and 50 MB. The same 1000000
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
 * {@link DenseMatrix1D}. However, constant factors are considerably
 * larger.
 *
 * @author wolfgang.hoschek@cern.ch
 * @version 1.0, 09/24/99
 *
 * @author Piotr Wendykier (piotr.wendykier@gmail.com)
 * @version 1.1, 08/22/2007
 */
@specialized(Double)
@SerialVersionUID(1L)
class SparseMatrix1D[T: Manifest: FastUtilLongMap](size_p: Int, initialCapacity: Int, loadFactor: Double) extends StrideMatrix1D[T] {

  protected var elements = new Long2DoubleOpenHashMap(initialCapacity, loadFactor.toFloat)

  setUp(size_p)

  /**
   * Constructs a matrix with a given number of cells. All entries are
   * initially <tt>0</tt>.
   *
   * @param size
   *            the number of cells the matrix shall have.
   * @throws IllegalArgumentException
   *             if <tt>size<0</tt>.
   */
  def this(size: Int) {
    this(size, size / 1000, 0.35)
  }

  /**
   * Constructs a matrix with a copy of the given values. The values are
   * copied. So subsequent changes in <tt>values</tt> are not reflected in the
   * matrix, and vice-versa.
   *
   * @param values
   *            The values to be filled into the new matrix.
   */
  def this(values: Array[T]) {
    this(values.length)
    assign(values)
  }

  /**
   * Sets all cells to the state specified by <tt>value</tt>.
   *
   * @param value
   *            the value to be filled into the cells.
   * @return <tt>this</tt> (for convenience only).
   */
  override def assignConstant(value: T): SparseMatrix1D[T] = {
    if (this.isNoView && value == 0) this.elements.clear() else super.assignConstant(value)
    this
  }

  /**
   * Returns the number of cells having non-zero values.
   */
  override def numNonZero = {
    if (this.isNoView) this.elements.size else super.numNonZero
  }

  /**
   * Ensures that the receiver can hold at least the specified number of
   * non-zero cells without needing to allocate new internal memory. If
   * necessary, allocates new internal memory and increases the capacity of
   * the receiver.
   * <p>
   * This method never need be called; it is for performance tuning only.
   * Calling this method before tt>set()</tt>ing a large number of non-zero
   * values boosts performance, because the receiver will grow only once
   * instead of potentially many times and hash collisions get less probable.
   *
   * //@param minCapacity
   *            the desired minimum number of non-zero cells.
   */
/*
  override def ensureCapacity(minCapacity: Int) {
    this.elements.ensureCapacity(minCapacity)
  }
*/

  /**
   * Returns the matrix cell value at coordinate <tt>index</tt>.
   *
   * <p>
   * Provided with invalid parameters this method may return invalid objects
   * without throwing any exception. <b>You should only use this method when
   * you are absolutely sure that the coordinate is within bounds.</b>
   * Precondition (unchecked): <tt>index&lt;0 || index&gt;=size()</tt>.
   *
   * @param index
   *            the index of the cell.
   * @return the value of the specified cell.
   */
  def getQuick(index: Int): T = {
    elements.get(toRawIndex(index)).asInstanceOf[T]
  }

  override def forEachNonZero(f: Function2[Int, T, T]): SparseMatrix1D[T] = {
    val iter = this.elements.long2DoubleEntrySet().iterator()
    while(iter.hasNext) {
      val elem = iter.next()
      val index = elem.getLongKey
      val oldValue = elem.getValue
      val newValue = f.apply(index.toInt, oldValue.asInstanceOf[T])
      if (newValue != oldValue) {
        if (newValue != 0.0) elements.put(index, newValue.asInstanceOf[Double]) else elements.remove(index)
      }
    }
    this
  }

  /**
   * Construct and returns a new empty matrix <i>of the same dynamic type</i>
   * as the receiver, having the specified size. For example, if the receiver
   * is an instance of type <tt>DenseDoubleMatrix1D</tt> the new matrix must
   * also be of type <tt>DenseDoubleMatrix1D</tt>, if the receiver is an
   * instance of type <tt>SparseDoubleMatrix1D</tt> the new matrix must also
   * be of type <tt>SparseDoubleMatrix1D</tt>, etc. In general, the new matrix
   * should have internal parametrization as similar as possible.
   *
   * @param size
   *            the number of cell the matrix shall have.
   * @return a new empty matrix of the same dynamic type.
   */
  def like1D(size: Int): SparseMatrix1D[T] = new SparseMatrix1D[T](size)

  /**
   * Construct and returns a new 2-d matrix <i>of the corresponding dynamic
   * type</i>, entirelly independent of the receiver. For example, if the
   * receiver is an instance of type <tt>DenseDoubleMatrix1D</tt> the new
   * matrix must be of type <tt>DenseDoubleMatrix2D</tt>, if the receiver is
   * an instance of type <tt>SparseDoubleMatrix1D</tt> the new matrix must be
   * of type <tt>SparseDoubleMatrix2D</tt>, etc.
   *
   * @param rows
   *            the number of rows the matrix shall have.
   * @param columns
   *            the number of columns the matrix shall have.
   * @return a new matrix of the corresponding dynamic type.
   */
  def like2D(rows: Int, columns: Int): Matrix2D[T] = new SparseHashMatrix2D[T](rows, columns)

  def reshape(rows: Int, columns: Int): StrideMatrix2D[T] = {
    if (rows * columns != size) {
      throw new IllegalArgumentException("rows*columns != size")
    }
    val M = new SparseHashMatrix2D[T](rows, columns)
    var idx = 0
    for (c <- 0 until columns; r <- 0 until rows) {
      val elem = getQuick(idx)
      idx += 1
      if (elem != 0) {
        M.setQuick(r, c, elem)
      }
    }
    M
  }

  /**
   * Sets the matrix cell at coordinate <tt>index</tt> to the specified value.
   *
   * <p>
   * Provided with invalid parameters this method may access illegal indexes
   * without throwing any exception. <b>You should only use this method when
   * you are absolutely sure that the coordinate is within bounds.</b>
   * Precondition (unchecked): <tt>index&lt;0 || index&gt;=size()</tt>.
   *
   * @param index
   *            the index of the cell.
   * @param value
   *            the value to be filled into the specified cell.
   */
  def setQuick(index: Int, value: T) {
    val i = toRawIndex(index)
    if (value == 0) this.elements.remove(i) else this.elements.put(i, value.asInstanceOf[Double])
  }

  override def toString: String = {
    val builder = new StringBuilder()
    builder.append("1 x ").append(size).append(" sparse matrix, nnz = ")
      .append(numNonZero)
      .append('\n')
    for (i <- 0 until size.toInt) {
      val elem = getQuick(i)
      if (elem != 0) {
        builder.append('(').append(i).append(')').append('\t')
          .append(elem)
          .append('\n')
      }
    }
    builder.toString()
  }

  override def trimToSize() {
    this.elements.trim()
  }
}
