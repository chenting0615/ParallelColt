package cern.colt.matrix.tdouble.impl

import cern.colt.function.tdouble.Function2
import cern.colt.map.tdouble.AbstractLongDoubleMap
import cern.colt.map.tdouble.OpenLongDoubleHashMap
import cern.colt.matrix.tdouble.StrideMatrix1D
import cern.colt.matrix.tdouble.StrideMatrix2D
import cern.colt.matrix.tdouble.DoubleMatrix3D
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Sparse hashed 1-d matrix (aka <i>vector</i>) holding <tt>double</tt>
 * elements. First see the <a href="package-summary.html">package summary</a>
 * and javadoc <a href="package-tree.html">tree view</a> to get the broad
 * picture.
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
 * {@link DenseDoubleMatrix1D}. However, constant factors are considerably
 * larger.
 *
 * @author wolfgang.hoschek@cern.ch
 * @version 1.0, 09/24/99
 *
 * @author Piotr Wendykier (piotr.wendykier@gmail.com)
 * @version 1.1, 08/22/2007
 */
@SerialVersionUID(1L)
class SparseDoubleMatrix1D(size: Int,
    initialCapacity: Int,
    minLoadFactor: Double,
    maxLoadFactor: Double) extends StrideMatrix1D {

  protected var elements: AbstractLongDoubleMap = new OpenLongDoubleHashMap(initialCapacity, minLoadFactor,
    maxLoadFactor)

  setUp(size)

  /**
   * Constructs a matrix with a copy of the given values. The values are
   * copied. So subsequent changes in <tt>values</tt> are not reflected in the
   * matrix, and vice-versa.
   *
   * @param values
   *            The values to be filled into the new matrix.
   */
  def this(values: Array[Double]) {
    this(values.length)
    assign(values)
  }

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
    this(size, size / 1000, 0.2, 0.5)
  }

  /**
   * Constructs a matrix view with a given number of parameters.
   *
   * @param size
   *            the number of cells the matrix shall have.
   * @param elements
   *            the cells.
   * @param offset
   *            the index of the first element.
   * @param stride
   *            the number of indexes between any two elements, i.e.
   *            <tt>index(i+1)-index(i)</tt>.
   * @throws IllegalArgumentException
   *             if <tt>size<0</tt>.
   */
  protected def this(size: Int,
      elements: AbstractLongDoubleMap,
      offset: Int,
      stride: Int) {
    this()
    setUp(size, offset, stride)
    this.elements = elements
    this.isNoView = false
  }

  /**
   * Sets all cells to the state specified by <tt>value</tt>.
   *
   * @param value
   *            the value to be filled into the cells.
   * @return <tt>this</tt> (for convenience only).
   */
  def assign(value: Double): StrideMatrix1D = {
    if (this.isNoView && value == 0) this.elements.clear() else super.assign(value)
    this
  }

  /**
   * Returns the number of cells having non-zero values.
   */
  def cardinality(): Int = {
    if (this.isNoView) this.elements.size else super.numNonZero()
  }

  /**
   * Returns the elements of this matrix.
   *
   * @return the elements
   */
  def elements(): AbstractLongDoubleMap = elements

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
   * @param minCapacity
   *            the desired minimum number of non-zero cells.
   */
  def ensureCapacity(minCapacity: Int) {
    this.elements.ensureCapacity(minCapacity)
  }

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
  def getQuick(index: Int): Double = {
    synchronized {
      elements.get(zero.toLong + index.toLong * stride.toLong)
    }
  }

  /**
   * Returns the position of the element with the given relative rank within
   * the (virtual or non-virtual) internal 1-dimensional array. You may want
   * to override this method for performance.
   *
   * @param rank
   *            the rank of the element.
   */
  def index(rank: Int): Long = {
    zero.toLong + rank.toLong * stride.toLong
  }

  def forEachNonZero(f: Function2): StrideMatrix1D = {
    elements.forEachPair(new LongDoubleProcedure() {

      override def apply(index: Long, oldValue: Double): Boolean = {
        val newValue = f.apply(index.toInt, oldValue)
        if (newValue != oldValue) {
          if (newValue != 0.0) elements.put(index, newValue) else elements.removeKey(index)
        }
        return true
      }
    })
    this
  }

  def forEachNegativeValue(f: Function2): StrideMatrix1D = {
    elements.forEachPair(new LongDoubleProcedure() {

      override def apply(index: Long, oldValue: Double): Boolean = {
        if (oldValue < 0) {
          val newValue = f.apply(index.toInt, oldValue)
          if (newValue != oldValue) {
            if (newValue != 0.0) elements.put(index, newValue) else elements.removeKey(index)
          }
        }
        return true
      }
    })
    this
  }

  def forEachPositiveValue(f: Function2): StrideMatrix1D = {
    elements.forEachPair(new LongDoubleProcedure() {

      override def apply(index: Long, oldValue: Double): Boolean = {
        if (oldValue > 0) {
          val newValue = f.apply(index.toInt, oldValue)
          if (newValue != oldValue) {
            if (newValue != 0.0) elements.put(index, newValue) else elements.removeKey(index)
          }
        }
        return true
      }
    })
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
  def like(size: Int): StrideMatrix1D = new SparseDoubleMatrix1D(size)

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
  def like2D(rows: Int, columns: Int): StrideMatrix2D = new SparseDoubleMatrix2D(rows, columns)

  def reshape(rows: Int, columns: Int): StrideMatrix2D = {
    if (rows * columns != size) {
      throw new IllegalArgumentException("rows*columns != size")
    }
    val M = new SparseDoubleMatrix2D(rows, columns)
    val idx = 0
    for (c <- 0 until columns; r <- 0 until rows) {
      val elem = getQuick(idx += 1)
      if (elem != 0) {
        M.setQuick(r, c, elem)
      }
    }
    M
  }

  def reshape(slices: Int, rows: Int, columns: Int): DoubleMatrix3D = {
    if (slices * rows * columns != size) {
      throw new IllegalArgumentException("slices*rows*columns != size")
    }
    val M = new SparseDoubleMatrix3D(slices, rows, columns)
    val idx = 0
    for (s <- 0 until slices; c <- 0 until columns; r <- 0 until rows) {
      val elem = getQuick(idx += 1)
      if (elem != 0) {
        M.setQuick(s, r, c, elem)
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
  def setQuick(index: Int, value: Double) {
    synchronized {
      val i = zero.toLong + index.toLong * stride.toLong
      if (value == 0) this.elements.removeKey(i) else this.elements.put(i, value)
    }
  }

  override def toString(): String = {
    val builder = new StringBuilder()
    builder.append("1 x ").append(size).append(" sparse matrix, nnz = ")
      .append(cardinality())
      .append('\n')
    for (i <- 0 until size) {
      val elem = getQuick(i)
      if (elem != 0) {
        builder.append('(').append(i).append(')').append('\t')
          .append(elem)
          .append('\n')
      }
    }
    builder.toString
  }

  def trimToSize() {
    this.elements.trimToSize()
  }

  /**
   * Returns <tt>true</tt> if both matrices share at least one identical cell.
   */
  protected def haveSharedCellsRaw(other: StrideMatrix1D): Boolean = {
    if (other.isInstanceOf[SelectedSparseDoubleMatrix1D]) {
      val otherMatrix = other.asInstanceOf[SelectedSparseDoubleMatrix1D]
      return this.elements == otherMatrix.elements
    } else if (other.isInstanceOf[SparseDoubleMatrix1D]) {
      val otherMatrix = other.asInstanceOf[SparseDoubleMatrix1D]
      return this.elements == otherMatrix.elements
    }
    false
  }

  /**
   * Construct and returns a new selection view.
   *
   * @param offsets
   *            the offsets of the visible elements.
   * @return a new view.
   */
  protected def viewSelectionLike(offsets: Array[Int]): StrideMatrix1D = {
    new SelectedSparseDoubleMatrix1D(this.elements, offsets)
  }
}
