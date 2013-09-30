package cern.colt.matrix.impl

import cern.colt.function.{Procedure2, Procedure1}
import it.unimi.dsi.fastutil.ints.IntArrayList
import cern.colt.matrix._

/**
 * Abstract base class for 1-d matrices (aka <i>vectors</i>) holding objects or
 * primitive data types such as <code>int</code>, <code>double</code>, etc.
 * First see the <a href="package-summary.html">package summary</a> and javadoc
 * <a href="package-tree.html">tree view</a> to get the broad picture.
 * <p>
 * <b>Note that this implementation is not synchronized.</b>
 *
 * @author wolfgang.hoschek@cern.ch
 * @version 1.0, 09/24/99
 */
@specialized
@SerialVersionUID(1L)
abstract class AbstractMatrix1D[T: Manifest] extends Matrix1D[T] {

  /**
   the number of cells this matrix (view) has
   */
  protected var sizeVar: Int = 0

  /**
   * Sanity check for operations requiring an index to be within bounds.
   *
   * @throws IndexOutOfBoundsException
   *             if <tt>index < 0 || index >= size()</tt>.
   */
  override protected def checkIndex(index: Int) {
    if (index < 0 || index >= sizeVar)
      throw new IndexOutOfBoundsException("Attempted to access " + toShapeString + " at index=" + index)
  }

  /**
   * Checks whether indexes are legal and throws an exception, if necessary.
   *
   * @throws IndexOutOfBoundsException
   *             if <tt>! (0 <= indexes[i] < size())</tt> for any
   *             i=0..indexes.length()-1.
   */
  override protected def checkIndexes(indexes: Array[Int]) {
    var i = indexes.length
    while (i >= 0) {
      val index = indexes(i)
      if (index < 0 || index >= sizeVar)
        throw new IndexOutOfBoundsException("Index " + i + ": Attempted to access " + toShapeString + " at index=" + index)
      i -= 1
    }
  }

  /**
   * Checks whether the receiver contains the given range and throws an
   * exception, if necessary.
   *
   * @throws IndexOutOfBoundsException
   *             if <tt>index<0 || index+width>size()</tt>.
   */
  override protected def checkRange(index: Int, width: Int) {
    if (index < 0 || index + width > sizeVar)
      throw new IndexOutOfBoundsException("index: " + index + ", width: " + width + ", size=" + sizeVar)
  }

  /**
   * Sanity check for operations requiring two matrices with the same size.
   *
   * @throws IllegalArgumentException
   *             if <tt>size() != B.size()</tt>.
   */
  override def checkSize(B: Matrix1D[T]) {
    if (sizeVar != B.size)
      throw new IllegalArgumentException("Incompatible sizes: " + toShapeString + " and " + B.toShapeString)
  }

  /**
   * Returns the number of cells.
   */
  override def size: Long = sizeVar

  /**
   * Sets all cells to the state specified by <tt>value</tt>.
   *
   * @param value
   * the value to be filled into the cells.
   * @return <tt>this</tt> (for convenience only).
   */
  def assignConstant(value: T) = {
    for(i <- 0 until sizeVar)
      setQuick(i, value)
    this
  }

  /**
   * Sets all cells to the state specified by <tt>values</tt>. <tt>values</tt>
   * is required to have the same number of cells as the receiver.
   * <p>
   * The values are copied. So subsequent changes in <tt>values</tt> are not
   * reflected in the matrix, and vice-versa.
   *
   * @param values
   *            the values to be filled into the cells.
   * @return <tt>this</tt> (for convenience only).
   * @throws IllegalArgumentException
   *             if <tt>values.length != size()</tt>.
   */
  def assign(values: Array[T]) = {
    checkSize(values.length)
    values.indices.foreach(idx => {setQuick(idx, values(idx))})
    this
  }

  /**
   * Replaces all cell values of the receiver with the values of another
   * matrix. Both matrices must have the same size. If both matrices share the
   * same cells (as is the case if they are views derived from the same
   * matrix) and intersect in an ambiguous way, then replaces <i>as if</i>
   * using an intermediate auxiliary deep copy of <tt>other</tt>.
   *
   * @param other
   *            the source matrix to copy from (may be identical to the
   *            receiver).
   * @return <tt>this</tt> (for convenience only).
   * @throws IllegalArgumentException
   *             if <tt>size() != other.size()</tt>.
   */
  def assign(other: Matrix1D[T]): Matrix1D[T] = {
    if (other == this) return this
    checkSize(other)
    for (i <- 0 until size.toInt) {
      setQuick(i, other.getQuick(i))
    }
    this
  }

  /**
   * Returns the number of cells having non-zero values; ignores tolerance.
   *
   * @return the number of cells having non-zero values.
   */
  def numNonZero: Long = {
    var cardinality = 0L
    for (i <- 0 until size.toInt if getQuick(i) != 0.0) cardinality += 1
    cardinality
  }

  /**
   * Returns <tt>true</tt> if both matrices share at least one identical cell.
   */
  protected def haveSharedCells(other: Matrix1D[T]): Boolean = {
    if (other == null) return false
    if (this == other) return true
    getStorageMatrix.haveSharedCells(other.getStorageMatrix)
  }

  /**
   * Returns whether all cells are equal to the given value.
   *
   * @param value
   *            the value to test against.
   * @return <tt>true</tt> if all cells are equal to the given value,
   *         <tt>false</tt> otherwise.
   */
  def everyCellEquals(value: T): Boolean = {
    for (i <- 0 until size.toInt)  if (getQuick(i) != value) return false
    true
  }

  /**
   * Compares this object against the specified object. The result is
   * <code>true</code> if and only if the argument is not <code>null</code>
   * and is at least a <code>AbstractMatrix1D</code> object that has the same
   * sizes as the receiver and has exactly the same values at the same
   * indexes.
   *
   * @param obj
   *            the object to compare with.
   * @return <code>true</code> if the objects are the same; <code>false</code>
   *         otherwise.
   */
  override def equals(obj: Any): Boolean = {
    if (this == obj) return true
    if (obj == null) return false
    if (! obj.isInstanceOf[AbstractMatrix1D[T]]) return false
    val other = obj.asInstanceOf[AbstractMatrix1D[T]]
    if (size != other.size) return false
    for (i <- 0 until size.toInt)  if (getQuick(i) != other.getQuick(i)) return false
    true
  }

  /**
   * Constructs and returns a 1-dimensional array containing the cell values.
   * The values are copied. So subsequent changes in <tt>values</tt> are not
   * reflected in the matrix, and vice-versa. The returned array
   * <tt>values</tt> has the form <br>
   * <tt>for (int i=0; i < size(); i++) values[i] = get(i);</tt>
   *
   * @return an array filled with the values of the cells.
   */
  def toArray: Array[T] = {
    val values = Array.ofDim[T](size.toInt)
    toArray(values)
  }

  /**
   * Fills the cell values into the specified 1-dimensional array. The values
   * are copied. So subsequent changes in <tt>values</tt> are not reflected in
   * the matrix, and vice-versa. After this call returns the array
   * <tt>values</tt> has the form <br>
   * <tt>for (int i=0; i < size(); i++) values[i] = get(i);</tt>
   *
   * @throws IllegalArgumentException
   *             if <tt>values.length < size()</tt>.
   */
  def toArray(values: Array[T]): Array[T] = {
    checkSize(values.length)
    for (i <- 0 until size.toInt) {
      values(i) = getQuick(i)
    }
    values
  }

  /**
   * Constructs and returns a new <i>selection view</i> that is a matrix
   * holding the cells matching the given condition. Applies the condition to
   * each cell and takes only those cells where
   * <tt>condition.apply(get(i))</tt> yields <tt>true</tt>.
   * <p>
   * <b>Example:</b> <br>
   *
   * <pre>
   * 	 // extract and view all cells with even value
   * 	 matrix = 0 1 2 3
   * 	 matrix.viewSelection(
   * 	    new DoubleProcedure() {
   * 	       public final boolean apply(double a) { return a % 2 == 0; }
   * 	    }
   * 	 );
   * 	 --&gt;
   * 	 matrix ==  0 2
   *
   * </pre>
   *
   * For further examples, see the <a
   * href="package-summary.html#FunctionObjects">package doc</a>. The returned
   * view is backed by this matrix, so changes in the returned view are
   * reflected in this matrix, and vice-versa.
   *
   * @param condition
   *            The condition to be matched.
   * @return the new view.
   */
  def viewSelection(condition: Procedure1[T]): Matrix1D[T] = {
    val matches = new IntArrayList()
    for (i <- 0 until size.toInt) if (condition.apply(getQuick(i))) matches.add(i)
    viewSelection(matches.toIntArray)
  }

  /**
   * Constructs and returns a new <i>selection view</i> that is a matrix
   * holding the cells matching the given condition. Applies the condition to
   * each cell and takes only those cells where
   * <tt>condition.apply(get(i))</tt> yields <tt>true</tt>.
   * <p>
   * <b>Example:</b> <br>
   *
   * <pre>
   * 	 // extract and view all cells with even value
   * 	 matrix = 0 1 2 3
   * 	 matrix.viewSelection(
   * 	    new DoubleProcedure() {
   * 	       public final boolean apply(double a) { return a % 2 == 0; }
   * 	    }
   * 	 );
   * 	 --&gt;
   * 	 matrix ==  0 2
   *
   * </pre>
   *
   * For further examples, see the <a
   * href="package-summary.html#FunctionObjects">package doc</a>. The returned
   * view is backed by this matrix, so changes in the returned view are
   * reflected in this matrix, and vice-versa.
   *
   * @param condition
   *            The condition to be matched.
   * @return the new view.
   */
  def viewSelection(condition: Procedure2[Int, T]): Matrix1D[T] = {
    val matches = new IntArrayList()
    for (i <- 0 until size.toInt) if (condition.apply(i, getQuick(i))) matches.add(i)
    viewSelection(matches.toIntArray)
  }

  override def forEachNonZero(function: Function2[Int, T, T]) = {
    for(idx <- 0 until sizeVar) {
      val oldValue = getQuick(idx)
      if (oldValue != 0) {
        val newValue = function(idx, oldValue)
        if (newValue != oldValue)
          setQuick(idx, newValue)
      }
    }
    this
  }

  abstract class AbstractIndexIterator1D extends IndexIterator1D[T] {
    var indexVar = -1

    def checkIndex(): Boolean

    def hasNext: Boolean = {
      while(indexVar < sizeVar-1) {
        indexVar += 1
        if (checkIndex())
          return true
      }
      false
    }

    def index: Int = indexVar

    def next(): T = getQuick(indexVar)
  }

  /**
   * Returns an iterator that can traverse all non-zero values in the matrix.
   */
  def iteratorNonZeros: IndexIterator1D[T] = new AbstractIndexIterator1D() {
    def checkIndex(): Boolean = {
      val value = getQuick(indexVar)
      value != 0.0
    }
  }

  /**
   * Returns an iterator that can traverse all non-zero values in the matrix
   * which return true from the given condition.
   */
  def iteratorNonZeros(condition: Procedure2[Int, T]): IndexIterator1D[T] = new AbstractIndexIterator1D() {
    def checkIndex(): Boolean = {
      val value = getQuick(indexVar)
      value != 0.0 && condition(indexVar, value)
    }
  }

  /**
   * Returns an iterator that can traverse all values in the matrix.
   */
  def iteratorAllCells: IndexIterator1D[T] = new AbstractIndexIterator1D() {
    def checkIndex(): Boolean = true
  }

  /**
   * Returns an iterator that can traverse all values in the matrix
   * which return true from the given condition.
   */
  def iterator(condition: Procedure2[Int, T]): IndexIterator1D[T] = new AbstractIndexIterator1D() {
    def checkIndex(): Boolean = {
      val value = getQuick(indexVar)
      condition(indexVar, value)
    }
  }

  /**
   * @return Return the ParallelStrategy object used by this matrix.
   *         The ParallelStrategy manages the division of matrix operations into
   *         rows/columns.
   */
  def getParallelStrategy: ParallelStrategy = null

  def setParallelStrategy(s: ParallelStrategy) {}

  /**
   * @return Returns true if this matrix uses a sparse representation for storing cell values
   */
  def isSparse: Boolean = false

  /**
   * @return Return the MatrixFactory which can produce more matrices like this one.
   */
  def getFactory: MatrixFactory = null

  protected def setFactory(f: MatrixFactory) {}

  /**
   * @return Return the algebra object with matrix operations for use with this
   *         matrix.
   */
  def getAlgebra: MatrixAlgebra = null

  def setAlgebra(m: MatrixAlgebra) {}
}
