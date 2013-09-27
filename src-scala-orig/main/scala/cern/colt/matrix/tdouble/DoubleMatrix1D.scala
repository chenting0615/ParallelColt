package cern.colt.matrix.tdouble

import cern.colt.matrix.{Matrix1D, AbstractMatrix1D}
import it.unimi.dsi.fastutil.ints.IntArrayList
import cern.colt.function.{Procedure2, Procedure1}

/**
 * Abstract base class for 1-d matrices (aka <i>vectors</i>) holding
 * <tt>double</tt> elements. First see the <a
 * href="package-summary.html">package summary</a> and javadoc <a
 * href="package-tree.html">tree view</a> to get the broad picture.
 * <p>
 * A matrix has a number of cells (its <i>size</i>), which are assigned upon
 * instance construction. Elements are accessed via zero based indexes. Legal
 * indexes are of the form <tt>[0..size()-1]</tt>. Any attempt to access an
 * element at a coordinate <tt>index&lt;0 || index&gt;=size()</tt> will throw an
 * <tt>IndexOutOfBoundsException</tt>.
 *
 * @author wolfgang.hoschek@cern.ch
 * @version 1.0, 09/24/99
 *
 * @author Piotr Wendykier (piotr.wendykier@gmail.com)
 *
 */
@specialized
@SerialVersionUID(1L)
abstract class DoubleMatrix1D[T] extends AbstractMatrix1D[T] {

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
  def assign(values: Array[T]): Matrix1D[T] = {
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
   * and is at least a <code>DoubleMatrix1D</code> object that has the same
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
    if (! obj.isInstanceOf[StrideMatrix1D[T]]) return false
    val other = obj.asInstanceOf[StrideMatrix1D[T]]
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
    for (i <- 0 until size.toInt) {
      values(i) = getQuick(i)
    }
    values
  }

  /**
   * Constructs and returns a new <i>flip view</i>. What used to be index
   * <tt>0</tt> is now index <tt>size()-1</tt>, ..., what used to be index
   * <tt>size()-1</tt> is now index <tt>0</tt>. The returned view is backed by
   * this matrix, so changes in the returned view are reflected in this
   * matrix, and vice-versa.
   *
   * @return a new flip view.
   */
  override def viewFlip(): Matrix1D[T] = {
    view().vFlip().asInstanceOf[Matrix1D[T]]
  }

  /**
   * Constructs and returns a new <i>sub-range view</i> that is a
   * <tt>width</tt> sub matrix starting at <tt>index</tt>.
   *
   * Operations on the returned view can only be applied to the restricted
   * range. Any attempt to access coordinates not contained in the view will
   * throw an <tt>IndexOutOfBoundsException</tt>.
   * <p>
   * <b>Note that the view is really just a range restriction:</b> The
   * returned matrix is backed by this matrix, so changes in the returned
   * matrix are reflected in this matrix, and vice-versa.
   * <p>
   * The view contains the cells from <tt>index..index+width-1</tt>. and has
   * <tt>view.size() == width</tt>. A view's legal coordinates are again zero
   * based, as usual. In other words, legal coordinates of the view are
   * <tt>0 .. view.size()-1==width-1</tt>. As usual, any attempt to access a
   * cell at other coordinates will throw an
   * <tt>IndexOutOfBoundsException</tt>.
   *
   * @param index
   *            The index of the first cell.
   * @param width
   *            The width of the range.
   * @throws IndexOutOfBoundsException
   *             if <tt>index<0 || width<0 || index+width>size()</tt>.
   * @return the new view.
   *
   */
  def viewPart(index: Int, width: Int): Matrix1D[T] = {
    view().vPart(index, width).asInstanceOf[Matrix1D[T]]
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
  def viewSelection(condition: Procedure1[T]): StrideMatrix1D[T] = {
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
  def viewSelection(condition: Procedure2[Int, T]): StrideMatrix1D[T] = {
    val matches = new IntArrayList()
    for (i <- 0 until size.toInt) if (condition.apply(i, getQuick(i))) matches.add(i)
    viewSelection(matches.toIntArray)
  }

  /**
   * Constructs and returns a new <i>selection view</i> that is a matrix
   * holding the indicated cells. There holds
   * <tt>view.size() == indexes.length</tt> and
   * <tt>view.get(i) == this.get(indexes[i])</tt>. Indexes can occur multiple
   * times and can be in arbitrary order.
   * <p>
   * <b>Example:</b> <br>
   *
   * <pre>
   * 	 this     = (0,0,8,0,7)
   * 	 indexes  = (0,2,4,2)
   * 	 --&gt;
   * 	 view     = (0,8,7,8)
   *
   * </pre>
   *
   * Note that modifying <tt>indexes</tt> after this call has returned has no
   * effect on the view. The returned view is backed by this matrix, so
   * changes in the returned view are reflected in this matrix, and
   * vice-versa.
   *
   * @param indexes
   *            The indexes of the cells that shall be visible in the new
   *            view. To indicate that <i>all</i> cells shall be visible,
   *            simply set this parameter to <tt>null</tt>.
   * @return the new view.
   * @throws IndexOutOfBoundsException
   *             if <tt>!(0 <= indexes[i] < size())</tt> for any
   *             <tt>i=0..indexes.length()-1</tt>.
   */
  def viewSelection(indexes: Array[Int]): StrideMatrix1D[T] = {
    if (indexes == null) return view()
    checkIndexes(indexes)
    val offsets = Array.ofDim[Int](indexes.length)
    for (i <- 0 until indexes.length) {
      offsets(i) = index(indexes(i)).toInt
    }
    viewSelectionLike(offsets)
  }

  /**
   * Constructs and returns a new <i>stride view</i> which is a sub matrix
   * consisting of every i-th cell. More specifically, the view has size
   * <tt>this.size()/stride</tt> holding cells <tt>this.get(i*stride)</tt> for
   * all <tt>i = 0..size()/stride - 1</tt>.
   *
   * @param stride
   *            the step factor.
   * @throws IndexOutOfBoundsException
   *             if <tt>stride <= 0</tt>.
   * @return the new view.
   *
   */
  def viewStrides(stride: Int): StrideMatrix1D = {
    view().vStrides(stride).asInstanceOf[StrideMatrix1D]
  }

  /**
   * Returns the number of cells having non-zero values, but at most
   * maxCardinality; ignores tolerance.
   */
  protected def numNonZero(maxCardinality: Int): Int = {
    var cardinality = 0
    val i = size.toInt
    while (i >= 0 && cardinality < maxCardinality) {
      if (getQuick(i) != 0) cardinality += 1
    }
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
   * Constructs and returns a new view equal to the receiver. The view is a
   * shallow clone. Calls <code>clone()</code> and casts the result.
   * <p>
   * <b>Note that the view is not a deep copy.</b> The returned matrix is
   * backed by this matrix, so changes in the returned matrix are reflected in
   * this matrix, and vice-versa.
   * <p>
   * Use {@link #copy()} to construct an independent deep copy rather than a
   * new view.
   *
   * @return a new view of the receiver.
   */
  protected def view(): StrideMatrix1D[T] = clone().asInstanceOf[StrideMatrix1D[T]]

  /**
   * Construct and returns a new selection view.
   *
   * @param offsets
   *            the offsets of the visible elements.
   * @return a new view.
   */
  protected def viewSelectionLike(offsets: Array[Int]): StrideMatrix1D[T]

}
