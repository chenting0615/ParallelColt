package cern.colt.matrix

import cern.colt.function.{Procedure2, Procedure1}

/**
 * Trait for 1-d matrices (aka <i>vectors</i>) holding objects or
 * primitive data types such as <code>int</code>, <code>double</code>, etc.
 * First see the <a href="package-summary.html">package summary</a> and javadoc
 * <a href="package-tree.html">tree view</a> to get the broad picture.
 * <p>
 * There is no difference between a row vector and a column vector.
 *
 * @author wolfgang.hoschek@cern.ch
 * @version 1.0, 09/24/99
 */
@specialized
@SerialVersionUID(1L)
trait Matrix1D[T] extends Matrix[T] {

  /**
   * Sanity check for operations requiring an index to be within bounds.
   *
   * @throws IndexOutOfBoundsException
   *             if <tt>index < 0 || index >= size()</tt>.
   */
  def checkIndex(index: Int) {
    if (index < 0 || index >= size)
      throw new IndexOutOfBoundsException("Attempted to access " + toShapeString + " at index=" + index)
  }

  /**
   * Checks whether indexes are legal and throws an exception, if necessary.
   *
   * @throws IndexOutOfBoundsException
   *             if <tt>! (0 <= indexes[i] < size())</tt> for any
   *             i=0..indexes.length()-1.
   */
  def checkIndexes(indexes: Array[Int]) {
    var i = indexes.length
    while (i >= 0) {
      val index = indexes(i)
      if (index < 0 || index >= size)
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
  def checkRange(index: Int, width: Int) {
    if (index < 0 || index + width > size)
      throw new IndexOutOfBoundsException("index: " + index + ", width: " + width + ", size=" + size)
  }

  /**
   * Sanity check for operations requiring two matrices with the same size.
   *
   * @throws IllegalArgumentException
   *             if <tt>size() != size</tt>.
   */
  def checkSize(size: Long) {
    if (this.size != size)
      throw new IllegalArgumentException("Incompatible sizes: " + toShapeString + " and " + size)
  }

  /**
   * Sanity check for operations requiring two matrices with the same size.
   *
   * @throws IllegalArgumentException
   *             if <tt>size() != B.size()</tt>.
   */
  def checkSize(B: Matrix1D[T]) {
    if (size != B.size)
      throw new IllegalArgumentException("Incompatible sizes: " + toShapeString + " and " + B.toShapeString)
  }

  /**
   * Returns a string representation of the receiver's shape.
   */
  def toShapeString: String = AbstractFormatter.shape(this)

  /**
   * Sets all cells to the state specified by <tt>value</tt>.
   *
   * @param value
   *            the value to be filled into the cells.
   * @return <tt>this</tt> (for convenience only).
   */
  def assignConstant(value: T): Matrix1D[T]

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
  def assign(values: Array[T]): Matrix1D[T]

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
  def assign(other: Matrix1D[T]): Matrix1D[T]

  /**
   * Constructs and returns a deep copy of the receiver.
   * <p>
   * <b>Note that the returned matrix is an independent deep copy.</b> The
   * returned matrix is not backed by this matrix, so changes in the returned
   * matrix are not reflected in this matrix, and vice-versa.
   *
   * @return a deep copy of the receiver.
   */
  def copy(): Matrix1D[T] = {
    val copy = like()
    copy.assign(this)
    copy
  }

  /**
   * Returns the matrix cell value at coordinate <tt>index</tt>.
   *
   * @param index
   *            the index of the cell.
   * @return the value of the specified cell.
   * @throws IndexOutOfBoundsException
   *             if <tt>index&lt;0 || index&gt;=size()</tt>.
   */
  def get(index: Int): T = {
    checkIndex(index)
    getQuick(index)
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
  def getQuick(index: Int): T

  /**
   * Sets the matrix cell at coordinate <tt>index</tt> to the specified value.
   *
   * @param index
   *            the index of the cell.
   * @param value
   *            the value to be filled into the specified cell.
   * @throws IndexOutOfBoundsException
   *             if <tt>index&lt;0 || index&gt;=size()</tt>.
   */
  def set(index: Int, value: T) {
    checkIndex(index)
    setQuick(index, value)
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
  def setQuick(index: Int, value: T): Unit

  /**
   * Assigns the result of a function to each <i>non-zero</i> cell;
   * <tt>x[idx] = function(x[idx], value)</tt>. Use this method for fast
   * special-purpose iteration. If you want to modify another matrix instead
   * of <tt>this</tt> (i.e. work in read-only mode), simply return the input
   * value unchanged.
   *
   * Parameters to function are as follows: <tt>first==row</tt>,
   * <tt>second==nonZeroValue</tt>.
   *
   * @param function
   *            a function object taking as argument the current non-zero
   *            cell's row, column and value.
   * @return <tt>this</tt> (for convenience only).
   */
  def forEachNonZero(function: Function2[Int, T, T]): Matrix1D[T]

  /**
   * Returns an iterator that can traverse all non-zero values in the matrix.
   */
  def iteratorNonZeros: IndexIterator1D[T]

  /**
   * Returns an iterator that can traverse all non-zero values in the matrix
   * which return true from the given condition.
   */
  def iteratorNonZeros(condition: Procedure2[Int, T]): IndexIterator1D[T]

  /**
   * Returns an iterator that can traverse all values in the matrix.
   */
  def iteratorAllCells: IndexIterator1D[T]

  /**
   * Returns an iterator that can traverse all values in the matrix
   * which return true from the given condition.
   */
  def iterator(condition: Procedure2[Int, T]): IndexIterator1D[T]

  /**
   * Construct and returns a new empty matrix <i>of the same dynamic type</i>
   * as the receiver, having the same size. For example, if the receiver is an
   * instance of type <tt>DenseDoubleMatrix1D</tt> the new matrix must also be
   * of type <tt>DenseDoubleMatrix1D</tt>, if the receiver is an instance of
   * type <tt>SparseDoubleMatrix1D</tt> the new matrix must also be of type
   * <tt>SparseDoubleMatrix1D</tt>, etc. In general, the new matrix should
   * have internal parametrization as similar as possible.
   *
   * @return a new empty matrix of the same dynamic type.
   */
  def like(): Matrix1D[T] = like1D(size.toInt)

  /**
   * Returns new Matrix2D of size rows x columns whose elements are
   * taken column-wise from this matrix.
   *
   * @param rows
   *            number of rows
   * @param columns
   *            number of columns
   * @return new 2D matrix with columns being the elements of this matrix.
   */
  def reshape(rows: Int, columns: Int): Matrix2D[T]

  /**
   * Constructs and returns a 1-dimensional array containing the cell values.
   * The values are copied. So subsequent changes in <tt>values</tt> are not
   * reflected in the matrix, and vice-versa. The returned array
   * <tt>values</tt> has the form <br>
   * <tt>for (int i=0; i < size(); i++) values[i] = get(i);</tt>
   *
   * @return an array filled with the values of the cells.
   */
  def toArray: Array[T]

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
  def toArray(values: Array[T]): Array[T]

  /**
   * Constructs and returns a new <i>flip view</i>. What used to be index
   * <tt>0</tt> is now index <tt>size()-1</tt>, ..., what used to be index
   * <tt>size()-1</tt> is now index <tt>0</tt>. The returned view is backed by
   * this matrix, so changes in the returned view are reflected in this
   * matrix, and vice-versa.
   *
   * @return a new flip view.
   */
  def viewFlip(): Matrix1D[T]

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
  def viewPart(index: Int, width: Int): Matrix1D[T]

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
  def viewSelection(condition: Procedure1[T]): Matrix1D[T]

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
  def viewSelection(condition: Procedure2[Int, T]): Matrix1D[T]

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
  def viewSelection(indexes: Array[Int]): Matrix1D[T]

  /**
   * TODO: Do we need/want this?
   *
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
  def viewStrides(stride: Int): Matrix1D[T]
}
