package cern.colt.matrix.impl

import cern.colt.matrix._

/**
 * Dense 1-d matrix (aka <i>vector</i>) holding <tt>double</tt> elements. First
 * see the <a href="package-summary.html">package summary</a> and javadoc <a
 * href="package-tree.html">tree view</a> to get the broad picture.
 * <p>
 * <b>Implementation:</b>
 * <p>
 * Internally holds one single contigous one-dimensional array. Note that this
 * implementation is not synchronized.
 * <p>
 * <b>Time complexity:</b>
 * <p>
 * <tt>O(1)</tt> (i.e. constant time) for the basic operations <tt>get</tt>,
 * <tt>getQuick</tt>, <tt>set</tt>, <tt>setQuick</tt> and <tt>size</tt>,
 * <p>
 *
 * @author wolfgang.hoschek@cern.ch
 * @version 1.0, 09/24/99
 *
 * @author Piotr Wendykier (piotr.wendykier@gmail.com)
 *
 * @param size_p
 *            the number of cells the matrix shall have.
 * @param elementsVar
 *            the cells.
 * @param zero_p
 *            the index of the first element.
 * @param stride_p
 *            the number of indexes between any two elements, i.e.
 *            <tt>index(i+1)-index(i)</tt>.
 * @param isView
 *            if true then a matrix view is constructed
 * @throws IllegalArgumentException
 *             if <tt>size<0</tt>.
 */
@SerialVersionUID(1L)
class DenseMatrix1D[@specialized T: Manifest: Numeric](size_p: Int, protected var elementsVar: Array[T], zero_p: Int, stride_p: Int, isView: Boolean) extends StrideMatrix1D[T] {

  // This has to be defined because can't have nested @specialized classes.
  def numeric = implicitly[Numeric[T]]

  isNoView = ! isView
  setUp(size_p, zero_p, stride_p)

  /**
   * Constructs an empty matrix with the given size.
   *
   * @param size
   *            The size of the new matrix.
   */
  def this(size: Int) {
    this(size, Array.ofDim[T](size), 0, 1, false)
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

  def getElements = elementsVar

  override def assignConstant(value: T) = {
    if (zeroVar == 0 && strideVar == 1) {
      for(i <- 0 until sizeVar)
        elementsVar(i) = value
    }
    else {
      for (i <- 0 until sizeVar)
        elementsVar(toRawIndex(i)) = value
    }
    this
  }

  override def assign(values: Array[T]) = {
    checkSize(values.length)
    if (zeroVar == 0 && strideVar == 1) {
      System.arraycopy(elementsVar, 0, values, 0, sizeVar)
    }
    else {
      for (i <- 0 until sizeVar) {
        elementsVar(toRawIndex(i)) = values(i)
      }
    }
    this
  }

  override def assign(source: Matrix1D[T]): Matrix1D[T] = {
    if (source eq this) return this
    checkSize(source)
    if (! source.isInstanceOf[DenseMatrix1D[T]]) {
      super.assign(source)
      return this
    }
    var other = source.asInstanceOf[DenseMatrix1D[T]]
    if (isNoView && ! other.isView) {
      System.arraycopy(other.getElements, 0, this.elementsVar, 0, this.elementsVar.length)
      return this
    }
    if (haveSharedCells(other)) {
      val c = other.copy()
      if (! c.isInstanceOf[DenseMatrix1D[T]]) {
        return super.assign(source)
      }
      other = c.asInstanceOf[DenseMatrix1D[T]]
    }
    var idx = zeroVar
    var idxOther = other.zeroIndex
    for (k <- 0 until sizeVar) {
      elementsVar(idx) = other.getElements(idxOther)
      idx += strideVar
      idxOther += other.stride
    }
    this
  }


  override def numNonZero: Long = {
    var cardinality = 0
    for (i <- 0 until sizeVar)
      if (elementsVar(toRawIndex(i)) != numeric.zero) cardinality += 1
    cardinality
  }

  override def forEachNonZero(f: Function2[Int, T, T]) = {
    val rem = sizeVar % 2
    var idx = zeroVar
    if (rem == 1) {
      val value = elementsVar(idx)
      if (value != numeric.zero) {
        elementsVar(idx) = f.apply(0, value)
      }
      idx += stride
    }
    var i = rem
    while (i < sizeVar) {
      var value = elementsVar(idx)
      if (value != numeric.zero)
        elementsVar(idx) = f.apply(i, value)
      idx += strideVar
      value = elementsVar(idx)
      if (value != numeric.zero)
        elementsVar(idx) = f.apply(i + 1, value)
      idx += strideVar
      i += 2
    }
    this
  }

  def getQuick(index: Int): T = elementsVar(zeroVar + index * strideVar)

  def like1D(size: Int): Matrix1D[T] = new DenseMatrix1D[T](size)

  def like2D(rows: Int, columns: Int): Matrix2D[T] = new DenseMatrix2D[T](rows, columns)

  def reshape(rows: Int, columns: Int): Matrix2D[T] = {
    if (rows * columns != size)
      throw new IllegalArgumentException("rows*columns != size")

    val M = new DenseMatrix2D[T](rows, columns)
    M.viewTranspose().assign(elementsVar, zeroVar, strideVar)
    M
  }

  def setQuick(index: Int, value: T) {
    elementsVar(zeroVar + index * strideVar) = value
  }

  def toArray: Array[T] = {
    val values = Array.ofDim[T](size.toInt)
    toArray(values)
  }

  override def toArray(values: Array[T]): Array[T] = {
    checkSize(values.length)
    if (this.isNoView)
      System.arraycopy(this.elementsVar, 0, values, 0, this.elementsVar.length)
    else {
      for (i <- 0 until size.toInt) {
        values(i) = getQuick(i)
      }
    }
    values
  }

  def viewSelection(indexes: Array[Int]): Matrix1D[T] = {
    if (indexes == null)
      return this
    checkIndexes(indexes)
    new WrapperMatrix1D[T](this) {
      sizeVar = indexes.size
      override def remapIndex(index: Int) = indexes(index)
    }
  }
}
