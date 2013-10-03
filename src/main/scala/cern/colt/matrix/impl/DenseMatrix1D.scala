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
 */
@specialized
@SerialVersionUID(1L)
class DenseMatrix1D[T: Manifest](size: Int) extends StrideMatrix1D[T] {

  protected var elementsVar: Array[T] = null

  setUp(size)

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
   * Constructs a matrix with the given parameters.
   *
   * @param size
   *            the number of cells the matrix shall have.
   * @param elements
   *            the cells.
   * @param zero
   *            the index of the first element.
   * @param stride
   *            the number of indexes between any two elements, i.e.
   *            <tt>index(i+1)-index(i)</tt>.
   * @param isView
   *            if true then a matrix view is constructed
   * @throws IllegalArgumentException
   *             if <tt>size<0</tt>.
   */
  def this(size: Int, elements: Array[T], zero: Int, stride: Int, isView: Boolean) {
    // TODO: Much check size, zero, stride == elements.length
    this(size)
    setUp(size, zero, stride)
    if (elements != null)
      this.elementsVar = elements
    this.isNoView = !isView
  }

  def elements: Array[T] = elementsVar

  override def assignConstant(value: T) = {
    if (zeroVar == 0 && strideVar == 1) {
      for(i <- 0 until sizeVar)
        elementsVar(i) = value
    }
    else {
      for (i <- zeroVar until sizeVar by strideVar)
        elementsVar(i) = value
    }
    this
  }

  override def assign(values: Array[T]) = {
    checkSize(values.length)
    if (zeroVar == 0 && strideVar == 1) {
      System.arraycopy(elementsVar, 0, values, 0, sizeVar)
    }
    else {
      var idx = zeroVar
      for (i <- 0 until sizeVar) {
        elementsVar(idx) = values(i)
        idx += strideVar
      }
    }
    this
  }

  override def assign(source: Matrix1D[T]): Matrix1D[T] = {
    if (source == this) return this
    checkSize(source)
    if (! source.isInstanceOf[DenseMatrix1D[T]]) {
      super.assign(source)
      return this
    }
    var other = source.asInstanceOf[DenseMatrix1D[T]]
    if (isNoView && other.isNoView) {
      System.arraycopy(other.elementsVar, 0, this.elementsVar, 0, this.elementsVar.length)
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
    var idxOther = other.zeroVar
    for (k <- 0 until size.toInt) {
      elementsVar(idx) = other.elementsVar(idxOther)
      idx += stride
      idxOther += other.strideVar
    }
    this
  }


  override def numNonZero: Long = {
    var cardinality = 0
    for (idx <- zeroVar until sizeVar by strideVar)
      if (elementsVar(idx) != 0) cardinality += 1
    cardinality
  }

  override def forEachNonZero(f: Function2[Int, T, T]) = {
    val rem = sizeVar % 2
    var idx = zeroVar
    if (rem == 1) {
      val value = elementsVar(idx)
      if (value != 0.0) {
        elementsVar(idx) = f.apply(0, value)
      }
      idx += stride
    }
    var i = rem
    while (i < sizeVar) {
      var value = elementsVar(idx)
      if (value != 0.0)
        elementsVar(idx) = f.apply(i, value)
      idx += strideVar
      value = elementsVar(idx)
      if (value != 0.0)
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
    M.assign(elementsVar, zeroVar, strideVar)
    M
  }

  def setQuick(index: Int, value: T) {
    elementsVar(zeroVar + index * strideVar) = value
  }

  override def toArray(values: Array[T]): Array[T] = {
    if (values.length < sizeVar) throw new IllegalArgumentException("destination array too small (length = " + values.length + ", but requires " + sizeVar + ")")
    if (this.isNoView) System.arraycopy(this.elementsVar, 0, values, 0, this.elementsVar.length) else super.toArray(values)
    values
  }
}
