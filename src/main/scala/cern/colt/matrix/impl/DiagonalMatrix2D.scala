package cern.colt.matrix.impl

import cern.colt.matrix.Matrix2D

/**
 * Diagonal 2-d matrix holding <tt>double</tt> elements. First see the <a
 * href="package-summary.html">package summary</a> and javadoc <a
 * href="package-tree.html">tree view</a> to get the broad picture.
 * <p>
 *
 * @author Piotr Wendykier (piotr.wendykier@gmail.com)
 */
@specialized
@SerialVersionUID(1L)
class DiagonalMatrix2D[T: Manifest](rows: Int, columns: Int, protected val dindex: Int = 0) extends RemappedMatrix2D[T] {

  if (dindex < -rows + 1 || dindex > columns - 1)
    throw new IllegalArgumentException("index is out of bounds")

  protected var dlength = if (dindex == 0) {
      Math.min(rows, columns)
    } else if (dindex > 0) {
      if (rows >= columns) {
        columns - dindex
      } else {
        val diff = columns - rows
        if (dindex <= diff) rows else rows - (dindex - diff)
      }
    } else {
      if (rows >= columns) {
        val diff = rows - columns
        if (-dindex <= diff) columns else columns + dindex + diff
      } else {
        rows + dindex
      }
    }

  protected var elementsVar: Array[T] = Array.ofDim[T](dlength)

  try {
    setUp(rows, columns)
  } catch {
    case exc: IllegalArgumentException => if ("matrix too large" != exc.getMessage) throw exc
  }

  /**
   * Constructs a matrix with a copy of the given values. <tt>values</tt> is
   * required to have the form <tt>values[row][column]</tt> and have exactly
   * the same number of columns in every row. Only the values on the main
   * diagonal, i.e. values[i][i] are used.
   * <p>
   * The values are copied. So subsequent changes in <tt>values</tt> are not
   * reflected in the matrix, and vice-versa.
   *
   * @param values
   *            The values to be filled into the new matrix.
   * @param dindex
   *            index of the diagonal.
   * @throws IllegalArgumentException
   *             if
   *
   *             <tt>for any 1 &lt;= row &lt; values.length: values[row].length != values[row-1].length || index < -rows+1 || index > columns - 1</tt>
   *             .
   */
  def this(values: Array[Array[T]], dindex: Int = 0) {
    this(values.length, if (values.length == 0) 0 else values(0).length, dindex)
    assign(values)
  }

  override def isSparse = true

  override def isRowMajor = true

  override def assignConstant(value: T) = {
    for(i <- 0 until dlength)
      elementsVar(i) = value
    this
  }

  override def assign(values: Array[T]) = {
    if (values.length != dlength)
      throw new IllegalArgumentException("Must have same length: length=" + values.length + " dlength=" + dlength)
    for(r <- 0 until dlength)
      elementsVar(r) = values(r)
    this
  }

  override def assign(values: Array[Array[T]]) = {
    if (values.length != rows)
      throw new IllegalArgumentException("Must have same number of rows: rows=" + values.length + "rows()=" + rows)
    var r: Int = 0
    var c: Int = 0
    if (dindex >= 0) {
      r = 0
      c = dindex
    }
    else {
      r = -dindex
      c = 0
    }
    for (i <- 0 until dlength) {
      if (values(i).length != columns)
        throw new IllegalArgumentException("Must have same number of columns in every row: columns=" + values(r).length + "columns()=" + columns)
      elementsVar(i) = values(r)(c)
      r += 1
      c += 1
    }
    this
  }

  override def assign(source: Matrix2D[T]): DiagonalMatrix2D[T] = {
    if (source == null) return this
    if (source == this) return this
    checkShape(source)
    source match {
      case other: DiagonalMatrix2D[T] => {
        if (dindex != other.dindex || dlength != other.dlength)
          throw new IllegalArgumentException("source is DiagonalDoubleMatrix2D with different diagonal stored.")

        System.arraycopy(other.elementsVar, 0, this.elementsVar, 0, this.elementsVar.length)
      }
      case _ => super.assign(source)
    }
    this
  }

  override def numNonZero: Long = {
    var cardinality = 0L
    for (r <- 0 until dlength) if (elementsVar(r) != 0) cardinality += 1
    cardinality
  }

  override def everyCellEquals(value: T): Boolean = {
    for (r <- 0 until dlength) {
      if (elementsVar(r) != value)
        return false
    }
    true
  }

  override def equals(obj: Any): Boolean = {
    if (obj == null) return false
    if (this == obj) return true
    obj match {
      case other: DiagonalMatrix2D[T] => {
        if (columns != other.columns || rows != other.rows) return false
        if (dindex != other.dindex || dlength != other.dlength) return false

        for (r <- 0 until dlength) {
          if (elementsVar(r) != other.elementsVar(r))
            return false
        }
        true
      }
      case _ => super.equals(obj)
    }
  }

  override def forEachNonZero(function: Function3[Int, Int, T, T]) = {
    var r: Int = 0
    var c: Int = 0
    if (dindex >= 0) {
      r = 0
      c = dindex
    }
    else {
      r = -dindex
      c = 0
    }
    for(i <- 0 until dlength) {
      val value = elementsVar(i)
      if (value != 0)
        elementsVar(i) = function.apply(i+r, i+c, value)
    }
    this
  }

  /**
   * Returns the length of the diagonal
   *
   * @return the length of the diagonal
   */
  def diagonalLength: Int = dlength

  /**
   * Returns the index of the diagonal
   *
   * @return the index of the diagonal
   */
  def diagonalIndex: Int = dindex

  override def getQuick(row: Int, column: Int): T = {
    if (dindex >= 0) {
      if (column >= dindex && row < dlength && row + dindex == column)
          return elementsVar(row)
    }
    else if (row >= -dindex && column < dlength && row + dindex == column)
      return elementsVar(column)
    0.asInstanceOf[T]
  }

  override def setQuick(row: Int, column: Int, value: T) {
    if (dindex >= 0) {
      if (column >= dindex && row < dlength && row + dindex == column)
          elementsVar(row) = value
    }
    else if (row >= -dindex && column < dlength && row + dindex == column)
      elementsVar(column) = value
  }

  override def like2D(rows: Int, columns: Int) = new SparseHashMatrix2D[T](rows, columns)

  override def like1D(size: Int) = new SparseMatrix1D[T](size)
}
