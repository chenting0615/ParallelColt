package cern.colt.matrix.impl

import cern.colt.matrix.{Matrix, Matrix2D}

/**
 * Diagonal 2-d matrix holding <tt>double</tt> elements. First see the <a
 * href="package-summary.html">package summary</a> and javadoc <a
 * href="package-tree.html">tree view</a> to get the broad picture.
 * <p>
 *
 * @author Piotr Wendykier (piotr.wendykier@gmail.com)
 */
@SerialVersionUID(1L)
class DiagonalMatrix2D[@specialized T: Manifest: Numeric](rows: Int, columns: Int, protected val dindex: Int = 0) extends RemappedMatrix2D[T] {

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

  def this(rowsAndColumns: Int, value: T) {
    this(rowsAndColumns, rowsAndColumns, 0)
    assignConstant(value)
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

  def this(storageMatrix: DiagonalMatrix2D[T], rows: Int, columns: Int, dindex: Int) {
    this(rows, columns, dindex)
    this.elementsVar = storageMatrix.getElements
    this.dlength = this.elementsVar.length
    this.isNoView = false
  }

  def getElements = elementsVar

  override def isSparse = true

  override def isRowMajor = true

  override def allCellsAreSettable = false

  override def canSetCellAt(row: Int, column: Int) = toRawIndex(row, column) >= 0

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
      throw new IllegalArgumentException("Must have same number of rows: rows=" + values.length + "rows=" + rows)
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
        throw new IllegalArgumentException("Must have same number of columns in every row: columns=" + values(r).length + "columns=" + columns)
      elementsVar(i) = values(r)(c)
      r += 1
      c += 1
    }
    this
  }

  override def assign(source: Matrix2D[T]): DiagonalMatrix2D[T] = {
    if (source == null) return this
    if (source eq this) return this
    checkShape(source)
    source match {
      case other: DiagonalMatrix2D[T] => {
        if (dindex != other.diagonalIndex || dlength != other.diagonalLength)
          throw new IllegalArgumentException("source is DiagonalDoubleMatrix2D with different diagonal stored.")

        System.arraycopy(other.getElements, 0, this.elementsVar, 0, this.elementsVar.length)
      }
      case _ => super.assign(source)
    }
    this
  }

  override def numNonZero: Long = {
    var cardinality = 0L
    for (r <- 0 until dlength) if (elementsVar(r) != zero) cardinality += 1
    cardinality
  }

  override def everyCellEquals(value: T): Boolean = {
    for (r <- 0 until dlength) {
      if (elementsVar(r) != value)
        return false
    }
    true
  }


  /**
   * Returns whether all cells are equal to the given value.
   *
   * @param value
   * the value to test against, within the given tolerance.
   * @return <tt>true</tt> if all cells are equal to the given value,
   *         <tt>false</tt> otherwise.
   */
  override def everyCellEquals(value: T, tolerance: Double): Boolean = {
    for (r <- 0 until dlength) {
      if (numeric.toDouble(elementsVar(r)) - numeric.toDouble(value) > tolerance)
        return false
    }
    true
  }

  override def equals(obj: Any): Boolean = {
    if (obj == null) return false
    obj match {
      case other: DiagonalMatrix2D[T] => {
        if (this eq other) return true
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

  override def forEachNonZeroRowMajor(function: Function3[Int, Int, T, T]) = {
    var r: Int = 0
    var c: Int = 0
    if (dindex >= 0)
      c = dindex
    else
      r = -dindex

    for(i <- 0 until dlength) {
      val value = elementsVar(i)
      if (value != zero)
        elementsVar(i) = function.apply(i+r, i+c, value)
    }
    this
  }

  override def forEachNonZeroColumnMajor(function: Function3[Int, Int, T, T]) = {
    forEachNonZeroRowMajor(function)
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

  def toRawIndex(row: Int, column: Int): Int = {
    if (dindex >= 0) {
      if (column >= dindex && row < dlength && row + dindex == column)
          return row
    }
    else if (row >= -dindex && column < dlength && row + dindex == column)
      return column

    -1
  }

  override def getQuick(row: Int, column: Int): T = {
    val idx = toRawIndex(row, column)
    if (idx >= 0)
      elementsVar(idx)
    else
      zero
  }

  override def setQuick(row: Int, column: Int, value: T) {
    val idx = toRawIndex(row, column)
    if (idx >= 0)
      elementsVar(idx) = value
  }

  override def like2D(rows: Int, columns: Int) = {
    new SparseHashMatrix2D[T](rows, columns)
  }

  override def like1D(size: Int) = new SparseHashMatrix1D[T](size)

  // Can't override here because c.elementsVar can't be assigned because of @specialized
/*
  override def copy() = {
    val c = clone().asInstanceOf[DiagonalMatrix2D[T]]
    c.elementsVar = elementsVar.clone()
    c
  }
*/

  /**
   * Compares this object against the specified object. The result is
   * <code>true</code> if and only if the argument is not <code>null</code>
   * and is  a <code>Matrix</code> of the same dimensions (rows, columns, etc.)
   * and type [T] as the receiver and has the same values at
   * the same coordinates within the given tolerance.
   *
   * @param mtrx
   * the object to compare with.
   * @return <code>true</code> if the objects are the same within the given tolerance <code>false</code>
   *         otherwise.
   */
  override def equals(mtrx: Matrix[T], tolerance: Double): Boolean = {
    if (tolerance == 0.0)
      return equals(mtrx)
    mtrx match {
      case other: DiagonalMatrix2D[T] => {
        if (columnsVar != other.columns || rowsVar != other.rows) return false
        if (dindex != other.diagonalIndex || dlength != other.diagonalLength) return false

        for (r <- 0 until dlength) {
          if (numeric.toDouble(numeric.minus(elementsVar(r), other.getElements(r))) > tolerance)
            return false
        }
        true
      }
      case _ => super.equals(mtrx, tolerance)
    }
    true
  }

  override def viewColumnFlip() = {
    if (columnsVar == 0)
      this
    else {
      val view = new DiagonalMatrix2D[T](this, rowsVar, columnsVar, dindex) {
        override def toRawIndex(row: Int, column: Int): Int = {
          super.toRawIndex(row, columnsVar - 1 - column)
        }
      }
      view
    }
  }

  override def viewTranspose() = {
    val view = new DiagonalMatrix2D[T](this, columnsVar, rowsVar, -dindex)
    view
  }

  override def viewPart(boxRow: Int, boxColumn: Int, height: Int, width: Int) = {
    checkBox(boxRow, boxColumn, height, width)
    val view = new WrapperMatrix2D[T](this, height, width) {
      override protected def remapIndexes(row: Int, column: Int): Tuple2[Int, Int] = (row+boxRow, column+boxColumn)
    }
    view
  }

  override def viewRowFlip() = {
    if (rowsVar == 0)
      this
    else {
      val view = new DiagonalMatrix2D[T](this, rowsVar, columnsVar, dindex) {
        override def toRawIndex(row: Int, column: Int): Int = {
          super.toRawIndex(rowsVar - 1 - row, column)
        }
      }
      view
    }
  }

  override def viewSelection(rowIndexes: Array[Int], columnIndexes: Array[Int]) = {
    checkRowIndexes(rowIndexes)
    checkColumnIndexes(columnIndexes)
    val viewRows = if (rowIndexes == null) rowsVar else rowIndexes.length
    val viewColumns = if (columnIndexes == null) columnsVar else columnIndexes.length
    val view = new DiagonalMatrix2D(this, viewRows, viewColumns, dindex) {
      override def toRawIndex(row: Int, column: Int): Int = {
        super.toRawIndex(if (rowIndexes == null) row else rowIndexes(row), if (columnIndexes == null) column else columnIndexes(column))
      }
    }
    view
  }

  override def viewStrides(rowStride: Int, columnStride: Int) = {
    if (rowStride <= 0 || columnStride <= 0) throw new IndexOutOfBoundsException("illegal stride")
    val viewRows = if (rowsVar != 0) (rowsVar - 1) / rowStride + 1 else rowsVar
    val viewColumns = if (columnsVar != 0) (columnsVar - 1) / columnStride + 1 else columnsVar
    val view = new WrapperMatrix2D(this, viewRows, viewColumns) {
      override protected def remapIndexes(row: Int, column: Int): Tuple2[Int, Int] = {
        (rowStride * row, columnStride * column)
      }
    }
    view
  }
}
