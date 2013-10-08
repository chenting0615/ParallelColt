
object SpecializedBug2 {

  trait Matrix2D[@specialized T]  {

    protected var isNoView: Boolean = true

    def isView: Boolean = !this.isNoView

    def setIsView(isView: Boolean) {
      this.isNoView = ! isView
    }

    def isSparse: Boolean

    def like2D(rows: Int, columns: Int): Matrix2D[T]

    def haveSharedCells(other: Matrix2D[T]): Boolean = {
      if (other == null) return false
      if (this eq other) return true
      val thisStorage = getStorageMatrix
      val otherStorage = other.getStorageMatrix
      if ((otherStorage eq null) || (thisStorage eq null)) return false
      if (thisStorage eq otherStorage) return true
      if ((this eq thisStorage) && (other eq otherStorage)) return false
      thisStorage.haveSharedCells(otherStorage)
    }

    def getStorageMatrix: Matrix2D[T] = this

    def setStorageMatrix(m: Matrix2D[T]) {}

    def equals(obj: Any): Boolean

    def equals(other: Matrix2D[T], tolerance: Double): Boolean

    def everyCellEquals(value: T): Boolean

    def everyCellEquals(value: T, tolerance: Double): Boolean

    override def toString: String
/////////////////////////////////////////////////

    def isRowMajor: Boolean

    def columns: Int

    def rows: Int

    def size: Long = rows * columns

    def numNonZero: Long

    def get(row: Int, column: Int): T = {
      checkColumn(column)
      checkRow(row)
      getQuick(row, column)
    }

    def getQuick(row: Int, column: Int): T

    def set(row: Int, column: Int, value: T) {
      checkColumn(column)
      checkRow(row)
      setQuick(row, column, value)
    }

    def setQuick(row: Int, column: Int, value: T): Unit

    def assignConstant(value: T): Matrix2D[T]

    def assign(values: Array[T]): Matrix2D[T]

    def assign(other: Array[Array[T]]): Matrix2D[T]

    def assign(other: Matrix2D[T]): Matrix2D[T]

    def forEachNonZeroRowMajor(function: Function3[Int, Int, T, T]): Matrix2D[T]

    def forEachNonZeroColumnMajor(function: Function3[Int, Int, T, T]): Matrix2D[T]

    def forEachNonZeroInRow(rowIdx: Int, function: Function3[Int, Int, T, T]): Matrix2D[T]

    def forEachNonZeroInColumn(colIdx: Int, function: Function3[Int, Int, T, T]): Matrix2D[T]

    def viewTranspose(): Matrix2D[T]

    def viewPart(row: Int, column: Int, height: Int, width: Int): Matrix2D[T]

    def viewColumnFlip(): Matrix2D[T]

    def viewRowFlip(): Matrix2D[T]

    def viewStrides(rowStride: Int, columnStride: Int): Matrix2D[T]

    def viewSelection(rowIndexes: Array[Int], columnIndexes: Array[Int]): Matrix2D[T]

    def copy(): Matrix2D[T]

    def toArray: Array[Array[T]]

    def toArray(values: Array[Array[T]]): Array[Array[T]]

    def checkColumn(column: Int)

    def checkRow(row: Int)

    def checkRowShape(rowSize: Int)

    def checkColumnShape(columnSize: Int)

    def checkShape(B: Matrix2D[T])

    def checkShape(B: Matrix2D[T], C: Matrix2D[T])
  }

  abstract class AbstractMatrix2D[@specialized T: Manifest: Numeric] extends Matrix2D[T] {

    val numeric = implicitly[Numeric[T]]
    val zero = numeric.zero

    protected var columnsVar: Int = 0

    protected var rowsVar: Int = 0

    protected def setUp(rows: Int, columns: Int) {
      rowsVar = rows
      columnsVar = columns
    }

    def columns: Int = columnsVar

    def rows: Int = rowsVar

    override def size: Long = rowsVar * columnsVar

    def checkBox(row: Int, column: Int, height: Int, width: Int) {
      if (column < 0 || width < 0 || column + width > columnsVar ||
        row < 0 || height < 0 || row + height > rowsVar) {

        throw new IndexOutOfBoundsException(", column:" + column + ", row:" + row + " ,width:" + width + ", height:" + height)
      }
    }

    def checkColumn(column: Int) {
      if (column < 0 || column >= columnsVar)
        throw new IndexOutOfBoundsException("Attempted to access  at column=" + column)
    }

    def checkRowShape(rowIdx: Int) {
      if (rowIdx != rowsVar)
        throw new IllegalArgumentException("Incompatible dimensions:  and rows=" + rowIdx)
    }

    def checkColumnShape(colIdx: Int) {
      if (colIdx != columnsVar)
        throw new IllegalArgumentException("Incompatible dimensions:  and columns=" + colIdx)
    }

    def checkRow(row: Int) {
      if (row < 0 || row >= rowsVar)
        throw new IndexOutOfBoundsException("Attempted to access  at row=" + row)
    }

    protected def checkColumnIndexes(indexes: Array[Int]) {
      if (indexes != null) {
        for(i <- 0 until indexes.length) {
          val index = indexes(i)
          if (index < 0 || index >= columnsVar) checkColumn(index)
        }
      }
    }

    def checkRowIndexes(indexes: Array[Int]) {
      if (indexes != null) {
        for(i <- 0 until indexes.length) {
          val index = indexes(i)
          if (index < 0 || index >= rowsVar) checkRow(index)
        }
      }
    }

    def checkShape(B: Matrix2D[T]) {
      if (columnsVar != B.columns || rowsVar != B.rows)
        throw new IllegalArgumentException("Incompatible dimensions: ")
    }

    def checkShape(B: Matrix2D[T], C: Matrix2D[T]) {
      if (columnsVar != B.columns || rowsVar != B.rows || columnsVar != C.columns || rowsVar != C.rows)
        throw new IllegalArgumentException("Incompatible dimensions: ")
    }

    def assignConstant(value: T) = {
      for (r <- 0 until rowsVar; c <- 0 until columnsVar) {
        setQuick(r, c, value)
      }
      this
    }

    def assign(values: Array[T]) = {
      if (values.length < size)
        throw new IllegalArgumentException("Must have same length: length=" + values.length + " rows*columns=" + rows * columns)
      var idx = 0
      for (r <- 0 until rowsVar; c <- 0 until columnsVar) {
        setQuick(r, c, values(idx))
        idx += 1
      }
      this
    }

    def assign(values: Array[Array[T]]) = {
      if (values.length < rowsVar)
        throw new IllegalArgumentException("Must have same number of rows: rows=" + values.length + "rows=" + rows)
      for (r <- 0 until rowsVar) {
        val currentRow = values(r)
        if (currentRow.length < columnsVar)
          throw new IllegalArgumentException("Must have same number of columns in every row: columns=" + currentRow.length + "columns=" + columns)
        for (c <- 0 until columnsVar) {
          setQuick(r, c, currentRow(c))
        }
      }
      this
    }

    def assign(other: Matrix2D[T]) = {
      if (other ne this) {
        checkShape(other)
        val source = if (haveSharedCells(other)) other.copy() else other
        for (r <- 0 until rowsVar; c <- 0 until columnsVar) {
          setQuick(r, c, source.getQuick(r, c))
        }
      }
      this
    }

    def numNonZero: Long = {
      var cardinality = 0
      for (r <- 0 until rowsVar; c <- 0 until columnsVar)
        if (getQuick(r, c) != zero) cardinality += 1
      cardinality
    }

    override def everyCellEquals(value: T): Boolean = {
      for (r <- 0 until rowsVar; c <- 0 until columnsVar)
        if (getQuick(r, c) != value) return false
      true
    }

    def everyCellEquals(value: T, tolerance: Double): Boolean = {
      true
    }

    override def equals(obj: Any): Boolean = {
      if (obj == null) return false
      if (! obj.isInstanceOf[Matrix2D[T]]) return false
      val other = obj.asInstanceOf[Matrix2D[T]]
      if (this eq other) return true
      if (other.rows != rowsVar || other.columns != columnsVar) return false
      for (r <- 0 until rowsVar; c <- 0 until columnsVar)
        if (getQuick(r, c) != other.getQuick(r, c)) return false

      true
    }

    def equals(other: Matrix2D[T], tolerance: Double): Boolean = {
      if (tolerance == 0.0)
        return equals(other)
      if (other == null)
        return false
      if (other eq this)
        return true
      if ( ! other.isInstanceOf[Matrix2D[T]])
        return false

      val other2D = other.asInstanceOf[Matrix2D[T]]
      if (other2D.rows != rowsVar)
        return false
      if (other2D.columns != columnsVar)
        return false
      true
    }

    def copy() = like2D(rowsVar, columnsVar).assign(this)

    def forEachNonZeroRowMajor(function: Function3[Int, Int, T, T]): Matrix2D[T] = {
      for (r <- 0 until rows; c <- 0 until columns) {
        val value = getQuick(r, c)
        if (value != zero) {
          val a = function.apply(r, c, value)
          if (a != value) setQuick(r, c, a)
        }
      }
      this
    }

    def forEachNonZeroColumnMajor(function: Function3[Int, Int, T, T]): Matrix2D[T] = {
      for (c <- 0 until columns; r <- 0 until rows) {
        val value = getQuick(r, c)
        if (value != zero) {
          val a = function.apply(r, c, value)
          if (a != value) setQuick(r, c, a)
        }
      }
      this
    }

    def forEachNonZeroInRow(rowIdx: Int, function: Function3[Int, Int, T, T]): Matrix2D[T] = {
      for (c <- 0 until columns) {
        val value = getQuick(rowIdx, c)
        if (value != zero) {
          val a = function.apply(rowIdx, c, value)
          if (a != value) setQuick(rowIdx, c, a)
        }
      }
      this
    }

    def forEachNonZeroInColumn(colIdx: Int, function: Function3[Int, Int, T, T]): Matrix2D[T] = {
      for (r <- 0 until rows) {
        val value = getQuick(r, colIdx)
        if (value != zero) {
          val a = function.apply(r, colIdx, value)
          if (a != value) setQuick(r, colIdx, a)
        }
      }
      this
    }

    def isRowMajor: Boolean = true

    def toArray: Array[Array[T]] = {
      val values = Array.ofDim[T](rows, columns)
      toArray(values)
    }

    def toArray(values: Array[Array[T]]) = {
      checkRowShape(values.length)
      for (row <- 0 until rowsVar) {
        val currentRow = values(row)
        checkColumnShape(currentRow.length)
        for(column <- 0 until columnsVar)
          currentRow(column) = getQuick(row, column)
      }
      values
    }

    def isSparse: Boolean = false
  }

  abstract class RemappedMatrix2D[@specialized T: Manifest: Numeric] extends AbstractMatrix2D[T] {

    isNoView = false

    override def viewColumnFlip(): AbstractMatrix2D[T] = {
      if (columnsVar == 0) return this
      val view = new WrapperMatrix2D[T](this) {
        override protected def remapIndexes(row: Int, column: Int): Tuple2[Int, Int] = (row, columns - 1 - column)
      }
      view
    }

    override def viewTranspose(): WrapperMatrix2D[T] = {
      val view = new WrapperMatrix2D[T](this, columnsVar, rowsVar) {
        override protected def remapIndexes(row: Int, column: Int): Tuple2[Int, Int] = (column, row)
      }
      view
    }

    override def viewPart(boxRow: Int, boxColumn: Int, height: Int, width: Int): WrapperMatrix2D[T] = {
      checkBox(boxRow, boxColumn, height, width)
      val view = new WrapperMatrix2D[T](this, height, width) {
        override protected def remapIndexes(row: Int, column: Int): Tuple2[Int, Int] = (row+boxRow, column+boxColumn)
      }
      view
    }

    override def viewRowFlip(): AbstractMatrix2D[T] = {
      if (rowsVar == 0) return this
      val view = new WrapperMatrix2D[T](this) {
        override protected def remapIndexes(row: Int, column: Int): Tuple2[Int, Int] = (rowsVar - 1 - row, column)
      }
      view
    }

    override def viewSelection(rowIndexes: Array[Int], columnIndexes: Array[Int]) = {
      checkRowIndexes(rowIndexes)
      checkColumnIndexes(columnIndexes)
      val viewRowsVar = if (rowIndexes == null) rowsVar else rowIndexes.length
      val viewColumnsVar = if (columnIndexes == null) columnsVar else columnIndexes.length
      val view = new WrapperMatrix2D(this, viewRowsVar, viewColumnsVar) {
        override protected def remapIndexes(row: Int, column: Int): Tuple2[Int, Int] = {
          (if (rowIndexes == null) row else rowIndexes(row), if (columnIndexes == null) column else columnIndexes(column))
        }
      }
      view
    }

    override def viewStrides(rowStride: Int, columnStride: Int): WrapperMatrix2D[T] = {
      if (rowStride <= 0 || columnStride <= 0) throw new IndexOutOfBoundsException("illegal stride")
      val viewRowsVar = if (rowsVar != 0) (rowsVar - 1) / rowStride + 1 else rowsVar
      val viewColumnsVar = if (columnsVar != 0) (columnsVar - 1) / columnStride + 1 else columnsVar
      val view = new WrapperMatrix2D(this, viewRowsVar, viewColumnsVar) {
        override protected def remapIndexes(row: Int, column: Int): Tuple2[Int, Int] = {
          (rowStride * row, columnStride * column)
        }
      }
      view
    }
  }

  class WrapperMatrix2D[@specialized T: Manifest: Numeric](protected val content: Matrix2D[T], rows: Int, columns: Int) extends RemappedMatrix2D[T] {

    isNoView = false
    if (content == null)
      throw new NullPointerException("Wrapped content matrix is null")
    try {
      setUp(rows, columns)
    }
    catch {
      case exc: IllegalArgumentException => if ("matrix too large" != exc.getMessage) throw exc
    }

    def this(content: Matrix2D[T]) {
      this(content, content.rows, content.columns)
    }

    protected def remapIndexes(row: Int, column: Int): Tuple2[Int, Int] = (row, column)

    def getQuick(row: Int, column: Int): T = {
      val t = remapIndexes(row, column)
      content.getQuick(t._1, t._2)
    }

    def setQuick(row: Int, column: Int, value: T) {
      val t = remapIndexes(row, column)
      content.setQuick(t._1, t._2, value)
    }

    def like2D(rows: Int, columns: Int): Matrix2D[T] = content.like2D(rows, columns)

    override def getStorageMatrix = content.getStorageMatrix

    /**
     * @return Returns true if this matrix uses a sparse representation for storing cell values
     */
    override def isSparse: Boolean = content.isSparse

    override def isRowMajor: Boolean = content.isRowMajor
  }

  // If @specialized is removed here, the problem doesn't happen on this class
  class SparseRCMatrix2D[ T: Manifest: Numeric](rows_p: Int, columns_p: Int, nzmax: Int) extends WrapperMatrix2D[T](null) {

    //protected var rowPointers: Array[Int] = Array.ofDim[Int](rows_p + 1)

    protected var columnIndexes: Array[Int] = Array.ofDim[Int](nzmax)

    protected var values: Array[T] = Array.ofDim[T](nzmax)

    def this(rows: Int,
        columns: Int,
        rowIndexes: Array[Int],
        columnIndexes: Array[Int],
        values: Array[T],
        sortColumnIndexes_p: Boolean) {

      this(rows, columns, Math.max(rowIndexes.length, 1))
      val nz = nzmax
/*
      if (rowIndexes.length != columnIndexes.length) {
        throw new IllegalArgumentException("rowIndexes.length != columnIndexes.length")
      } else if (rowIndexes.length != values.length) {
        throw new IllegalArgumentException("rowIndexes.length != values.length")
      }
      this.columnIndexes = Array.ofDim[Int](nz)
      this.values = Array.ofDim[T](nz)
      this.rowPointers = Array.ofDim[Int](rows + 1)
*/
      val w = Array.ofDim[Int](rows)
      for (k <- 0 until nz) {
        w(rowIndexes(k)) += 1
      }
      var r: Int = 0
      for (k <- 0 until nz) {
        r = w(rowIndexes(k))
        w(rowIndexes(k)) += 1
        this.columnIndexes(r) = columnIndexes(k)
        this.values(r) = values(k)
      }
    }
  }

  // If @specialized is removed here, the problem doesn't happen on this class
  class DiagonalMatrix2D[ T: Manifest: Numeric](rows: Int, columns: Int, protected val dindex: Int = 0) extends RemappedMatrix2D[T] {

/*
    if (dindex < -rows + 1 || dindex > columns - 1)
      throw new IllegalArgumentException("index is out of bounds")
*/

/*
    protected var dlength = 0
*/

    protected var elementsVar: Array[T] = Array.ofDim[T](10)

/*
    try {
      setUp(rows, columns)
    } catch {
      case exc: IllegalArgumentException => if ("matrix too large" != exc.getMessage) throw exc
    }
*/

    def this(rowsAndColumns: Int, value: T) {
      this(rowsAndColumns, rowsAndColumns, 0)
      for(i <- 0 until rowsAndColumns)
        setQuick(i, i, value)
    }

/*
    def this(values: Array[Array[T]], dindex: Int = 0) {
      this(values.length, if (values.length == 0) 0 else values(0).length, dindex)
      assign(values)
    }
*/

/*
    def getElements = elementsVar

    override def isSparse = true

    override def isRowMajor = true
*/

/*
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

    override def everyCellEquals(value: T, tolerance: Double): Boolean = {
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

    def diagonalLength: Int = dlength

    def diagonalIndex: Int = dindex

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
*/

    override def getQuick(row: Int, column: Int): T = {
      zero
    }

    override def setQuick(row: Int, column: Int, value: T) {
    }

    override def like2D(rows: Int, columns: Int) = {
      null
    }

  }
}
