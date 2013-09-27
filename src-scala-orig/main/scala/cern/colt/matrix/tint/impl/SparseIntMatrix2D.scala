package cern.colt.matrix.tint.impl

import java.io.IOException
import cern.colt.map.tlong.AbstractLongIntMap
import cern.colt.map.tlong.OpenLongIntHashMap
import cern.colt.matrix.io.MatrixInfo
import cern.colt.matrix.io.MatrixSize
import cern.colt.matrix.io.MatrixVectorReader
import cern.colt.matrix.tint.IntMatrix1D
import cern.colt.matrix.tint.IntMatrix2D
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Sparse hashed 2-d matrix holding <tt>int</tt> elements. First see the <a
 * href="package-summary.html">package summary</a> and javadoc <a
 * href="package-tree.html">tree view</a> to get the broad picture.
 * <p>
 * <b>Implementation:</b>
 * <p>
 * Note that this implementation is not synchronized. Uses a
 * {@link cern.colt.map.tint.OpenIntIntHashMap}, which is a compact and
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
 * Thus, a 1000 x 1000 matrix with minLoadFactor=0.25 and maxLoadFactor=0.5 and
 * 1000000 non-zero cells consumes between 25 MB and 50 MB. The same 1000 x 1000
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
 * {@link DenseIntMatrix2D}. However, constant factors are considerably larger.
 * <p>
 * Cells are internally addressed in row-major. Performance sensitive
 * applications can exploit this fact. Setting values in a loop row-by-row is
 * quicker than column-by-column, because fewer hash collisions occur. Thus
 *
 * <pre>
 * for (int row = 0; row &lt; rows; row++) {
 *     for (int column = 0; column &lt; columns; column++) {
 *         matrix.setQuick(row, column, someValue);
 *     }
 * }
 * </pre>
 *
 * is quicker than
 *
 * <pre>
 * for (int column = 0; column &lt; columns; column++) {
 *     for (int row = 0; row &lt; rows; row++) {
 *         matrix.setQuick(row, column, someValue);
 *     }
 * }
 * </pre>
 *
 * @see cern.colt.map
 * @see cern.colt.map.tint.OpenIntIntHashMap
 * @author wolfgang.hoschek@cern.ch
 * @version 1.0, 09/24/99
 *
 * @author Piotr Wendykier (piotr.wendykier@gmail.com)
 */
@SerialVersionUID(1L)
class SparseIntMatrix2D(rows: Int,
    columns: Int,
    initialCapacity: Int,
    minLoadFactor: Double,
    maxLoadFactor: Double) extends IntMatrix2D {

  protected var elements: AbstractLongIntMap = new OpenLongIntHashMap(initialCapacity, minLoadFactor,
    maxLoadFactor)

  try {
    setUp(rows, columns)
  } catch {
    case exc: IllegalArgumentException => if ("matrix too large" != exc.getMessage) throw exc
  }

  /**
   * Constructs a matrix with a copy of the given values. <tt>values</tt> is
   * required to have the form <tt>values[row][column]</tt> and have exactly
   * the same number of columns in every row.
   * <p>
   * The values are copied. So subsequent changes in <tt>values</tt> are not
   * reflected in the matrix, and vice-versa.
   *
   * @param values
   *            The values to be filled into the new matrix.
   * @throws IllegalArgumentException
   *             if
   *             <tt>for any 1 &lt;= row &lt; values.length: values[row].length != values[row-1].length</tt>
   *             .
   */
  def this(values: Array[Array[Int]]) {
    this(values.length, if (values.length == 0) 0 else values(0).length)
    assign(values)
  }

  /**
   * Constructs a matrix with a given number of rows and columns and default
   * memory usage. All entries are initially <tt>0</tt>.
   *
   * @param rows
   *            the number of rows the matrix shall have.
   * @param columns
   *            the number of columns the matrix shall have.
   * @throws IllegalArgumentException
   *             if
   *             <tt>rows<0 || columns<0 || (double)columns*rows > Integer.MAX_VALUE</tt>
   *             .
   */
  def this(rows: Int, columns: Int) {
    this(rows, columns, rows * (columns / 1000), 0.2, 0.5)
  }

  /**
   * Constructs a matrix with a copy of the given indexes and a single value.
   *
   * @param rows
   *            the number of rows the matrix shall have.
   * @param columns
   *            the number of columns the matrix shall have.
   * @param rowIndexes
   *            row indexes
   * @param columnIndexes
   *            column indexes
   * @param value
   *            numerical value
   */
  def this(rows: Int,
      columns: Int,
      rowIndexes: Array[Int],
      columnIndexes: Array[Int],
      value: Int) {
    this()
    try {
      setUp(rows, columns)
    } catch {
      case exc: IllegalArgumentException => if ("matrix too large" != exc.getMessage) throw exc
    }
    this.elements = new OpenLongIntHashMap(rowIndexes.length)
    insert(rowIndexes, columnIndexes, value)
  }

  /**
   * Constructs a matrix with a copy of the given indexes and values.
   *
   * @param rows
   *            the number of rows the matrix shall have.
   * @param columns
   *            the number of columns the matrix shall have.
   * @param rowIndexes
   *            row indexes
   * @param columnIndexes
   *            column indexes
   * @param values
   *            numerical values
   */
  def this(rows: Int,
      columns: Int,
      rowIndexes: Array[Int],
      columnIndexes: Array[Int],
      values: Array[Int]) {
    this()
    try {
      setUp(rows, columns)
    } catch {
      case exc: IllegalArgumentException => if ("matrix too large" != exc.getMessage) throw exc
    }
    this.elements = new OpenLongIntHashMap(rowIndexes.length)
    insert(rowIndexes, columnIndexes, values)
  }

  /**
   * Constructs a matrix from MatrixVectorReader.
   *
   * @param reader
   *            matrix reader
   * @throws IOException
   */
  def this(reader: MatrixVectorReader) {
    this()
    var info: MatrixInfo = null
    info = if (reader.hasInfo()) reader.readMatrixInfo() else new MatrixInfo(true, MatrixInfo.MatrixField.Real,
      MatrixInfo.MatrixSymmetry.General)
    if (info.isPattern) throw new UnsupportedOperationException("Pattern matrices are not supported")
    if (info.isDense) throw new UnsupportedOperationException("Dense matrices are not supported")
    if (info.isComplex) throw new UnsupportedOperationException("Complex matrices are not supported")
    val size = reader.readMatrixSize(info)
    try {
      setUp(size.numRows(), size.numColumns())
    } catch {
      case exc: IllegalArgumentException => if ("matrix too large" != exc.getMessage) throw exc
    }
    val numEntries = size.numEntries()
    val columnIndexes = Array.ofDim[Int](numEntries)
    val rowIndexes = Array.ofDim[Int](numEntries)
    val values = Array.ofDim[Int](numEntries)
    reader.readCoordinate(rowIndexes, columnIndexes, values)
    this.elements = if (info.isSymmetric || info.isSkewSymmetric) new OpenLongIntHashMap(2 * rowIndexes.length) else new OpenLongIntHashMap(rowIndexes.length)
    insert(rowIndexes, columnIndexes, values)
    if (info.isSymmetric) {
      for (i <- 0 until numEntries if rowIndexes(i) != columnIndexes(i)) {
        set(columnIndexes(i), rowIndexes(i), values(i))
      }
    } else if (info.isSkewSymmetric) {
      for (i <- 0 until numEntries if rowIndexes(i) != columnIndexes(i)) {
        set(columnIndexes(i), rowIndexes(i), -values(i))
      }
    }
  }

  /**
   * Constructs a view with the given parameters.
   *
   * @param rows
   *            the number of rows the matrix shall have.
   * @param columns
   *            the number of columns the matrix shall have.
   * @param elements
   *            the cells.
   * @param rowZero
   *            the position of the first element.
   * @param columnZero
   *            the position of the first element.
   * @param rowStride
   *            the number of elements between two rows, i.e.
   *            <tt>index(i+1,j)-index(i,j)</tt>.
   * @param columnStride
   *            the number of elements between two columns, i.e.
   *            <tt>index(i,j+1)-index(i,j)</tt>.
   * @throws IllegalArgumentException
   *             if
   *             <tt>rows<0 || columns<0 || (double)columns*rows > Integer.MAX_VALUE</tt>
   *             or flip's are illegal.
   */
  protected def this(rows: Int,
      columns: Int,
      elements: AbstractLongIntMap,
      rowZero: Int,
      columnZero: Int,
      rowStride: Int,
      columnStride: Int) {
    this()
    try {
      setUp(rows, columns, rowZero, columnZero, rowStride, columnStride)
    } catch {
      case exc: IllegalArgumentException => if ("matrix too large" != exc.getMessage) throw exc
    }
    this.elements = elements
    this.isNoView = false
  }

  def assign(function: cern.colt.function.tint.IntFunction): IntMatrix2D = {
    if (this.isNoView && function.isInstanceOf[cern.jet.math.tint.IntMult]) {
      this.elements.assign(function)
    } else {
      super.assign(function)
    }
    this
  }

  def assign(value: Int): IntMatrix2D = {
    if (this.isNoView && value == 0) this.elements.clear() else super.assign(value)
    this
  }

  def assign(source: IntMatrix2D): IntMatrix2D = {
    if (!(source.isInstanceOf[SparseIntMatrix2D])) {
      return super.assign(source)
    }
    val other = source.asInstanceOf[SparseIntMatrix2D]
    if (other == this) return this
    checkShape(other)
    if (this.isNoView && other.isNoView) {
      this.elements.assign(other.elements)
      return this
    }
    super.assign(source)
  }

  def assign(y: IntMatrix2D, function: cern.colt.function.tint.IntIntFunction): IntMatrix2D = {
    if (!this.isNoView) return super.assign(y, function)
    checkShape(y)
    if (function.isInstanceOf[cern.jet.math.tint.IntPlusMultSecond]) {
      val alpha = function.asInstanceOf[cern.jet.math.tint.IntPlusMultSecond].multiplicator
      if (alpha == 0) return this
      y.forEachNonZero(new cern.colt.function.tint.IntIntIntFunction() {

        def apply(i: Int, j: Int, value: Int): Int = {
          setQuick(i, j, getQuick(i, j) + alpha * value)
          return value
        }
      })
    } else if (function == cern.jet.math.tint.IntFunctions.mult) {
      this.elements.forEachPair(new cern.colt.function.tlong.LongIntProcedure() {

        def apply(key: Long, value: Int): Boolean = {
          val i = (key / columns).toInt
          val j = (key % columns).toInt
          val r = value * y.getQuick(i, j)
          if (r != value) elements.put(key, r)
          return true
        }
      })
    } else if (function == cern.jet.math.tint.IntFunctions.div) {
      this.elements.forEachPair(new cern.colt.function.tlong.LongIntProcedure() {

        def apply(key: Long, value: Int): Boolean = {
          val i = (key / columns).toInt
          val j = (key % columns).toInt
          val r = value / y.getQuick(i, j)
          if (r != value) elements.put(key, r)
          return true
        }
      })
    } else {
      super.assign(y, function)
    }
    this
  }

  /**
   * Assigns the result of a function to each cell;
   * <tt>x[row,col] = function(x[row,col],y[row,col])</tt>, where y is given
   * in the coordinate form with single numerical value.
   *
   * @param rowIndexes
   *            row indexes of y
   * @param columnIndexes
   *            column indexes of y
   * @param value
   *            numerical value of y
   * @param function
   *            a function object taking as first argument the current cell's
   *            value of <tt>this</tt>, and as second argument the current
   *            cell's value of <tt>y</tt>,
   * @return <tt>this</tt> (for convenience only).
   */
  def assign(rowIndexes: Array[Int],
      columnIndexes: Array[Int],
      value: Int,
      function: cern.colt.function.tint.IntIntFunction): SparseIntMatrix2D = {
    val size = rowIndexes.length
    if (function == cern.jet.math.tint.IntFunctions.plus) {
      for (i <- 0 until size) {
        val row = rowIndexes(i)
        val column = columnIndexes(i)
        if (row >= rows || column >= columns) {
          throw new IndexOutOfBoundsException("row: " + row + ", column: " + column)
        }
        val index = rowZero + row * rowStride + columnZero + column * columnStride
        val elem = elements.get(index)
        val sum = elem + value
        if (sum != 0) {
          elements.put(index, sum)
        } else {
          elements.removeKey(index)
        }
      }
    } else {
      for (i <- 0 until size) {
        val row = rowIndexes(i)
        val column = columnIndexes(i)
        if (row >= rows || column >= columns) {
          throw new IndexOutOfBoundsException("row: " + row + ", column: " + column)
        }
        val index = rowZero + row * rowStride + columnZero + column * columnStride
        val elem = elements.get(index)
        val result = function.apply(elem, value)
        if (result != 0) {
          elements.put(index, result)
        } else {
          elements.removeKey(index)
        }
      }
    }
    this
  }

  /**
   * Assigns the result of a function to each cell;
   * <tt>x[row,col] = function(x[row,col],y[row,col])</tt>, where y is given
   * in the coordinate form.
   *
   * @param rowIndexes
   *            row indexes of y
   * @param columnIndexes
   *            column indexes of y
   * @param values
   *            numerical values of y
   * @param function
   *            a function object taking as first argument the current cell's
   *            value of <tt>this</tt>, and as second argument the current
   *            cell's value of <tt>y</tt>,
   * @return <tt>this</tt> (for convenience only).
   */
  def assign(rowIndexes: Array[Int],
      columnIndexes: Array[Int],
      values: Array[Int],
      function: cern.colt.function.tint.IntIntFunction): SparseIntMatrix2D = {
    val size = rowIndexes.length
    if (function == cern.jet.math.tint.IntFunctions.plus) {
      for (i <- 0 until size) {
        var value = values(i)
        val row = rowIndexes(i)
        val column = columnIndexes(i)
        if (row >= rows || column >= columns) {
          throw new IndexOutOfBoundsException("row: " + row + ", column: " + column)
        }
        val index = rowZero + row * rowStride + columnZero + column * columnStride
        val elem = elements.get(index)
        value += elem
        if (value != 0) {
          elements.put(index, value)
        } else {
          elements.removeKey(index)
        }
      }
    } else {
      for (i <- 0 until size) {
        var value = values(i)
        val row = rowIndexes(i)
        val column = columnIndexes(i)
        if (row >= rows || column >= columns) {
          throw new IndexOutOfBoundsException("row: " + row + ", column: " + column)
        }
        val index = rowZero + row * rowStride + columnZero + column * columnStride
        val elem = elements.get(index)
        value = function.apply(elem, value)
        if (value != 0) {
          elements.put(index, value)
        } else {
          elements.removeKey(index)
        }
      }
    }
    this
  }

  def cardinality(): Int = {
    if (this.isNoView) this.elements.size else super.cardinality()
  }

  /**
   * Returns a new matrix that has the same elements as this matrix, but is in
   * a column-compressed form. This method creates a new object (not a view),
   * so changes in the returned matrix are NOT reflected in this matrix.
   *
   * @param sortRowIndexes
   *            if true, then row indexes in column compressed matrix are
   *            sorted
   *
   * @return this matrix in a column-compressed form
   */
  def getColumnCompressed(sortRowIndexes: Boolean): SparseCCIntMatrix2D = {
    val nnz = cardinality()
    val keys = elements.keys.elements()
    val values = elements.values.elements()
    val rowIndexes = Array.ofDim[Int](nnz)
    val columnIndexes = Array.ofDim[Int](nnz)
    for (k <- 0 until nnz) {
      val key = keys(k)
      rowIndexes(k) = (key / columns).toInt
      columnIndexes(k) = (key % columns).toInt
    }
    new SparseCCIntMatrix2D(rows, columns, rowIndexes, columnIndexes, values, false, false, sortRowIndexes)
  }

  /**
   * Returns a new matrix that has the same elements as this matrix, but is in
   * a column-compressed modified form. This method creates a new object (not
   * a view), so changes in the returned matrix are NOT reflected in this
   * matrix.
   *
   * @return this matrix in a column-compressed modified form
   */
  def getColumnCompressedModified(): SparseCCMIntMatrix2D = {
    val A = new SparseCCMIntMatrix2D(rows, columns)
    val nnz = cardinality()
    val keys = elements.keys.elements()
    val values = elements.values.elements()
    for (i <- 0 until nnz) {
      val row = (keys(i) / columns).toInt
      val column = (keys(i) % columns).toInt
      A.setQuick(row, column, values(i))
    }
    A
  }

  /**
   * Returns a new matrix that has the same elements as this matrix, but is in
   * a row-compressed form. This method creates a new object (not a view), so
   * changes in the returned matrix are NOT reflected in this matrix.
   *
   * @param sortColumnIndexes
   *            if true, then column indexes in row compressed matrix are
   *            sorted
   *
   * @return this matrix in a row-compressed form
   */
  def getRowCompressed(sortColumnIndexes: Boolean): SparseRCIntMatrix2D = {
    val nnz = cardinality()
    val keys = elements.keys.elements()
    val values = elements.values.elements()
    val rowIndexes = Array.ofDim[Int](nnz)
    val columnIndexes = Array.ofDim[Int](nnz)
    for (k <- 0 until nnz) {
      val key = keys(k)
      rowIndexes(k) = (key / columns).toInt
      columnIndexes(k) = (key % columns).toInt
    }
    new SparseRCIntMatrix2D(rows, columns, rowIndexes, columnIndexes, values, false, false, sortColumnIndexes)
  }

  /**
   * Returns a new matrix that has the same elements as this matrix, but is in
   * a row-compressed modified form. This method creates a new object (not a
   * view), so changes in the returned matrix are NOT reflected in this
   * matrix.
   *
   * @return this matrix in a row-compressed modified form
   */
  def getRowCompressedModified(): SparseRCMIntMatrix2D = {
    val A = new SparseRCMIntMatrix2D(rows, columns)
    val nnz = cardinality()
    val keys = elements.keys.elements()
    val values = elements.values.elements()
    for (i <- 0 until nnz) {
      val row = (keys(i) / columns).toInt
      val column = (keys(i) % columns).toInt
      A.setQuick(row, column, values(i))
    }
    A
  }

  def elements(): AbstractLongIntMap = elements

  def ensureCapacity(minCapacity: Int) {
    this.elements.ensureCapacity(minCapacity)
  }

  def forEachNonZero(function: cern.colt.function.tint.IntIntIntFunction): IntMatrix2D = {
    if (this.isNoView) {
      this.elements.forEachPair(new cern.colt.function.tlong.LongIntProcedure() {

        def apply(key: Long, value: Int): Boolean = {
          val i = (key / columns).toInt
          val j = (key % columns).toInt
          val r = function.apply(i, j, value)
          if (r != value) elements.put(key, r)
          return true
        }
      })
    } else {
      super.forEachNonZero(function)
    }
    this
  }

  def getQuick(row: Int, column: Int): Int = {
    synchronized {
      this.elements.get(rowZero.toLong + row.toLong * rowStride.toLong + columnZero.toLong +
        column.toLong * columnStride.toLong)
    }
  }

  def index(row: Int, column: Int): Long = {
    rowZero.toLong + row.toLong * rowStride.toLong + columnZero.toLong +
      column.toLong * columnStride.toLong
  }

  def like(rows: Int, columns: Int): IntMatrix2D = new SparseIntMatrix2D(rows, columns)

  def like1D(size: Int): IntMatrix1D = new SparseIntMatrix1D(size)

  def setQuick(row: Int, column: Int, value: Int) {
    synchronized {
      val index = rowZero.toLong + row.toLong * rowStride.toLong + columnZero.toLong +
        column.toLong * columnStride.toLong
      if (value == 0) this.elements.removeKey(index) else this.elements.put(index, value)
    }
  }

  override def toString(): String = {
    val builder = new StringBuilder()
    builder.append(rows).append(" x ").append(columns).append(" sparse matrix, nnz = ")
      .append(cardinality())
      .append('\n')
    for (r <- 0 until rows; c <- 0 until columns) {
      val elem = getQuick(r, c)
      if (elem != 0) {
        builder.append('(').append(r).append(',').append(c)
          .append(')')
          .append('\t')
          .append(elem)
          .append('\n')
      }
    }
    builder.toString
  }

  def trimToSize() {
    this.elements.trimToSize()
  }

  def vectorize(): IntMatrix1D = {
    val v = new SparseIntMatrix1D(size.toInt)
    var idx = 0
    for (c <- 0 until columns; r <- 0 until rows) {
      val elem = getQuick(r, c)
      v.setQuick(idx += 1, elem)
    }
    v
  }

  def zMult(y: IntMatrix1D,
      z: IntMatrix1D,
      alpha: Int,
      beta: Int,
      transposeA: Boolean): IntMatrix1D = {
    var rowsA = rows
    var columnsA = columns
    if (transposeA) {
      rowsA = columns
      columnsA = rows
    }
    val ignore = (z == null)
    if (z == null) z = new DenseIntMatrix1D(rowsA)
    if (!(this.isNoView && y.isInstanceOf[DenseIntMatrix1D] && z.isInstanceOf[DenseIntMatrix1D])) {
      return super.zMult(y, z, alpha, beta, transposeA)
    }
    if (columnsA != y.size || rowsA > z.size) throw new IllegalArgumentException("Incompatible args: " +
      ((if (transposeA) viewDice() else this).toShapeString()) +
      ", " +
      y.toShapeString() +
      ", " +
      z.toShapeString())
    if (!ignore) z.assign(cern.jet.math.tint.IntFunctions.mult(beta))
    val zz = z.asInstanceOf[DenseIntMatrix1D]
    val elementsZ = zz.elements
    val strideZ = zz.stride()
    val zeroZ = z.index(0).toInt
    val yy = y.asInstanceOf[DenseIntMatrix1D]
    val elementsY = yy.elements
    val strideY = yy.stride()
    val zeroY = y.index(0).toInt
    if (elementsY == null || elementsZ == null) throw new InternalError()
    this.elements.forEachPair(new cern.colt.function.tlong.LongIntProcedure() {

      def apply(key: Long, value: Int): Boolean = {
        var i = (key / columns).toInt
        var j = (key % columns).toInt
        if (transposeA) {
          val tmp = i
          i = j
          j = tmp
        }
        elementsZ(zeroZ + strideZ * i) += alpha * value * elementsY(zeroY + strideY * j)
        return true
      }
    })
    z
  }

  def zMult(B: IntMatrix2D,
      C: IntMatrix2D,
      alpha: Int,
      beta: Int,
      transposeA: Boolean,
      transposeB: Boolean): IntMatrix2D = {
    if (!(this.isNoView)) {
      return super.zMult(B, C, alpha, beta, transposeA, transposeB)
    }
    if (transposeB) B = B.viewDice()
    var rowsA = rows
    var columnsA = columns
    if (transposeA) {
      rowsA = columns
      columnsA = rows
    }
    val p = B.columns()
    val ignore = (C == null)
    if (C == null) C = new DenseIntMatrix2D(rowsA, p)
    if (B.rows() != columnsA) throw new IllegalArgumentException("Matrix2D inner dimensions must agree:" + toShapeString() +
      ", " +
      (if (transposeB) B.viewDice() else B).toShapeString())
    if (C.rows() != rowsA || C.columns() != p) throw new IllegalArgumentException("Incompatibel result matrix: " + toShapeString() + ", " +
      (if (transposeB) B.viewDice() else B).toShapeString() +
      ", " +
      C.toShapeString())
    if (this == C || B == C) throw new IllegalArgumentException("Matrices must not be identical")
    if (!ignore) C.assign(cern.jet.math.tint.IntFunctions.mult(beta))
    val Brows = Array.ofDim[IntMatrix1D](columnsA)
    var i = columnsA
    while (i >= 0) Brows(i) = B.viewRow(i)
    val Crows = Array.ofDim[IntMatrix1D](rowsA)
    var i = rowsA
    while (i >= 0) Crows(i) = C.viewRow(i)
    val fun = cern.jet.math.tint.IntPlusMultSecond.plusMult(0)
    this.elements.forEachPair(new cern.colt.function.tlong.LongIntProcedure() {

      def apply(key: Long, value: Int): Boolean = {
        val i = (key / columns).toInt
        val j = (key % columns).toInt
        fun.multiplicator = value * alpha
        if (!transposeA) Crows(i).assign(Brows(j), fun) else Crows(j).assign(Brows(i), fun)
        return true
      }
    })
    C
  }

  private def insert(rowIndexes: Array[Int], columnIndexes: Array[Int], value: Int) {
    val size = rowIndexes.length
    for (i <- 0 until size) {
      val row = rowIndexes(i)
      val column = columnIndexes(i)
      if (row >= rows || column >= columns) {
        throw new IndexOutOfBoundsException("row: " + row + ", column: " + column)
      }
      if (value != 0) {
        val index = rowZero + row * rowStride + columnZero + column * columnStride
        val elem = elements.get(index)
        if (elem == 0) {
          elements.put(index, value)
        } else {
          val sum = elem + value
          if (sum == 0) {
            elements.removeKey(index)
          } else {
            elements.put(index, sum)
          }
        }
      }
    }
  }

  private def insert(rowIndexes: Array[Int], columnIndexes: Array[Int], values: Array[Int]) {
    val size = rowIndexes.length
    for (i <- 0 until size) {
      val value = values(i)
      val row = rowIndexes(i)
      val column = columnIndexes(i)
      if (row >= rows || column >= columns) {
        throw new IndexOutOfBoundsException("row: " + row + ", column: " + column)
      }
      if (value != 0) {
        val index = rowZero + row * rowStride + columnZero + column * columnStride
        val elem = elements.get(index)
        if (elem == 0) {
          elements.put(index, value)
        } else {
          val sum = elem + value
          if (sum == 0) {
            elements.removeKey(index)
          } else {
            elements.put(index, sum)
          }
        }
      }
    }
  }

  protected def haveSharedCellsRaw(other: IntMatrix2D): Boolean = {
    if (other.isInstanceOf[SelectedSparseIntMatrix2D]) {
      val otherMatrix = other.asInstanceOf[SelectedSparseIntMatrix2D]
      return this.elements == otherMatrix.elements
    } else if (other.isInstanceOf[SparseIntMatrix2D]) {
      val otherMatrix = other.asInstanceOf[SparseIntMatrix2D]
      return this.elements == otherMatrix.elements
    }
    false
  }

  protected def like1D(size: Int, offset: Int, stride: Int): IntMatrix1D = {
    new SparseIntMatrix1D(size, this.elements, offset, stride)
  }

  protected def viewSelectionLike(rowOffsets: Array[Int], columnOffsets: Array[Int]): IntMatrix2D = {
    new SelectedSparseIntMatrix2D(this.elements, rowOffsets, columnOffsets, 0)
  }
}
