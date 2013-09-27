package cern.colt.matrix.tint.impl

import java.util.concurrent.Callable
import java.util.concurrent.ExecutionException
import java.util.concurrent.Future
import cern.colt.matrix.tint.IntMatrix1D
import cern.colt.matrix.tint.IntMatrix2D
import edu.emory.mathcs.utils.ConcurrencyUtils
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Diagonal 2-d matrix holding <tt>int</tt> elements. First see the <a
 * href="package-summary.html">package summary</a> and javadoc <a
 * href="package-tree.html">tree view</a> to get the broad picture.
 * <p>
 *
 * @author Piotr Wendykier (piotr.wendykier@gmail.com)
 */
@SerialVersionUID(1L)
class DiagonalIntMatrix2D(rows: Int, columns: Int, dindex: Int) extends WrapperIntMatrix2D(null) {

  protected var elements: Array[Int] = new Array[Int](dlength)

  protected var dlength: Int = _

  protected var dindex: Int = _

  try {
    setUp(rows, columns)
  } catch {
    case exc: IllegalArgumentException => if ("matrix too large" != exc.getMessage) throw exc
  }

  if ((dindex < -rows + 1) || (dindex > columns - 1)) {
    throw new IllegalArgumentException("index is out of bounds")
  } else {
    this.dindex = dindex
  }

  if (dindex == 0) {
    dlength = Math.min(rows, columns)
  } else if (dindex > 0) {
    if (rows >= columns) {
      dlength = columns - dindex
    } else {
      val diff = columns - rows
      dlength = if (dindex <= diff) rows else rows - (dindex - diff)
    }
  } else {
    if (rows >= columns) {
      val diff = rows - columns
      dlength = if (-dindex <= diff) columns else columns + dindex + diff
    } else {
      dlength = rows + dindex
    }
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
  def this(values: Array[Array[Int]], dindex: Int) {
    this(values.length, if (values.length == 0) 0 else values(0).length, dindex)
    assign(values)
  }

  def assign(function: cern.colt.function.tint.IntFunction): IntMatrix2D = {
    if (function.isInstanceOf[cern.jet.math.tint.IntMult]) {
      val alpha = function.asInstanceOf[cern.jet.math.tint.IntMult].multiplicator
      if (alpha == 1) return this
      if (alpha == 0) return assign(0)
      if (alpha != alpha) return assign(alpha)
      var j = dlength
      while (j >= 0) {
        elements(j) *= alpha
      }
    } else {
      var j = dlength
      while (j >= 0) {
        elements(j) = function.apply(elements(j))
      }
    }
    this
  }

  def assign(value: Int): IntMatrix2D = {
    var i = dlength
    while (i >= 0) elements(i) = value
    this
  }

  def assign(values: Array[Int]): IntMatrix2D = {
    if (values.length != dlength) throw new IllegalArgumentException("Must have same length: length=" + values.length + " dlength=" +
      dlength)
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (dlength >= ConcurrencyUtils.getThreadsBeginN_2D)) {
      nthreads = Math.min(nthreads, dlength)
      val futures = Array.ofDim[Future](nthreads)
      val k = dlength / nthreads
      for (j <- 0 until nthreads) {
        val firstRow = j * k
        val lastRow = if ((j == nthreads - 1)) dlength else firstRow + k
        futures(j) = ConcurrencyUtils.submit(new Runnable() {

          def run() {
            for (r <- firstRow until lastRow) {
              elements(r) = values(r)
            }
          }
        })
      }
      ConcurrencyUtils.waitForCompletion(futures)
    } else {
      var r = dlength
      while (r >= 0) {
        elements(r) = values(r)
      }
    }
    this
  }

  def assign(values: Array[Array[Int]]): IntMatrix2D = {
    if (values.length != rows) throw new IllegalArgumentException("Must have same number of rows: rows=" + values.length +
      "rows()=" +
      rows())
    var r: Int = 0
    var c: Int = 0
    if (dindex >= 0) {
      r = 0
      c = dindex
    } else {
      r = -dindex
      c = 0
    }
    for (i <- 0 until dlength) {
      if (values(i).length != columns) {
        throw new IllegalArgumentException("Must have same number of columns in every row: columns=" +
          values(r).length +
          "columns()=" +
          columns())
      }
      elements(i) = values(r += 1)(c += 1)
    }
    this
  }

  def assign(source: IntMatrix2D): IntMatrix2D = {
    if (source == this) return this
    checkShape(source)
    if (source.isInstanceOf[DiagonalIntMatrix2D]) {
      val other = source.asInstanceOf[DiagonalIntMatrix2D]
      if ((dindex != other.dindex) || (dlength != other.dlength)) {
        throw new IllegalArgumentException("source is DiagonalIntMatrix2D with different diagonal stored.")
      }
      System.arraycopy(other.elements, 0, this.elements, 0, this.elements.length)
      this
    } else {
      super.assign(source)
    }
  }

  def assign(y: IntMatrix2D, function: cern.colt.function.tint.IntIntFunction): IntMatrix2D = {
    checkShape(y)
    if (y.isInstanceOf[DiagonalIntMatrix2D]) {
      val other = y.asInstanceOf[DiagonalIntMatrix2D]
      if ((dindex != other.dindex) || (dlength != other.dlength)) {
        throw new IllegalArgumentException("y is DiagonalIntMatrix2D with different diagonal stored.")
      }
      if (function.isInstanceOf[cern.jet.math.tint.IntPlusMultSecond]) {
        val alpha = function.asInstanceOf[cern.jet.math.tint.IntPlusMultSecond].multiplicator
        if (alpha == 0) {
          return this
        }
      }
      val otherElements = other.elements
      var nthreads = ConcurrencyUtils.getNumberOfThreads
      if ((nthreads > 1) && (dlength >= ConcurrencyUtils.getThreadsBeginN_2D)) {
        nthreads = Math.min(nthreads, dlength)
        val futures = Array.ofDim[Future](nthreads)
        val k = dlength / nthreads
        for (j <- 0 until nthreads) {
          val firstRow = j * k
          val lastRow = if ((j == nthreads - 1)) dlength else firstRow + k
          futures(j) = ConcurrencyUtils.submit(new Runnable() {

            def run() {
              if (function.isInstanceOf[cern.jet.math.tint.IntPlusMultSecond]) {
                val alpha = function.asInstanceOf[cern.jet.math.tint.IntPlusMultSecond].multiplicator
                if (alpha == 1) {
                  for (j <- firstRow until lastRow) {
                    elements(j) += otherElements(j)
                  }
                } else {
                  for (j <- firstRow until lastRow) {
                    elements(j) = elements(j) + alpha * otherElements(j)
                  }
                }
              } else if (function == cern.jet.math.tint.IntFunctions.mult) {
                for (j <- firstRow until lastRow) {
                  elements(j) = elements(j) * otherElements(j)
                }
              } else if (function == cern.jet.math.tint.IntFunctions.div) {
                for (j <- firstRow until lastRow) {
                  elements(j) = elements(j) / otherElements(j)
                }
              } else {
                for (j <- firstRow until lastRow) {
                  elements(j) = function.apply(elements(j), otherElements(j))
                }
              }
            }
          })
        }
        ConcurrencyUtils.waitForCompletion(futures)
      } else {
        if (function.isInstanceOf[cern.jet.math.tint.IntPlusMultSecond]) {
          val alpha = function.asInstanceOf[cern.jet.math.tint.IntPlusMultSecond].multiplicator
          if (alpha == 1) {
            var j = dlength
            while (j >= 0) {
              elements(j) += otherElements(j)
            }
          } else {
            var j = dlength
            while (j >= 0) {
              elements(j) = elements(j) + alpha * otherElements(j)
            }
          }
        } else if (function == cern.jet.math.tint.IntFunctions.mult) {
          var j = dlength
          while (j >= 0) {
            elements(j) = elements(j) * otherElements(j)
          }
        } else if (function == cern.jet.math.tint.IntFunctions.div) {
          var j = dlength
          while (j >= 0) {
            elements(j) = elements(j) / otherElements(j)
          }
        } else {
          var j = dlength
          while (j >= 0) {
            elements(j) = function.apply(elements(j), otherElements(j))
          }
        }
      }
      this
    } else {
      super.assign(y, function)
    }
  }

  def cardinality(): Int = {
    var cardinality = 0
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (dlength >= ConcurrencyUtils.getThreadsBeginN_2D)) {
      nthreads = Math.min(nthreads, dlength)
      val futures = Array.ofDim[Future](nthreads)
      val results = Array.ofDim[Integer](nthreads)
      val k = dlength / nthreads
      for (j <- 0 until nthreads) {
        val firstRow = j * k
        val lastRow = if ((j == nthreads - 1)) dlength else firstRow + k
        futures(j) = ConcurrencyUtils.submit(new Callable[Integer]() {

          def call(): java.lang.Integer = {
            var cardinality = 0
            for (r <- firstRow until lastRow if elements(r) != 0) cardinality += 1
            return cardinality
          }
        })
      }
      try {
        for (j <- 0 until nthreads) {
          results(j) = futures(j).get.asInstanceOf[java.lang.Integer]
        }
        cardinality = results(0)
        for (j <- 1 until nthreads) {
          cardinality += results(j)
        }
      } catch {
        case ex: ExecutionException => ex.printStackTrace()
        case e: InterruptedException => e.printStackTrace()
      }
    } else {
      for (r <- 0 until dlength if elements(r) != 0) cardinality += 1
    }
    cardinality
  }

  def elements(): Array[Int] = elements

  override def equals(value: Int): Boolean = {
    for (r <- 0 until dlength) {
      val x = elements(r)
      val diff = value - x
      if (diff != 0) {
        return false
      }
    }
    true
  }

  override def equals(obj: Any): Boolean = {
    if (obj.isInstanceOf[DiagonalIntMatrix2D]) {
      val other = obj.asInstanceOf[DiagonalIntMatrix2D]
      if (this == obj) return true
      if (!(this != null && obj != null)) return false
      val rows = this.rows()
      val columns = this.columns()
      if (columns != other.columns() || rows != other.rows()) return false
      if ((dindex != other.dindex) || (dlength != other.dlength)) {
        return false
      }
      val otherElements = other.elements
      for (r <- 0 until dlength) {
        val x = elements(r)
        val value = otherElements(r)
        val diff = value - x
        if (diff != 0) {
          return false
        }
      }
      true
    } else {
      super == obj
    }
  }

  def forEachNonZero(function: cern.colt.function.tint.IntIntIntFunction): IntMatrix2D = {
    var j = dlength
    while (j >= 0) {
      val value = elements(j)
      if (value != 0) {
        elements(j) = function.apply(j, j, value)
      }
    }
    this
  }

  /**
   * Returns the length of the diagonal
   *
   * @return the length of the diagonal
   */
  def diagonalLength(): Int = dlength

  /**
   * Returns the index of the diagonal
   *
   * @return the index of the diagonal
   */
  def diagonalIndex(): Int = dindex

  def getMaxLocation(): Array[Int] = {
    var location = 0
    var maxValue = 0
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (dlength >= ConcurrencyUtils.getThreadsBeginN_2D)) {
      nthreads = Math.min(nthreads, dlength)
      val futures = Array.ofDim[Future](nthreads)
      val results = Array.ofDim[Int](nthreads, 2)
      val k = dlength / nthreads
      for (j <- 0 until nthreads) {
        val firstRow = j * k
        val lastRow = if ((j == nthreads - 1)) dlength else firstRow + k
        futures(j) = ConcurrencyUtils.submit(new Callable[Array[Int]]() {

          def call(): Array[Int] = {
            var location = firstRow
            var maxValue = elements(location)
            var elem: Int = 0
            for (r <- firstRow + 1 until lastRow) {
              elem = elements(r)
              if (maxValue < elem) {
                maxValue = elem
                location = r
              }
            }
            return Array(maxValue, location, location)
          }
        })
      }
      try {
        for (j <- 0 until nthreads) {
          results(j) = futures(j).get.asInstanceOf[Array[Int]]
        }
        maxValue = results(0)(0)
        location = results(0)(1).toInt
        for (j <- 1 until nthreads if maxValue < results(j)(0)) {
          maxValue = results(j)(0)
          location = results(j)(1).toInt
        }
      } catch {
        case ex: ExecutionException => ex.printStackTrace()
        case e: InterruptedException => e.printStackTrace()
      }
    } else {
      maxValue = elements(0)
      var elem: Int = 0
      for (r <- 1 until dlength) {
        elem = elements(r)
        if (maxValue < elem) {
          maxValue = elem
          location = r
        }
      }
    }
    var rowLocation: Int = 0
    var columnLocation: Int = 0
    if (dindex > 0) {
      rowLocation = location
      columnLocation = location + dindex
    } else if (dindex < 0) {
      rowLocation = location - dindex
      columnLocation = location
    } else {
      rowLocation = location
      columnLocation = location
    }
    Array(maxValue, rowLocation, columnLocation)
  }

  def getMinLocation(): Array[Int] = {
    var location = 0
    var minValue = 0
    var nthreads = ConcurrencyUtils.getNumberOfThreads
    if ((nthreads > 1) && (dlength >= ConcurrencyUtils.getThreadsBeginN_2D)) {
      nthreads = Math.min(nthreads, dlength)
      val futures = Array.ofDim[Future](nthreads)
      val results = Array.ofDim[Int](nthreads, 2)
      val k = dlength / nthreads
      for (j <- 0 until nthreads) {
        val firstRow = j * k
        val lastRow = if ((j == nthreads - 1)) dlength else firstRow + k
        futures(j) = ConcurrencyUtils.submit(new Callable[Array[Int]]() {

          def call(): Array[Int] = {
            var location = firstRow
            var minValue = elements(location)
            var elem: Int = 0
            for (r <- firstRow + 1 until lastRow) {
              elem = elements(r)
              if (minValue > elem) {
                minValue = elem
                location = r
              }
            }
            return Array(minValue, location, location)
          }
        })
      }
      try {
        for (j <- 0 until nthreads) {
          results(j) = futures(j).get.asInstanceOf[Array[Int]]
        }
        minValue = results(0)(0)
        location = results(0)(1).toInt
        for (j <- 1 until nthreads if minValue > results(j)(0)) {
          minValue = results(j)(0)
          location = results(j)(1).toInt
        }
      } catch {
        case ex: ExecutionException => ex.printStackTrace()
        case e: InterruptedException => e.printStackTrace()
      }
    } else {
      minValue = elements(0)
      var elem: Int = 0
      for (r <- 1 until dlength) {
        elem = elements(r)
        if (minValue > elem) {
          minValue = elem
          location = r
        }
      }
    }
    var rowLocation: Int = 0
    var columnLocation: Int = 0
    if (dindex > 0) {
      rowLocation = location
      columnLocation = location + dindex
    } else if (dindex < 0) {
      rowLocation = location - dindex
      columnLocation = location
    } else {
      rowLocation = location
      columnLocation = location
    }
    Array(minValue, rowLocation, columnLocation)
  }

  def getQuick(row: Int, column: Int): Int = {
    if (dindex >= 0) {
      if (column < dindex) {
        0
      } else {
        if ((row < dlength) && (row + dindex == column)) {
          elements(row)
        } else {
          0
        }
      }
    } else {
      if (row < -dindex) {
        0
      } else {
        if ((column < dlength) && (row + dindex == column)) {
          elements(column)
        } else {
          0
        }
      }
    }
  }

  def like(rows: Int, columns: Int): IntMatrix2D = new SparseIntMatrix2D(rows, columns)

  def like1D(size: Int): IntMatrix1D = new SparseIntMatrix1D(size)

  def setQuick(row: Int, column: Int, value: Int) {
    if (dindex >= 0) {
      if (column < dindex) {
      } else {
        if ((row < dlength) && (row + dindex == column)) {
          elements(row) = value
        } else {
        }
      }
    } else {
      if (row < -dindex) {
      } else {
        if ((column < dlength) && (row + dindex == column)) {
          elements(column) = value
        } else {
        }
      }
    }
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
    if ((!ignore) && ((beta) != 1)) z.assign(cern.jet.math.tint.IntFunctions.mult(beta))
    val zz = z.asInstanceOf[DenseIntMatrix1D]
    val elementsZ = zz.elements
    val strideZ = zz.stride()
    val zeroZ = z.index(0).toInt
    val yy = y.asInstanceOf[DenseIntMatrix1D]
    val elementsY = yy.elements
    val strideY = yy.stride()
    val zeroY = y.index(0).toInt
    if (elementsY == null || elementsZ == null) throw new InternalError()
    if (!transposeA) {
      if (dindex >= 0) {
        var i = dlength
        while (i >= 0) {
          elementsZ(zeroZ + strideZ * i) += alpha * elements(i) * elementsY(dindex + zeroY + strideY * i)
        }
      } else {
        var i = dlength
        while (i >= 0) {
          elementsZ(-dindex + zeroZ + strideZ * i) += alpha * elements(i) * elementsY(zeroY + strideY * i)
        }
      }
    } else {
      if (dindex >= 0) {
        var i = dlength
        while (i >= 0) {
          elementsZ(dindex + zeroZ + strideZ * i) += alpha * elements(i) * elementsY(zeroY + strideY * i)
        }
      } else {
        var i = dlength
        while (i >= 0) {
          elementsZ(zeroZ + strideZ * i) += alpha * elements(i) * elementsY(-dindex + zeroY + strideY * i)
        }
      }
    }
    z
  }

  protected def getStorageMatrix(): IntMatrix2D = this
}
