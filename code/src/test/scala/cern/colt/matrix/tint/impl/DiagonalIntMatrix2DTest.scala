package cern.colt.matrix.tint.impl

import cern.colt.list.tint.IntArrayList
import cern.colt.matrix.tint.IntMatrix1D
import cern.colt.matrix.tint.IntMatrix1DProcedure
import cern.colt.matrix.tint.IntMatrix2D
import cern.colt.matrix.tint.IntMatrix2DTest
import cern.jet.math.tint.IntFunctions
import edu.emory.mathcs.utils.ConcurrencyUtils
//remove if not needed
import scala.collection.JavaConversions._

class DiagonalIntMatrix2DTest(arg0: String) extends IntMatrix2DTest(arg0) {

  protected var DLENGTH: Int = _

  protected var DINDEX: Int = _

  protected def createMatrices() {
    DINDEX = 3
    A = new DiagonalIntMatrix2D(NROWS, NCOLUMNS, DINDEX)
    B = new DiagonalIntMatrix2D(NROWS, NCOLUMNS, DINDEX)
    Bt = new DiagonalIntMatrix2D(NCOLUMNS, NROWS, -DINDEX)
    DLENGTH = A.asInstanceOf[DiagonalIntMatrix2D].diagonalLength()
  }

  protected def populateMatrices() {
    ConcurrencyUtils.setThreadsBeginN_2D(1)
    if (DINDEX >= 0) {
      for (r <- 0 until DLENGTH) {
        A.setQuick(r, r + DINDEX, Math.max(1, rand.nextInt() % A.rows()))
      }
      for (r <- 0 until DLENGTH) {
        B.setQuick(r, r + DINDEX, Math.max(1, rand.nextInt() % A.rows()))
      }
      for (r <- 0 until DLENGTH) {
        Bt.setQuick(r - DINDEX, r, Math.max(1, rand.nextInt() % A.rows()))
      }
    } else {
      for (r <- 0 until DLENGTH) {
        A.setQuick(r - DINDEX, r, Math.max(1, rand.nextInt() % A.rows()))
      }
      for (r <- 0 until DLENGTH) {
        B.setQuick(r - DINDEX, r, Math.max(1, rand.nextInt() % A.rows()))
      }
      for (r <- 0 until DLENGTH) {
        Bt.setQuick(r, r + DINDEX, Math.max(1, rand.nextInt() % A.rows()))
      }
    }
  }

  def testAssignInt() {
    val value = Math.max(1, rand.nextInt() % A.rows())
    A.assign(value)
    if (DINDEX >= 0) {
      for (r <- 0 until DLENGTH) {
        assertEquals(value, A.getQuick(r, r + DINDEX))
      }
    } else {
      for (r <- 0 until DLENGTH) {
        assertEquals(value, A.getQuick(r - DINDEX, r))
      }
    }
  }

  def testAssignIntArrayArray() {
    val expected = Array.ofDim[Int](NROWS, NCOLUMNS)
    for (r <- 0 until NROWS; c <- 0 until NCOLUMNS) {
      expected(r)(c) = Math.max(1, rand.nextInt() % A.rows())
    }
    A.assign(expected)
    if (DINDEX >= 0) {
      for (r <- 0 until DLENGTH) {
        assertEquals(expected(r)(r + DINDEX), A.getQuick(r, r + DINDEX))
      }
    } else {
      for (r <- 0 until DLENGTH) {
        assertEquals(expected(r - DINDEX)(r), A.getQuick(r - DINDEX, r))
      }
    }
  }

  def testAssignIntFunction() {
    val Acopy = A.copy()
    A.assign(IntFunctions.neg)
    if (DINDEX >= 0) {
      for (r <- 0 until DLENGTH) {
        val expected = -Acopy.getQuick(r, r + DINDEX)
        assertEquals(expected, A.getQuick(r, r + DINDEX))
      }
    } else {
      for (r <- 0 until DLENGTH) {
        val expected = -Acopy.getQuick(r - DINDEX, r)
        assertEquals(expected, A.getQuick(r - DINDEX, r))
      }
    }
  }

  def testAssignIntMatrix2DIntIntFunction() {
    val Acopy = A.copy()
    A.assign(B, IntFunctions.div)
    if (DINDEX >= 0) {
      for (r <- 0 until DLENGTH) {
        assertEquals(Acopy.getQuick(r, r + DINDEX) / B.getQuick(r, r + DINDEX), A.getQuick(r, r + DINDEX))
      }
    } else {
      for (r <- 0 until DLENGTH) {
        assertEquals(Acopy.getQuick(r - DINDEX, r) / B.getQuick(r - DINDEX, r), A.getQuick(r - DINDEX, 
          r))
      }
    }
  }

  def testAssignIntMatrix2DIntIntFunctionIntArrayListIntArrayList() {
    val rowList = new IntArrayList()
    val columnList = new IntArrayList()
    if (DINDEX >= 0) {
      for (r <- 0 until DLENGTH) {
        rowList.add(r)
        columnList.add(r + DINDEX)
      }
      val Acopy = A.copy()
      A.assign(B, IntFunctions.div, rowList, columnList)
      for (r <- 0 until DLENGTH) {
        assertEquals(Acopy.getQuick(r, r + DINDEX) / B.getQuick(r, r + DINDEX), A.getQuick(r, r + DINDEX))
      }
    } else {
      for (r <- 0 until DLENGTH) {
        rowList.add(r - DINDEX)
        columnList.add(r)
      }
      val Acopy = A.copy()
      A.assign(B, IntFunctions.div, rowList, columnList)
      for (r <- 0 until DLENGTH) {
        assertEquals(Acopy.getQuick(r - DINDEX, r) / B.getQuick(r - DINDEX, r), A.getQuick(r - DINDEX, 
          r))
      }
    }
  }

  def testCardinality() {
    val card = A.cardinality()
    assertEquals(DLENGTH, card)
  }

  def testMaxLocation() {
    A.assign(0)
    if (DINDEX >= 0) {
      A.setQuick(NROWS / 3, NROWS / 3 + DINDEX, 7)
      A.setQuick(NROWS / 2, NROWS / 2 + DINDEX, 1)
      val maxAndLoc = A.getMaxLocation
      assertEquals(7, maxAndLoc(0))
      assertEquals(NROWS / 3, maxAndLoc(1).toInt)
      assertEquals(NROWS / 3 + DINDEX, maxAndLoc(2).toInt)
    } else {
      A.setQuick(NROWS / 3 - DINDEX, NROWS / 3, 7)
      A.setQuick(NROWS / 2 - DINDEX, NROWS / 2, 1)
      val maxAndLoc = A.getMaxLocation
      assertEquals(7, maxAndLoc(0))
      assertEquals(NROWS / 3 - DINDEX, maxAndLoc(1).toInt)
      assertEquals(NROWS / 3, maxAndLoc(2).toInt)
    }
  }

  def testMinLocation() {
    A.assign(0)
    if (DINDEX >= 0) {
      A.setQuick(NROWS / 3, NROWS / 3 + DINDEX, -7)
      A.setQuick(NROWS / 2, NROWS / 2 + DINDEX, -1)
      val minAndLoc = A.getMinLocation
      assertEquals(-7, minAndLoc(0))
      assertEquals(NROWS / 3, minAndLoc(1).toInt)
      assertEquals(NROWS / 3 + DINDEX, minAndLoc(2).toInt)
    } else {
      A.setQuick(NROWS / 3 - DINDEX, NROWS / 3, -7)
      A.setQuick(NROWS / 2 - DINDEX, NROWS / 2, -1)
      val minAndLoc = A.getMinLocation
      assertEquals(-7, minAndLoc(0))
      assertEquals(NROWS / 3 - DINDEX, minAndLoc(1).toInt)
      assertEquals(NROWS / 3, minAndLoc(2).toInt)
    }
  }

  def testGetNegativeValues() {
    A.assign(0)
    if (DINDEX >= 0) {
      A.setQuick(NROWS / 3, NROWS / 3 + DINDEX, -7)
      A.setQuick(NROWS / 2, NROWS / 2 + DINDEX, -1)
      val rowList = new IntArrayList()
      val columnList = new IntArrayList()
      val valueList = new IntArrayList()
      A.getNegativeValues(rowList, columnList, valueList)
      assertEquals(2, rowList.size)
      assertEquals(2, columnList.size)
      assertEquals(2, valueList.size)
      assertTrue(rowList.contains(NROWS / 3))
      assertTrue(rowList.contains(NROWS / 2))
      assertTrue(columnList.contains(NROWS / 3 + DINDEX))
      assertTrue(columnList.contains(NROWS / 2 + DINDEX))
      assertTrue(valueList.contains(-7))
      assertTrue(valueList.contains(-1))
    } else {
      A.setQuick(NROWS / 3 - DINDEX, NROWS / 3, -7)
      A.setQuick(NROWS / 2 - DINDEX, NROWS / 2, -1)
      val rowList = new IntArrayList()
      val columnList = new IntArrayList()
      val valueList = new IntArrayList()
      A.getNegativeValues(rowList, columnList, valueList)
      assertEquals(2, rowList.size)
      assertEquals(2, columnList.size)
      assertEquals(2, valueList.size)
      assertTrue(rowList.contains(NROWS / 3 - DINDEX))
      assertTrue(rowList.contains(NROWS / 2 - DINDEX))
      assertTrue(columnList.contains(NROWS / 3))
      assertTrue(columnList.contains(NROWS / 2))
      assertTrue(valueList.contains(-7))
      assertTrue(valueList.contains(-1))
    }
  }

  def testGetNonZeros() {
    A.assign(0)
    if (DINDEX >= 0) {
      A.setQuick(NROWS / 3, NROWS / 3 + DINDEX, 7)
      A.setQuick(NROWS / 2, NROWS / 2 + DINDEX, 1)
      val rowList = new IntArrayList()
      val columnList = new IntArrayList()
      val valueList = new IntArrayList()
      A.getNonZeros(rowList, columnList, valueList)
      assertEquals(2, rowList.size)
      assertEquals(2, columnList.size)
      assertEquals(2, valueList.size)
      assertTrue(rowList.contains(NROWS / 3))
      assertTrue(rowList.contains(NROWS / 2))
      assertTrue(columnList.contains(NROWS / 3 + DINDEX))
      assertTrue(columnList.contains(NROWS / 2 + DINDEX))
      assertTrue(valueList.contains(7))
      assertTrue(valueList.contains(1))
    } else {
      A.setQuick(NROWS / 3 - DINDEX, NROWS / 3, 7)
      A.setQuick(NROWS / 2 - DINDEX, NROWS / 2, 1)
      val rowList = new IntArrayList()
      val columnList = new IntArrayList()
      val valueList = new IntArrayList()
      A.getNonZeros(rowList, columnList, valueList)
      assertEquals(2, rowList.size)
      assertEquals(2, columnList.size)
      assertEquals(2, valueList.size)
      assertTrue(rowList.contains(NROWS / 3 - DINDEX))
      assertTrue(rowList.contains(NROWS / 2 - DINDEX))
      assertTrue(columnList.contains(NROWS / 3))
      assertTrue(columnList.contains(NROWS / 2))
      assertTrue(valueList.contains(7))
      assertTrue(valueList.contains(1))
    }
  }

  def testGetPositiveValues() {
    A.assign(0)
    if (DINDEX >= 0) {
      A.setQuick(NROWS / 3, NROWS / 3 + DINDEX, 7)
      A.setQuick(NROWS / 2, NROWS / 2 + DINDEX, 1)
      val rowList = new IntArrayList()
      val columnList = new IntArrayList()
      val valueList = new IntArrayList()
      A.getPositiveValues(rowList, columnList, valueList)
      assertEquals(2, rowList.size)
      assertEquals(2, columnList.size)
      assertEquals(2, valueList.size)
      assertTrue(rowList.contains(NROWS / 3))
      assertTrue(rowList.contains(NROWS / 2))
      assertTrue(columnList.contains(NROWS / 3 + DINDEX))
      assertTrue(columnList.contains(NROWS / 2 + DINDEX))
      assertTrue(valueList.contains(7))
      assertTrue(valueList.contains(1))
    } else {
      A.setQuick(NROWS / 3 - DINDEX, NROWS / 3, 7)
      A.setQuick(NROWS / 2 - DINDEX, NROWS / 2, 1)
      val rowList = new IntArrayList()
      val columnList = new IntArrayList()
      val valueList = new IntArrayList()
      A.getPositiveValues(rowList, columnList, valueList)
      assertEquals(2, rowList.size)
      assertEquals(2, columnList.size)
      assertEquals(2, valueList.size)
      assertTrue(rowList.contains(NROWS / 3 - DINDEX))
      assertTrue(rowList.contains(NROWS / 2 - DINDEX))
      assertTrue(columnList.contains(NROWS / 3))
      assertTrue(columnList.contains(NROWS / 2))
      assertTrue(valueList.contains(7))
      assertTrue(valueList.contains(1))
    }
  }

  def testToArray() {
    val array = A.toArray()
    assertTrue(NROWS == array.length)
    for (r <- 0 until NROWS) {
      assertTrue(NCOLUMNS == array(r).length)
      for (c <- 0 until NCOLUMNS) {
        assertEquals(array(r)(c), A.getQuick(r, c))
      }
    }
  }

  def testVectorize() {
    val Avec = A.vectorize()
    var idx = 0
    for (c <- 0 until NCOLUMNS; r <- 0 until NROWS) {
      assertEquals(A.getQuick(r, c), Avec.getQuick(idx += 1))
    }
  }

  def testViewColumn() {
    val col = A.viewColumn(NCOLUMNS / 2)
    assertEquals(NROWS, col.size)
    for (r <- 0 until NROWS) {
      assertEquals(A.getQuick(r, NCOLUMNS / 2), col.getQuick(r))
    }
  }

  def testViewColumnFlip() {
    val B = A.viewColumnFlip()
    assertEquals(A.size, B.size)
    for (r <- 0 until NROWS; c <- 0 until NCOLUMNS) {
      assertEquals(A.getQuick(r, NCOLUMNS - 1 - c), B.getQuick(r, c))
    }
  }

  def testViewDice() {
    val B = A.viewDice()
    assertEquals(NROWS, B.columns())
    assertEquals(NCOLUMNS, B.rows())
    for (r <- 0 until NROWS; c <- 0 until NCOLUMNS) {
      assertEquals(A.getQuick(r, c), B.getQuick(c, r))
    }
  }

  def testViewPart() {
    val B = A.viewPart(NROWS / 2, NCOLUMNS / 2, NROWS / 3, NCOLUMNS / 3)
    assertEquals(NROWS / 3, B.rows())
    assertEquals(NCOLUMNS / 3, B.columns())
    for (r <- 0 until NROWS / 3; c <- 0 until NCOLUMNS / 3) {
      assertEquals(A.getQuick(NROWS / 2 + r, NCOLUMNS / 2 + c), B.getQuick(r, c))
    }
  }

  def testViewRow() {
    val B = A.viewRow(NROWS / 2)
    assertEquals(NCOLUMNS, B.size)
    for (r <- 0 until NCOLUMNS) {
      assertEquals(A.getQuick(NROWS / 2, r), B.getQuick(r))
    }
  }

  def testViewRowFlip() {
    val B = A.viewRowFlip()
    assertEquals(A.size, B.size)
    for (r <- 0 until NROWS; c <- 0 until NCOLUMNS) {
      assertEquals(A.getQuick(NROWS - 1 - r, c), B.getQuick(r, c))
    }
  }

  def testViewSelectionIntMatrix1DProcedure() {
    val value = 2
    A.assign(0)
    if (DINDEX >= 0) {
      A.setQuick(NROWS / 4, NROWS / 4 + DINDEX, value)
      A.setQuick(NROWS / 2, NROWS / 2 + DINDEX, value)
      val B = A.viewSelection(new IntMatrix1DProcedure() {

        def apply(element: IntMatrix1D): Boolean = {
          if (element.getQuick(NROWS / 4 + DINDEX) == value) {
            return true
          } else {
            return false
          }
        }
      })
      assertEquals(1, B.rows())
      assertEquals(NCOLUMNS, B.columns())
      assertEquals(A.getQuick(NROWS / 4, NROWS / 4 + DINDEX), B.getQuick(0, NROWS / 4 + DINDEX))
    } else {
      A.setQuick(NROWS / 4 - DINDEX, NROWS / 4, value)
      A.setQuick(NROWS / 2 - DINDEX, NROWS / 2, value)
      val B = A.viewSelection(new IntMatrix1DProcedure() {

        def apply(element: IntMatrix1D): Boolean = {
          if (element.getQuick(NROWS / 4) == value) {
            return true
          } else {
            return false
          }
        }
      })
      assertEquals(1, B.rows())
      assertEquals(NCOLUMNS, B.columns())
      assertEquals(A.getQuick(NROWS / 4 - DINDEX, NROWS / 4), B.getQuick(0, NROWS / 4))
    }
  }

  def testViewSelectionIntArrayIntArray() {
    val rowIndexes = Array(NROWS / 6, NROWS / 5, NROWS / 4, NROWS / 3, NROWS / 2)
    val colIndexes = Array(NROWS / 6, NROWS / 5, NROWS / 4, NROWS / 3, NROWS / 2, NROWS - 1)
    val B = A.viewSelection(rowIndexes, colIndexes)
    assertEquals(rowIndexes.length, B.rows())
    assertEquals(colIndexes.length, B.columns())
    for (r <- 0 until rowIndexes.length; c <- 0 until colIndexes.length) {
      assertEquals(A.getQuick(rowIndexes(r), colIndexes(c)), B.getQuick(r, c))
    }
  }

  def testViewSorted() {
    val B = A.viewSorted(1)
    for (r <- 0 until NROWS - 1) {
      assertTrue(B.getQuick(r + 1, 1) >= B.getQuick(r, 1))
    }
  }

  def testViewStrides() {
    val rowStride = 3
    val colStride = 5
    val B = A.viewStrides(rowStride, colStride)
    for (r <- 0 until B.rows(); c <- 0 until B.columns()) {
      assertEquals(A.getQuick(r * rowStride, c * colStride), B.getQuick(r, c))
    }
  }

  def testZMultIntMatrix2DIntMatrix2DIntIntBooleanBoolean() {
    val alpha = 3
    val beta = 5
    var C = new DiagonalIntMatrix2D(NROWS, NROWS, 0)
    for (i <- 0 until DLENGTH) {
      C.setQuick(i, i, Math.max(1, rand.nextInt() % A.rows()))
    }
    var expected = C.toArray()
    C = A.zMult(Bt, C, alpha, beta, false, false)
    for (j <- 0 until NROWS; i <- 0 until NROWS) {
      var s = 0
      for (k <- 0 until NCOLUMNS) {
        s += A.getQuick(i, k) * Bt.getQuick(k, j)
      }
      expected(i)(j) = s * alpha + expected(i)(j) * beta
    }
    for (r <- 0 until NROWS; c <- 0 until NROWS) {
      assertEquals(expected(r)(c), C.getQuick(r, c))
    }
    C = null
    C = A.zMult(Bt, C, alpha, beta, false, false)
    expected = Array.ofDim[Int](NROWS, NROWS)
    for (j <- 0 until NROWS; i <- 0 until NROWS) {
      var s = 0
      for (k <- 0 until NCOLUMNS) {
        s += A.getQuick(i, k) * Bt.getQuick(k, j)
      }
      expected(i)(j) = s * alpha
    }
    for (r <- 0 until NROWS; c <- 0 until NROWS) {
      assertEquals(expected(r)(c), C.getQuick(r, c))
    }
    C = new DiagonalIntMatrix2D(NCOLUMNS, NCOLUMNS, 0)
    for (i <- 0 until DLENGTH) {
      C.setQuick(i, i, Math.max(1, rand.nextInt() % A.rows()))
    }
    expected = C.toArray()
    C = A.zMult(B, C, alpha, beta, true, false)
    for (j <- 0 until NCOLUMNS; i <- 0 until NCOLUMNS) {
      var s = 0
      for (k <- 0 until NROWS) {
        s += A.getQuick(k, i) * B.getQuick(k, j)
      }
      expected(i)(j) = s * alpha + expected(i)(j) * beta
    }
    for (r <- 0 until NCOLUMNS; c <- 0 until NCOLUMNS) {
      assertEquals(expected(r)(c), C.getQuick(r, c))
    }
    C = null
    C = A.zMult(B, C, alpha, beta, true, false)
    expected = Array.ofDim[Int](NCOLUMNS, NCOLUMNS)
    for (j <- 0 until NCOLUMNS; i <- 0 until NCOLUMNS) {
      var s = 0
      for (k <- 0 until NROWS) {
        s += A.getQuick(k, i) * B.getQuick(k, j)
      }
      expected(i)(j) = s * alpha
    }
    for (r <- 0 until NCOLUMNS; c <- 0 until NCOLUMNS) {
      assertEquals(expected(r)(c), C.getQuick(r, c))
    }
    C = new DiagonalIntMatrix2D(NROWS, NROWS, 0)
    for (i <- 0 until DLENGTH) {
      C.setQuick(i, i, Math.max(1, rand.nextInt() % A.rows()))
    }
    expected = C.toArray()
    C = A.zMult(B, C, alpha, beta, false, true)
    for (j <- 0 until NROWS; i <- 0 until NROWS) {
      var s = 0
      for (k <- 0 until NCOLUMNS) {
        s += A.getQuick(i, k) * B.getQuick(j, k)
      }
      expected(i)(j) = s * alpha + expected(i)(j) * beta
    }
    for (r <- 0 until NROWS; c <- 0 until NROWS) {
      assertEquals(expected(r)(c), C.getQuick(r, c))
    }
    C = null
    C = A.zMult(B, C, alpha, beta, false, true)
    expected = Array.ofDim[Int](NROWS, NROWS)
    for (j <- 0 until NROWS; i <- 0 until NROWS) {
      var s = 0
      for (k <- 0 until NCOLUMNS) {
        s += A.getQuick(i, k) * B.getQuick(j, k)
      }
      expected(i)(j) = s * alpha
    }
    for (r <- 0 until NROWS; c <- 0 until NROWS) {
      assertEquals(expected(r)(c), C.getQuick(r, c))
    }
    C = new DiagonalIntMatrix2D(NCOLUMNS, NCOLUMNS, 0)
    for (i <- 0 until DLENGTH) {
      C.setQuick(i, i, Math.max(1, rand.nextInt() % A.rows()))
    }
    expected = C.toArray()
    C = A.zMult(Bt, C, alpha, beta, true, true)
    for (j <- 0 until NCOLUMNS; i <- 0 until NCOLUMNS) {
      var s = 0
      for (k <- 0 until NROWS) {
        s += A.getQuick(k, i) * Bt.getQuick(j, k)
      }
      expected(i)(j) = s * alpha + expected(i)(j) * beta
    }
    for (r <- 0 until NCOLUMNS; c <- 0 until NCOLUMNS) {
      assertEquals(expected(r)(c), C.getQuick(r, c))
    }
    C = null
    C = A.zMult(Bt, C, alpha, beta, true, true)
    expected = Array.ofDim[Int](NCOLUMNS, NCOLUMNS)
    for (j <- 0 until NCOLUMNS; i <- 0 until NCOLUMNS) {
      var s = 0
      for (k <- 0 until NROWS) {
        s += A.getQuick(k, i) * Bt.getQuick(j, k)
      }
      expected(i)(j) = s * alpha
    }
    for (r <- 0 until NCOLUMNS; c <- 0 until NCOLUMNS) {
      assertEquals(expected(r)(c), C.getQuick(r, c))
    }
  }
}
