package cern.colt.matrix.tfloat.impl

import cern.colt.list.tfloat.FloatArrayList
import cern.colt.list.tint.IntArrayList
import cern.colt.matrix.tfloat.FloatMatrix1D
import cern.colt.matrix.tfloat.FloatMatrix1DProcedure
import cern.colt.matrix.tfloat.FloatMatrix2D
import cern.colt.matrix.tfloat.FloatMatrix2DTest
import cern.jet.math.tfloat.FloatFunctions
import edu.emory.mathcs.utils.ConcurrencyUtils
//remove if not needed
import scala.collection.JavaConversions._

class DiagonalFloatMatrix2DTest(arg0: String) extends FloatMatrix2DTest(arg0) {

  protected var DLENGTH: Int = _

  protected var DINDEX: Int = _

  protected def createMatrices() {
    DINDEX = 3
    A = new DiagonalFloatMatrix2D(NROWS, NCOLUMNS, DINDEX)
    B = new DiagonalFloatMatrix2D(NROWS, NCOLUMNS, DINDEX)
    Bt = new DiagonalFloatMatrix2D(NCOLUMNS, NROWS, -DINDEX)
    DLENGTH = A.asInstanceOf[DiagonalFloatMatrix2D].diagonalLength()
  }

  protected def populateMatrices() {
    ConcurrencyUtils.setThreadsBeginN_2D(1)
    if (DINDEX >= 0) {
      for (r <- 0 until DLENGTH) {
        A.setQuick(r, r + DINDEX, Math.random().toFloat)
      }
      for (r <- 0 until DLENGTH) {
        B.setQuick(r, r + DINDEX, Math.random().toFloat)
      }
      for (r <- 0 until DLENGTH) {
        Bt.setQuick(r - DINDEX, r, Math.random().toFloat)
      }
    } else {
      for (r <- 0 until DLENGTH) {
        A.setQuick(r - DINDEX, r, Math.random().toFloat)
      }
      for (r <- 0 until DLENGTH) {
        B.setQuick(r - DINDEX, r, Math.random().toFloat)
      }
      for (r <- 0 until DLENGTH) {
        Bt.setQuick(r, r + DINDEX, Math.random().toFloat)
      }
    }
  }

  def testAssignFloat() {
    val value = Math.random().toFloat
    A.assign(value)
    if (DINDEX >= 0) {
      for (r <- 0 until DLENGTH) {
        assertEquals(value, A.getQuick(r, r + DINDEX), TOL)
      }
    } else {
      for (r <- 0 until DLENGTH) {
        assertEquals(value, A.getQuick(r - DINDEX, r), TOL)
      }
    }
  }

  def testAssignFloatArrayArray() {
    val expected = Array.ofDim[Float](NROWS, NCOLUMNS)
    for (r <- 0 until NROWS; c <- 0 until NCOLUMNS) {
      expected(r)(c) = Math.random().toFloat
    }
    A.assign(expected)
    if (DINDEX >= 0) {
      for (r <- 0 until DLENGTH) {
        assertEquals(expected(r)(r + DINDEX), A.getQuick(r, r + DINDEX), TOL)
      }
    } else {
      for (r <- 0 until DLENGTH) {
        assertEquals(expected(r - DINDEX)(r), A.getQuick(r - DINDEX, r), TOL)
      }
    }
  }

  def testAssignFloatFunction() {
    val Acopy = A.copy()
    A.assign(FloatFunctions.acos)
    if (DINDEX >= 0) {
      for (r <- 0 until DLENGTH) {
        val expected = Math.acos(Acopy.getQuick(r, r + DINDEX)).toFloat
        assertEquals(expected, A.getQuick(r, r + DINDEX), TOL)
      }
    } else {
      for (r <- 0 until DLENGTH) {
        val expected = Math.acos(Acopy.getQuick(r - DINDEX, r)).toFloat
        assertEquals(expected, A.getQuick(r - DINDEX, r), TOL)
      }
    }
  }

  def testAssignFloatMatrix2DFloatFloatFunction() {
    val Acopy = A.copy()
    A.assign(B, FloatFunctions.div)
    if (DINDEX >= 0) {
      for (r <- 0 until DLENGTH) {
        assertEquals(Acopy.getQuick(r, r + DINDEX) / B.getQuick(r, r + DINDEX), A.getQuick(r, r + DINDEX), 
          TOL)
      }
    } else {
      for (r <- 0 until DLENGTH) {
        assertEquals(Acopy.getQuick(r - DINDEX, r) / B.getQuick(r - DINDEX, r), A.getQuick(r - DINDEX, 
          r), TOL)
      }
    }
  }

  def testAssignFloatMatrix2DFloatFloatFunctionIntArrayListIntArrayList() {
    val rowList = new IntArrayList()
    val columnList = new IntArrayList()
    if (DINDEX >= 0) {
      for (r <- 0 until DLENGTH) {
        rowList.add(r)
        columnList.add(r + DINDEX)
      }
      val Acopy = A.copy()
      A.assign(B, FloatFunctions.div, rowList, columnList)
      for (r <- 0 until DLENGTH) {
        assertEquals(Acopy.getQuick(r, r + DINDEX) / B.getQuick(r, r + DINDEX), A.getQuick(r, r + DINDEX), 
          TOL)
      }
    } else {
      for (r <- 0 until DLENGTH) {
        rowList.add(r - DINDEX)
        columnList.add(r)
      }
      val Acopy = A.copy()
      A.assign(B, FloatFunctions.div, rowList, columnList)
      for (r <- 0 until DLENGTH) {
        assertEquals(Acopy.getQuick(r - DINDEX, r) / B.getQuick(r - DINDEX, r), A.getQuick(r - DINDEX, 
          r), TOL)
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
      A.setQuick(NROWS / 3, NROWS / 3 + DINDEX, 0.7f)
      A.setQuick(NROWS / 2, NROWS / 2 + DINDEX, 0.1f)
      val maxAndLoc = A.getMaxLocation
      assertEquals(0.7, maxAndLoc(0), TOL)
      assertEquals(NROWS / 3, maxAndLoc(1).toInt)
      assertEquals(NROWS / 3 + DINDEX, maxAndLoc(2).toInt)
    } else {
      A.setQuick(NROWS / 3 - DINDEX, NROWS / 3, 0.7f)
      A.setQuick(NROWS / 2 - DINDEX, NROWS / 2, 0.1f)
      val maxAndLoc = A.getMaxLocation
      assertEquals(0.7, maxAndLoc(0), TOL)
      assertEquals(NROWS / 3 - DINDEX, maxAndLoc(1).toInt)
      assertEquals(NROWS / 3, maxAndLoc(2).toInt)
    }
  }

  def testMinLocation() {
    A.assign(0)
    if (DINDEX >= 0) {
      A.setQuick(NROWS / 3, NROWS / 3 + DINDEX, -0.7f)
      A.setQuick(NROWS / 2, NROWS / 2 + DINDEX, -0.1f)
      val minAndLoc = A.getMinLocation
      assertEquals(-0.7, minAndLoc(0), TOL)
      assertEquals(NROWS / 3, minAndLoc(1).toInt)
      assertEquals(NROWS / 3 + DINDEX, minAndLoc(2).toInt)
    } else {
      A.setQuick(NROWS / 3 - DINDEX, NROWS / 3, -0.7f)
      A.setQuick(NROWS / 2 - DINDEX, NROWS / 2, -0.1f)
      val minAndLoc = A.getMinLocation
      assertEquals(-0.7, minAndLoc(0), TOL)
      assertEquals(NROWS / 3 - DINDEX, minAndLoc(1).toInt)
      assertEquals(NROWS / 3, minAndLoc(2).toInt)
    }
  }

  def testGetNegativeValues() {
    A.assign(0)
    if (DINDEX >= 0) {
      A.setQuick(NROWS / 3, NROWS / 3 + DINDEX, -0.7f)
      A.setQuick(NROWS / 2, NROWS / 2 + DINDEX, -0.1f)
      val rowList = new IntArrayList()
      val columnList = new IntArrayList()
      val valueList = new FloatArrayList()
      A.getNegativeValues(rowList, columnList, valueList)
      assertEquals(2, rowList.size)
      assertEquals(2, columnList.size)
      assertEquals(2, valueList.size)
      assertTrue(rowList.contains(NROWS / 3))
      assertTrue(rowList.contains(NROWS / 2))
      assertTrue(columnList.contains(NROWS / 3 + DINDEX))
      assertTrue(columnList.contains(NROWS / 2 + DINDEX))
      assertTrue(valueList.contains(-0.7f))
      assertTrue(valueList.contains(-0.1f))
    } else {
      A.setQuick(NROWS / 3 - DINDEX, NROWS / 3, -0.7f)
      A.setQuick(NROWS / 2 - DINDEX, NROWS / 2, -0.1f)
      val rowList = new IntArrayList()
      val columnList = new IntArrayList()
      val valueList = new FloatArrayList()
      A.getNegativeValues(rowList, columnList, valueList)
      assertEquals(2, rowList.size)
      assertEquals(2, columnList.size)
      assertEquals(2, valueList.size)
      assertTrue(rowList.contains(NROWS / 3 - DINDEX))
      assertTrue(rowList.contains(NROWS / 2 - DINDEX))
      assertTrue(columnList.contains(NROWS / 3))
      assertTrue(columnList.contains(NROWS / 2))
      assertTrue(valueList.contains(-0.7f))
      assertTrue(valueList.contains(-0.1f))
    }
  }

  def testGetNonZeros() {
    A.assign(0)
    if (DINDEX >= 0) {
      A.setQuick(NROWS / 3, NROWS / 3 + DINDEX, 0.7f)
      A.setQuick(NROWS / 2, NROWS / 2 + DINDEX, 0.1f)
      val rowList = new IntArrayList()
      val columnList = new IntArrayList()
      val valueList = new FloatArrayList()
      A.getNonZeros(rowList, columnList, valueList)
      assertEquals(2, rowList.size)
      assertEquals(2, columnList.size)
      assertEquals(2, valueList.size)
      assertTrue(rowList.contains(NROWS / 3))
      assertTrue(rowList.contains(NROWS / 2))
      assertTrue(columnList.contains(NROWS / 3 + DINDEX))
      assertTrue(columnList.contains(NROWS / 2 + DINDEX))
      assertTrue(valueList.contains(0.7f))
      assertTrue(valueList.contains(0.1f))
    } else {
      A.setQuick(NROWS / 3 - DINDEX, NROWS / 3, 0.7f)
      A.setQuick(NROWS / 2 - DINDEX, NROWS / 2, 0.1f)
      val rowList = new IntArrayList()
      val columnList = new IntArrayList()
      val valueList = new FloatArrayList()
      A.getNonZeros(rowList, columnList, valueList)
      assertEquals(2, rowList.size)
      assertEquals(2, columnList.size)
      assertEquals(2, valueList.size)
      assertTrue(rowList.contains(NROWS / 3 - DINDEX))
      assertTrue(rowList.contains(NROWS / 2 - DINDEX))
      assertTrue(columnList.contains(NROWS / 3))
      assertTrue(columnList.contains(NROWS / 2))
      assertTrue(valueList.contains(0.7f))
      assertTrue(valueList.contains(0.1f))
    }
  }

  def testGetPositiveValues() {
    A.assign(0)
    if (DINDEX >= 0) {
      A.setQuick(NROWS / 3, NROWS / 3 + DINDEX, 0.7f)
      A.setQuick(NROWS / 2, NROWS / 2 + DINDEX, 0.1f)
      val rowList = new IntArrayList()
      val columnList = new IntArrayList()
      val valueList = new FloatArrayList()
      A.getPositiveValues(rowList, columnList, valueList)
      assertEquals(2, rowList.size)
      assertEquals(2, columnList.size)
      assertEquals(2, valueList.size)
      assertTrue(rowList.contains(NROWS / 3))
      assertTrue(rowList.contains(NROWS / 2))
      assertTrue(columnList.contains(NROWS / 3 + DINDEX))
      assertTrue(columnList.contains(NROWS / 2 + DINDEX))
      assertTrue(valueList.contains(0.7f))
      assertTrue(valueList.contains(0.1f))
    } else {
      A.setQuick(NROWS / 3 - DINDEX, NROWS / 3, 0.7f)
      A.setQuick(NROWS / 2 - DINDEX, NROWS / 2, 0.1f)
      val rowList = new IntArrayList()
      val columnList = new IntArrayList()
      val valueList = new FloatArrayList()
      A.getPositiveValues(rowList, columnList, valueList)
      assertEquals(2, rowList.size)
      assertEquals(2, columnList.size)
      assertEquals(2, valueList.size)
      assertTrue(rowList.contains(NROWS / 3 - DINDEX))
      assertTrue(rowList.contains(NROWS / 2 - DINDEX))
      assertTrue(columnList.contains(NROWS / 3))
      assertTrue(columnList.contains(NROWS / 2))
      assertTrue(valueList.contains(0.7f))
      assertTrue(valueList.contains(0.1f))
    }
  }

  def testToArray() {
    val array = A.toArray()
    assertTrue(NROWS == array.length)
    for (r <- 0 until NROWS) {
      assertTrue(NCOLUMNS == array(r).length)
      for (c <- 0 until NCOLUMNS) {
        assertEquals(array(r)(c), A.getQuick(r, c), TOL)
      }
    }
  }

  def testVectorize() {
    val Avec = A.vectorize()
    var idx = 0
    for (c <- 0 until NCOLUMNS; r <- 0 until NROWS) {
      assertEquals(A.getQuick(r, c), Avec.getQuick(idx += 1), TOL)
    }
  }

  def testViewColumn() {
    val col = A.viewColumn(NCOLUMNS / 2)
    assertEquals(NROWS, col.size)
    for (r <- 0 until NROWS) {
      assertEquals(A.getQuick(r, NCOLUMNS / 2), col.getQuick(r), TOL)
    }
  }

  def testViewColumnFlip() {
    val B = A.viewColumnFlip()
    assertEquals(A.size, B.size)
    for (r <- 0 until NROWS; c <- 0 until NCOLUMNS) {
      assertEquals(A.getQuick(r, NCOLUMNS - 1 - c), B.getQuick(r, c), TOL)
    }
  }

  def testViewDice() {
    val B = A.viewDice()
    assertEquals(NROWS, B.columns())
    assertEquals(NCOLUMNS, B.rows())
    for (r <- 0 until NROWS; c <- 0 until NCOLUMNS) {
      assertEquals(A.getQuick(r, c), B.getQuick(c, r), TOL)
    }
  }

  def testViewPart() {
    val B = A.viewPart(NROWS / 2, NCOLUMNS / 2, NROWS / 3, NCOLUMNS / 3)
    assertEquals(NROWS / 3, B.rows())
    assertEquals(NCOLUMNS / 3, B.columns())
    for (r <- 0 until NROWS / 3; c <- 0 until NCOLUMNS / 3) {
      assertEquals(A.getQuick(NROWS / 2 + r, NCOLUMNS / 2 + c), B.getQuick(r, c), TOL)
    }
  }

  def testViewRow() {
    val B = A.viewRow(NROWS / 2)
    assertEquals(NCOLUMNS, B.size)
    for (r <- 0 until NCOLUMNS) {
      assertEquals(A.getQuick(NROWS / 2, r), B.getQuick(r), TOL)
    }
  }

  def testViewRowFlip() {
    val B = A.viewRowFlip()
    assertEquals(A.size, B.size)
    for (r <- 0 until NROWS; c <- 0 until NCOLUMNS) {
      assertEquals(A.getQuick(NROWS - 1 - r, c), B.getQuick(r, c), TOL)
    }
  }

  def testViewSelectionFloatMatrix1DProcedure() {
    val value = 2
    A.assign(0)
    if (DINDEX >= 0) {
      A.setQuick(NROWS / 4, NROWS / 4 + DINDEX, value)
      A.setQuick(NROWS / 2, NROWS / 2 + DINDEX, value)
      val B = A.viewSelection(new FloatMatrix1DProcedure() {

        def apply(element: FloatMatrix1D): Boolean = {
          if (Math.abs(element.getQuick(NROWS / 4 + DINDEX) - value) < 
            TOL) {
            return true
          } else {
            return false
          }
        }
      })
      assertEquals(1, B.rows())
      assertEquals(NCOLUMNS, B.columns())
      assertEquals(A.getQuick(NROWS / 4, NROWS / 4 + DINDEX), B.getQuick(0, NROWS / 4 + DINDEX), TOL)
    } else {
      A.setQuick(NROWS / 4 - DINDEX, NROWS / 4, value)
      A.setQuick(NROWS / 2 - DINDEX, NROWS / 2, value)
      val B = A.viewSelection(new FloatMatrix1DProcedure() {

        def apply(element: FloatMatrix1D): Boolean = {
          if (Math.abs(element.getQuick(NROWS / 4) - value) < TOL) {
            return true
          } else {
            return false
          }
        }
      })
      assertEquals(1, B.rows())
      assertEquals(NCOLUMNS, B.columns())
      assertEquals(A.getQuick(NROWS / 4 - DINDEX, NROWS / 4), B.getQuick(0, NROWS / 4), TOL)
    }
  }

  def testViewSelectionIntArrayIntArray() {
    val rowIndexes = Array(NROWS / 6, NROWS / 5, NROWS / 4, NROWS / 3, NROWS / 2)
    val colIndexes = Array(NROWS / 6, NROWS / 5, NROWS / 4, NROWS / 3, NROWS / 2, NROWS - 1)
    val B = A.viewSelection(rowIndexes, colIndexes)
    assertEquals(rowIndexes.length, B.rows())
    assertEquals(colIndexes.length, B.columns())
    for (r <- 0 until rowIndexes.length; c <- 0 until colIndexes.length) {
      assertEquals(A.getQuick(rowIndexes(r), colIndexes(c)), B.getQuick(r, c), TOL)
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
      assertEquals(A.getQuick(r * rowStride, c * colStride), B.getQuick(r, c), TOL)
    }
  }

  def testZMultFloatMatrix2DFloatMatrix2DFloatFloatBooleanBoolean() {
    val alpha = 3
    val beta = 5
    var C = new DiagonalFloatMatrix2D(NROWS, NROWS, 0)
    for (i <- 0 until DLENGTH) {
      C.setQuick(i, i, Math.random().toFloat)
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
      assertEquals(expected(r)(c), C.getQuick(r, c), TOL)
    }
    C = null
    C = A.zMult(Bt, C, alpha, beta, false, false)
    expected = Array.ofDim[Float](NROWS, NROWS)
    for (j <- 0 until NROWS; i <- 0 until NROWS) {
      var s = 0
      for (k <- 0 until NCOLUMNS) {
        s += A.getQuick(i, k) * Bt.getQuick(k, j)
      }
      expected(i)(j) = s * alpha
    }
    for (r <- 0 until NROWS; c <- 0 until NROWS) {
      assertEquals(expected(r)(c), C.getQuick(r, c), TOL)
    }
    C = new DiagonalFloatMatrix2D(NCOLUMNS, NCOLUMNS, 0)
    for (i <- 0 until DLENGTH) {
      C.setQuick(i, i, Math.random().toFloat)
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
      assertEquals(expected(r)(c), C.getQuick(r, c), TOL)
    }
    C = null
    C = A.zMult(B, C, alpha, beta, true, false)
    expected = Array.ofDim[Float](NCOLUMNS, NCOLUMNS)
    for (j <- 0 until NCOLUMNS; i <- 0 until NCOLUMNS) {
      var s = 0
      for (k <- 0 until NROWS) {
        s += A.getQuick(k, i) * B.getQuick(k, j)
      }
      expected(i)(j) = s * alpha
    }
    for (r <- 0 until NCOLUMNS; c <- 0 until NCOLUMNS) {
      assertEquals(expected(r)(c), C.getQuick(r, c), TOL)
    }
    C = new DiagonalFloatMatrix2D(NROWS, NROWS, 0)
    for (i <- 0 until DLENGTH) {
      C.setQuick(i, i, Math.random().toFloat)
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
      assertEquals(expected(r)(c), C.getQuick(r, c), TOL)
    }
    C = null
    C = A.zMult(B, C, alpha, beta, false, true)
    expected = Array.ofDim[Float](NROWS, NROWS)
    for (j <- 0 until NROWS; i <- 0 until NROWS) {
      var s = 0
      for (k <- 0 until NCOLUMNS) {
        s += A.getQuick(i, k) * B.getQuick(j, k)
      }
      expected(i)(j) = s * alpha
    }
    for (r <- 0 until NROWS; c <- 0 until NROWS) {
      assertEquals(expected(r)(c), C.getQuick(r, c), TOL)
    }
    C = new DiagonalFloatMatrix2D(NCOLUMNS, NCOLUMNS, 0)
    for (i <- 0 until DLENGTH) {
      C.setQuick(i, i, Math.random().toFloat)
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
      assertEquals(expected(r)(c), C.getQuick(r, c), TOL)
    }
    C = null
    C = A.zMult(Bt, C, alpha, beta, true, true)
    expected = Array.ofDim[Float](NCOLUMNS, NCOLUMNS)
    for (j <- 0 until NCOLUMNS; i <- 0 until NCOLUMNS) {
      var s = 0
      for (k <- 0 until NROWS) {
        s += A.getQuick(k, i) * Bt.getQuick(j, k)
      }
      expected(i)(j) = s * alpha
    }
    for (r <- 0 until NCOLUMNS; c <- 0 until NCOLUMNS) {
      assertEquals(expected(r)(c), C.getQuick(r, c), TOL)
    }
  }
}
