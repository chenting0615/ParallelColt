package cern.colt.matrix.tfloat.impl

import cern.colt.matrix.tfloat.FloatMatrix2DTest
import cern.jet.math.tfloat.FloatFunctions
import edu.emory.mathcs.utils.ConcurrencyUtils
import cern.colt.matrix.MatrixTypes.{FloatMatrix2D, DiagonalFloatMatrix2D}
import org.junit.Assert
import cern.colt.matrix.MatrixOperators._

class DiagonalFloatMatrix2DTest(arg0: String) extends FloatMatrix2DTest(arg0) {

  protected var DLENGTH: Int = _

  protected var DINDEX: Int = _

  override protected def createMatrices() {
    DINDEX = 3
    A = new DiagonalFloatMatrix2D(NROWS, NCOLUMNS, DINDEX)
    B = new DiagonalFloatMatrix2D(NROWS, NCOLUMNS, DINDEX)
    Bt = new DiagonalFloatMatrix2D(NCOLUMNS, NROWS, -DINDEX)
    DLENGTH = A.asInstanceOf[DiagonalFloatMatrix2D].diagonalLength
  }

  override protected def populateMatrices() {
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

  override def testAssignFloat() {
    val value = Math.random().toFloat
    A.assignConstant(value)
    if (DINDEX >= 0) {
      for (r <- 0 until DLENGTH) {
        Assert.assertEquals(value, A.getQuick(r, r + DINDEX), TOL)
      }
    } else {
      for (r <- 0 until DLENGTH) {
        Assert.assertEquals(value, A.getQuick(r - DINDEX, r), TOL)
      }
    }
  }

  override def testAssignFloatArrayArray() {
    val expected = Array.ofDim[Float](NROWS, NCOLUMNS)
    for (r <- 0 until NROWS; c <- 0 until NCOLUMNS) {
      expected(r)(c) = Math.random().toFloat
    }
    A.assign(expected)
    if (DINDEX >= 0) {
      for (r <- 0 until DLENGTH) {
        Assert.assertEquals(expected(r)(r + DINDEX), A.getQuick(r, r + DINDEX), TOL)
      }
    } else {
      for (r <- 0 until DLENGTH) {
        Assert.assertEquals(expected(r - DINDEX)(r), A.getQuick(r - DINDEX, r), TOL)
      }
    }
  }

  override def testAssignFloatFunction() {
    val Acopy = A.copy()
    A.assign(FloatFunctions.acos)
    if (DINDEX >= 0) {
      for (r <- 0 until DLENGTH) {
        val expected = Math.acos(Acopy.getQuick(r, r + DINDEX)).toFloat
        Assert.assertEquals(expected, A.getQuick(r, r + DINDEX), TOL)
      }
    } else {
      for (r <- 0 until DLENGTH) {
        val expected = Math.acos(Acopy.getQuick(r - DINDEX, r)).toFloat
        Assert.assertEquals(expected, A.getQuick(r - DINDEX, r), TOL)
      }
    }
  }

  override def testAssignFloatMatrix2DFloatFloatFunction() {
    val Acopy = A.copy()
    A.assign(B, FloatFunctions.div)
    if (DINDEX >= 0) {
      for (r <- 0 until DLENGTH) {
        Assert.assertEquals(Acopy.getQuick(r, r + DINDEX) / B.getQuick(r, r + DINDEX), A.getQuick(r, r + DINDEX),
          TOL)
      }
    } else {
      for (r <- 0 until DLENGTH) {
        Assert.assertEquals(Acopy.getQuick(r - DINDEX, r) / B.getQuick(r - DINDEX, r), A.getQuick(r - DINDEX,
          r), TOL)
      }
    }
  }

/*
  def testAssignFloatMatrix2DFloatFloatFunctionIntArrayListIntArrayList() {
    val rowList = new IntArrayList()
    val columnList = new ArrayTypes.IntArrayList()
    if (DINDEX >= 0) {
      for (r <- 0 until DLENGTH) {
        rowList.add(r)
        columnList.add(r + DINDEX)
      }
      val Acopy = A.copy()
      A.assign(B, FloatFunctions.div, rowList, columnList)
      for (r <- 0 until DLENGTH) {
        Assert.assertEquals(Acopy.getQuick(r, r + DINDEX) / B.getQuick(r, r + DINDEX), A.getQuick(r, r + DINDEX),
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
        Assert.assertEquals(Acopy.getQuick(r - DINDEX, r) / B.getQuick(r - DINDEX, r), A.getQuick(r - DINDEX,
          r), TOL)
      }
    }
  }
*/

  override def testCardinality() {
    val card = A.numNonZero
    Assert.assertEquals(DLENGTH, card)
  }

  override def testMaxLocation() {
    A.assignConstant(0f)
    if (DINDEX >= 0) {
      A.setQuick(NROWS / 3, NROWS / 3 + DINDEX, 0.7f)
      A.setQuick(NROWS / 2, NROWS / 2 + DINDEX, 0.1f)
      val maxAndLoc = A.getMaxLocation
      Assert.assertEquals(0.7, maxAndLoc._3, TOL)
      Assert.assertEquals(NROWS / 3, maxAndLoc._1)
      Assert.assertEquals(NROWS / 3 + DINDEX, maxAndLoc._2)
    } else {
      A.setQuick(NROWS / 3 - DINDEX, NROWS / 3, 0.7f)
      A.setQuick(NROWS / 2 - DINDEX, NROWS / 2, 0.1f)
      val maxAndLoc = A.getMaxLocation
      Assert.assertEquals(0.7, maxAndLoc._3, TOL)
      Assert.assertEquals(NROWS / 3 - DINDEX, maxAndLoc._1)
      Assert.assertEquals(NROWS / 3, maxAndLoc._2)
    }
  }

  override def testMinLocation() {
    A.assignConstant(0f)
    if (DINDEX >= 0) {
      A.setQuick(NROWS / 3, NROWS / 3 + DINDEX, -0.7f)
      A.setQuick(NROWS / 2, NROWS / 2 + DINDEX, -0.1f)
      val minAndLoc = A.getMinLocation
      Assert.assertEquals(-0.7, minAndLoc._3, TOL)
      Assert.assertEquals(NROWS / 3, minAndLoc._1)
      Assert.assertEquals(NROWS / 3 + DINDEX, minAndLoc._2)
    } else {
      A.setQuick(NROWS / 3 - DINDEX, NROWS / 3, -0.7f)
      A.setQuick(NROWS / 2 - DINDEX, NROWS / 2, -0.1f)
      val minAndLoc = A.getMinLocation
      Assert.assertEquals(-0.7, minAndLoc._3, TOL)
      Assert.assertEquals(NROWS / 3 - DINDEX, minAndLoc._1)
      Assert.assertEquals(NROWS / 3, minAndLoc._2)
    }
  }

/*
  def testGetNegativeValues() {
    A.assignConstant(0f)
    if (DINDEX >= 0) {
      A.setQuick(NROWS / 3, NROWS / 3 + DINDEX, -0.7f)
      A.setQuick(NROWS / 2, NROWS / 2 + DINDEX, -0.1f)
      val rowList = new IntArrayList()
      val columnList = new IntArrayList()
      val valueList = new FloatArrayList()
      A.getNegativeValues(rowList, columnList, valueList)
      Assert.assertEquals(2, rowList.size)
      Assert.assertEquals(2, columnList.size)
      Assert.assertEquals(2, valueList.size)
      Assert.assertTrue(rowList.contains(NROWS / 3))
      Assert.assertTrue(rowList.contains(NROWS / 2))
      Assert.assertTrue(columnList.contains(NROWS / 3 + DINDEX))
      Assert.assertTrue(columnList.contains(NROWS / 2 + DINDEX))
      Assert.assertTrue(valueList.contains(-0.7f))
      Assert.assertTrue(valueList.contains(-0.1f))
    } else {
      A.setQuick(NROWS / 3 - DINDEX, NROWS / 3, -0.7f)
      A.setQuick(NROWS / 2 - DINDEX, NROWS / 2, -0.1f)
      val rowList = new IntArrayList()
      val columnList = new IntArrayList()
      val valueList = new FloatArrayList()
      A.getNegativeValues(rowList, columnList, valueList)
      Assert.assertEquals(2, rowList.size)
      Assert.assertEquals(2, columnList.size)
      Assert.assertEquals(2, valueList.size)
      Assert.assertTrue(rowList.contains(NROWS / 3 - DINDEX))
      Assert.assertTrue(rowList.contains(NROWS / 2 - DINDEX))
      Assert.assertTrue(columnList.contains(NROWS / 3))
      Assert.assertTrue(columnList.contains(NROWS / 2))
      Assert.assertTrue(valueList.contains(-0.7f))
      Assert.assertTrue(valueList.contains(-0.1f))
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
      Assert.assertEquals(2, rowList.size)
      Assert.assertEquals(2, columnList.size)
      Assert.assertEquals(2, valueList.size)
      Assert.assertTrue(rowList.contains(NROWS / 3))
      Assert.assertTrue(rowList.contains(NROWS / 2))
      Assert.assertTrue(columnList.contains(NROWS / 3 + DINDEX))
      Assert.assertTrue(columnList.contains(NROWS / 2 + DINDEX))
      Assert.assertTrue(valueList.contains(0.7f))
      Assert.assertTrue(valueList.contains(0.1f))
    } else {
      A.setQuick(NROWS / 3 - DINDEX, NROWS / 3, 0.7f)
      A.setQuick(NROWS / 2 - DINDEX, NROWS / 2, 0.1f)
      val rowList = new IntArrayList()
      val columnList = new IntArrayList()
      val valueList = new FloatArrayList()
      A.getNonZeros(rowList, columnList, valueList)
      Assert.assertEquals(2, rowList.size)
      Assert.assertEquals(2, columnList.size)
      Assert.assertEquals(2, valueList.size)
      Assert.assertTrue(rowList.contains(NROWS / 3 - DINDEX))
      Assert.assertTrue(rowList.contains(NROWS / 2 - DINDEX))
      Assert.assertTrue(columnList.contains(NROWS / 3))
      Assert.assertTrue(columnList.contains(NROWS / 2))
      Assert.assertTrue(valueList.contains(0.7f))
      Assert.assertTrue(valueList.contains(0.1f))
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
      Assert.assertEquals(2, rowList.size)
      Assert.assertEquals(2, columnList.size)
      Assert.assertEquals(2, valueList.size)
      Assert.assertTrue(rowList.contains(NROWS / 3))
      Assert.assertTrue(rowList.contains(NROWS / 2))
      Assert.assertTrue(columnList.contains(NROWS / 3 + DINDEX))
      Assert.assertTrue(columnList.contains(NROWS / 2 + DINDEX))
      Assert.assertTrue(valueList.contains(0.7f))
      Assert.assertTrue(valueList.contains(0.1f))
    } else {
      A.setQuick(NROWS / 3 - DINDEX, NROWS / 3, 0.7f)
      A.setQuick(NROWS / 2 - DINDEX, NROWS / 2, 0.1f)
      val rowList = new IntArrayList()
      val columnList = new IntArrayList()
      val valueList = new FloatArrayList()
      A.getPositiveValues(rowList, columnList, valueList)
      Assert.assertEquals(2, rowList.size)
      Assert.assertEquals(2, columnList.size)
      Assert.assertEquals(2, valueList.size)
      Assert.assertTrue(rowList.contains(NROWS / 3 - DINDEX))
      Assert.assertTrue(rowList.contains(NROWS / 2 - DINDEX))
      Assert.assertTrue(columnList.contains(NROWS / 3))
      Assert.assertTrue(columnList.contains(NROWS / 2))
      Assert.assertTrue(valueList.contains(0.7f))
      Assert.assertTrue(valueList.contains(0.1f))
    }
  }
*/

  override def testToArray() {
    val array = A.toArray
    Assert.assertTrue(NROWS == array.length)
    for (r <- 0 until NROWS) {
      Assert.assertTrue(NCOLUMNS == array(r).length)
      for (c <- 0 until NCOLUMNS) {
        Assert.assertEquals(array(r)(c), A.getQuick(r, c), TOL)
      }
    }
  }

  override def testVectorize() {
    val Avec = A.vectorize()
    var idx = 0
    for (c <- 0 until NCOLUMNS; r <- 0 until NROWS) {
      Assert.assertEquals(A.getQuick(r, c), Avec.getQuick(idx), TOL)
      idx += 1
    }
  }

  override def testViewColumn() {
    val col = A.viewColumn(NCOLUMNS / 2)
    Assert.assertEquals(NROWS, col.size)
    for (r <- 0 until NROWS) {
      Assert.assertEquals(A.getQuick(r, NCOLUMNS / 2), col.getQuick(r), TOL)
    }
  }

  override def testViewColumnFlip() {
    val B = A.viewColumnFlip()
    Assert.assertEquals(A.size, B.size)
    for (r <- 0 until NROWS; c <- 0 until NCOLUMNS) {
      Assert.assertEquals(A.getQuick(r, NCOLUMNS - 1 - c), B.getQuick(r, c), TOL)
    }
  }

  override def testViewDice() {
    val B = A.viewTranspose()
    Assert.assertEquals(NROWS, B.columns)
    Assert.assertEquals(NCOLUMNS, B.rows)
    for (r <- 0 until NROWS; c <- 0 until NCOLUMNS) {
      Assert.assertEquals(A.getQuick(r, c), B.getQuick(c, r), TOL)
    }
  }

  override def testViewPart() {
    val B = A.viewPart(NROWS / 2, NCOLUMNS / 2, NROWS / 3, NCOLUMNS / 3)
    Assert.assertEquals(NROWS / 3, B.rows)
    Assert.assertEquals(NCOLUMNS / 3, B.columns)
    for (r <- 0 until NROWS / 3; c <- 0 until NCOLUMNS / 3) {
      Assert.assertEquals(A.getQuick(NROWS / 2 + r, NCOLUMNS / 2 + c), B.getQuick(r, c), TOL)
    }
  }

  override def testViewRow() {
    val B = A.viewRow(NROWS / 2)
    Assert.assertEquals(NCOLUMNS, B.size)
    for (r <- 0 until NCOLUMNS) {
      Assert.assertEquals(A.getQuick(NROWS / 2, r), B.getQuick(r), TOL)
    }
  }

  override def testViewRowFlip() {
    val B = A.viewRowFlip()
    Assert.assertEquals(A.size, B.size)
    for (r <- 0 until NROWS; c <- 0 until NCOLUMNS) {
      Assert.assertEquals(A.getQuick(NROWS - 1 - r, c), B.getQuick(r, c), TOL)
    }
  }

/*
  def testViewSelectionFloatMatrix1DProcedure() {
    val value = 2
    A.assignConstant(0f)
    if (DINDEX >= 0) {
      A.setQuick(NROWS / 4, NROWS / 4 + DINDEX, value)
      A.setQuick(NROWS / 2, NROWS / 2 + DINDEX, value)
      val B = A.viewSelection(new Matrix1DProcedure[Float]() {
        def apply(element: FloatMatrix1D): Boolean = {
          Math.abs(element.getQuick(NROWS / 4 + DINDEX) - value
        }
      })
      Assert.assertEquals(1, B.rows)
      Assert.assertEquals(NCOLUMNS, B.columns)
      Assert.assertEquals(A.getQuick(NROWS / 4, NROWS / 4 + DINDEX), B.getQuick(0, NROWS / 4 + DINDEX), TOL)
    } else {
      A.setQuick(NROWS / 4 - DINDEX, NROWS / 4, value)
      A.setQuick(NROWS / 2 - DINDEX, NROWS / 2, value)
      val B = A.viewSelection(new Matrix1DProcedure[Float]() {

        def apply(element: FloatMatrix1D): Boolean = {
          Math.abs(element.getQuick(NROWS / 4) - value) < TOL
        }
      })
      Assert.assertEquals(1, B.rows)
      Assert.assertEquals(NCOLUMNS, B.columns)
      Assert.assertEquals(A.getQuick(NROWS / 4 - DINDEX, NROWS / 4), B.getQuick(0, NROWS / 4), TOL)
    }
  }
*/

  override def testViewSelectionIntArrayIntArray() {
    val rowIndexes = Array(NROWS / 6, NROWS / 5, NROWS / 4, NROWS / 3, NROWS / 2)
    val colIndexes = Array(NROWS / 6, NROWS / 5, NROWS / 4, NROWS / 3, NROWS / 2, NROWS - 1)
    val B = A.viewSelection(rowIndexes, colIndexes)
    Assert.assertEquals(rowIndexes.length, B.rows)
    Assert.assertEquals(colIndexes.length, B.columns)
    for (r <- 0 until rowIndexes.length; c <- 0 until colIndexes.length) {
      Assert.assertEquals(A.getQuick(rowIndexes(r), colIndexes(c)), B.getQuick(r, c), TOL)
    }
  }

/*
  def testViewSorted() {
    val B = A.viewSorted(1)
    for (r <- 0 until NROWS - 1) {
      Assert.assertTrue(B.getQuick(r + 1, 1) >= B.getQuick(r, 1))
    }
  }
*/

  override def testViewStrides() {
    val rowStride = 3
    val colStride = 5
    val B = A.viewStrides(rowStride, colStride)
    for (r <- 0 until B.rows; c <- 0 until B.columns) {
      Assert.assertEquals(A.getQuick(r * rowStride, c * colStride), B.getQuick(r, c), TOL)
    }
  }

  override def testZMultFloatMatrix2DFloatMatrix2DFloatFloatBooleanBoolean() {
    val alpha = 3
    val beta = 5
    var C: FloatMatrix2D = new DiagonalFloatMatrix2D(NROWS, NROWS, 0)
    for (i <- 0 until DLENGTH) {
      C.setQuick(i, i, Math.random().toFloat)
    }
    var expected = C.toArray
    C = A.dot(Bt, C, alpha, beta, false, false)
    for (j <- 0 until NROWS; i <- 0 until NROWS) {
      var s = 0f
      for (k <- 0 until NCOLUMNS) {
        s += A.getQuick(i, k) * Bt.getQuick(k, j)
      }
      expected(i)(j) = s * alpha + expected(i)(j) * beta
    }
    for (r <- 0 until NROWS; c <- 0 until NROWS) {
      Assert.assertEquals(expected(r)(c), C.getQuick(r, c), TOL)
    }
    C = null
    C = A.dot(Bt, C, alpha, beta, false, false)
    expected = Array.ofDim[Float](NROWS, NROWS)
    for (j <- 0 until NROWS; i <- 0 until NROWS) {
      var s = 0f
      for (k <- 0 until NCOLUMNS) {
        s += A.getQuick(i, k) * Bt.getQuick(k, j)
      }
      expected(i)(j) = s * alpha
    }
    for (r <- 0 until NROWS; c <- 0 until NROWS) {
      Assert.assertEquals(expected(r)(c), C.getQuick(r, c), TOL)
    }
    C = new DiagonalFloatMatrix2D(NCOLUMNS, NCOLUMNS, 0)
    for (i <- 0 until DLENGTH) {
      C.setQuick(i, i, Math.random().toFloat)
    }
    expected = C.toArray
    C = A.dot(B, C, alpha, beta, true, false)
    for (j <- 0 until NCOLUMNS; i <- 0 until NCOLUMNS) {
      var s = 0f
      for (k <- 0 until NROWS) {
        s += A.getQuick(k, i) * B.getQuick(k, j)
      }
      expected(i)(j) = s * alpha + expected(i)(j) * beta
    }
    for (r <- 0 until NCOLUMNS; c <- 0 until NCOLUMNS) {
      Assert.assertEquals(expected(r)(c), C.getQuick(r, c), TOL)
    }
    C = null
    C = A.dot(B, C, alpha, beta, true, false)
    expected = Array.ofDim[Float](NCOLUMNS, NCOLUMNS)
    for (j <- 0 until NCOLUMNS; i <- 0 until NCOLUMNS) {
      var s = 0f
      for (k <- 0 until NROWS) {
        s += A.getQuick(k, i) * B.getQuick(k, j)
      }
      expected(i)(j) = s * alpha
    }
    for (r <- 0 until NCOLUMNS; c <- 0 until NCOLUMNS) {
      Assert.assertEquals(expected(r)(c), C.getQuick(r, c), TOL)
    }
    C = new DiagonalFloatMatrix2D(NROWS, NROWS, 0)
    for (i <- 0 until DLENGTH) {
      C.setQuick(i, i, Math.random().toFloat)
    }
    expected = C.toArray
    C = A.dot(B, C, alpha, beta, false, true)
    for (j <- 0 until NROWS; i <- 0 until NROWS) {
      var s = 0f
      for (k <- 0 until NCOLUMNS) {
        s += A.getQuick(i, k) * B.getQuick(j, k)
      }
      expected(i)(j) = s * alpha + expected(i)(j) * beta
    }
    for (r <- 0 until NROWS; c <- 0 until NROWS) {
      Assert.assertEquals(expected(r)(c), C.getQuick(r, c), TOL)
    }
    C = null
    C = A.dot(B, C, alpha, beta, false, true)
    expected = Array.ofDim[Float](NROWS, NROWS)
    for (j <- 0 until NROWS; i <- 0 until NROWS) {
      var s = 0f
      for (k <- 0 until NCOLUMNS) {
        s += A.getQuick(i, k) * B.getQuick(j, k)
      }
      expected(i)(j) = s * alpha
    }
    for (r <- 0 until NROWS; c <- 0 until NROWS) {
      Assert.assertEquals(expected(r)(c), C.getQuick(r, c), TOL)
    }
    C = new DiagonalFloatMatrix2D(NCOLUMNS, NCOLUMNS, 0)
    for (i <- 0 until DLENGTH) {
      C.setQuick(i, i, Math.random().toFloat)
    }
    expected = C.toArray
    C = A.dot(Bt, C, alpha, beta, true, true)
    for (j <- 0 until NCOLUMNS; i <- 0 until NCOLUMNS) {
      var s = 0f
      for (k <- 0 until NROWS) {
        s += A.getQuick(k, i) * Bt.getQuick(j, k)
      }
      expected(i)(j) = s * alpha + expected(i)(j) * beta
    }
    for (r <- 0 until NCOLUMNS; c <- 0 until NCOLUMNS) {
      Assert.assertEquals(expected(r)(c), C.getQuick(r, c), TOL)
    }
    C = null
    C = A.dot(Bt, C, alpha, beta, true, true)
    expected = Array.ofDim[Float](NCOLUMNS, NCOLUMNS)
    for (j <- 0 until NCOLUMNS; i <- 0 until NCOLUMNS) {
      var s = 0f
      for (k <- 0 until NROWS) {
        s += A.getQuick(k, i) * Bt.getQuick(j, k)
      }
      expected(i)(j) = s * alpha
    }
    for (r <- 0 until NCOLUMNS; c <- 0 until NCOLUMNS) {
      Assert.assertEquals(expected(r)(c), C.getQuick(r, c), TOL)
    }
  }
}
