package cern.colt.matrix.tfcomplex.impl

import java.util.ArrayList
import cern.colt.list.tint.IntArrayList
import cern.colt.matrix.tfcomplex.FComplexMatrix1D
import cern.colt.matrix.tfcomplex.FComplexMatrix1DProcedure
import cern.colt.matrix.tfcomplex.FComplexMatrix2D
import cern.colt.matrix.tfcomplex.FComplexMatrix2DTest
import cern.colt.matrix.tfloat.FloatFactory2D
import cern.colt.matrix.tfloat.FloatMatrix2D
import cern.jet.math.tfcomplex.FComplex
import cern.jet.math.tfcomplex.FComplexFunctions
import edu.emory.mathcs.utils.ConcurrencyUtils
//remove if not needed
import scala.collection.JavaConversions._

class DiagonalFComplexMatrix2DTest(arg0: String) extends FComplexMatrix2DTest(arg0) {

  protected var DLENGTH: Int = _

  protected var DINDEX: Int = _

  protected def createMatrices() {
    DINDEX = 3
    A = new DiagonalFComplexMatrix2D(NROWS, NCOLUMNS, DINDEX)
    B = new DiagonalFComplexMatrix2D(NROWS, NCOLUMNS, DINDEX)
    Bt = new DiagonalFComplexMatrix2D(NCOLUMNS, NROWS, -DINDEX)
    DLENGTH = A.asInstanceOf[DiagonalFComplexMatrix2D].diagonalLength()
  }

  protected def populateMatrices() {
    ConcurrencyUtils.setThreadsBeginN_2D(1)
    if (DINDEX >= 0) {
      for (r <- 0 until DLENGTH) {
        A.setQuick(r, r + DINDEX, Math.random().toFloat, Math.random().toFloat)
      }
      for (r <- 0 until DLENGTH) {
        B.setQuick(r, r + DINDEX, Math.random().toFloat, Math.random().toFloat)
      }
      for (r <- 0 until DLENGTH) {
        Bt.setQuick(r - DINDEX, r, Math.random().toFloat, Math.random().toFloat)
      }
    } else {
      for (r <- 0 until DLENGTH) {
        A.setQuick(r - DINDEX, r, Math.random().toFloat, Math.random().toFloat)
      }
      for (r <- 0 until DLENGTH) {
        B.setQuick(r - DINDEX, r, Math.random().toFloat, Math.random().toFloat)
      }
      for (r <- 0 until DLENGTH) {
        Bt.setQuick(r, r + DINDEX, Math.random().toFloat, Math.random().toFloat)
      }
    }
  }

  def testAssignFloatFloat() {
    val value = Array(Math.random().toFloat, Math.random().toFloat)
    A.assign(value(0), value(1))
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

  def testAssignFloatArray() {
    val expected = Array.ofDim[Float](2 * DLENGTH)
    for (i <- 0 until 2 * DLENGTH) {
      expected(i) = Math.random().toFloat
    }
    A.assign(expected)
    if (DINDEX >= 0) {
      for (r <- 0 until DLENGTH) {
        assertEquals(expected(2 * r), A.getQuick(r, r + DINDEX)(0), TOL)
        assertEquals(expected(2 * r + 1), A.getQuick(r, r + DINDEX)(1), TOL)
      }
    } else {
      for (r <- 0 until DLENGTH) {
        assertEquals(expected(2 * r), A.getQuick(r - DINDEX, r)(0), TOL)
        assertEquals(expected(2 * r + 1), A.getQuick(r - DINDEX, r)(1), TOL)
      }
    }
  }

  def testAssignImaginary() {
    val Im = FloatFactory2D.dense.random(A.rows(), A.columns())
    val Acopy = A.copy()
    A.assignImaginary(Im)
    if (DINDEX >= 0) {
      for (r <- 0 until DLENGTH) {
        assertEquals(Acopy.getQuick(r, r + DINDEX)(0), A.getQuick(r, r + DINDEX)(0), TOL)
        assertEquals(Im.getQuick(r, r + DINDEX), A.getQuick(r, r + DINDEX)(1), TOL)
      }
    } else {
      for (r <- 0 until DLENGTH) {
        assertEquals(Acopy.getQuick(r - DINDEX, r)(0), A.getQuick(r - DINDEX, r)(0), TOL)
        assertEquals(Im.getQuick(r - DINDEX, r), A.getQuick(r - DINDEX, r)(1), TOL)
      }
    }
  }

  def testAssignReal() {
    val Re = FloatFactory2D.dense.random(A.rows(), A.columns())
    val Acopy = A.copy()
    A.assignReal(Re)
    if (DINDEX >= 0) {
      for (r <- 0 until DLENGTH) {
        assertEquals(Acopy.getQuick(r, r + DINDEX)(1), A.getQuick(r, r + DINDEX)(1), TOL)
        assertEquals(Re.getQuick(r, r + DINDEX), A.getQuick(r, r + DINDEX)(0), TOL)
      }
    } else {
      for (r <- 0 until DLENGTH) {
        assertEquals(Acopy.getQuick(r - DINDEX, r)(1), A.getQuick(r - DINDEX, r)(1), TOL)
        assertEquals(Re.getQuick(r - DINDEX, r), A.getQuick(r - DINDEX, r)(0), TOL)
      }
    }
  }

  def testAssignFloatArrayArray() {
    val expected = Array.ofDim[Float](NROWS, 2 * NCOLUMNS)
    for (r <- 0 until NROWS; c <- 0 until NCOLUMNS) {
      expected(r)(2 * c) = Math.random().toFloat
      expected(r)(2 * c + 1) = Math.random().toFloat
    }
    A.assign(expected)
    if (DINDEX >= 0) {
      for (r <- 0 until DLENGTH) {
        assertEquals(expected(r)(2 * (r + DINDEX)), A.getQuick(r, r + DINDEX)(0), TOL)
        assertEquals(expected(r)(2 * (r + DINDEX) + 1), A.getQuick(r, r + DINDEX)(1), TOL)
      }
    } else {
      for (r <- 0 until DLENGTH) {
        assertEquals(expected(r - DINDEX)(2 * r), A.getQuick(r - DINDEX, r)(0), TOL)
        assertEquals(expected(r - DINDEX)(2 * r + 1), A.getQuick(r - DINDEX, r)(1), TOL)
      }
    }
  }

  def testAssignComplexComplexFunction() {
    val Acopy = A.copy()
    A.assign(FComplexFunctions.acos)
    if (DINDEX >= 0) {
      for (r <- 0 until DLENGTH) {
        val expected = FComplex.acos(Acopy.getQuick(r, r + DINDEX))
        assertEquals(expected(0), A.getQuick(r, r + DINDEX)(0), TOL)
        assertEquals(expected(1), A.getQuick(r, r + DINDEX)(1), TOL)
      }
    } else {
      for (r <- 0 until DLENGTH) {
        val expected = FComplex.acos(Acopy.getQuick(r - DINDEX, r))
        assertEquals(expected(0), A.getQuick(r - DINDEX, r)(0), TOL)
        assertEquals(expected(1), A.getQuick(r - DINDEX, r)(1), TOL)
      }
    }
  }

  def testAssignComplexMatrix2FComplexComplexComplexFunction() {
    val Acopy = A.copy()
    A.assign(B, FComplexFunctions.div)
    if (DINDEX >= 0) {
      for (r <- 0 until DLENGTH) {
        assertEquals(FComplex.div(Acopy.getQuick(r, r + DINDEX), B.getQuick(r, r + DINDEX))(0), A.getQuick(r, 
          r + DINDEX)(0), TOL)
        assertEquals(FComplex.div(Acopy.getQuick(r, r + DINDEX), B.getQuick(r, r + DINDEX))(1), A.getQuick(r, 
          r + DINDEX)(1), TOL)
      }
    } else {
      for (r <- 0 until DLENGTH) {
        assertEquals(FComplex.div(Acopy.getQuick(r - DINDEX, r), B.getQuick(r - DINDEX, r))(0), A.getQuick(r - DINDEX, 
          r)(0), TOL)
        assertEquals(FComplex.div(Acopy.getQuick(r - DINDEX, r), B.getQuick(r - DINDEX, r))(1), A.getQuick(r - DINDEX, 
          r)(1), TOL)
      }
    }
  }

  def testAssignFComplexMatrix2DFComplexFComplexFunctionIntArrayListIntArrayList() {
    val rowList = new IntArrayList()
    val columnList = new IntArrayList()
    if (DINDEX >= 0) {
      for (r <- 0 until DLENGTH) {
        rowList.add(r)
        columnList.add(r + DINDEX)
      }
      val Acopy = A.copy()
      A.assign(B, FComplexFunctions.div, rowList, columnList)
      for (r <- 0 until DLENGTH) {
        assertEquals(FComplex.div(Acopy.getQuick(r, r + DINDEX), B.getQuick(r, r + DINDEX))(0), A.getQuick(r, 
          r + DINDEX)(0), TOL)
        assertEquals(FComplex.div(Acopy.getQuick(r, r + DINDEX), B.getQuick(r, r + DINDEX))(1), A.getQuick(r, 
          r + DINDEX)(1), TOL)
      }
    } else {
      for (r <- 0 until DLENGTH) {
        rowList.add(r - DINDEX)
        columnList.add(r)
      }
      val Acopy = A.copy()
      A.assign(B, FComplexFunctions.div, rowList, columnList)
      for (r <- 0 until DLENGTH) {
        assertEquals(FComplex.div(Acopy.getQuick(r - DINDEX, r), B.getQuick(r - DINDEX, r))(0), A.getQuick(r - DINDEX, 
          r)(0), TOL)
        assertEquals(FComplex.div(Acopy.getQuick(r - DINDEX, r), B.getQuick(r - DINDEX, r))(1), A.getQuick(r - DINDEX, 
          r)(1), TOL)
      }
    }
  }

  def testCardinality() {
    val card = A.cardinality()
    assertEquals(DLENGTH, card)
  }

  def testGetNonZeros() {
    A.assign(0, 0)
    val elem1 = Array(0.7f, 0.8f)
    val elem2 = Array(0.1f, 0.2f)
    if (DINDEX >= 0) {
      A.setQuick(NROWS / 3, NROWS / 3 + DINDEX, elem1)
      A.setQuick(NROWS / 2, NROWS / 2 + DINDEX, elem2)
      val rowList = new IntArrayList()
      val columnList = new IntArrayList()
      val valueList = new ArrayList[Array[Float]]()
      A.getNonZeros(rowList, columnList, valueList)
      assertEquals(2, rowList.size)
      assertEquals(2, columnList.size)
      assertEquals(2, valueList.size)
      assertTrue(rowList.contains(NROWS / 3))
      assertTrue(rowList.contains(NROWS / 2))
      assertTrue(columnList.contains(NROWS / 3 + DINDEX))
      assertTrue(columnList.contains(NROWS / 2 + DINDEX))
      assertEquals(A.getQuick(rowList.get(0), columnList.get(0)), valueList.get(0), TOL)
      assertEquals(A.getQuick(rowList.get(1), columnList.get(1)), valueList.get(1), TOL)
    } else {
      A.setQuick(NROWS / 3 - DINDEX, NROWS / 3, elem1)
      A.setQuick(NROWS / 2 - DINDEX, NROWS / 2, elem2)
      val rowList = new IntArrayList()
      val columnList = new IntArrayList()
      val valueList = new ArrayList[Array[Float]]()
      A.getNonZeros(rowList, columnList, valueList)
      assertEquals(2, rowList.size)
      assertEquals(2, columnList.size)
      assertEquals(2, valueList.size)
      assertTrue(rowList.contains(NROWS / 3 - DINDEX))
      assertTrue(rowList.contains(NROWS / 2 - DINDEX))
      assertTrue(columnList.contains(NROWS / 3))
      assertTrue(columnList.contains(NROWS / 2))
      assertEquals(A.getQuick(rowList.get(0), columnList.get(0)), valueList.get(0), TOL)
      assertEquals(A.getQuick(rowList.get(1), columnList.get(1)), valueList.get(1), TOL)
    }
  }

  def testToArray() {
    val array = A.toArray()
    assertTrue(NROWS == array.length)
    for (r <- 0 until NROWS) {
      assertTrue(2 * NCOLUMNS == array(r).length)
      for (c <- 0 until NCOLUMNS) {
        assertEquals(array(r)(2 * c), A.getQuick(r, c)(0), TOL)
        assertEquals(array(r)(2 * c + 1), A.getQuick(r, c)(1), TOL)
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

  def testViewSelectionComplexMatrix1DProcedure() {
    val value = Array(2, 3)
    A.assign(0, 0)
    if (DINDEX >= 0) {
      A.setQuick(NROWS / 4, NROWS / 4 + DINDEX, value)
      A.setQuick(NROWS / 2, NROWS / 2 + DINDEX, value)
      val B = A.viewSelection(new FComplexMatrix1DProcedure() {

        def apply(element: FComplexMatrix1D): Boolean = {
          if (FComplex.abs(FComplex.minus(element.getQuick(NROWS / 4 + DINDEX), value)) < 
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
      val B = A.viewSelection(new FComplexMatrix1DProcedure() {

        def apply(element: FComplexMatrix1D): Boolean = {
          if (FComplex.abs(FComplex.minus(element.getQuick(NROWS / 4), value)) < 
            TOL) {
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

  def testViewStrides() {
    val rowStride = 3
    val colStride = 5
    val B = A.viewStrides(rowStride, colStride)
    for (r <- 0 until B.rows(); c <- 0 until B.columns()) {
      assertEquals(A.getQuick(r * rowStride, c * colStride), B.getQuick(r, c), TOL)
    }
  }

  def testZMultFComplexMatrix2DFComplexMatrix2DFComplexFComplexBooleanBoolean() {
    val alpha = Array(3, 4)
    val beta = Array(5, 6)
    var C = new DiagonalFComplexMatrix2D(NROWS, NROWS, 0)
    for (i <- 0 until DLENGTH) {
      C.setQuick(i, i, Math.random().toFloat, Math.random().toFloat)
    }
    var expected = C.toArray()
    C = A.zMult(Bt, C, alpha, beta, false, false)
    var elem = Array.ofDim[Float](2)
    for (j <- 0 until NROWS; i <- 0 until NROWS) {
      var s = Array.ofDim[Float](2)
      for (k <- 0 until NCOLUMNS) {
        s = FComplex.plus(s, FComplex.mult(A.getQuick(i, k), Bt.getQuick(k, j)))
      }
      elem(0) = expected(i)(2 * j)
      elem(1) = expected(i)(2 * j + 1)
      elem = FComplex.mult(beta, elem)
      s = FComplex.mult(alpha, s)
      expected(i)(2 * j) = s(0) + elem(0)
      expected(i)(2 * j + 1) = s(1) + elem(1)
    }
    for (r <- 0 until NROWS; c <- 0 until NROWS) {
      assertEquals(expected(r)(2 * c), C.getQuick(r, c)(0), TOL)
      assertEquals(expected(r)(2 * c + 1), C.getQuick(r, c)(1), TOL)
    }
    C = null
    C = A.zMult(Bt, C, alpha, beta, false, false)
    expected = Array.ofDim[Float](NROWS, 2 * NROWS)
    for (j <- 0 until NROWS; i <- 0 until NROWS) {
      var s = Array.ofDim[Float](2)
      for (k <- 0 until NCOLUMNS) {
        s = FComplex.plus(s, FComplex.mult(A.getQuick(i, k), Bt.getQuick(k, j)))
      }
      s = FComplex.mult(alpha, s)
      expected(i)(2 * j) = s(0)
      expected(i)(2 * j + 1) = s(1)
    }
    for (r <- 0 until NROWS; c <- 0 until NROWS) {
      assertEquals(expected(r)(2 * c), C.getQuick(r, c)(0), TOL)
      assertEquals(expected(r)(2 * c + 1), C.getQuick(r, c)(1), TOL)
    }
    C = new DiagonalFComplexMatrix2D(NCOLUMNS, NCOLUMNS, 0)
    for (i <- 0 until DLENGTH) {
      C.setQuick(i, i, Math.random().toFloat, Math.random().toFloat)
    }
    expected = C.toArray()
    C = A.zMult(B, C, alpha, beta, true, false)
    for (j <- 0 until NCOLUMNS; i <- 0 until NCOLUMNS) {
      var s = Array.ofDim[Float](2)
      for (k <- 0 until NROWS) {
        s = FComplex.plus(s, FComplex.mult(FComplex.conj(A.getQuick(k, i)), B.getQuick(k, j)))
      }
      elem(0) = expected(i)(2 * j)
      elem(1) = expected(i)(2 * j + 1)
      elem = FComplex.mult(beta, elem)
      s = FComplex.mult(alpha, s)
      expected(i)(2 * j) = s(0) + elem(0)
      expected(i)(2 * j + 1) = s(1) + elem(1)
    }
    for (r <- 0 until NCOLUMNS; c <- 0 until NCOLUMNS) {
      assertEquals(expected(r)(2 * c), C.getQuick(r, c)(0), TOL)
      assertEquals(expected(r)(2 * c + 1), C.getQuick(r, c)(1), TOL)
    }
    C = null
    C = A.zMult(B, C, alpha, beta, true, false)
    expected = Array.ofDim[Float](NCOLUMNS, 2 * NCOLUMNS)
    for (j <- 0 until NCOLUMNS; i <- 0 until NCOLUMNS) {
      var s = Array.ofDim[Float](2)
      for (k <- 0 until NROWS) {
        s = FComplex.plus(s, FComplex.mult(FComplex.conj(A.getQuick(k, i)), B.getQuick(k, j)))
      }
      s = FComplex.mult(alpha, s)
      expected(i)(2 * j) = s(0)
      expected(i)(2 * j + 1) = s(1)
    }
    for (r <- 0 until NCOLUMNS; c <- 0 until NCOLUMNS) {
      assertEquals(expected(r)(2 * c), C.getQuick(r, c)(0), TOL)
      assertEquals(expected(r)(2 * c + 1), C.getQuick(r, c)(1), TOL)
    }
    C = new DiagonalFComplexMatrix2D(NROWS, NROWS, 0)
    for (i <- 0 until DLENGTH) {
      C.setQuick(i, i, Math.random().toFloat, Math.random().toFloat)
    }
    expected = C.toArray()
    C = A.zMult(B, C, alpha, beta, false, true)
    for (j <- 0 until NROWS; i <- 0 until NROWS) {
      var s = Array.ofDim[Float](2)
      for (k <- 0 until NCOLUMNS) {
        s = FComplex.plus(s, FComplex.mult(A.getQuick(i, k), FComplex.conj(B.getQuick(j, k))))
      }
      elem(0) = expected(i)(2 * j)
      elem(1) = expected(i)(2 * j + 1)
      elem = FComplex.mult(beta, elem)
      s = FComplex.mult(alpha, s)
      expected(i)(2 * j) = s(0) + elem(0)
      expected(i)(2 * j + 1) = s(1) + elem(1)
    }
    for (r <- 0 until NROWS; c <- 0 until NROWS) {
      assertEquals(expected(r)(2 * c), C.getQuick(r, c)(0), TOL)
      assertEquals(expected(r)(2 * c + 1), C.getQuick(r, c)(1), TOL)
    }
    C = null
    C = A.zMult(B, C, alpha, beta, false, true)
    expected = Array.ofDim[Float](NROWS, 2 * NROWS)
    for (j <- 0 until NROWS; i <- 0 until NROWS) {
      var s = Array.ofDim[Float](2)
      for (k <- 0 until NCOLUMNS) {
        s = FComplex.plus(s, FComplex.mult(A.getQuick(i, k), FComplex.conj(B.getQuick(j, k))))
      }
      s = FComplex.mult(alpha, s)
      expected(i)(2 * j) = s(0)
      expected(i)(2 * j + 1) = s(1)
    }
    for (r <- 0 until NROWS; c <- 0 until NROWS) {
      assertEquals(expected(r)(2 * c), C.getQuick(r, c)(0), TOL)
      assertEquals(expected(r)(2 * c + 1), C.getQuick(r, c)(1), TOL)
    }
    C = new DiagonalFComplexMatrix2D(NCOLUMNS, NCOLUMNS, 0)
    for (i <- 0 until DLENGTH) {
      C.setQuick(i, i, Math.random().toFloat, Math.random().toFloat)
    }
    expected = C.toArray()
    C = A.zMult(Bt, C, alpha, beta, true, true)
    for (j <- 0 until NCOLUMNS; i <- 0 until NCOLUMNS) {
      var s = Array.ofDim[Float](2)
      for (k <- 0 until NROWS) {
        s = FComplex.plus(s, FComplex.mult(FComplex.conj(A.getQuick(k, i)), FComplex.conj(Bt.getQuick(j, 
          k))))
      }
      elem(0) = expected(i)(2 * j)
      elem(1) = expected(i)(2 * j + 1)
      elem = FComplex.mult(beta, elem)
      s = FComplex.mult(alpha, s)
      expected(i)(2 * j) = s(0) + elem(0)
      expected(i)(2 * j + 1) = s(1) + elem(1)
    }
    for (r <- 0 until NCOLUMNS; c <- 0 until NCOLUMNS) {
      assertEquals(expected(r)(2 * c), C.getQuick(r, c)(0), TOL)
      assertEquals(expected(r)(2 * c + 1), C.getQuick(r, c)(1), TOL)
    }
    C = null
    C = A.zMult(Bt, C, alpha, beta, true, true)
    expected = Array.ofDim[Float](NCOLUMNS, 2 * NCOLUMNS)
    for (j <- 0 until NCOLUMNS; i <- 0 until NCOLUMNS) {
      var s = Array.ofDim[Float](2)
      for (k <- 0 until NROWS) {
        s = FComplex.plus(s, FComplex.mult(A.getQuick(k, i), Bt.getQuick(j, k)))
      }
      s = FComplex.mult(alpha, s)
      expected(i)(2 * j) = s(0)
      expected(i)(2 * j + 1) = s(1)
    }
    for (r <- 0 until NCOLUMNS; c <- 0 until NCOLUMNS) {
      assertEquals(expected(r)(2 * c), C.getQuick(r, c)(0), TOL)
      assertEquals(expected(r)(2 * c + 1), C.getQuick(r, c)(1), TOL)
    }
  }
}
