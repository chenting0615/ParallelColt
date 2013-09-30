package cern.colt.matrix.tfloat

import java.util.Random
import junit.framework.TestCase
import cern.colt.function.tfloat.FloatProcedure
import cern.colt.function.tfloat.IntIntFloatFunction
import cern.colt.list.tfloat.FloatArrayList
import cern.colt.list.tint.IntArrayList
import cern.colt.matrix.tfloat.impl.DenseFloatMatrix1D
import cern.jet.math.tfloat.FloatFunctions
import edu.emory.mathcs.utils.ConcurrencyUtils
import FloatMatrix2DTest._
//remove if not needed
import scala.collection.JavaConversions._

object FloatMatrix2DTest {

  protected val random = new Random(0)
}

abstract class FloatMatrix2DTest(arg0: String) extends TestCase(arg0) {

  /**
   * Matrix to test
   */
  protected var A: FloatMatrix2D = _

  /**
   * Matrix of the same size as A
   */
  protected var B: FloatMatrix2D = _

  /**
   * Matrix of the size A.columns() x A.rows()
   */
  protected var Bt: FloatMatrix2D = _

  protected var NROWS: Int = 13

  protected var NCOLUMNS: Int = 17

  protected var TOL: Float = 1e-3f

  protected def setUp() {
    createMatrices()
    populateMatrices()
  }

  protected def createMatrices(): Unit

  protected def populateMatrices() {
    ConcurrencyUtils.setThreadsBeginN_2D(1)
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) {
      A.setQuick(r, c, Math.random().toFloat)
    }
    for (r <- 0 until B.rows(); c <- 0 until B.columns()) {
      B.setQuick(r, c, Math.random().toFloat)
    }
    for (r <- 0 until Bt.rows(); c <- 0 until Bt.columns()) {
      Bt.setQuick(r, c, Math.random().toFloat)
    }
  }

  protected def tearDown() {
    A = B = Bt = null
  }

  def testAggregateFloatFloatFunctionFloatFunction() {
    var expected = 0
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) {
      val elem = A.getQuick(r, c)
      expected += elem * elem
    }
    val result = A.aggregate(FloatFunctions.plus, FloatFunctions.square)
    assertEquals(expected, result, TOL)
  }

  def testAggregateFloatFloatFunctionFloatFunctionFloatProcedure() {
    val procedure = new FloatProcedure() {

      def apply(element: Float): Boolean = {
        if (Math.abs(element) > 0.2) {
          return true
        } else {
          return false
        }
      }
    }
    var expected = 0
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) {
      val elem = A.getQuick(r, c)
      if (Math.abs(elem) > 0.2) {
        expected += elem * elem
      }
    }
    val result = A.aggregate(FloatFunctions.plus, FloatFunctions.square, procedure)
    assertEquals(expected, result, TOL)
  }

  def testAggregateFloatFloatFunctionFloatFunctionIntArrayListIntArrayList() {
    val rowList = new IntArrayList()
    val columnList = new IntArrayList()
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) {
      rowList.add(r)
      columnList.add(c)
    }
    var expected = 0
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) {
      val elem = A.getQuick(r, c)
      expected += elem * elem
    }
    val result = A.aggregate(FloatFunctions.plus, FloatFunctions.square, rowList, columnList)
    assertEquals(expected, result, TOL)
  }

  def testAggregateFloatMatrix2DFloatFloatFunctionFloatFloatFunction() {
    var expected = 0
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) {
      val elemA = A.getQuick(r, c)
      val elemB = B.getQuick(r, c)
      expected += elemA * elemB
    }
    val result = A.aggregate(B, FloatFunctions.plus, FloatFunctions.mult)
    assertEquals(expected, result, TOL)
  }

  def testAssignFloat() {
    val value = Math.random().toFloat
    A.assign(value)
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) assertEquals(value, A.getQuick(r, c), TOL)
  }

  def testAssignFloatArrayArray() {
    val expected = Array.ofDim[Float](A.rows(), A.columns())
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) {
      expected(r)(c) = Math.random().toFloat
    }
    A.assign(expected)
    for (r <- 0 until A.rows()) {
      assertTrue(A.columns() == expected(r).length)
      for (c <- 0 until A.columns()) assertEquals(expected(r)(c), A.getQuick(r, c), TOL)
    }
  }

  def testAssignFloatFunction() {
    val Acopy = A.copy()
    A.assign(FloatFunctions.acos)
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) {
      val expected = Math.acos(Acopy.getQuick(r, c)).toFloat
      assertEquals(expected, A.getQuick(r, c), TOL)
    }
  }

  def testAssignFloatMatrix2D() {
    A.assign(B)
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) assertEquals(B.getQuick(r, c), A.getQuick(r, 
      c), TOL)
  }

  def testAssignFloatMatrix2DFloatFloatFunction() {
    val Acopy = A.copy()
    A.assign(B, FloatFunctions.plus)
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) {
      assertEquals(Acopy.getQuick(r, c) + B.getQuick(r, c), A.getQuick(r, c), TOL)
    }
  }

  def testAssignFloatMatrix2DFloatFloatFunctionIntArrayListIntArrayList() {
    val rowList = new IntArrayList()
    val columnList = new IntArrayList()
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) {
      rowList.add(r)
      columnList.add(c)
    }
    val Acopy = A.copy()
    A.assign(B, FloatFunctions.div, rowList, columnList)
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) {
      assertEquals(Acopy.getQuick(r, c) / B.getQuick(r, c), A.getQuick(r, c), TOL)
    }
  }

  def testAssignFloatProcedureFloat() {
    val procedure = new FloatProcedure() {

      def apply(element: Float): Boolean = {
        if (Math.abs(element) > 0.1) {
          return true
        } else {
          return false
        }
      }
    }
    val Acopy = A.copy()
    A.assign(procedure, -1.0f)
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) {
      if (Math.abs(Acopy.getQuick(r, c)) > 0.1) {
        assertEquals(-1.0, A.getQuick(r, c), TOL)
      } else {
        assertEquals(Acopy.getQuick(r, c), A.getQuick(r, c), TOL)
      }
    }
  }

  def testAssignFloatProcedureFloatFunction() {
    val procedure = new FloatProcedure() {

      def apply(element: Float): Boolean = {
        if (Math.abs(element) > 0.1) {
          return true
        } else {
          return false
        }
      }
    }
    val Acopy = A.copy()
    A.assign(procedure, FloatFunctions.tan)
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) {
      if (Math.abs(Acopy.getQuick(r, c)) > 0.1) {
        assertEquals(Math.tan(Acopy.getQuick(r, c)), A.getQuick(r, c), TOL)
      } else {
        assertEquals(Acopy.getQuick(r, c), A.getQuick(r, c), TOL)
      }
    }
  }

  def testCardinality() {
    val card = A.cardinality()
    assertEquals(A.rows() * A.columns(), card)
  }

  def testEqualsFloat() {
    val value = 1
    A.assign(value)
    var eq = A == value
    assertTrue(eq)
    eq = A == 2
    assertFalse(eq)
  }

  def testEqualsObject() {
    var eq = A == A
    assertTrue(eq)
    eq = A == B
    assertFalse(eq)
  }

  def testForEachNonZero() {
    val Acopy = A.copy()
    val function = new IntIntFloatFunction() {

      def apply(first: Int, second: Int, third: Float): Float = return Math.sqrt(third).toFloat
    }
    A.forEachNonZero(function)
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) {
      assertEquals(Math.sqrt(Acopy.getQuick(r, c)), A.getQuick(r, c), TOL)
    }
  }

  def testMaxLocation() {
    A.assign(0)
    A.setQuick(A.rows() / 3, A.columns() / 3, 0.7f)
    A.setQuick(A.rows() / 2, A.columns() / 2, 0.1f)
    val maxAndLoc = A.getMaxLocation
    assertEquals(0.7f, maxAndLoc(0), TOL)
    assertEquals(A.rows() / 3, maxAndLoc(1).toInt)
    assertEquals(A.columns() / 3, maxAndLoc(2).toInt)
  }

  def testMinLocation() {
    A.assign(0)
    A.setQuick(A.rows() / 3, A.columns() / 3, -0.7f)
    A.setQuick(A.rows() / 2, A.columns() / 2, -0.1f)
    val minAndLoc = A.getMinLocation
    assertEquals(-0.7f, minAndLoc(0), TOL)
    assertEquals(A.rows() / 3, minAndLoc(1).toInt)
    assertEquals(A.columns() / 3, minAndLoc(2).toInt)
  }

  def testGetNegativeValues() {
    A.assign(0)
    A.setQuick(A.rows() / 3, A.columns() / 3, -0.7f)
    A.setQuick(A.rows() / 2, A.columns() / 2, -0.1f)
    val rowList = new IntArrayList()
    val columnList = new IntArrayList()
    val valueList = new FloatArrayList()
    A.getNegativeValues(rowList, columnList, valueList)
    assertEquals(2, rowList.size)
    assertEquals(2, columnList.size)
    assertEquals(2, valueList.size)
    assertTrue(rowList.contains(A.rows() / 3))
    assertTrue(rowList.contains(A.rows() / 2))
    assertTrue(columnList.contains(A.columns() / 3))
    assertTrue(columnList.contains(A.columns() / 2))
    assertTrue(valueList.contains(-0.7f))
    assertTrue(valueList.contains(-0.1f))
  }

  def testGetNonZeros() {
    A.assign(0)
    A.setQuick(A.rows() / 3, A.columns() / 3, 0.7f)
    A.setQuick(A.rows() / 2, A.columns() / 2, 0.1f)
    val rowList = new IntArrayList()
    val columnList = new IntArrayList()
    val valueList = new FloatArrayList()
    A.getNonZeros(rowList, columnList, valueList)
    assertEquals(2, rowList.size)
    assertEquals(2, columnList.size)
    assertEquals(2, valueList.size)
    assertTrue(rowList.contains(A.rows() / 3))
    assertTrue(rowList.contains(A.rows() / 2))
    assertTrue(columnList.contains(A.columns() / 3))
    assertTrue(columnList.contains(A.columns() / 2))
    assertTrue(valueList.contains(0.7f))
    assertTrue(valueList.contains(0.1f))
  }

  def testGetPositiveValues() {
    A.assign(0)
    A.setQuick(A.rows() / 3, A.columns() / 3, 0.7f)
    A.setQuick(A.rows() / 2, A.columns() / 2, 0.1f)
    val rowList = new IntArrayList()
    val columnList = new IntArrayList()
    val valueList = new FloatArrayList()
    A.getPositiveValues(rowList, columnList, valueList)
    assertEquals(2, rowList.size)
    assertEquals(2, columnList.size)
    assertEquals(2, valueList.size)
    assertTrue(rowList.contains(A.rows() / 3))
    assertTrue(rowList.contains(A.rows() / 2))
    assertTrue(columnList.contains(A.columns() / 3))
    assertTrue(columnList.contains(A.columns() / 2))
    assertTrue(valueList.contains(0.7f))
    assertTrue(valueList.contains(0.1f))
  }

  def testToArray() {
    val array = A.toArray()
    assertTrue(A.rows() == array.length)
    for (r <- 0 until A.rows()) {
      assertTrue(A.columns() == array(r).length)
      for (c <- 0 until A.columns()) assertEquals(0, Math.abs(array(r)(c) - A.getQuick(r, c)), TOL)
    }
  }

  def testVectorize() {
    val Avec = A.vectorize()
    var idx = 0
    for (c <- 0 until A.columns(); r <- 0 until A.rows()) {
      assertEquals(A.getQuick(r, c), Avec.getQuick(idx += 1), TOL)
    }
  }

  def testViewColumn() {
    val col = A.viewColumn(A.columns() / 2)
    assertEquals(A.rows(), col.size)
    for (r <- 0 until A.rows()) {
      assertEquals(A.getQuick(r, A.columns() / 2), col.getQuick(r), TOL)
    }
  }

  def testViewColumnFlip() {
    val B = A.viewColumnFlip()
    assertEquals(A.size, B.size)
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) {
      assertEquals(A.getQuick(r, A.columns() - 1 - c), B.getQuick(r, c), TOL)
    }
  }

  def testViewDice() {
    val B = A.viewDice()
    assertEquals(A.rows(), B.columns())
    assertEquals(A.columns(), B.rows())
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) {
      assertEquals(A.getQuick(r, c), B.getQuick(c, r), TOL)
    }
  }

  def testViewPart() {
    val B = A.viewPart(A.rows() / 2, A.columns() / 2, A.rows() / 3, A.columns() / 3)
    assertEquals(A.rows() / 3, B.rows())
    assertEquals(A.columns() / 3, B.columns())
    for (r <- 0 until A.rows() / 3; c <- 0 until A.columns() / 3) {
      assertEquals(A.getQuick(A.rows() / 2 + r, A.columns() / 2 + c), B.getQuick(r, c), TOL)
    }
  }

  def testViewRow() {
    val B = A.viewRow(A.rows() / 2)
    assertEquals(A.columns(), B.size)
    for (r <- 0 until A.columns()) {
      assertEquals(A.getQuick(A.rows() / 2, r), B.getQuick(r), TOL)
    }
  }

  def testViewRowFlip() {
    val B = A.viewRowFlip()
    assertEquals(A.size, B.size)
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) {
      assertEquals(A.getQuick(A.rows() - 1 - r, c), B.getQuick(r, c), TOL)
    }
  }

  def testViewSelectionFloatMatrix1DProcedure() {
    val value = 2
    A.assign(0)
    A.setQuick(A.rows() / 4, 0, value)
    A.setQuick(A.rows() / 2, 0, value)
    val B = A.viewSelection(new FloatMatrix1DProcedure() {

      def apply(element: FloatMatrix1D): Boolean = {
        if (Math.abs(element.getQuick(0) - value) < TOL) {
          return true
        } else {
          return false
        }
      }
    })
    assertEquals(2, B.rows())
    assertEquals(A.columns(), B.columns())
    assertEquals(A.getQuick(A.rows() / 4, 0), B.getQuick(0, 0), TOL)
    assertEquals(A.getQuick(A.rows() / 2, 0), B.getQuick(1, 0), TOL)
  }

  def testViewSelectionIntArrayIntArray() {
    val rowIndexes = Array(A.rows() / 6, A.rows() / 5, A.rows() / 4, A.rows() / 3, A.rows() / 2)
    val colIndexes = Array(A.columns() / 6, A.columns() / 5, A.columns() / 4, A.columns() / 3, A.columns() / 2, A.columns() - 1)
    val B = A.viewSelection(rowIndexes, colIndexes)
    assertEquals(rowIndexes.length, B.rows())
    assertEquals(colIndexes.length, B.columns())
    for (r <- 0 until rowIndexes.length; c <- 0 until colIndexes.length) {
      assertEquals(A.getQuick(rowIndexes(r), colIndexes(c)), B.getQuick(r, c), TOL)
    }
  }

  def testViewSorted() {
    val B = A.viewSorted(1)
    for (r <- 0 until A.rows() - 1) {
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

  def testZMultFloatMatrix1DFloatMatrix1DFloatFloatBoolean() {
    var y = new DenseFloatMatrix1D(A.columns())
    for (i <- 0 until y.size) {
      y.setQuick(i, Math.random().toFloat)
    }
    val alpha = 3
    val beta = 5
    var z = FloatFactory1D.dense.random(A.rows())
    var expected = z.toArray()
    z = A.zMult(y, z, alpha, beta, false)
    for (r <- 0 until A.rows()) {
      var s = 0
      for (c <- 0 until A.columns()) {
        s += A.getQuick(r, c) * y.getQuick(c)
      }
      expected(r) = s * alpha + expected(r) * beta
    }
    for (r <- 0 until A.rows()) {
      assertEquals(expected(r), z.getQuick(r), TOL)
    }
    z = null
    z = A.zMult(y, z, alpha, beta, false)
    expected = Array.ofDim[Float](A.rows())
    for (r <- 0 until A.rows()) {
      var s = 0
      for (c <- 0 until A.columns()) {
        s += A.getQuick(r, c) * y.getQuick(c)
      }
      expected(r) = s * alpha
    }
    for (r <- 0 until A.rows()) {
      assertEquals(expected(r), z.getQuick(r), TOL)
    }
    y = new DenseFloatMatrix1D(A.rows())
    for (i <- 0 until y.size) {
      y.setQuick(i, Math.random().toFloat)
    }
    z = FloatFactory1D.dense.random(A.columns())
    expected = z.toArray()
    z = A.zMult(y, z, alpha, beta, true)
    for (r <- 0 until A.columns()) {
      var s = 0
      for (c <- 0 until A.rows()) {
        s += A.getQuick(c, r) * y.getQuick(c)
      }
      expected(r) = s * alpha + expected(r) * beta
    }
    for (r <- 0 until A.columns()) {
      assertEquals(expected(r), z.getQuick(r), TOL)
    }
    z = null
    z = A.zMult(y, z, alpha, beta, true)
    expected = Array.ofDim[Float](A.columns())
    for (r <- 0 until A.columns()) {
      var s = 0
      for (c <- 0 until A.rows()) {
        s += A.getQuick(c, r) * y.getQuick(c)
      }
      expected(r) = s * alpha
    }
    for (r <- 0 until A.columns()) {
      assertEquals(expected(r), z.getQuick(r), TOL)
    }
  }

  def testZMultFloatMatrix2DFloatMatrix2DFloatFloatBooleanBoolean() {
    val alpha = 3
    val beta = 5
    var C = FloatFactory2D.dense.random(A.rows(), A.rows())
    var expected = C.toArray()
    C = A.zMult(Bt, C, alpha, beta, false, false)
    for (j <- 0 until A.rows(); i <- 0 until A.rows()) {
      var s = 0
      for (k <- 0 until A.columns()) {
        s += A.getQuick(i, k) * Bt.getQuick(k, j)
      }
      expected(i)(j) = s * alpha + expected(i)(j) * beta
    }
    for (r <- 0 until A.rows(); c <- 0 until A.rows()) {
      assertEquals(expected(r)(c), C.getQuick(r, c), TOL)
    }
    C = null
    C = A.zMult(Bt, C, alpha, beta, false, false)
    expected = Array.ofDim[Float](A.rows(), A.rows())
    for (j <- 0 until A.rows(); i <- 0 until A.rows()) {
      var s = 0
      for (k <- 0 until A.columns()) {
        s += A.getQuick(i, k) * Bt.getQuick(k, j)
      }
      expected(i)(j) = s * alpha
    }
    for (r <- 0 until A.rows(); c <- 0 until A.rows()) {
      assertEquals(expected(r)(c), C.getQuick(r, c), TOL)
    }
    C = FloatFactory2D.dense.random(A.columns(), A.columns())
    expected = C.toArray()
    C = A.zMult(B, C, alpha, beta, true, false)
    for (j <- 0 until A.columns(); i <- 0 until A.columns()) {
      var s = 0
      for (k <- 0 until A.rows()) {
        s += A.getQuick(k, i) * B.getQuick(k, j)
      }
      expected(i)(j) = s * alpha + expected(i)(j) * beta
    }
    for (r <- 0 until A.columns(); c <- 0 until A.columns()) {
      assertEquals(expected(r)(c), C.getQuick(r, c), TOL)
    }
    C = null
    C = A.zMult(B, C, alpha, beta, true, false)
    expected = Array.ofDim[Float](A.columns(), A.columns())
    for (j <- 0 until A.columns(); i <- 0 until A.columns()) {
      var s = 0
      for (k <- 0 until A.rows()) {
        s += A.getQuick(k, i) * B.getQuick(k, j)
      }
      expected(i)(j) = s * alpha
    }
    for (r <- 0 until A.columns(); c <- 0 until A.columns()) {
      assertEquals(expected(r)(c), C.getQuick(r, c), TOL)
    }
    C = FloatFactory2D.dense.random(A.rows(), A.rows())
    expected = C.toArray()
    C = A.zMult(B, C, alpha, beta, false, true)
    for (j <- 0 until A.rows(); i <- 0 until A.rows()) {
      var s = 0
      for (k <- 0 until A.columns()) {
        s += A.getQuick(i, k) * B.getQuick(j, k)
      }
      expected(i)(j) = s * alpha + expected(i)(j) * beta
    }
    for (r <- 0 until A.rows(); c <- 0 until A.rows()) {
      assertEquals(expected(r)(c), C.getQuick(r, c), TOL)
    }
    C = null
    C = A.zMult(B, C, alpha, beta, false, true)
    expected = Array.ofDim[Float](A.rows(), A.rows())
    for (j <- 0 until A.rows(); i <- 0 until A.rows()) {
      var s = 0
      for (k <- 0 until A.columns()) {
        s += A.getQuick(i, k) * B.getQuick(j, k)
      }
      expected(i)(j) = s * alpha
    }
    for (r <- 0 until A.rows(); c <- 0 until A.rows()) {
      assertEquals(expected(r)(c), C.getQuick(r, c), TOL)
    }
    C = FloatFactory2D.dense.random(A.columns(), A.columns())
    expected = C.toArray()
    C = A.zMult(Bt, C, alpha, beta, true, true)
    for (j <- 0 until A.columns(); i <- 0 until A.columns()) {
      var s = 0
      for (k <- 0 until A.rows()) {
        s += A.getQuick(k, i) * Bt.getQuick(j, k)
      }
      expected(i)(j) = s * alpha + expected(i)(j) * beta
    }
    for (r <- 0 until A.columns(); c <- 0 until A.columns()) {
      assertEquals(expected(r)(c), C.getQuick(r, c), TOL)
    }
    C = null
    C = A.zMult(Bt, C, alpha, beta, true, true)
    expected = Array.ofDim[Float](A.columns(), A.columns())
    for (j <- 0 until A.columns(); i <- 0 until A.columns()) {
      var s = 0
      for (k <- 0 until A.rows()) {
        s += A.getQuick(k, i) * Bt.getQuick(j, k)
      }
      expected(i)(j) = s * alpha
    }
    for (r <- 0 until A.columns(); c <- 0 until A.columns()) {
      assertEquals(expected(r)(c), C.getQuick(r, c), TOL)
    }
  }

  def testZSum() {
    val sum = A.zSum()
    var expected = 0
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) {
      expected += A.getQuick(r, c)
    }
    assertEquals(expected, sum, TOL)
  }
}
