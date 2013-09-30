package cern.colt.matrix.tfloat

import junit.framework.TestCase
import org.junit.Test
import cern.colt.function.tfloat.FloatProcedure
import cern.colt.list.tfloat.FloatArrayList
import cern.colt.list.tint.IntArrayList
import cern.jet.math.tfloat.FloatFunctions
import edu.emory.mathcs.utils.ConcurrencyUtils
//remove if not needed
import scala.collection.JavaConversions._

abstract class FloatMatrix1DTest(arg0: String) extends TestCase(arg0) {

  /**
   * Matrix to test
   */
  protected var A: FloatMatrix1D = _

  /**
   * Matrix of the same size as a
   */
  protected var B: FloatMatrix1D = _

  protected var SIZE: Int = 2 * 17 * 5

  protected var TOL: Float = 1e-3f

  protected def setUp() {
    createMatrices()
    populateMatrices()
  }

  protected def createMatrices(): Unit

  protected def populateMatrices() {
    ConcurrencyUtils.setThreadsBeginN_1D(1)
    for (i <- 0 until A.size.toInt) {
      A.setQuick(i, Math.random().toFloat)
    }
    for (i <- 0 until B.size.toInt) {
      B.setQuick(i, Math.random().toFloat)
    }
  }

  protected def tearDown() {
    A = B = null
  }

  def testAggregateFloatFloatFunctionFloatFunction() {
    var expected = 0
    for (i <- 0 until A.size.toInt) {
      val elem = A.getQuick(i)
      expected += elem * elem
    }
    val result = A.aggregate(FloatFunctions.plus, FloatFunctions.square)
    assertEquals(expected, result, TOL)
  }

  def testAggregateFloatFloatFunctionFloatFunctionIntArrayList() {
    val indexList = new IntArrayList()
    for (i <- 0 until A.size.toInt) {
      indexList.add(i)
    }
    var expected = 0
    for (i <- 0 until A.size.toInt) {
      val elem = A.getQuick(i)
      expected += elem * elem
    }
    val result = A.aggregate(FloatFunctions.plus, FloatFunctions.square, indexList)
    assertEquals(expected, result, TOL)
  }

  def testAggregateFloatMatrix2DFloatFloatFunctionFloatFloatFunction() {
    var expected = 0
    for (i <- 0 until A.size.toInt) {
      val elemA = A.getQuick(i)
      val elemB = B.getQuick(i)
      expected += elemA * elemB
    }
    val result = A.aggregate(B, FloatFunctions.plus, FloatFunctions.mult)
    assertEquals(expected, result, TOL)
  }

  def testAssignFloat() {
    val value = Math.random().toFloat
    A.assign(value)
    for (i <- 0 until A.size.toInt) {
      assertEquals(value, A.getQuick(i), TOL)
    }
  }

  def testAssignFloatArray() {
    val expected = Array.ofDim[Float](A.size.toInt)
    for (i <- 0 until A.size.toInt) {
      expected(i) = Math.random().toFloat
    }
    A.assign(expected)
    for (i <- 0 until A.size.toInt) {
      assertEquals(expected(i), A.getQuick(i), TOL)
    }
  }

  def testAssignFloatFunction() {
    val Acopy = A.copy()
    A.assign(FloatFunctions.acos)
    for (i <- 0 until A.size.toInt) {
      val expected = Math.acos(Acopy.getQuick(i)).toFloat
      assertEquals(expected, A.getQuick(i), TOL)
    }
  }

  def testAssignFloatMatrix1D() {
    A.assign(B)
    assertTrue(A.size == B.size)
    for (i <- 0 until A.size.toInt) {
      assertEquals(B.getQuick(i), A.getQuick(i), TOL)
    }
  }

  def testAssignFloatMatrix1DFloatFloatFunction() {
    val Acopy = A.copy()
    A.assign(B, FloatFunctions.div)
    for (i <- 0 until A.size.toInt) {
      assertEquals(Acopy.getQuick(i) / B.getQuick(i), A.getQuick(i), TOL)
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
    for (i <- 0 until A.size.toInt) {
      if (Math.abs(Acopy.getQuick(i)) > 0.1) {
        assertEquals(-1.0, A.getQuick(i), TOL)
      } else {
        assertEquals(Acopy.getQuick(i), A.getQuick(i), TOL)
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
    for (i <- 0 until A.size.toInt) {
      if (Math.abs(Acopy.getQuick(i)) > 0.1) {
        assertEquals(Math.tan(Acopy.getQuick(i)), A.getQuick(i), TOL)
      } else {
        assertEquals(Acopy.getQuick(i), A.getQuick(i), TOL)
      }
    }
  }

  def testCardinality() {
    val card = A.cardinality()
    assertEquals(A.size.toInt, card)
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

  def testMaxLocation() {
    A.assign(0)
    A.setQuick(A.size.toInt / 3, 0.7f)
    A.setQuick(A.size.toInt / 2, 0.1f)
    val maxAndLoc = A.getMaxLocation
    assertEquals(0.7, maxAndLoc(0), TOL)
    assertEquals(A.size.toInt / 3, maxAndLoc(1).toInt)
  }

  def testMinLocation() {
    A.assign(0)
    A.setQuick(A.size.toInt / 3, -0.7f)
    A.setQuick(A.size.toInt / 2, -0.1f)
    val minAndLoc = A.getMinLocation
    assertEquals(-0.7f, minAndLoc(0), TOL)
    assertEquals(A.size.toInt / 3, minAndLoc(1).toInt)
  }

  def testGetNegativeValuesIntArrayListFloatArrayList() {
    A.assign(0)
    A.setQuick(A.size.toInt / 3, -0.7f)
    A.setQuick(A.size.toInt / 2, -0.1f)
    val indexList = new IntArrayList()
    val valueList = new FloatArrayList()
    A.getNegativeValues(indexList, valueList)
    assertEquals(2, indexList.size)
    assertEquals(2, valueList.size)
    assertTrue(indexList.contains(A.size.toInt / 3))
    assertTrue(indexList.contains(A.size.toInt / 2))
    assertTrue(valueList.contains(-0.7f))
    assertTrue(valueList.contains(-0.1f))
  }

  def testGetNonZerosIntArrayListFloatArrayList() {
    A.assign(0)
    A.setQuick(A.size.toInt / 3, 0.7f)
    A.setQuick(A.size.toInt / 2, 0.1f)
    val indexList = new IntArrayList()
    val valueList = new FloatArrayList()
    A.getNonZeros(indexList, valueList)
    assertEquals(2, indexList.size)
    assertEquals(2, valueList.size)
    assertTrue(indexList.contains(A.size.toInt / 3))
    assertTrue(indexList.contains(A.size.toInt / 2))
    assertTrue(valueList.contains(0.7f))
    assertTrue(valueList.contains(0.1f))
  }

  def testGetPositiveValuesIntArrayListFloatArrayList() {
    A.assign(0)
    A.setQuick(A.size.toInt / 3, 0.7f)
    A.setQuick(A.size.toInt / 2, 0.1f)
    val indexList = new IntArrayList()
    val valueList = new FloatArrayList()
    A.getPositiveValues(indexList, valueList)
    assertEquals(2, indexList.size)
    assertEquals(2, valueList.size)
    assertTrue(indexList.contains(A.size.toInt / 3))
    assertTrue(indexList.contains(A.size.toInt / 2))
    assertTrue(valueList.contains(0.7f))
    assertTrue(valueList.contains(0.1f))
  }

  def testToArray() {
    val array = A.toArray()
    assertTrue(A.size.toInt == array.length)
    for (i <- 0 until A.size.toInt) {
      assertEquals(array(i), A.getQuick(i), TOL)
    }
  }

  def testToArrayFloatArray() {
    val array = Array.ofDim[Float](A.size.toInt)
    A.toArray(array)
    for (i <- 0 until A.size.toInt) {
      assertEquals(A.getQuick(i), array(i), TOL)
    }
  }

  def testReshapeIntInt() {
    val rows = 10
    val columns = 17
    val B = A.reshape(rows, columns)
    var idx = 0
    for (c <- 0 until columns; r <- 0 until rows) {
      assertEquals(A.getQuick(idx += 1), B.getQuick(r, c), TOL)
    }
  }

  def testReshapeIntIntInt() {
    val slices = 2
    val rows = 5
    val columns = 17
    val B = A.reshape(slices, rows, columns)
    var idx = 0
    for (s <- 0 until slices; c <- 0 until columns; r <- 0 until rows) {
      assertEquals(A.getQuick(idx += 1), B.getQuick(s, r, c), TOL)
    }
  }

  def testSwap() {
    val Acopy = A.copy()
    val Bcopy = B.copy()
    A.swap(B)
    for (i <- 0 until A.size.toInt) {
      assertEquals(Bcopy.getQuick(i), A.getQuick(i), TOL)
      assertEquals(Acopy.getQuick(i), B.getQuick(i), TOL)
    }
  }

  def testViewFlip() {
    val b = A.viewFlip()
    assertEquals(A.size.toInt, b.size)
    for (i <- 0 until A.size.toInt) {
      assertEquals(A.getQuick(i), b.getQuick(A.size.toInt - 1 - i), TOL)
    }
  }

  def testViewPart() {
    val b = A.viewPart(15, 11)
    for (i <- 0 until 11) {
      assertEquals(A.getQuick(15 + i), b.getQuick(i), TOL)
    }
  }

  def testViewSelectionFloatProcedure() {
    val b = A.viewSelection(new FloatProcedure() {

      def apply(element: Float): Boolean = return element % 2 == 0
    })
    for (i <- 0 until b.size) {
      val el = b.getQuick(i)
      if (el % 2 != 0) {
        fail()
      }
    }
  }

  def testViewSelectionIntArray() {
    val indexes = Array(5, 11, 22, 37, 101)
    val b = A.viewSelection(indexes)
    for (i <- 0 until indexes.length) {
      assertEquals(A.getQuick(indexes(i)), b.getQuick(i), TOL)
    }
  }

  def testViewSorted() {
    val b = A.viewSorted()
    for (i <- 0 until A.size.toInt - 1) {
      assertTrue(b.getQuick(i + 1) >= b.getQuick(i))
    }
  }

  def testViewStrides() {
    val stride = 3
    val b = A.viewStrides(stride)
    for (i <- 0 until b.size) {
      assertEquals(A.getQuick(i * stride), b.getQuick(i), TOL)
    }
  }

  def testZDotProductFloatMatrix1D() {
    val product = A.zDotProduct(B)
    var expected = 0
    for (i <- 0 until A.size.toInt) {
      expected += A.getQuick(i) * B.getQuick(i)
    }
    assertEquals(expected, product, TOL)
  }

  def testZDotProductFloatMatrix1DIntInt() {
    val product = A.zDotProduct(B, 5, B.size.toInt - 10)
    var expected = 0
    for (i <- 5 until A.size.toInt - 5) {
      expected += A.getQuick(i) * B.getQuick(i)
    }
    assertEquals(expected, product, TOL)
  }

  @Test
  def testZDotProductFloatMatrix1DIntIntIntArrayList() {
    val indexList = new IntArrayList()
    val valueList = new FloatArrayList()
    B.getNonZeros(indexList, valueList)
    val product = A.zDotProduct(B, 5, B.size.toInt - 10, indexList)
    var expected = 0
    for (i <- 5 until A.size.toInt - 5) {
      expected += A.getQuick(i) * B.getQuick(i)
    }
    assertEquals(expected, product, TOL)
  }

  def testZSum() {
    val sum = A.zSum()
    var expected = 0
    for (i <- 0 until A.size.toInt) {
      expected += A.getQuick(i)
    }
    assertEquals(expected, sum, TOL)
  }
}
