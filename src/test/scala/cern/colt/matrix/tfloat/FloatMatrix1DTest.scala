package cern.colt.matrix.tfloat

import junit.framework.TestCase
import org.junit.Assert
import edu.emory.mathcs.utils.ConcurrencyUtils
import cern.colt.matrix.MatrixTypes.FloatMatrix1D
import cern.colt.matrix.MatrixOperators._
import cern.colt.function.Procedure1
import cern.jet.math.tfloat.FloatFunctions
import cern.colt.function.ProcedureTypes.FloatProcedure

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

  override protected def setUp() {
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

  override protected def tearDown() {
    A = null
    B = null
  }

  def testAggregateFloatFloatFunctionFloatFunction() {
    var expected = 0f
    for (i <- 0 until A.size.toInt) {
      val elem = A.getQuick(i)
      expected += elem * elem
    }
    val result = A.aggregate(FloatFunctions.plus, FloatFunctions.square)
    Assert.assertEquals(expected, result, TOL)
  }

/*
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
    Assert.assertEquals(expected, result, TOL)
  }
*/

  def testAggregateFloatMatrix2DFloatFloatFunctionFloatFloatFunction() {
    var expected = 0f
    for (i <- 0 until A.size.toInt) {
      val elemA = A.getQuick(i)
      val elemB = B.getQuick(i)
      expected += elemA * elemB
    }
    val result = A.aggregate(B, FloatFunctions.plus, FloatFunctions.mult)
    Assert.assertEquals(expected, result, TOL)
  }

  def testAssignFloat() {
    val value = Math.random().toFloat
    A.assignConstant(value)
    for (i <- 0 until A.size.toInt) {
      Assert.assertEquals(value, A.getQuick(i), TOL)
    }
  }

  def testAssignFloatArray() {
    val expected = Array.ofDim[Float](A.size.toInt)
    for (i <- 0 until A.size.toInt) {
      expected(i) = Math.random().toFloat
    }
    A.assign(expected)
    for (i <- 0 until A.size.toInt) {
      Assert.assertEquals(expected(i), A.getQuick(i), TOL)
    }
  }

  def testAssignFloatFunction() {
    val Acopy = A.copy()
    A.assign(FloatFunctions.acos)
    for (i <- 0 until A.size.toInt) {
      val expected = Math.acos(Acopy.getQuick(i)).toFloat
      Assert.assertEquals(expected, A.getQuick(i), TOL)
    }
  }

  def testAssignFloatMatrix1D() {
    A.assign(B)
    Assert.assertTrue(A.size == B.size)
    for (i <- 0 until A.size.toInt) {
      Assert.assertEquals(B.getQuick(i), A.getQuick(i), TOL)
    }
  }

  def testAssignFloatMatrix1DFloatFloatFunction() {
    val Acopy = A.copy()
    A.assign(B, FloatFunctions.div)
    for (i <- 0 until A.size.toInt) {
      Assert.assertEquals(Acopy.getQuick(i) / B.getQuick(i), A.getQuick(i), TOL)
    }
  }

  def testAssignFloatProcedureFloat() {
    val procedure = new FloatProcedure() {

      def apply(element: Float): Boolean = {
        Math.abs(element) > 0.1
      }
    }
    val Acopy = A.copy()
    A.assign(procedure, -1.0f)
    for (i <- 0 until A.size.toInt) {
      if (Math.abs(Acopy.getQuick(i)) > 0.1) {
        Assert.assertEquals(-1.0, A.getQuick(i), TOL)
      } else {
        Assert.assertEquals(Acopy.getQuick(i), A.getQuick(i), TOL)
      }
    }
  }

  def testAssignFloatProcedureFloatFunction() {
    val procedure = new FloatProcedure() {

      def apply(element: Float): Boolean = {
        Math.abs(element) > 0.1
      }
    }
    val Acopy = A.copy()
    A.assign(procedure, FloatFunctions.tan)
    for (i <- 0 until A.size.toInt) {
      if (Math.abs(Acopy.getQuick(i)) > 0.1) {
        Assert.assertEquals(Math.tan(Acopy.getQuick(i)), A.getQuick(i), TOL)
      } else {
        Assert.assertEquals(Acopy.getQuick(i), A.getQuick(i), TOL)
      }
    }
  }

  def testCardinality() {
    val card = A.numNonZero
    Assert.assertEquals(A.size.toInt, card)
  }

  def testEqualsFloat() {
    val value = 1f
    A.assignConstant(value)
    var eq = A.everyCellEquals(value)
    Assert.assertTrue(eq)
    eq = A.everyCellEquals(2f)
    Assert.assertFalse(eq)
  }

  def testEqualsObject() {
    var eq = A == A
    Assert.assertTrue(eq)
    eq = A == B
    Assert.assertFalse(eq)
  }

  def testMaxLocation() {
    A.assignConstant(0)
    A.setQuick(A.size.toInt / 3, 0.7f)
    A.setQuick(A.size.toInt / 2, 0.1f)
    val maxAndLoc = A.getMaxLocation
    Assert.assertEquals(0.7, maxAndLoc._2, TOL)
    Assert.assertEquals(A.size.toInt / 3, maxAndLoc._1)
  }

  def testMinLocation() {
    A.assignConstant(0)
    A.setQuick(A.size.toInt / 3, -0.7f)
    A.setQuick(A.size.toInt / 2, -0.1f)
    val minAndLoc = A.getMinLocation
    Assert.assertEquals(-0.7f, minAndLoc._2, TOL)
    Assert.assertEquals(A.size.toInt / 3, minAndLoc._1)
  }

/*
  def testGetNegativeValuesIntArrayListFloatArrayList() {
    A.assignConstant(0f)
    A.setQuick(A.size.toInt / 3, -0.7f)
    A.setQuick(A.size.toInt / 2, -0.1f)
    val indexList = new IntArrayList()
    val valueList = new FloatArrayList()
    A.getNegativeValues(indexList, valueList)
    Assert.assertEquals(2, indexList.size)
    Assert.assertEquals(2, valueList.size)
    Assert.assertTrue(indexList.contains(A.size.toInt / 3))
    Assert.assertTrue(indexList.contains(A.size.toInt / 2))
    Assert.assertTrue(valueList.contains(-0.7f))
    Assert.assertTrue(valueList.contains(-0.1f))
  }
*/

/*
  def testGetNonZerosIntArrayListFloatArrayList() {
    A.assign(0)
    A.setQuick(A.size.toInt / 3, 0.7f)
    A.setQuick(A.size.toInt / 2, 0.1f)
    val indexList = new IntArrayList()
    val valueList = new ArrayList[Float]()
    A.getNonZeros(indexList, valueList)
    Assert.assertEquals(2, indexList.size)
    Assert.assertEquals(2, valueList.size)
    Assert.assertTrue(indexList.contains(A.size.toInt / 3))
    Assert.assertTrue(indexList.contains(A.size.toInt / 2))
    Assert.assertTrue(valueList.contains(0.7f))
    Assert.assertTrue(valueList.contains(0.1f))
  }

  def testGetPositiveValuesIntArrayListFloatArrayList() {
    A.assignConstant(0)
    A.setQuick(A.size.toInt / 3, 0.7f)
    A.setQuick(A.size.toInt / 2, 0.1f)
    val indexList = new IntArrayList()
    val valueList = new FloatArrayList()
    A.getPositiveValues(indexList, valueList)
    Assert.assertEquals(2, indexList.size)
    Assert.assertEquals(2, valueList.size)
    Assert.assertTrue(indexList.contains(A.size.toInt / 3))
    Assert.assertTrue(indexList.contains(A.size.toInt / 2))
    Assert.assertTrue(valueList.contains(0.7f))
    Assert.assertTrue(valueList.contains(0.1f))
  }
*/

  def testToArray() {
    val array = A.toArray
    Assert.assertTrue(A.size.toInt == array.length)
    for (i <- 0 until A.size.toInt) {
      Assert.assertEquals(array(i), A.getQuick(i), TOL)
    }
  }

  def testToArrayFloatArray() {
    val array = Array.ofDim[Float](A.size.toInt)
    A.toArray(array)
    for (i <- 0 until A.size.toInt) {
      Assert.assertEquals(A.getQuick(i), array(i), TOL)
    }
  }

  def testReshapeIntInt() {
    val rows = 10
    val columns = 17
    val B = A.reshape(rows, columns)
    var idx = 0
    for (c <- 0 until columns; r <- 0 until rows) {
      Assert.assertEquals(A.getQuick(idx), B.getQuick(r, c), TOL)
      idx += 1
    }
  }

/*
  def testReshapeIntIntInt() {
    val slices = 2
    val rows = 5
    val columns = 17
    val B = A.reshape(slices, rows, columns)
    var idx = 0
    for (s <- 0 until slices; c <- 0 until columns; r <- 0 until rows) {
      Assert.assertEquals(A.getQuick(idx += 1), B.getQuick(s, r, c), TOL)
    }
  }
*/

/*
  def testSwap() {
    val Acopy = A.copy()
    val Bcopy = B.copy()
    A.swap(B)
    for (i <- 0 until A.size.toInt) {
      Assert.assertEquals(Bcopy.getQuick(i), A.getQuick(i), TOL)
      Assert.assertEquals(Acopy.getQuick(i), B.getQuick(i), TOL)
    }
  }
*/

  def testViewFlip() {
    val b = A.viewFlip()
    Assert.assertEquals(A.size.toInt, b.size)
    for (i <- 0 until A.size.toInt) {
      Assert.assertEquals(A.getQuick(i), b.getQuick(A.size.toInt - 1 - i), TOL)
    }
  }

  def testViewPart() {
    val b = A.viewPart(15, 11)
    for (i <- 0 until 11) {
      Assert.assertEquals(A.getQuick(15 + i), b.getQuick(i), TOL)
    }
  }

  def testViewSelectionFloatProcedure() {
    val b = A.viewSelection(new Procedure1[Float]() {

      def apply(element: Float): Boolean = element % 2 == 0
    })
    for (i <- 0 until b.size.toInt) {
      val el = b.getQuick(i)
      Assert.assertEquals(0, el % 2, TOL)
    }
  }

  def testViewSelectionIntArray() {
    val indexes = Array(5, 11, 22, 37, 101)
    val b = A.viewSelection(indexes)
    for (i <- 0 until indexes.length) {
      Assert.assertEquals(A.getQuick(indexes(i)), b.getQuick(i), TOL)
    }
  }

/*
  def testViewSorted() {
    val b = A.viewSorted()
    for (i <- 0 until A.size.toInt - 1) {
      Assert.assertTrue(b.getQuick(i + 1) >= b.getQuick(i))
    }
  }
*/

  def testViewStrides() {
    val stride = 3
    val b = A.viewStrides(stride)
    for (i <- 0 until b.size.toInt) {
      Assert.assertEquals(A.getQuick(i * stride), b.getQuick(i), TOL)
    }
  }

  def testZDotProductFloatMatrix1D() {
    val product = A.dot(B)
    var expected = 0f
    for (i <- 0 until A.size.toInt) {
      expected += A.getQuick(i) * B.getQuick(i)
    }
    Assert.assertEquals(expected, product, TOL)
  }

  def testZDotProductFloatMatrix1DIntInt() {
    val product = A.dot(B, 5, B.size.toInt - 10)
    var expected = 0f
    for (i <- 5 until B.size.toInt - 10) {
      expected += A.getQuick(i) * B.getQuick(i)
    }
    Assert.assertEquals(expected, product, TOL)
  }

/*
  @Test
  def testZDotProductFloatMatrix1DIntIntIntArrayList() {
    val indexList = new IntArrayList()
    val valueList = new FloatArrayList()
    B.getNonZeros(indexList, valueList)
    val product = A.dot(B, 5, B.size.toInt - 10, indexList)
    var expected = 0
    for (i <- 5 until A.size.toInt - 5) {
      expected += A.getQuick(i) * B.getQuick(i)
    }
    Assert.assertEquals(expected, product, TOL)
  }
*/

  def testZSum() {
    val sum = A.sumAllCells
    var expected = 0f
    for (i <- 0 until A.size.toInt) {
      expected += A.getQuick(i)
    }
    Assert.assertEquals(expected, sum, TOL)
  }
}
