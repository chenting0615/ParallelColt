package cern.colt.matrix.tdouble.impl

import junit.framework.TestCase
import org.junit.{Assert, Test}
import cern.jet.math.tdouble.DoubleFunctions
import edu.emory.mathcs.utils.ConcurrencyUtils
import cern.colt.matrix.impl.StrideMatrix1D
import it.unimi.dsi.fastutil.ints.IntArrayList
import cern.colt.function.Procedure1
import it.unimi.dsi.fastutil.doubles.DoubleArrayList

abstract class StrideMatrix1DTest(arg0: String) extends TestCase(arg0) {

  /**
   * Matrix to test
   */
  protected var A: StrideMatrix1D[Double] = _

  /**
   * Matrix of the same size as a
   */
  protected var B: StrideMatrix1D[Double] = _

  protected var SIZE: Int = 2 * 17 * 5

  protected var TOL: Double = 1e-10

  override protected def setUp() {
    createMatrices()
    populateMatrices()
  }

  protected def createMatrices(): Unit

  protected def populateMatrices() {
    ConcurrencyUtils.setThreadsBeginN_1D(1)
    for (i <- 0 until A.size.toInt) {
      A.setQuick(i, Math.random())
    }
    for (i <- 0 until B.size.toInt) {
      B.setQuick(i, Math.random())
    }
  }

  override protected def tearDown() {
    A = null
    B = null
  }

  def testAggregateDoubleDoubleFunctionDoubleFunction() {
    var expected = 0
    for (i <- 0 until A.size.toInt) {
      val elem = A.getQuick(i)
      expected += elem * elem
    }
    val result = A.aggregate(DoubleFunctions.plus, DoubleFunctions.square)
    Assert.assertEquals(expected, result, TOL)
  }

  def testAggregateDoubleDoubleFunctionDoubleFunctionIntArrayList() {
    val indexList = new IntArrayList()
    for (i <- 0 until A.size.toInt) {
      indexList.add(i)
    }
    var expected = 0
    for (i <- 0 until A.size.toInt) {
      val elem = A.getQuick(i)
      expected += elem * elem
    }
    val result = A.aggregate(DoubleFunctions.plus, DoubleFunctions.square, indexList)
    Assert.assertEquals(expected, result, TOL)
  }

  def testAggregateDoubleMatrix2DDoubleDoubleFunctionDoubleDoubleFunction() {
    var expected = 0
    for (i <- 0 until A.size.toInt) {
      val elemA = A.getQuick(i)
      val elemB = B.getQuick(i)
      expected += elemA * elemB
    }
    val result = A.aggregate(B, DoubleFunctions.plus, DoubleFunctions.mult)
    Assert.assertEquals(expected, result, TOL)
  }

  def testAssignDouble() {
    val value = Math.random()
    A.assignConstant(value)
    for (i <- 0 until A.size.toInt) {
      Assert.assertEquals(value, A.getQuick(i), TOL)
    }
  }

  def testAssignDoubleArray() {
    val expected = Array.ofDim[Double](A.size.toInt)
    for (i <- 0 until A.size.toInt) {
      expected(i) = Math.random()
    }
    A.assign(expected)
    for (i <- 0 until A.size.toInt) {
      Assert.assertEquals(expected(i), A.getQuick(i), TOL)
    }
  }

  def testAssignDoubleFunction() {
    val Acopy = A.copy()
    A.assign(DoubleFunctions.acos)
    for (i <- 0 until A.size.toInt) {
      val expected = Math.acos(Acopy.getQuick(i))
      Assert.assertEquals(expected, A.getQuick(i), TOL)
    }
  }

  def testAssignDoubleMatrix1D() {
    A.assign(B)
    Assert.assertTrue(A.size == B.size)
    for (i <- 0 until A.size.toInt) {
      Assert.assertEquals(B.getQuick(i), A.getQuick(i), TOL)
    }
  }

  def testAssignDoubleMatrix1DDoubleDoubleFunction() {
    val Acopy = A.copy()
    A.assign(B, DoubleFunctions.div)
    for (i <- 0 until A.size.toInt) {
      Assert.assertEquals(Acopy.getQuick(i) / B.getQuick(i), A.getQuick(i), TOL)
    }
  }

  def testAssignDoubleProcedureDouble() {
    val procedure = new Procedure1[Double]() {

      def apply(element: Double): Boolean = {
        Math.abs(element) > 0.1
      }
    }
    val Acopy = A.copy()
    A.assign(procedure, -1.0)
    for (i <- 0 until A.size.toInt) {
      if (Math.abs(Acopy.getQuick(i)) > 0.1) {
        Assert.assertEquals(-1.0, A.getQuick(i), TOL)
      } else {
        Assert.assertEquals(Acopy.getQuick(i), A.getQuick(i), TOL)
      }
    }
  }

  def testAssignDoubleProcedureDoubleFunction() {
    val procedure = new Procedure1[Double]() {

      def apply(element: Double): Boolean = {
        Math.abs(element) > 0.1
      }
    }
    val Acopy = A.copy()
    A.assign(procedure, DoubleFunctions.tan)
    for (i <- 0 until A.size.toInt) {
      if (Math.abs(Acopy.getQuick(i)) > 0.1) {
        Assert.assertEquals(Math.tan(Acopy.getQuick(i)), A.getQuick(i), TOL)
      } else {
        Asert.assertEquals(Acopy.getQuick(i), A.getQuick(i), TOL)
      }
    }
  }

  def testCardinality() {
    val card = A.numNonZero()
    Assert.assertEquals(A.size.toInt, card)
  }

  def testEqualsDouble() {
    val value = 1
    A.assignConstant(value)
    var eq = A == value
    Assert.assertTrue(eq)
    eq = A == 2
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
    A.setQuick(A.size.toInt / 3, 0.7)
    A.setQuick(A.size.toInt / 2, 0.1)
    val maxAndLoc = A.getMaxLocation
    Assert.assertEquals(0.7, maxAndLoc(0), TOL)
    Assert.assertEquals(A.size.toInt / 3, maxAndLoc(1).toInt)
  }

  def testMinLocation() {
    A.assignConstant(0)
    A.setQuick(A.size.toInt / 3, -0.7)
    A.setQuick(A.size.toInt / 2, -0.1)
    val minAndLoc = A.getMinLocation
    Assert.assertEquals(-0.7, minAndLoc(0), TOL)
    Assert.assertEquals(A.size.toInt / 3, minAndLoc(1).toInt)
  }

  def testGetNegativeValuesIntArrayListDoubleArrayList() {
    A.assignConstant(0)
    A.setQuick(A.size.toInt / 3, -0.7)
    A.setQuick(A.size.toInt / 2, -0.1)
    val indexList = new IntArrayList()
    val valueList = new DoubleArrayList()
    A.getNegativeValues(indexList, valueList)
    Assert.assertEquals(2, indexList.size)
    Assert.assertEquals(2, valueList.size)
    Assert.assertTrue(indexList.contains(A.size.toInt / 3))
    Assert.assertTrue(indexList.contains(A.size.toInt / 2))
    Assert.assertTrue(valueList.contains(-0.7))
    Assert.assertTrue(valueList.contains(-0.1))
  }

  def testGetNonZerosIntArrayListDoubleArrayList() {
    A.assignConstant(0)
    A.setQuick(A.size.toInt / 3, 0.7)
    A.setQuick(A.size.toInt / 2, 0.1)
    val indexList = new IntArrayList()
    val valueList = new DoubleArrayList()
    A.getNonZeros(indexList, valueList)
    Assert.assertEquals(2, indexList.size)
    Assert.assertEquals(2, valueList.size)
    Assert.assertTrue(indexList.contains(A.size.toInt / 3))
    Assert.assertTrue(indexList.contains(A.size.toInt / 2))
    Assert.assertTrue(valueList.contains(0.7))
    Assert.assertTrue(valueList.contains(0.1))
  }

  def testGetPositiveValuesIntArrayListDoubleArrayList() {
    A.assignConstant(0)
    A.setQuick(A.size.toInt / 3, 0.7)
    A.setQuick(A.size.toInt / 2, 0.1)
    val indexList = new IntArrayList()
    val valueList = new DoubleArrayList()
    A.getPositiveValues(indexList, valueList)
    Assert.assertEquals(2, indexList.size)
    Assert.assertEquals(2, valueList.size)
    Assert.assertTrue(indexList.contains(A.size.toInt / 3))
    Assert.assertTrue(indexList.contains(A.size.toInt / 2))
    Assert.assertTrue(valueList.contains(0.7))
    Assert.assertTrue(valueList.contains(0.1))
  }

  def testToArray() {
    val array = A.toArray
    Assert.assertTrue(A.size.toInt == array.length)
    for (i <- 0 until A.size.toInt) {
      Assert.assertEquals(array(i), A.getQuick(i), TOL)
    }
  }

  def testToArrayDoubleArray() {
    val array = Array.ofDim[Double](A.size.toInt)
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

/* 3D
  def testReshapeIntIntInt() {
    val slices = 2
    val rows = 5
    val columns = 17
    val B = A.reshape(slices, rows, columns)
    var idx = 0
    for (s <- 0 until slices; c <- 0 until columns; r <- 0 until rows) {
      Assert.assertEquals(A.getQuick(idx), B.getQuick(s, r, c), TOL)
      idx += 1
    }
  }
*/

  def testSwap() {
    val Acopy = A.copy()
    val Bcopy = B.copy()
    A.swap(B)
    for (i <- 0 until A.size.toInt) {
      Assert.assertEquals(Bcopy.getQuick(i), A.getQuick(i), TOL)
      Assert.assertEquals(Acopy.getQuick(i), B.getQuick(i), TOL)
    }
  }

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

  def testViewSelectionDoubleProcedure() {
    val b = A.viewSelection(new Procedure1[Double]() {

      def apply(element: Double): Boolean = element % 2 == 0
    })
    for (i <- 0 until b.size.toInt) {
      val el = b.getQuick(i)
      if (el % 2 != 0) {
        Assert.fail()
      }
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
    for (i <- 0 until b.size) {
      Assert.assertEquals(A.getQuick(i * stride), b.getQuick(i), TOL)
    }
  }

  def testZDotProductDoubleMatrix1D() {
    val product = A.zDotProduct(B)
    var expected = 0
    for (i <- 0 until A.size.toInt) {
      expected += A.getQuick(i) * B.getQuick(i)
    }
    Assert.assertEquals(expected, product, TOL)
  }

  def testZDotProductDoubleMatrix1DIntInt() {
    val product = A.zDotProduct(B, 5, B.size.toInt - 10)
    var expected = 0
    for (i <- 5 until A.size.toInt - 5) {
      expected += A.getQuick(i) * B.getQuick(i)
    }
    Assert.assertEquals(expected, product, TOL)
  }

  @Test
  def testZDotProductDoubleMatrix1DIntIntIntArrayList() {
    val indexList = new IntArrayList()
    val valueList = new DoubleArrayList()
    B.getNonZeros(indexList, valueList)
    val product = A.zDotProduct(B, 5, B.size.toInt - 10, indexList)
    var expected = 0
    for (i <- 5 until A.size.toInt - 5) {
      expected += A.getQuick(i) * B.getQuick(i)
    }
    Assert.assertEquals(expected, product, TOL)
  }

  def testZSum() {
    val sum = A.zSum()
    var expected = 0
    for (i <- 0 until A.size.toInt) {
      expected += A.getQuick(i)
    }
    Assert.assertEquals(expected, sum, TOL)
  }
}
