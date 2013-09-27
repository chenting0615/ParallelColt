package cern.colt.matrix.tdcomplex

import java.util.ArrayList
import junit.framework.TestCase
import cern.colt.function.tdcomplex.DComplexProcedure
import cern.colt.list.tint.IntArrayList
import cern.colt.matrix.tdouble.DoubleFactory1D
import cern.colt.matrix.tdouble.StrideMatrix1D
import cern.jet.math.tdcomplex.DComplex
import cern.jet.math.tdcomplex.DComplexFunctions
import edu.emory.mathcs.utils.ConcurrencyUtils
//remove if not needed
import scala.collection.JavaConversions._

abstract class DComplexMatrix1DTest(arg0: String) extends TestCase(arg0) {

  /**
   * Matrix to test
   */
  protected var A: DComplexMatrix1D = _

  /**
   * Matrix of the same size as a
   */
  protected var B: DComplexMatrix1D = _

  protected var SIZE: Int = 2 * 17 * 5

  protected var TOL: Double = 1e-10

  protected var F: cern.jet.math.tdouble.DoubleFunctions = cern.jet.math.tdouble.DoubleFunctions.functions

  protected def setUp() {
    createMatrices()
    populateMatrices()
  }

  protected def createMatrices(): Unit

  protected def populateMatrices() {
    ConcurrencyUtils.setThreadsBeginN_1D(1)
    for (i <- 0 until A.size.toInt) {
      A.setQuick(i, Array(Math.random(), Math.random()))
    }
    for (i <- 0 until A.size.toInt) {
      B.setQuick(i, Array(Math.random(), Math.random()))
    }
  }

  protected def tearDown() {
    A = B = null
  }

  def testAggregateDoubleDoubleFunctionDoubleFunction() {
    var expected = Array.ofDim[Double](2)
    for (i <- 0 until A.size.toInt) {
      expected = DComplex.plus(expected, DComplex.square(A.getQuick(i)))
    }
    val result = A.aggregate(DComplexFunctions.plus, DComplexFunctions.square)
    assertEquals(expected, result, TOL)
  }

  def testAggregateComplexMatrix1DComplexComplexFunctionComplexComplexFunction() {
    val actual = A.aggregate(B, DComplexFunctions.plus, DComplexFunctions.mult)
    var expected = Array.ofDim[Double](2)
    for (i <- 0 until A.size.toInt) {
      expected = DComplex.plus(expected, DComplex.mult(A.getQuick(i), B.getQuick(i)))
    }
    assertEquals(expected, actual, TOL)
  }

  def testAssignComplexComplexFunction() {
    val Acopy = A.copy()
    A.assign(DComplexFunctions.acos)
    for (i <- 0 until A.size.toInt) {
      val expected = DComplex.acos(Acopy.getQuick(i))
      assertEquals(expected, A.getQuick(i), TOL)
    }
  }

  def testAssignComplexMatrix1D() {
    A.assign(B)
    assertTrue(A.size == B.size)
    for (i <- 0 until A.size.toInt) {
      assertEquals(B.getQuick(i), A.getQuick(i), TOL)
    }
  }

  def testAssignComplexMatrix1DComplexComplexFunction() {
    val Acopy = A.copy()
    A.assign(B, DComplexFunctions.div)
    for (i <- 0 until A.size.toInt) {
      assertEquals(DComplex.div(Acopy.getQuick(i), B.getQuick(i)), A.getQuick(i), TOL)
    }
  }

  def testAssignComplexProcedureComplexComplexFunction() {
    val procedure = new DComplexProcedure() {

      def apply(element: Array[Double]): Boolean = {
        if (DComplex.abs(element) > 0.1) {
          return true
        } else {
          return false
        }
      }
    }
    val Acopy = A.copy()
    A.assign(procedure, DComplexFunctions.tan)
    for (i <- 0 until A.size.toInt) {
      if (DComplex.abs(Acopy.getQuick(i)) > 0.1) {
        assertEquals(DComplex.tan(Acopy.getQuick(i)), A.getQuick(i), TOL)
      } else {
        assertEquals(Acopy.getQuick(i), A.getQuick(i), TOL)
      }
    }
  }

  def testAssignComplexProcedureDoubleArray() {
    val procedure = new DComplexProcedure() {

      def apply(element: Array[Double]): Boolean = {
        if (DComplex.abs(element) > 0.1) {
          return true
        } else {
          return false
        }
      }
    }
    val Acopy = A.copy()
    val value = Array(-1, -1)
    A.assign(procedure, value)
    for (i <- 0 until A.size.toInt) {
      if (DComplex.abs(Acopy.getQuick(i)) > 0.1) {
        assertEquals(value, A.getQuick(i), TOL)
      } else {
        assertEquals(Acopy.getQuick(i), A.getQuick(i), TOL)
      }
    }
  }

  def testAssignComplexRealFunction() {
    val Acopy = A.copy()
    A.assign(DComplexFunctions.abs)
    for (i <- 0 until A.size.toInt) {
      val elem = A.getQuick(i)
      assertEquals(DComplex.abs(Acopy.getQuick(i)), elem(0), TOL)
      assertEquals(0, elem(1), TOL)
    }
  }

  def testAssignDoubleArray() {
    val expected = Array.ofDim[Double](2 * A.size.toInt)
    for (i <- 0 until 2 * A.size.toInt) {
      expected(i) = Math.random()
    }
    A.assign(expected)
    for (i <- 0 until A.size.toInt) {
      val elem = A.getQuick(i)
      assertEquals(expected(2 * i), elem(0), TOL)
      assertEquals(expected(2 * i + 1), elem(1), TOL)
    }
  }

  def testAssignDoubleDouble() {
    val re = Math.random()
    val im = Math.random()
    A.assign(re, im)
    for (i <- 0 until A.size.toInt) {
      val elem = A.getQuick(i)
      assertEquals(re, elem(0), TOL)
      assertEquals(im, elem(1), TOL)
    }
  }

  def testAssignImaginary() {
    val Acopy = A.copy()
    val Im = DoubleFactory1D.dense.random(A.size.toInt)
    A.assignImaginary(Im)
    for (i <- 0 until A.size.toInt) {
      val elem = A.getQuick(i)
      assertEquals(Acopy.getQuick(i)(0), elem(0), TOL)
      assertEquals(Im.getQuick(i), elem(1), TOL)
    }
  }

  def testAssignReal() {
    val Acopy = A.copy()
    val Re = DoubleFactory1D.dense.random(A.size.toInt)
    A.assignReal(Re)
    for (i <- 0 until A.size.toInt) {
      val elem = A.getQuick(i)
      assertEquals(Acopy.getQuick(i)(1), elem(1), TOL)
      assertEquals(Re.getQuick(i), elem(0), TOL)
    }
  }

  def testCardinality() {
    val card = A.cardinality()
    assertEquals(A.size.toInt, card)
  }

  def testEqualsDouble() {
    val value = Array(1, 2)
    A.assign(value(0), value(1))
    var eq = A == value
    assertTrue(eq)
    eq = A == Array(2, 2)
    assertFalse(eq)
  }

  def testEqualsObject() {
    var eq = A == A
    assertTrue(eq)
    eq = A == B
    assertFalse(eq)
  }

  def testGetImaginaryPart() {
    val Im = A.getImaginaryPart
    for (i <- 0 until A.size.toInt) {
      assertEquals(A.getQuick(i)(1), Im.getQuick(i), TOL)
    }
  }

  def testGetRealPart() {
    val Re = A.getRealPart
    for (i <- 0 until A.size.toInt) {
      assertEquals(A.getQuick(i)(0), Re.getQuick(i), TOL)
    }
  }

  def testGetNonZerosIntArrayListArrayListOfdouble() {
    val indexList = new IntArrayList()
    val valueList = new ArrayList[Array[Double]]()
    A.getNonZeros(indexList, valueList)
    assertEquals(A.size.toInt, indexList.size)
    assertEquals(A.size.toInt, valueList.size)
    for (i <- 0 until A.size.toInt) {
      assertEquals(A.getQuick(indexList.get(i)), valueList.get(i), TOL)
      assertTrue(valueList.get(i)(0) != 0 || valueList.get(i)(1) != 0)
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

  def testToArray() {
    val array = A.toArray()
    for (i <- 0 until A.size.toInt) {
      val elem = A.getQuick(i)
      assertEquals(elem(0), array(2 * i), TOL)
      assertEquals(elem(1), array(2 * i + 1), TOL)
    }
  }

  def testToArrayDoubleArray() {
    val array = Array.ofDim[Double](2 * A.size.toInt)
    A.toArray(array)
    for (i <- 0 until A.size.toInt) {
      val elem = A.getQuick(i)
      assertEquals(elem(0), array(2 * i), TOL)
      assertEquals(elem(1), array(2 * i + 1), TOL)
    }
  }

  def testViewFlip() {
    val B = A.viewFlip()
    for (i <- 0 until A.size.toInt) {
      assertEquals(A.getQuick(A.size.toInt - 1 - i), B.getQuick(i), TOL)
    }
  }

  def testViewPart() {
    val B = A.viewPart(A.size.toInt / 2, A.size.toInt / 3)
    for (i <- 0 until A.size.toInt / 3) {
      assertEquals(A.getQuick(A.size.toInt / 2 + i), B.getQuick(i), TOL)
    }
  }

  def testViewSelectionComplexProcedure() {
    val B = A.viewSelection(new DComplexProcedure() {

      def apply(element: Array[Double]): Boolean = {
        if (element(0) < element(1)) {
          return true
        } else {
          return false
        }
      }
    })
    for (i <- 0 until B.size) {
      val el = B.getQuick(i)
      if (el(0) >= el(1)) {
        fail()
      }
    }
  }

  def testViewSelectionIntArray() {
    val indexes = Array(A.size.toInt / 6, A.size.toInt / 5, A.size.toInt / 4, A.size.toInt / 3, A.size.toInt / 2)
    val B = A.viewSelection(indexes)
    for (i <- 0 until indexes.length) {
      assertEquals(A.getQuick(indexes(i)), B.getQuick(i), TOL)
    }
  }

  def testViewStrides() {
    val stride = 3
    val B = A.viewStrides(stride)
    for (i <- 0 until B.size) {
      assertEquals(A.getQuick(i * stride), B.getQuick(i), TOL)
    }
  }

  def testZDotProductComplexMatrix1D() {
    val actual = A.zDotProduct(B)
    var expected = Array.ofDim[Double](2)
    for (i <- 0 until A.size.toInt) {
      expected = DComplex.plus(expected, DComplex.mult(DComplex.conj(B.getQuick(i)), A.getQuick(i)))
    }
    assertEquals(expected, actual, TOL)
  }

  def testZDotProductComplexMatrix1DIntInt() {
    val actual = A.zDotProduct(B, 5, B.size.toInt - 10)
    var expected = Array.ofDim[Double](2)
    for (i <- 5 until A.size.toInt - 5) {
      expected = DComplex.plus(expected, DComplex.mult(DComplex.conj(B.getQuick(i)), A.getQuick(i)))
    }
    assertEquals(expected, actual, TOL)
  }

  def testZDotProductComplexMatrix1DIntIntIntArrayList() {
    val indexList = new IntArrayList()
    val valueList = new ArrayList[Array[Double]]()
    B.getNonZeros(indexList, valueList)
    val actual = A.zDotProduct(B, 5, B.size.toInt - 10, indexList)
    var expected = Array.ofDim[Double](2)
    for (i <- 5 until A.size.toInt - 5) {
      expected = DComplex.plus(expected, DComplex.mult(A.getQuick(i), DComplex.conj(B.getQuick(i))))
    }
    assertEquals(expected, actual, TOL)
  }

  def testZSum() {
    val actual = A.zSum()
    var expected = Array.ofDim[Double](2)
    for (i <- 0 until A.size.toInt) {
      expected = DComplex.plus(expected, A.getQuick(i))
    }
    assertEquals(expected, actual, TOL)
  }

  protected def assertEquals(expected: Array[Double], actual: Array[Double], tol: Double) {
    for (i <- 0 until actual.length) {
      assertEquals(expected(i), actual(i), tol)
    }
  }
}
