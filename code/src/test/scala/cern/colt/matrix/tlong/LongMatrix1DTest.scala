package cern.colt.matrix.tlong

import java.util.Random
import junit.framework.TestCase
import org.junit.Test
import cern.colt.function.tlong.LongProcedure
import cern.colt.list.tint.IntArrayList
import cern.colt.list.tlong.LongArrayList
import cern.jet.math.tlong.LongFunctions
import edu.emory.mathcs.utils.ConcurrencyUtils
//remove if not needed
import scala.collection.JavaConversions._

abstract class LongMatrix1DTest(arg0: String) extends TestCase(arg0) {

  /**
   * Matrix to test
   */
  protected var A: LongMatrix1D = _

  /**
   * Matrix of the same size as a
   */
  protected var B: LongMatrix1D = _

  protected var SIZE: Int = 2 * 17 * 5

  protected var rand: Random = new Random(0)

  protected def setUp() {
    createMatrices()
    populateMatrices()
  }

  protected def createMatrices(): Unit

  protected def populateMatrices() {
    ConcurrencyUtils.setThreadsBeginN_1D(1)
    for (i <- 0 until A.size.toInt) {
      A.setQuick(i, Math.max(1, rand.nextLong() % A.size))
    }
    for (i <- 0 until B.size.toInt) {
      B.setQuick(i, Math.max(1, rand.nextLong() % A.size))
    }
  }

  protected def tearDown() {
    A = B = null
  }

  def testAggregateLongLongFunctionLongFunction() {
    var expected = 0
    for (i <- 0 until A.size.toInt) {
      val elem = A.getQuick(i)
      expected += elem * elem
    }
    val result = A.aggregate(LongFunctions.plus, LongFunctions.square)
    assertEquals(expected, result)
  }

  def testAggregateLongLongFunctionLongFunctionIntArrayList() {
    val indexList = new IntArrayList()
    for (i <- 0 until A.size.toInt) {
      indexList.add(i)
    }
    var expected = 0
    for (i <- 0 until A.size.toInt) {
      val elem = A.getQuick(i)
      expected += elem * elem
    }
    val result = A.aggregate(LongFunctions.plus, LongFunctions.square, indexList)
    assertEquals(expected, result)
  }

  def testAggregateLongMatrix2DLongLongFunctionLongLongFunction() {
    var expected = 0
    for (i <- 0 until A.size.toInt) {
      val elemA = A.getQuick(i)
      val elemB = B.getQuick(i)
      expected += elemA * elemB
    }
    val result = A.aggregate(B, LongFunctions.plus, LongFunctions.mult)
    assertEquals(expected, result)
  }

  def testAssignLong() {
    val value = rand.nextLong()
    A.assign(value)
    for (i <- 0 until A.size.toInt) {
      assertEquals(value, A.getQuick(i))
    }
  }

  def testAssignLongArray() {
    val expected = Array.ofDim[Long](A.size.toInt)
    for (i <- 0 until A.size.toInt) {
      expected(i) = rand.nextLong()
    }
    A.assign(expected)
    for (i <- 0 until A.size.toInt) {
      assertEquals(expected(i), A.getQuick(i))
    }
  }

  def testAssignLongFunction() {
    val Acopy = A.copy()
    A.assign(LongFunctions.neg)
    for (i <- 0 until A.size.toInt) {
      val expected = -Acopy.getQuick(i)
      assertEquals(expected, A.getQuick(i))
    }
  }

  def testAssignLongMatrix1D() {
    A.assign(B)
    assertTrue(A.size == B.size)
    for (i <- 0 until A.size.toInt) {
      assertEquals(B.getQuick(i), A.getQuick(i))
    }
  }

  def testAssignLongMatrix1DLongLongFunction() {
    val Acopy = A.copy()
    A.assign(B, LongFunctions.plus)
    for (i <- 0 until A.size.toInt) {
      assertEquals(Acopy.getQuick(i) + B.getQuick(i), A.getQuick(i))
    }
  }

  def testAssignLongProcedureLong() {
    val procedure = new LongProcedure() {

      def apply(element: Long): Boolean = {
        if (Math.abs(element) > 1) {
          return true
        } else {
          return false
        }
      }
    }
    val Acopy = A.copy()
    A.assign(procedure, -1)
    for (i <- 0 until A.size.toInt) {
      if (Math.abs(Acopy.getQuick(i)) > 1) {
        assertEquals(-1, A.getQuick(i))
      } else {
        assertEquals(Acopy.getQuick(i), A.getQuick(i))
      }
    }
  }

  def testAssignLongProcedureLongFunction() {
    val procedure = new LongProcedure() {

      def apply(element: Long): Boolean = {
        if (Math.abs(element) > 1) {
          return true
        } else {
          return false
        }
      }
    }
    val Acopy = A.copy()
    A.assign(procedure, LongFunctions.neg)
    for (i <- 0 until A.size.toInt) {
      if (Math.abs(Acopy.getQuick(i)) > 1) {
        assertEquals(-Acopy.getQuick(i), A.getQuick(i))
      } else {
        assertEquals(Acopy.getQuick(i), A.getQuick(i))
      }
    }
  }

  def testCardinality() {
    val card = A.cardinality()
    var expected = 0
    for (i <- 0 until A.size if A.getQuick(i) != 0) expected += 1
    assertEquals(expected, card)
  }

  def testEqualsLong() {
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
    A.setQuick(A.size.toInt / 3, 7)
    A.setQuick(A.size.toInt / 2, 1)
    val maxAndLoc = A.getMaxLocation
    assertEquals(7, maxAndLoc(0))
    assertEquals(A.size.toInt / 3, maxAndLoc(1).toInt)
  }

  def testMinLocation() {
    A.assign(0)
    A.setQuick(A.size.toInt / 3, -7)
    A.setQuick(A.size.toInt / 2, -1)
    val minAndLoc = A.getMinLocation
    assertEquals(-7, minAndLoc(0))
    assertEquals(A.size.toInt / 3, minAndLoc(1).toInt)
  }

  def testGetNegativeValuesIntArrayListLongArrayList() {
    A.assign(0)
    A.setQuick(A.size.toInt / 3, -7)
    A.setQuick(A.size.toInt / 2, -1)
    val indexList = new IntArrayList()
    val valueList = new LongArrayList()
    A.getNegativeValues(indexList, valueList)
    assertEquals(2, indexList.size)
    assertEquals(2, valueList.size)
    assertTrue(indexList.contains(A.size.toInt / 3))
    assertTrue(indexList.contains(A.size.toInt / 2))
    assertTrue(valueList.contains(-7))
    assertTrue(valueList.contains(-1))
  }

  def testGetNonZerosIntArrayListLongArrayList() {
    A.assign(0)
    A.setQuick(A.size.toInt / 3, 7)
    A.setQuick(A.size.toInt / 2, 1)
    val indexList = new IntArrayList()
    val valueList = new LongArrayList()
    A.getNonZeros(indexList, valueList)
    assertEquals(2, indexList.size)
    assertEquals(2, valueList.size)
    assertTrue(indexList.contains(A.size.toInt / 3))
    assertTrue(indexList.contains(A.size.toInt / 2))
    assertTrue(valueList.contains(7))
    assertTrue(valueList.contains(1))
  }

  def testGetPositiveValuesIntArrayListLongArrayList() {
    A.assign(0)
    A.setQuick(A.size.toInt / 3, 7)
    A.setQuick(A.size.toInt / 2, 1)
    val indexList = new IntArrayList()
    val valueList = new LongArrayList()
    A.getPositiveValues(indexList, valueList)
    assertEquals(2, indexList.size)
    assertEquals(2, valueList.size)
    assertTrue(indexList.contains(A.size.toInt / 3))
    assertTrue(indexList.contains(A.size.toInt / 2))
    assertTrue(valueList.contains(7))
    assertTrue(valueList.contains(1))
  }

  def testToArray() {
    val array = A.toArray()
    assertTrue(A.size.toInt == array.length)
    for (i <- 0 until A.size.toInt) {
      assertEquals(array(i), A.getQuick(i))
    }
  }

  def testToArrayLongArray() {
    val array = Array.ofDim[Long](A.size.toInt)
    A.toArray(array)
    for (i <- 0 until A.size.toInt) {
      assertEquals(A.getQuick(i), array(i))
    }
  }

  def testReshapeIntInt() {
    val rows = 10
    val columns = 17
    val B = A.reshape(rows, columns)
    var idx = 0
    for (c <- 0 until columns; r <- 0 until rows) {
      assertEquals(A.getQuick(idx += 1), B.getQuick(r, c))
    }
  }

  def testReshapeIntIntInt() {
    val slices = 2
    val rows = 5
    val columns = 17
    val B = A.reshape(slices, rows, columns)
    var idx = 0
    for (s <- 0 until slices; c <- 0 until columns; r <- 0 until rows) {
      assertEquals(A.getQuick(idx += 1), B.getQuick(s, r, c))
    }
  }

  def testSwap() {
    val Acopy = A.copy()
    val Bcopy = B.copy()
    A.swap(B)
    for (i <- 0 until A.size.toInt) {
      assertEquals(Bcopy.getQuick(i), A.getQuick(i))
      assertEquals(Acopy.getQuick(i), B.getQuick(i))
    }
  }

  def testViewFlip() {
    val b = A.viewFlip()
    assertEquals(A.size.toInt, b.size)
    for (i <- 0 until A.size.toInt) {
      assertEquals(A.getQuick(i), b.getQuick(A.size.toInt - 1 - i))
    }
  }

  def testViewPart() {
    val b = A.viewPart(15, 11)
    for (i <- 0 until 11) {
      assertEquals(A.getQuick(15 + i), b.getQuick(i))
    }
  }

  def testViewSelectionLongProcedure() {
    val b = A.viewSelection(new LongProcedure() {

      def apply(element: Long): Boolean = return element % 2 == 0
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
      assertEquals(A.getQuick(indexes(i)), b.getQuick(i))
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
      assertEquals(A.getQuick(i * stride), b.getQuick(i))
    }
  }

  def testZDotProductLongMatrix1D() {
    val product = A.zDotProduct(B)
    var expected = 0
    for (i <- 0 until A.size.toInt) {
      expected += A.getQuick(i) * B.getQuick(i)
    }
    assertEquals(expected, product)
  }

  def testZDotProductLongMatrix1DIntInt() {
    val product = A.zDotProduct(B, 5, B.size.toInt - 10)
    var expected = 0
    for (i <- 5 until A.size.toInt - 5) {
      expected += A.getQuick(i) * B.getQuick(i)
    }
    assertEquals(expected, product)
  }

  @Test
  def testZDotProductLongMatrix1DIntIntIntArrayList() {
    val indexList = new IntArrayList()
    val valueList = new LongArrayList()
    B.getNonZeros(indexList, valueList)
    val product = A.zDotProduct(B, 5, B.size.toInt - 10, indexList)
    var expected = 0
    for (i <- 5 until A.size.toInt - 5) {
      expected += A.getQuick(i) * B.getQuick(i)
    }
    assertEquals(expected, product)
  }

  def testZSum() {
    val sum = A.zSum()
    var expected = 0
    for (i <- 0 until A.size.toInt) {
      expected += A.getQuick(i)
    }
    assertEquals(expected, sum)
  }
}
