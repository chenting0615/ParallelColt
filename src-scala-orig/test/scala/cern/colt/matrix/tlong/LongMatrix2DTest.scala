package cern.colt.matrix.tlong

import java.util.Random
import junit.framework.TestCase
import cern.colt.function.tlong.IntIntLongFunction
import cern.colt.function.tlong.LongProcedure
import cern.colt.list.tint.IntArrayList
import cern.colt.list.tlong.LongArrayList
import cern.colt.matrix.tlong.impl.DenseLongMatrix1D
import cern.jet.math.tlong.LongFunctions
import edu.emory.mathcs.utils.ConcurrencyUtils
//remove if not needed
import scala.collection.JavaConversions._

abstract class LongMatrix2DTest(arg0: String) extends TestCase(arg0) {

  /**
   * Matrix to test
   */
  protected var A: LongMatrix2D = _

  /**
   * Matrix of the same size as A
   */
  protected var B: LongMatrix2D = _

  /**
   * Matrix of the size A.columns() x A.rows()
   */
  protected var Bt: LongMatrix2D = _

  protected var NROWS: Int = 13

  protected var NCOLUMNS: Int = 17

  protected var rand: Random = new Random(0)

  protected def setUp() {
    createMatrices()
    populateMatrices()
  }

  protected def createMatrices(): Unit

  protected def populateMatrices() {
    ConcurrencyUtils.setThreadsBeginN_2D(1)
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) {
      A.setQuick(r, c, Math.max(1, rand.nextLong() % A.rows()))
    }
    for (r <- 0 until B.rows(); c <- 0 until B.columns()) {
      B.setQuick(r, c, Math.max(1, rand.nextLong() % B.rows()))
    }
    for (r <- 0 until Bt.rows(); c <- 0 until Bt.columns()) {
      Bt.setQuick(r, c, Math.max(1, rand.nextLong() % Bt.rows()))
    }
  }

  protected def tearDown() {
    A = B = Bt = null
  }

  def testAggregateLongLongFunctionLongFunction() {
    var expected = 0
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) {
      val elem = A.getQuick(r, c)
      expected += elem * elem
    }
    val result = A.aggregate(LongFunctions.plus, LongFunctions.square)
    assertEquals(expected, result)
  }

  def testAggregateLongLongFunctionLongFunctionLongProcedure() {
    val procedure = new LongProcedure() {

      def apply(element: Long): Boolean = {
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
    val result = A.aggregate(LongFunctions.plus, LongFunctions.square, procedure)
    assertEquals(expected, result)
  }

  def testAggregateLongLongFunctionLongFunctionIntArrayListIntArrayList() {
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
    val result = A.aggregate(LongFunctions.plus, LongFunctions.square, rowList, columnList)
    assertEquals(expected, result)
  }

  def testAggregateLongMatrix2DLongLongFunctionLongLongFunction() {
    var expected = 0
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) {
      val elemA = A.getQuick(r, c)
      val elemB = B.getQuick(r, c)
      expected += elemA * elemB
    }
    val result = A.aggregate(B, LongFunctions.plus, LongFunctions.mult)
    assertEquals(expected, result)
  }

  def testAssignLong() {
    val value = rand.nextLong()
    A.assign(value)
    val rowList = new IntArrayList()
    val columnList = new IntArrayList()
    val valueList = new LongArrayList()
    A.getNonZeros(rowList, columnList, valueList)
    for (i <- 0 until valueList.size) {
      assertEquals(value, valueList.getQuick(i))
    }
  }

  def testAssignLongArrayArray() {
    val expected = Array.ofDim[Long](A.rows(), A.columns())
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) {
      expected(r)(c) = rand.nextLong()
    }
    A.assign(expected)
    for (r <- 0 until A.rows()) {
      assertTrue(A.columns() == expected(r).length)
      for (c <- 0 until A.columns()) assertEquals(expected(r)(c), A.getQuick(r, c))
    }
  }

  def testAssignLongFunction() {
    val Acopy = A.copy()
    A.assign(LongFunctions.neg)
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) {
      val expected = -Acopy.getQuick(r, c)
      assertEquals(expected, A.getQuick(r, c))
    }
  }

  def testAssignLongMatrix2D() {
    A.assign(B)
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) assertEquals(B.getQuick(r, c), A.getQuick(r, 
      c))
  }

  def testAssignLongMatrix2DLongLongFunction() {
    val Acopy = A.copy()
    A.assign(B, LongFunctions.plus)
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) {
      assertEquals(Acopy.getQuick(r, c) + B.getQuick(r, c), A.getQuick(r, c))
    }
  }

  def testAssignLongMatrix2DLongLongFunctionIntArrayListIntArrayList() {
    val rowList = new IntArrayList()
    val columnList = new IntArrayList()
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) {
      rowList.add(r)
      columnList.add(c)
    }
    val Acopy = A.copy()
    A.assign(B, LongFunctions.plus, rowList, columnList)
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) {
      assertEquals(Acopy.getQuick(r, c) + B.getQuick(r, c), A.getQuick(r, c))
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
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) {
      if (Math.abs(Acopy.getQuick(r, c)) > 1) {
        assertEquals(-1, A.getQuick(r, c))
      } else {
        assertEquals(Acopy.getQuick(r, c), A.getQuick(r, c))
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
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) {
      if (Math.abs(Acopy.getQuick(r, c)) > 1) {
        assertEquals(-Acopy.getQuick(r, c), A.getQuick(r, c))
      } else {
        assertEquals(Acopy.getQuick(r, c), A.getQuick(r, c))
      }
    }
  }

  def testCardinality() {
    val card = A.cardinality()
    var expected = 0
    for (r <- 0 until A.rows(); c <- 0 until A.columns() if A.getQuick(r, c) != 0) expected += 1
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

  def testForEachNonZero() {
    val Acopy = A.copy()
    val function = new IntIntLongFunction() {

      def apply(first: Int, second: Int, third: Long): Long = return -third
    }
    A.forEachNonZero(function)
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) {
      assertEquals(-Acopy.getQuick(r, c), A.getQuick(r, c))
    }
  }

  def testMaxLocation() {
    A.assign(0)
    A.setQuick(A.rows() / 3, A.columns() / 3, 7)
    A.setQuick(A.rows() / 2, A.columns() / 2, 1)
    val maxAndLoc = A.getMaxLocation
    assertEquals(7, maxAndLoc(0))
    assertEquals(A.rows() / 3, maxAndLoc(1).toInt)
    assertEquals(A.columns() / 3, maxAndLoc(2).toInt)
  }

  def testMinLocation() {
    A.assign(0)
    A.setQuick(A.rows() / 3, A.columns() / 3, -7)
    A.setQuick(A.rows() / 2, A.columns() / 2, -1)
    val minAndLoc = A.getMinLocation
    assertEquals(-7, minAndLoc(0))
    assertEquals(A.rows() / 3, minAndLoc(1).toInt)
    assertEquals(A.columns() / 3, minAndLoc(2).toInt)
  }

  def testGetNegativeValues() {
    A.assign(0)
    A.setQuick(A.rows() / 3, A.columns() / 3, -7)
    A.setQuick(A.rows() / 2, A.columns() / 2, -1)
    val rowList = new IntArrayList()
    val columnList = new IntArrayList()
    val valueList = new LongArrayList()
    A.getNegativeValues(rowList, columnList, valueList)
    assertEquals(2, rowList.size)
    assertEquals(2, columnList.size)
    assertEquals(2, valueList.size)
    assertTrue(rowList.contains(A.rows() / 3))
    assertTrue(rowList.contains(A.rows() / 2))
    assertTrue(columnList.contains(A.columns() / 3))
    assertTrue(columnList.contains(A.columns() / 2))
    assertTrue(valueList.contains(-7))
    assertTrue(valueList.contains(-1))
  }

  def testGetNonZeros() {
    A.assign(0)
    A.setQuick(A.rows() / 3, A.columns() / 3, 7)
    A.setQuick(A.rows() / 2, A.columns() / 2, 1)
    val rowList = new IntArrayList()
    val columnList = new IntArrayList()
    val valueList = new LongArrayList()
    A.getNonZeros(rowList, columnList, valueList)
    assertEquals(2, rowList.size)
    assertEquals(2, columnList.size)
    assertEquals(2, valueList.size)
    assertTrue(rowList.contains(A.rows() / 3))
    assertTrue(rowList.contains(A.rows() / 2))
    assertTrue(columnList.contains(A.columns() / 3))
    assertTrue(columnList.contains(A.columns() / 2))
    assertTrue(valueList.contains(7))
    assertTrue(valueList.contains(1))
  }

  def testGetPositiveValues() {
    A.assign(0)
    A.setQuick(A.rows() / 3, A.columns() / 3, 7)
    A.setQuick(A.rows() / 2, A.columns() / 2, 1)
    val rowList = new IntArrayList()
    val columnList = new IntArrayList()
    val valueList = new LongArrayList()
    A.getPositiveValues(rowList, columnList, valueList)
    assertEquals(2, rowList.size)
    assertEquals(2, columnList.size)
    assertEquals(2, valueList.size)
    assertTrue(rowList.contains(A.rows() / 3))
    assertTrue(rowList.contains(A.rows() / 2))
    assertTrue(columnList.contains(A.columns() / 3))
    assertTrue(columnList.contains(A.columns() / 2))
    assertTrue(valueList.contains(7))
    assertTrue(valueList.contains(1))
  }

  def testToArray() {
    val array = A.toArray()
    assertTrue(A.rows() == array.length)
    for (r <- 0 until A.rows()) {
      assertTrue(A.columns() == array(r).length)
      for (c <- 0 until A.columns()) assertEquals(0, Math.abs(array(r)(c) - A.getQuick(r, c)))
    }
  }

  def testVectorize() {
    val Avec = A.vectorize()
    var idx = 0
    for (c <- 0 until A.columns(); r <- 0 until A.rows()) {
      assertEquals(A.getQuick(r, c), Avec.getQuick(idx += 1))
    }
  }

  def testViewColumn() {
    val col = A.viewColumn(A.columns() / 2)
    assertEquals(A.rows(), col.size)
    for (r <- 0 until A.rows()) {
      assertEquals(A.getQuick(r, A.columns() / 2), col.getQuick(r))
    }
  }

  def testViewColumnFlip() {
    val B = A.viewColumnFlip()
    assertEquals(A.size, B.size)
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) {
      assertEquals(A.getQuick(r, A.columns() - 1 - c), B.getQuick(r, c))
    }
  }

  def testViewDice() {
    val B = A.viewDice()
    assertEquals(A.rows(), B.columns())
    assertEquals(A.columns(), B.rows())
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) {
      assertEquals(A.getQuick(r, c), B.getQuick(c, r))
    }
  }

  def testViewPart() {
    val B = A.viewPart(A.rows() / 2, A.columns() / 2, A.rows() / 3, A.columns() / 3)
    assertEquals(A.rows() / 3, B.rows())
    assertEquals(A.columns() / 3, B.columns())
    for (r <- 0 until A.rows() / 3; c <- 0 until A.columns() / 3) {
      assertEquals(A.getQuick(A.rows() / 2 + r, A.columns() / 2 + c), B.getQuick(r, c))
    }
  }

  def testViewRow() {
    val B = A.viewRow(A.rows() / 2)
    assertEquals(A.columns(), B.size)
    for (r <- 0 until A.columns()) {
      assertEquals(A.getQuick(A.rows() / 2, r), B.getQuick(r))
    }
  }

  def testViewRowFlip() {
    val B = A.viewRowFlip()
    assertEquals(A.size, B.size)
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) {
      assertEquals(A.getQuick(A.rows() - 1 - r, c), B.getQuick(r, c))
    }
  }

  def testViewSelectionLongMatrix1DProcedure() {
    val value = 2
    A.assign(0)
    A.setQuick(A.rows() / 4, 0, value)
    A.setQuick(A.rows() / 2, 0, value)
    val B = A.viewSelection(new LongMatrix1DProcedure() {

      def apply(element: LongMatrix1D): Boolean = {
        if (Math.abs(element.getQuick(0) - value) == 0) {
          return true
        } else {
          return false
        }
      }
    })
    assertEquals(2, B.rows())
    assertEquals(A.columns(), B.columns())
    assertEquals(A.getQuick(A.rows() / 4, 0), B.getQuick(0, 0))
    assertEquals(A.getQuick(A.rows() / 2, 0), B.getQuick(1, 0))
  }

  def testViewSelectionIntArrayIntArray() {
    val rowIndexes = Array(A.rows() / 6, A.rows() / 5, A.rows() / 4, A.rows() / 3, A.rows() / 2)
    val colIndexes = Array(A.columns() / 6, A.columns() / 5, A.columns() / 4, A.columns() / 3, A.columns() / 2, A.columns() - 1)
    val B = A.viewSelection(rowIndexes, colIndexes)
    assertEquals(rowIndexes.length, B.rows())
    assertEquals(colIndexes.length, B.columns())
    for (r <- 0 until rowIndexes.length; c <- 0 until colIndexes.length) {
      assertEquals(A.getQuick(rowIndexes(r), colIndexes(c)), B.getQuick(r, c))
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
      assertEquals(A.getQuick(r * rowStride, c * colStride), B.getQuick(r, c))
    }
  }

  def testZMultLongMatrix1DLongMatrix1DLongLongBoolean() {
    var y = new DenseLongMatrix1D(A.columns())
    for (i <- 0 until y.size) {
      y.setQuick(i, rand.nextLong() % A.rows())
    }
    val alpha = 3
    val beta = 5
    var z = LongFactory1D.dense.random(A.rows())
    z.assign(LongFunctions.mod(A.rows()))
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
      assertEquals(expected(r), z.getQuick(r))
    }
    z = null
    z = A.zMult(y, z, alpha, beta, false)
    expected = Array.ofDim[Long](A.rows())
    for (r <- 0 until A.rows()) {
      var s = 0
      for (c <- 0 until A.columns()) {
        s += A.getQuick(r, c) * y.getQuick(c)
      }
      expected(r) = s * alpha
    }
    for (r <- 0 until A.rows()) {
      assertEquals(expected(r), z.getQuick(r))
    }
    y = new DenseLongMatrix1D(A.rows())
    for (i <- 0 until y.size) {
      y.setQuick(i, rand.nextLong() % A.rows())
    }
    z = LongFactory1D.dense.random(A.columns())
    z.assign(LongFunctions.mod(A.rows()))
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
      assertEquals(expected(r), z.getQuick(r))
    }
    z = null
    z = A.zMult(y, z, alpha, beta, true)
    expected = Array.ofDim[Long](A.columns())
    for (r <- 0 until A.columns()) {
      var s = 0
      for (c <- 0 until A.rows()) {
        s += A.getQuick(c, r) * y.getQuick(c)
      }
      expected(r) = s * alpha
    }
    for (r <- 0 until A.columns()) {
      assertEquals(expected(r), z.getQuick(r))
    }
  }

  def testZMultLongMatrix2DLongMatrix2DLongLongBooleanBoolean() {
    val alpha = 3
    val beta = 5
    var C = LongFactory2D.dense.random(A.rows(), A.rows())
    C.assign(LongFunctions.mod(A.rows()))
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
      assertEquals(expected(r)(c), C.getQuick(r, c))
    }
    C = null
    C = A.zMult(Bt, C, alpha, beta, false, false)
    expected = Array.ofDim[Long](A.rows(), A.rows())
    for (j <- 0 until A.rows(); i <- 0 until A.rows()) {
      var s = 0
      for (k <- 0 until A.columns()) {
        s += A.getQuick(i, k) * Bt.getQuick(k, j)
      }
      expected(i)(j) = s * alpha
    }
    for (r <- 0 until A.rows(); c <- 0 until A.rows()) {
      assertEquals(expected(r)(c), C.getQuick(r, c))
    }
    C = LongFactory2D.dense.random(A.columns(), A.columns())
    C.assign(LongFunctions.mod(A.rows()))
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
      assertEquals(expected(r)(c), C.getQuick(r, c))
    }
    C = null
    C = A.zMult(B, C, alpha, beta, true, false)
    expected = Array.ofDim[Long](A.columns(), A.columns())
    for (j <- 0 until A.columns(); i <- 0 until A.columns()) {
      var s = 0
      for (k <- 0 until A.rows()) {
        s += A.getQuick(k, i) * B.getQuick(k, j)
      }
      expected(i)(j) = s * alpha
    }
    for (r <- 0 until A.columns(); c <- 0 until A.columns()) {
      assertEquals(expected(r)(c), C.getQuick(r, c))
    }
    C = LongFactory2D.dense.random(A.rows(), A.rows())
    C.assign(LongFunctions.mod(A.rows()))
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
      assertEquals(expected(r)(c), C.getQuick(r, c))
    }
    C = null
    C = A.zMult(B, C, alpha, beta, false, true)
    expected = Array.ofDim[Long](A.rows(), A.rows())
    for (j <- 0 until A.rows(); i <- 0 until A.rows()) {
      var s = 0
      for (k <- 0 until A.columns()) {
        s += A.getQuick(i, k) * B.getQuick(j, k)
      }
      expected(i)(j) = s * alpha
    }
    for (r <- 0 until A.rows(); c <- 0 until A.rows()) {
      assertEquals(expected(r)(c), C.getQuick(r, c))
    }
    C = LongFactory2D.dense.random(A.columns(), A.columns())
    C.assign(LongFunctions.mod(A.rows()))
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
      assertEquals(expected(r)(c), C.getQuick(r, c))
    }
    C = null
    C = A.zMult(Bt, C, alpha, beta, true, true)
    expected = Array.ofDim[Long](A.columns(), A.columns())
    for (j <- 0 until A.columns(); i <- 0 until A.columns()) {
      var s = 0
      for (k <- 0 until A.rows()) {
        s += A.getQuick(k, i) * Bt.getQuick(j, k)
      }
      expected(i)(j) = s * alpha
    }
    for (r <- 0 until A.columns(); c <- 0 until A.columns()) {
      assertEquals(expected(r)(c), C.getQuick(r, c))
    }
  }

  def testZSum() {
    val sum = A.zSum()
    var expected = 0
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) {
      expected += A.getQuick(r, c)
    }
    assertEquals(expected, sum)
  }
}
