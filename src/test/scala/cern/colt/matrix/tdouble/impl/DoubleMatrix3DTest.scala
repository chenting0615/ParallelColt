package cern.colt.matrix.impl

import cern.jet.math.tdouble.DoubleFunctions
import edu.emory.mathcs.utils.ConcurrencyUtils
import junit.framework.TestCase
import java.util.Random
import cern.colt.matrix.impl.StrideMatrix2D
import cern.colt.matrix.Matrix2DProcedure

//remove if not needed
import scala.collection.JavaConversions._

abstract class DoubleMatrix3DTest(arg0: String) extends TestCase(arg0) {

  /**
   * Matrix to test
   */
  protected var A: DoubleMatrix3D = _

  /**
   * Matrix of the same size as A
   */
  protected var B: DoubleMatrix3D = _

  protected var NSLICES: Int = 5

  protected var NROWS: Int = 13

  protected var NCOLUMNS: Int = 17

  protected var TOL: Double = 1e-10

  protected def setUp() {
    createMatrices()
    populateMatrices()
  }

  protected def createMatrices(): Unit

  protected def populateMatrices() {
    val rnd = new Random(100)
    ConcurrencyUtils.setThreadsBeginN_3D(1)
    for (s <- 0 until A.slices(); r <- 0 until A.rows(); c <- 0 until A.columns()) {
      A.setQuick(s, r, c, rnd.nextDouble())
    }
    for (s <- 0 until B.slices(); r <- 0 until B.rows(); c <- 0 until B.columns()) {
      B.setQuick(s, r, c, rnd.nextDouble())
    }
  }

  protected def tearDown() {
    A = B = null
  }

  def testAggregateDoubleDoubleFunctionDoubleFunction() {
    var expected = 0
    for (s <- 0 until A.slices(); r <- 0 until A.rows(); c <- 0 until A.columns()) {
      val elem = A.getQuick(s, r, c)
      expected += elem * elem
    }
    val result = A.aggregate(DoubleFunctions.plus, DoubleFunctions.square)
    assertEquals(expected, result, TOL)
  }

  def testAggregateDoubleDoubleFunctionDoubleFunctionDoubleProcedure() {
    val procedure = new Procedure1() {

      def apply(element: Double): Boolean = {
        if (Math.abs(element) > 0.2) {
          return true
        } else {
          return false
        }
      }
    }
    var expected = 0
    for (s <- 0 until A.slices(); r <- 0 until A.rows(); c <- 0 until A.columns()) {
      val elem = A.getQuick(s, r, c)
      if (Math.abs(elem) > 0.2) {
        expected += elem * elem
      }
    }
    val result = A.aggregate(DoubleFunctions.plus, DoubleFunctions.square, procedure)
    assertEquals(expected, result, TOL)
  }

  def testAggregateDoubleDoubleFunctionDoubleFunctionIntArrayListIntArrayListIntArrayList() {
    val sliceList = new IntArrayList()
    val rowList = new IntArrayList()
    val columnList = new IntArrayList()
    for (s <- 0 until A.slices(); r <- 0 until A.rows(); c <- 0 until A.columns()) {
      sliceList.add(s)
      rowList.add(r)
      columnList.add(c)
    }
    var expected = 0
    for (s <- 0 until A.slices(); r <- 0 until A.rows(); c <- 0 until A.columns()) {
      val elem = A.getQuick(s, r, c)
      expected += elem * elem
    }
    val result = A.aggregate(DoubleFunctions.plus, DoubleFunctions.square, sliceList, rowList, columnList)
    assertEquals(expected, result, TOL)
  }

  def testAggregateDoubleMatrix2DDoubleDoubleFunctionDoubleDoubleFunction() {
    var expected = 0
    for (s <- 0 until A.slices(); r <- 0 until A.rows(); c <- 0 until A.columns()) {
      val elemA = A.getQuick(s, r, c)
      val elemB = B.getQuick(s, r, c)
      expected += elemA * elemB
    }
    val result = A.aggregate(B, DoubleFunctions.plus, DoubleFunctions.mult)
    assertEquals(expected, result, TOL)
  }

  def testAssignDouble() {
    val value = Math.random()
    A.assign(value)
    for (s <- 0 until A.slices(); r <- 0 until A.rows(); c <- 0 until A.columns()) assertEquals(value,
      A.getQuick(s, r, c), TOL)
  }

  def testAssignDoubleArray() {
    val expected = Array.ofDim[Double](A.size.toInt)
    for (i <- 0 until A.size) {
      expected(i) = Math.random()
    }
    A.assign(expected)
    var idx = 0
    for (s <- 0 until A.slices(); r <- 0 until A.rows(); c <- 0 until A.columns()) {
      assertEquals(expected(idx += 1), A.getQuick(s, r, c), TOL)
    }
  }

  def testAssignDoubleArrayArrayArray() {
    val expected = Array.ofDim[Double](A.slices(), A.rows(), A.columns())
    for (s <- 0 until A.slices(); r <- 0 until A.rows(); c <- 0 until A.columns()) {
      expected(s)(r)(c) = Math.random()
    }
    A.assign(expected)
    for (s <- 0 until A.slices()) {
      assertTrue(A.rows() == expected(s).length)
      for (r <- 0 until A.rows()) {
        assertTrue(A.columns() == expected(s)(r).length)
        for (c <- 0 until A.columns()) {
          assertEquals(expected(s)(r)(c), A.getQuick(s, r, c), TOL)
        }
      }
    }
  }

  def testAssignDoubleFunction() {
    val Acopy = A.copy()
    A.assign(DoubleFunctions.acos)
    for (s <- 0 until A.slices(); r <- 0 until A.rows(); c <- 0 until A.columns()) {
      val expected = Math.acos(Acopy.getQuick(s, r, c))
      assertEquals(expected, A.getQuick(s, r, c), TOL)
    }
  }

  def testAssignDoubleMatrix3D() {
    A.assign(B)
    for (s <- 0 until A.slices(); r <- 0 until A.rows(); c <- 0 until A.columns()) assertEquals(B.getQuick(s,
      r, c), A.getQuick(s, r, c), TOL)
  }

  def testAssignDoubleMatrix3DDoubleDoubleFunction() {
    val Acopy = A.copy()
    A.assign(B, DoubleFunctions.div)
    for (s <- 0 until A.slices(); r <- 0 until A.rows(); c <- 0 until A.columns()) {
      assertEquals(Acopy.getQuick(s, r, c) / B.getQuick(s, r, c), A.getQuick(s, r, c), TOL)
    }
  }

  def testAssignDoubleMatrix3DDoubleDoubleFunctionIntArrayListIntArrayListIntArrayList() {
    val sliceList = new IntArrayList()
    val rowList = new IntArrayList()
    val columnList = new IntArrayList()
    for (s <- 0 until A.slices(); r <- 0 until A.rows(); c <- 0 until A.columns()) {
      sliceList.add(s)
      rowList.add(r)
      columnList.add(c)
    }
    val Acopy = A.copy()
    A.assign(B, DoubleFunctions.div, sliceList, rowList, columnList)
    for (s <- 0 until A.slices(); r <- 0 until A.rows(); c <- 0 until A.columns()) {
      assertEquals(Acopy.getQuick(s, r, c) / B.getQuick(s, r, c), A.getQuick(s, r, c), TOL)
    }
  }

  def testAssignDoubleProcedureDouble() {
    val procedure = new Procedure1() {

      def apply(element: Double): Boolean = {
        if (Math.abs(element) > 0.1) {
          return true
        } else {
          return false
        }
      }
    }
    val Acopy = A.copy()
    A.assign(procedure, -1.0)
    for (s <- 0 until A.slices(); r <- 0 until A.rows(); c <- 0 until A.columns()) {
      if (Math.abs(Acopy.getQuick(s, r, c)) > 0.1) {
        assertEquals(-1.0, A.getQuick(s, r, c), TOL)
      } else {
        assertEquals(Acopy.getQuick(s, r, c), A.getQuick(s, r, c), TOL)
      }
    }
  }

  def testAssignDoubleProcedureDoubleFunction() {
    val procedure = new Procedure1() {

      def apply(element: Double): Boolean = {
        if (Math.abs(element) > 0.1) {
          return true
        } else {
          return false
        }
      }
    }
    val Acopy = A.copy()
    A.assign(procedure, DoubleFunctions.tan)
    for (s <- 0 until A.slices(); r <- 0 until A.rows(); c <- 0 until A.columns()) {
      if (Math.abs(Acopy.getQuick(s, r, c)) > 0.1) {
        assertEquals(Math.tan(Acopy.getQuick(s, r, c)), A.getQuick(s, r, c), TOL)
      } else {
        assertEquals(Acopy.getQuick(s, r, c), A.getQuick(s, r, c), TOL)
      }
    }
  }

  def testCardinality() {
    val card = A.cardinality()
    assertEquals(A.slices() * A.rows() * A.columns(), card)
  }

  def testEqualsDouble() {
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
    A.setQuick(A.slices() / 3, A.rows() / 3, A.columns() / 3, 0.7)
    A.setQuick(A.slices() / 3, A.rows() / 2, A.columns() / 2, 0.1)
    val maxAndLoc = A.getMaxLocation
    assertEquals(0.7, maxAndLoc(0), TOL)
    assertEquals(A.slices() / 3, maxAndLoc(1).toInt)
    assertEquals(A.rows() / 3, maxAndLoc(2).toInt)
    assertEquals(A.columns() / 3, maxAndLoc(3).toInt)
  }

  def testMinLocation() {
    A.assign(0)
    A.setQuick(A.slices() / 3, A.rows() / 3, A.columns() / 3, -0.7)
    A.setQuick(A.slices() / 3, A.rows() / 2, A.columns() / 2, -0.1)
    val minAndLoc = A.getMinLocation
    assertEquals(-0.7, minAndLoc(0), TOL)
    assertEquals(A.slices() / 3, minAndLoc(1).toInt)
    assertEquals(A.rows() / 3, minAndLoc(2).toInt)
    assertEquals(A.columns() / 3, minAndLoc(3).toInt)
  }

  def testGetNegativeValues() {
    A.assign(0)
    A.setQuick(A.slices() / 3, A.rows() / 3, A.columns() / 3, -0.7)
    A.setQuick(A.slices() / 2, A.rows() / 2, A.columns() / 2, -0.1)
    val sliceList = new IntArrayList()
    val rowList = new IntArrayList()
    val columnList = new IntArrayList()
    val valueList = new DoubleArrayList()
    A.getNegativeValues(sliceList, rowList, columnList, valueList)
    assertEquals(2, sliceList.size)
    assertEquals(2, rowList.size)
    assertEquals(2, columnList.size)
    assertEquals(2, valueList.size)
    assertTrue(sliceList.contains(A.slices() / 3))
    assertTrue(sliceList.contains(A.slices() / 2))
    assertTrue(rowList.contains(A.rows() / 3))
    assertTrue(rowList.contains(A.rows() / 2))
    assertTrue(columnList.contains(A.columns() / 3))
    assertTrue(columnList.contains(A.columns() / 2))
    assertTrue(valueList.contains(-0.7))
    assertTrue(valueList.contains(-0.1))
  }

  def testGetNonZeros() {
    A.assign(0)
    A.setQuick(A.slices() / 3, A.rows() / 3, A.columns() / 3, 0.7)
    A.setQuick(A.slices() / 2, A.rows() / 2, A.columns() / 2, 0.1)
    val sliceList = new IntArrayList()
    val rowList = new IntArrayList()
    val columnList = new IntArrayList()
    val valueList = new DoubleArrayList()
    A.getNonZeros(sliceList, rowList, columnList, valueList)
    assertEquals(2, sliceList.size)
    assertEquals(2, rowList.size)
    assertEquals(2, columnList.size)
    assertEquals(2, valueList.size)
    assertTrue(sliceList.contains(A.slices() / 3))
    assertTrue(sliceList.contains(A.slices() / 2))
    assertTrue(rowList.contains(A.rows() / 3))
    assertTrue(rowList.contains(A.rows() / 2))
    assertTrue(columnList.contains(A.columns() / 3))
    assertTrue(columnList.contains(A.columns() / 2))
    assertTrue(valueList.contains(0.7))
    assertTrue(valueList.contains(0.1))
  }

  def testGetPositiveValues() {
    A.assign(0)
    A.setQuick(A.slices() / 3, A.rows() / 3, A.columns() / 3, 0.7)
    A.setQuick(A.slices() / 2, A.rows() / 2, A.columns() / 2, 0.1)
    val sliceList = new IntArrayList()
    val rowList = new IntArrayList()
    val columnList = new IntArrayList()
    val valueList = new DoubleArrayList()
    A.getPositiveValues(sliceList, rowList, columnList, valueList)
    assertEquals(2, sliceList.size)
    assertEquals(2, rowList.size)
    assertEquals(2, columnList.size)
    assertEquals(2, valueList.size)
    assertTrue(sliceList.contains(A.slices() / 3))
    assertTrue(sliceList.contains(A.slices() / 2))
    assertTrue(rowList.contains(A.rows() / 3))
    assertTrue(rowList.contains(A.rows() / 2))
    assertTrue(columnList.contains(A.columns() / 3))
    assertTrue(columnList.contains(A.columns() / 2))
    assertTrue(valueList.contains(0.7))
    assertTrue(valueList.contains(0.1))
  }

  def testToArray() {
    val array = A.toArray()
    for (s <- 0 until A.slices()) {
      assertTrue(A.rows() == array(s).length)
      for (r <- 0 until A.rows()) {
        assertTrue(A.columns() == array(s)(r).length)
        for (c <- 0 until A.columns()) assertEquals(0, Math.abs(array(s)(r)(c) - A.getQuick(s, r, c)),
          TOL)
      }
    }
  }

  def testVectorize() {
    val Avec = A.vectorize()
    var idx = 0
    for (s <- 0 until A.slices(); c <- 0 until A.columns(); r <- 0 until A.rows()) {
      assertEquals(A.getQuick(s, r, c), Avec.getQuick(idx += 1), TOL)
    }
  }

  def testViewColumn() {
    val B = A.viewColumn(A.columns() / 2)
    assertEquals(A.slices(), B.rows())
    assertEquals(A.rows(), B.columns())
    for (s <- 0 until A.slices(); r <- 0 until A.rows()) {
      assertEquals(A.getQuick(s, r, A.columns() / 2), B.getQuick(s, r), TOL)
    }
  }

  def testViewColumnFlip() {
    val B = A.viewColumnFlip()
    assertEquals(A.size, B.size)
    for (s <- 0 until A.slices(); r <- 0 until A.rows(); c <- 0 until A.columns()) {
      assertEquals(A.getQuick(s, r, A.columns() - 1 - c), B.getQuick(s, r, c), TOL)
    }
  }

  def testViewDice() {
    val B = A.viewDice(2, 1, 0)
    assertEquals(A.slices(), B.columns())
    assertEquals(A.rows(), B.rows())
    assertEquals(A.columns(), B.slices())
    for (s <- 0 until A.slices(); r <- 0 until A.rows(); c <- 0 until A.columns()) {
      assertEquals(A.getQuick(s, r, c), B.getQuick(c, r, s), TOL)
    }
  }

  def testViewPart() {
    val B = A.viewPart(A.slices() / 2, A.rows() / 2, A.columns() / 2, A.slices() / 3, A.rows() / 3, A.columns() / 3)
    for (s <- 0 until A.slices() / 3; r <- 0 until A.rows() / 3; c <- 0 until A.columns() / 3) {
      assertEquals(A.getQuick(A.slices() / 2 + s, A.rows() / 2 + r, A.columns() / 2 + c), B.getQuick(s,
        r, c), TOL)
    }
  }

  def testViewRow() {
    val B = A.viewRow(A.rows() / 2)
    assertEquals(A.slices(), B.rows())
    assertEquals(A.columns(), B.columns())
    for (s <- 0 until A.slices(); c <- 0 until A.columns()) {
      assertEquals(A.getQuick(s, A.rows() / 2, c), B.getQuick(s, c), TOL)
    }
  }

  def testViewRowFlip() {
    val B = A.viewRowFlip()
    assertEquals(A.size, B.size)
    for (s <- 0 until A.slices(); r <- 0 until A.rows(); c <- 0 until A.columns()) {
      assertEquals(A.getQuick(s, A.rows() - 1 - r, c), B.getQuick(s, r, c), TOL)
    }
  }

  def testViewSelectionDoubleMatrix2DProcedure() {
    A.assign(0)
    val value = 2
    A.setQuick(A.slices() / 2, A.rows() / 4, 0, value)
    val B = A.viewSelection(new Matrix2DProcedure() {

      def apply(element: StrideMatrix2D): Boolean = {
        if (Math.abs(element.getQuick(A.rows() / 4, 0) - value) <=
          TOL) {
          return true
        } else {
          return false
        }
      }
    })
    assertEquals(1, B.slices())
    assertEquals(A.rows(), B.rows())
    assertEquals(A.columns(), B.columns())
    assertEquals(A.getQuick(A.slices() / 2, A.rows() / 4, 0), B.getQuick(0, A.rows() / 4, 0), TOL)
  }

  def testViewSelectionIntArrayIntArrayIntArray() {
    val sliceIndexes = Array(A.slices() / 2, A.slices() / 3)
    val rowIndexes = Array(A.rows() / 6, A.rows() / 5, A.rows() / 4, A.rows() / 3, A.rows() / 2)
    val colIndexes = Array(A.columns() / 6, A.columns() / 5, A.columns() / 4, A.columns() / 3, A.columns() / 2, A.columns() - 1)
    val B = A.viewSelection(sliceIndexes, rowIndexes, colIndexes)
    assertEquals(sliceIndexes.length, B.slices())
    assertEquals(rowIndexes.length, B.rows())
    assertEquals(colIndexes.length, B.columns())
    for (s <- 0 until sliceIndexes.length; r <- 0 until rowIndexes.length; c <- 0 until colIndexes.length) {
      assertEquals(A.getQuick(sliceIndexes(s), rowIndexes(r), colIndexes(c)), B.getQuick(s, r, c), TOL)
    }
  }

  def testViewSlice() {
    val B = A.viewSlice(A.slices() / 2)
    assertEquals(A.rows(), B.rows())
    assertEquals(A.columns(), B.columns())
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) {
      assertEquals(A.getQuick(A.slices() / 2, r, c), B.getQuick(r, c), TOL)
    }
  }

  def testViewSliceFlip() {
    val B = A.viewSliceFlip()
    assertEquals(A.size, B.size)
    for (s <- 0 until A.slices(); r <- 0 until A.rows(); c <- 0 until A.columns()) {
      assertEquals(A.getQuick(A.slices() - 1 - s, r, c), B.getQuick(s, r, c), TOL)
    }
  }

  def testViewSorted() {
    val B = A.viewSorted(1, 1)
    for (s <- 0 until A.slices() - 1) {
      assertTrue(B.getQuick(s + 1, 1, 1) >= B.getQuick(s, 1, 1))
    }
  }

  def testViewStrides() {
    val sliceStride = 2
    val rowStride = 2
    val colStride = 2
    val B = A.viewStrides(sliceStride, rowStride, colStride)
    for (s <- 0 until B.slices(); r <- 0 until B.rows(); c <- 0 until B.columns()) {
      assertEquals(A.getQuick(s * sliceStride, r * rowStride, c * colStride), B.getQuick(s, r, c), TOL)
    }
  }

  def testZSum() {
    val sum = A.zSum()
    var expected = 0
    for (s <- 0 until A.slices(); r <- 0 until A.rows(); c <- 0 until A.columns()) {
      expected += A.getQuick(s, r, c)
    }
    assertEquals(expected, sum, TOL)
  }
}