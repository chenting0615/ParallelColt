package cern.colt.matrix.tdouble.impl

import cern.jet.math.tdouble.DoubleFunctions
import edu.emory.mathcs.utils.ConcurrencyUtils
import org.junit.Assert._
import java.util.Random
import cern.colt.matrix.impl.DenseMatrix1D
import cern.colt.matrix.{Matrix1D, Matrix2D}
import cern.colt.function.{Matrix1DProcedure, Procedure1}
import cern.colt.matrix.MatrixOperators._
import cern.colt.list.ArrayTypes.{DoubleArrayList, IntArrayList}
import junit.framework.TestCase
import cern.colt.function.FunctionTypes.IntIntDoubleFunction
import cern.colt.matrix.MatrixTypes.{DoubleMatrix2D, DenseDoubleMatrix2D, DoubleMatrix1D, DenseDoubleMatrix1D}

abstract class DoubleMatrix2DTest(arg0: String) extends TestCase(arg0) {

  protected val random = new Random(0)

  /**
   * Matrix to test
   */
  protected var A: Matrix2D[Double] = _

  /**
   * Matrix of the same size as A
   */
  protected var B: Matrix2D[Double] = _

  /**
   * Matrix of the same size as A
   */
  protected var B_diffType: Matrix2D[Double] = null

  /**
   * Matrix of the size A.columns x A.rows
   */
  protected var Bt: Matrix2D[Double] = _

  protected var NROWS: Int = 13

  protected var NCOLUMNS: Int = 17

  protected var TOL: Double = 1e-10

  override protected def setUp() {
    createMatrices()
    populateMatrices()
  }

  protected def createMatrices(): Unit

  protected def populateMatrices() {
    ConcurrencyUtils.setThreadsBeginN_2D(1)
    for (r <- 0 until A.rows; c <- 0 until A.columns) {
      A.setQuick(r, c, random.nextDouble())
    }
    for (r <- 0 until B.rows; c <- 0 until B.columns) {
      B.setQuick(r, c, random.nextDouble())
    }
    for (r <- 0 until Bt.rows; c <- 0 until Bt.columns) {
      Bt.setQuick(r, c, random.nextDouble())
    }
  }

  override protected def tearDown() {
    A = null
    B = null
    Bt = null
    B_diffType = null
  }

  def testAggregateDoubleDoubleFunctionDoubleFunction() {
    var expected = 0.0
    for (r <- 0 until A.rows; c <- 0 until A.columns) {
      val elem = A.getQuick(r, c)
      expected += elem * elem
    }
    val result = A.aggregate(DoubleFunctions.plus, DoubleFunctions.square)
    assertEquals(expected, result, TOL)
  }

  def testAggregateDoubleDoubleFunctionDoubleFunctionDoubleProcedure() {
    val procedure = new Procedure1[Double]() {
      def apply(element: Double): Boolean = {
        Math.abs(element) > 0.2
      }
    }
    var expected = 0.0
    for (r <- 0 until A.rows; c <- 0 until A.columns) {
      val elem = A.getQuick(r, c)
      if (Math.abs(elem) > 0.2) {
        expected += elem * elem
      }
    }
    val result = A.aggregate(DoubleFunctions.plus, DoubleFunctions.square, procedure)
    assertEquals(expected, result, TOL)
  }

/*
  def testAggregateDoubleDoubleFunctionDoubleFunctionIntArrayListIntArrayList() {
    val rowList = new IntArrayList()
    val columnList = new IntArrayList()
    for (r <- 0 until A.rows; c <- 0 until A.columns) {
      rowList.add(r)
      columnList.add(c)
    }
    var expected = 0.0
    for (r <- 0 until A.rows; c <- 0 until A.columns) {
      val elem = A.getQuick(r, c)
      expected += elem * elem
    }
    val result = A.aggregate(DoubleFunctions.plus, DoubleFunctions.square, rowList, columnList)
    assertEquals(expected, result, TOL)
  }
*/

  def testAggregateDoubleMatrix2DDoubleDoubleFunctionDoubleDoubleFunction() {
    var expected = 0.0
    for (r <- 0 until A.rows; c <- 0 until A.columns) {
      val elemA = A.getQuick(r, c)
      val elemB = B.getQuick(r, c)
      expected += elemA * elemB
    }
    val result = A.aggregate(B, DoubleFunctions.plus, DoubleFunctions.mult)
    assertEquals(expected, result, TOL)
  }

  def testAssignDouble() {
    val value = Math.random()
    A.assignConstant(value)
    for (r <- 0 until A.rows; c <- 0 until A.columns) assertEquals(value, A.getQuick(r, c), TOL)
  }

  def testAssignDoubleArrayArray() {
    val expected = Array.ofDim[Double](A.rows, A.columns)
    for (r <- 0 until A.rows; c <- 0 until A.columns) {
      expected(r)(c) = Math.random()
    }
    A.assign(expected)
    for (r <- 0 until A.rows) {
      assertTrue(A.columns == expected(r).length)
      for (c <- 0 until A.columns) assertEquals(expected(r)(c), A.getQuick(r, c), TOL)
    }
  }

  def testAssignDoubleFunction() {
    val Acopy = A.copy()
    A.assign(DoubleFunctions.acos)
    for (r <- 0 until A.rows; c <- 0 until A.columns) {
      val expected = Math.acos(Acopy.getQuick(r, c))
      assertEquals(expected, A.getQuick(r, c), TOL)
    }
  }

  def testAssignDoubleMatrix2D() {
    A.assign(B)
    for (r <- 0 until A.rows; c <- 0 until A.columns)
      assertEquals(B.getQuick(r, c), A.getQuick(r, c), TOL)
    if (B_diffType != null) {
      B_diffType.assign(B)
      for (r <- 0 until B.rows; c <- 0 until B.columns)
        assertEquals(B.getQuick(r, c), B_diffType.getQuick(r, c), TOL)
      A.assign(B_diffType)
      for (r <- 0 until A.rows; c <- 0 until A.columns)
        assertEquals(B_diffType.getQuick(r, c), A.getQuick(r, c), TOL)
    }
  }

  def testAssignDoubleMatrix2DDoubleDoubleFunction() {
    val Acopy = A.copy()
    A.assign(B, DoubleFunctions.plus)
    for (r <- 0 until A.rows; c <- 0 until A.columns) {
      assertEquals(Acopy.getQuick(r, c) + B.getQuick(r, c), A.getQuick(r, c), TOL)
    }
  }

/*
  def testAssignDoubleMatrix2DDoubleDoubleFunctionIntArrayListIntArrayList() {
    val rowList = new IntArrayList()
    val columnList = new IntArrayList()
    for (r <- 0 until A.rows; c <- 0 until A.columns) {
      rowList.add(r)
      columnList.add(c)
    }
    val Acopy = A.copy()
    A.assign(B, DoubleFunctions.div, rowList, columnList)
    for (r <- 0 until A.rows; c <- 0 until A.columns) {
      assertEquals(Acopy.getQuick(r, c) / B.getQuick(r, c), A.getQuick(r, c), TOL)
    }
  }
*/

  def testAssignDoubleProcedureDouble() {
    val procedure = new Procedure1[Double]() {
      def apply(element: Double): Boolean = {
        Math.abs(element) > 0.1
      }
    }
    val Acopy = A.copy()
    A.assign(procedure, -1.0)
    for (r <- 0 until A.rows; c <- 0 until A.columns) {
      if (Math.abs(Acopy.getQuick(r, c)) > 0.1) {
        assertEquals(-1.0, A.getQuick(r, c), TOL)
      } else {
        assertEquals(Acopy.getQuick(r, c), A.getQuick(r, c), TOL)
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
    for (r <- 0 until A.rows; c <- 0 until A.columns) {
      if (Math.abs(Acopy.getQuick(r, c)) > 0.1) {
        assertEquals(Math.tan(Acopy.getQuick(r, c)), A.getQuick(r, c), TOL)
      } else {
        assertEquals(Acopy.getQuick(r, c), A.getQuick(r, c), TOL)
      }
    }
  }

  def testCardinality() {
    val card = A.numNonZero
    assertEquals(A.rows * A.columns, card)
  }

  def testEqualsDouble() {
    val value = 1.0
    A.assignConstant(value)
    var eq = A.everyCellEquals(value)
    assertTrue(eq)
    eq = A.everyCellEquals(2.0)
    assertFalse(eq)

    eq = A.equals(value)
    assertFalse(eq)
    eq = A.equals(2.0)
    assertFalse(eq)
    eq = A.equals(1)
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
    val function = new IntIntDoubleFunction() {
      def apply(first: Int, second: Int, third: Double): Double = Math.sqrt(third)
    }
    A.forEachNonZeroRowMajor(function)
    for (r <- 0 until A.rows; c <- 0 until A.columns) {
      assertEquals(Math.sqrt(Acopy.getQuick(r, c)), A.getQuick(r, c), TOL)
    }
  }

  def testMaxLocation() {
    A.assignConstant(0)
    A.setQuick(A.rows / 3, A.columns / 3, 0.7)
    A.setQuick(A.rows / 2, A.columns / 2, 0.1)
    val maxAndLoc = A.getMaxLocation
    assertEquals(0.7, maxAndLoc._3, TOL)
    assertEquals(A.rows / 3, maxAndLoc._1)
    assertEquals(A.columns / 3, maxAndLoc._2)
  }

  def testMinLocation() {
    A.assignConstant(0)
    A.setQuick(A.rows / 3, A.columns / 3, -0.7)
    A.setQuick(A.rows / 2, A.columns / 2, -0.1)
    val minAndLoc = A.getMinLocation
    assertEquals(-0.7, minAndLoc._3, TOL)
    assertEquals(A.rows / 3, minAndLoc._1)
    assertEquals(A.columns / 3, minAndLoc._2)
  }

/*
  def testGetNegativeValues() {
    A.assignConstant(0)
    A.setQuick(A.rows / 3, A.columns / 3, -0.7)
    A.setQuick(A.rows / 2, A.columns / 2, -0.1)
    val rowList = new IntArrayList()
    val columnList = new IntArrayList()
    val valueList = new DoubleArrayList()
    A.getNegativeValues(rowList, columnList, valueList)
    assertEquals(2, rowList.size)
    assertEquals(2, columnList.size)
    assertEquals(2, valueList.size)
    assertTrue(rowList.contains(A.rows / 3))
    assertTrue(rowList.contains(A.rows / 2))
    assertTrue(columnList.contains(A.columns / 3))
    assertTrue(columnList.contains(A.columns / 2))
    assertTrue(valueList.contains(-0.7))
    assertTrue(valueList.contains(-0.1))
  }
*/

  def testGetNonZeros() {
    A.assignConstant(0)
    A.setQuick(A.rows / 3, A.columns / 3, 0.7)
    A.setQuick(A.rows / 2, A.columns / 2, 0.1)
    val rowList = new IntArrayList()
    val columnList = new IntArrayList()
    val valueList = new DoubleArrayList()
    A.forEachNonZeroRowMajor(new IntIntDoubleFunction() {
      def apply(row: Int, column: Int, value: Double) = {
        rowList.add(row)
        columnList.add(column)
        valueList.add(value)
        value
      }
    })
    assertEquals(2, rowList.size)
    assertEquals(2, columnList.size)
    assertEquals(2, valueList.size)
    assertTrue(rowList.contains(A.rows / 3))
    assertTrue(rowList.contains(A.rows / 2))
    assertTrue(columnList.contains(A.columns / 3))
    assertTrue(columnList.contains(A.columns / 2))
    assertTrue(valueList.contains(0.7))
    assertTrue(valueList.contains(0.1))
  }

/*
  def testGetPositiveValues() {
    A.assignConstant(0)
    A.setQuick(A.rows / 3, A.columns / 3, 0.7)
    A.setQuick(A.rows / 2, A.columns / 2, 0.1)
    val rowList = new IntArrayList()
    val columnList = new IntArrayList()
    val valueList = new DoubleArrayList()
    A.getPositiveValues(rowList, columnList, valueList)
    assertEquals(2, rowList.size)
    assertEquals(2, columnList.size)
    assertEquals(2, valueList.size)
    assertTrue(rowList.contains(A.rows / 3))
    assertTrue(rowList.contains(A.rows / 2))
    assertTrue(columnList.contains(A.columns / 3))
    assertTrue(columnList.contains(A.columns / 2))
    assertTrue(valueList.contains(0.7))
    assertTrue(valueList.contains(0.1))
  }
*/

  def testToArray() {
    val array = A.toArray
    assertTrue(A.rows == array.length)
    for (r <- 0 until A.rows) {
      assertTrue(A.columns == array(r).length)
      for (c <- 0 until A.columns) assertEquals(0, Math.abs(array(r)(c) - A.getQuick(r, c)), TOL)
    }
  }

  def testVectorize() {
    val Avec = A.vectorize()
    var idx = 0
    for (c <- 0 until A.columns; r <- 0 until A.rows) {
      assertEquals(A.getQuick(r, c), Avec.getQuick(idx), TOL)
      idx += 1
    }
  }

  def testViewColumn() {
    val col = A.viewColumn(A.columns / 2)
    assertEquals(A.rows, col.size)
    for (r <- 0 until A.rows) {
      assertEquals(A.getQuick(r, A.columns / 2), col.getQuick(r), TOL)
    }
  }

  def testViewColumnFlip() {
    val B = A.viewColumnFlip()
    assertEquals(A.size, B.size)
    for (r <- 0 until A.rows; c <- 0 until A.columns) {
      assertEquals(A.getQuick(r, A.columns - 1 - c), B.getQuick(r, c), TOL)
    }
  }

  def testViewDice() {
    val B = A.viewTranspose()
    assertEquals(A.rows, B.columns)
    assertEquals(A.columns, B.rows)
    for (r <- 0 until A.rows; c <- 0 until A.columns) {
      assertEquals(A.getQuick(r, c), B.getQuick(c, r), TOL)
    }
  }

  def testViewPart() {
    val B = A.viewPart(A.rows / 2, A.columns / 2, A.rows / 3, A.columns / 3)
    assertEquals(A.rows / 3, B.rows)
    assertEquals(A.columns / 3, B.columns)
    for (r <- 0 until A.rows / 3; c <- 0 until A.columns / 3) {
      assertEquals(A.getQuick(A.rows / 2 + r, A.columns / 2 + c), B.getQuick(r, c), TOL)
    }
  }

  def testViewRow() {
    val B = A.viewRow(A.rows / 2)
    assertEquals(A.columns, B.size)
    for (r <- 0 until A.columns) {
      assertEquals(A.getQuick(A.rows / 2, r), B.getQuick(r), TOL)
    }
  }

  def testViewRowFlip() {
    val B = A.viewRowFlip()
    assertEquals(A.size, B.size)
    for (r <- 0 until A.rows; c <- 0 until A.columns) {
      assertEquals(A.getQuick(A.rows - 1 - r, c), B.getQuick(r, c), TOL)
    }
  }

  def testViewSelectionDoubleMatrix1DProcedure() {
    val value = 2
    A.assignConstant(0)
    A.setQuick(A.rows / 4, 0, value)
    A.setQuick(A.rows / 2, 0, value)
    val B = A.viewRowSelection(new Matrix1DProcedure[Double]() {
          def apply(element: Matrix1D[Double]): Boolean = Math.abs(element.getQuick(0) - value) < TOL
        })
    assertEquals(2, B.rows)
    assertEquals(A.columns, B.columns)
    assertEquals(A.getQuick(A.rows / 4, 0), B.getQuick(0, 0), TOL)
    assertEquals(A.getQuick(A.rows / 2, 0), B.getQuick(1, 0), TOL)
  }

  def testViewSelectionIntArrayIntArray() {
    val rowIndexes = Array(A.rows / 6, A.rows / 5, A.rows / 4, A.rows / 3, A.rows / 2)
    val colIndexes = Array(A.columns / 6, A.columns / 5, A.columns / 4, A.columns / 3, A.columns / 2, A.columns - 1)
    val B = A.viewSelection(rowIndexes, colIndexes)
    assertEquals(rowIndexes.length, B.rows)
    assertEquals(colIndexes.length, B.columns)
    for (r <- 0 until rowIndexes.length; c <- 0 until colIndexes.length) {
      assertEquals(A.getQuick(rowIndexes(r), colIndexes(c)), B.getQuick(r, c), TOL)
    }
  }

/*
  def testViewSorted() {
    val B = A.viewSorted(1)
    for (r <- 0 until A.rows - 1) {
      assertTrue(B.getQuick(r + 1, 1) >= B.getQuick(r, 1))
    }
  }
*/

  def testViewStrides() {
    val rowStride = 3
    val colStride = 5
    val B = A.viewStrides(rowStride, colStride)
    for (r <- 0 until B.rows; c <- 0 until B.columns) {
      assertEquals(A.getQuick(r * rowStride, c * colStride), B.getQuick(r, c), TOL)
    }
  }

  def testZMultDoubleMatrix1DDoubleMatrix1DDoubleDoubleBoolean() {
    var y = new DenseDoubleMatrix1D(A.columns)
    y.assign(DoubleFunctions.random())
    val alpha = 3.0
    val beta = 5.0
    var z: DoubleMatrix1D = new DenseDoubleMatrix1D(A.rows)
    z.assign(DoubleFunctions.random())
    var expected = z.toArray
    z = A.dot(y, z, alpha, beta, transposeSelf = false)
    for (r <- 0 until A.rows) {
      var s = 0.0
      for (c <- 0 until A.columns) {
        s += A.getQuick(r, c) * y.getQuick(c)
      }
      expected(r) = s * alpha + expected(r) * beta
    }
    for (r <- 0 until A.rows) {
      assertEquals(expected(r), z.getQuick(r), TOL)
    }
    z = null
    z = A.dot(y, z, alpha, beta, transposeSelf=false)
    expected = Array.ofDim[Double](A.rows)
    for (r <- 0 until A.rows) {
      var s = 0.0
      for (c <- 0 until A.columns) {
        s += A.getQuick(r, c) * y.getQuick(c)
      }
      expected(r) = s * alpha
    }
    for (r <- 0 until A.rows) {
      assertEquals(expected(r), z.getQuick(r), TOL)
    }
    y = new DenseMatrix1D(A.rows)
    y.assign(DoubleFunctions.random())
    z = new DenseDoubleMatrix1D(A.columns)
    z.assign(DoubleFunctions.random())
    expected = z.toArray
    z = A.dot(y, z, alpha, beta, transposeSelf=true)
    for (r <- 0 until A.columns) {
      var s = 0.0
      for (c <- 0 until A.rows) {
        s += A.getQuick(c, r) * y.getQuick(c)
      }
      expected(r) = s * alpha + expected(r) * beta
    }
    for (r <- 0 until A.columns) {
      assertEquals("idx=" + r, expected(r), z.getQuick(r), TOL)
    }
    z = null
    z = A.dot(y, z, alpha, beta, transposeSelf=true)
    expected = Array.ofDim[Double](A.columns)
    for (r <- 0 until A.columns) {
      var s = 0.0
      for (c <- 0 until A.rows) {
        s += A.getQuick(c, r) * y.getQuick(c)
      }
      expected(r) = s * alpha
    }
    for (r <- 0 until A.columns) {
      assertEquals("idx=" + r, expected(r), z.getQuick(r), TOL)
    }
  }

  def testZMultDoubleMatrix2DDoubleMatrix2DDoubleDoubleBooleanBoolean() {
    val alpha = 3
    val beta = 5
    var C: DoubleMatrix2D = new DenseDoubleMatrix2D(A.rows, A.rows)
    C.assign(DoubleFunctions.random())
    var expected: Array[Array[Double]] = C.toArray
    C = A.dot(Bt, C, alpha, beta, transposeSelf=false, transposeOther=false)
    for (j <- 0 until A.rows; i <- 0 until A.rows) {
      var s = 0.0
      for (k <- 0 until A.columns) {
        s += A.getQuick(i, k) * Bt.getQuick(k, j)
      }
      expected(i)(j) = s * alpha + expected(i)(j) * beta
    }
    for (r <- 0 until A.rows; c <- 0 until A.rows) {
      assertEquals("("+r+", "+c+")", expected(r)(c), C.getQuick(r, c), TOL)
    }
    C = null
    C = A.dot(Bt, C, alpha, beta, transposeSelf=false, transposeOther=false)
    expected = Array.ofDim[Double](A.rows, A.rows)
    for (j <- 0 until A.rows; i <- 0 until A.rows) {
      var s = 0.0
      for (k <- 0 until A.columns) {
        s += A.getQuick(i, k) * Bt.getQuick(k, j)
      }
      expected(i)(j) = s * alpha
    }
    for (r <- 0 until A.rows; c <- 0 until A.rows) {
      assertEquals("("+r+", "+c+")", expected(r)(c), C.getQuick(r, c), TOL)
    }
    C = new DenseDoubleMatrix2D(A.columns, A.columns)
    C.assign(DoubleFunctions.random())
    expected = C.toArray
    C = A.dot(B, C, alpha, beta, transposeSelf=true, transposeOther=false)
    for (j <- 0 until A.columns; i <- 0 until A.columns) {
      var s = 0.0
      for (k <- 0 until A.rows) {
        s += A.getQuick(k, i) * B.getQuick(k, j)
      }
      expected(i)(j) = s * alpha + expected(i)(j) * beta
    }
    for (r <- 0 until A.columns; c <- 0 until A.columns) {
      assertEquals("("+r+", "+c+")", expected(r)(c), C.getQuick(r, c), TOL)
    }
    C = null
    C = A.dot(B, C, alpha, beta, transposeSelf=true, transposeOther=false)
    expected = Array.ofDim[Double](A.columns, A.columns)
    for (j <- 0 until A.columns; i <- 0 until A.columns) {
      var s = 0.0
      for (k <- 0 until A.rows) {
        s += A.getQuick(k, i) * B.getQuick(k, j)
      }
      expected(i)(j) = s * alpha
    }
    for (r <- 0 until A.columns; c <- 0 until A.columns) {
      assertEquals("("+r+", "+c+")", expected(r)(c), C.getQuick(r, c), TOL)
    }
    C = new DenseDoubleMatrix2D(A.rows, A.rows)
    C.assign(DoubleFunctions.random())
    expected = C.toArray
    C = A.dot(B, C, alpha, beta, transposeSelf=false, transposeOther=true)
    for (j <- 0 until A.rows; i <- 0 until A.rows) {
      var s = 0.0
      for (k <- 0 until A.columns) {
        s += A.getQuick(i, k) * B.getQuick(j, k)
      }
      expected(i)(j) = s * alpha + expected(i)(j) * beta
    }
    for (r <- 0 until A.rows; c <- 0 until A.rows) {
      assertEquals("("+r+", "+c+")", expected(r)(c), C.getQuick(r, c), TOL)
    }
    C = null
    C = A.dot(B, C, alpha, beta, transposeSelf=false, transposeOther=true)
    expected = Array.ofDim[Double](A.rows, A.rows)
    for (j <- 0 until A.rows; i <- 0 until A.rows) {
      var s = 0.0
      for (k <- 0 until A.columns) {
        s += A.getQuick(i, k) * B.getQuick(j, k)
      }
      expected(i)(j) = s * alpha
    }
    for (r <- 0 until A.rows; c <- 0 until A.rows) {
      assertEquals("("+r+", "+c+")", expected(r)(c), C.getQuick(r, c), TOL)
    }
    C = new DenseDoubleMatrix2D(A.columns, A.columns)
    C.assign(DoubleFunctions.random())
    expected = C.toArray
    C = A.dot(Bt, C, alpha, beta, transposeSelf=true, transposeOther=true)
    for (j <- 0 until A.columns; i <- 0 until A.columns) {
      var s = 0.0
      for (k <- 0 until A.rows) {
        s += A.getQuick(k, i) * Bt.getQuick(j, k)
      }
      expected(i)(j) = s * alpha + expected(i)(j) * beta
    }
    for (r <- 0 until A.columns; c <- 0 until A.columns) {
      assertEquals("("+r+", "+c+")", expected(r)(c), C.getQuick(r, c), TOL)
    }
    C = null
    C = A.dot(Bt, C, alpha, beta, transposeSelf=true, transposeOther=true)
    expected = Array.ofDim[Double](A.columns, A.columns)
    for (j <- 0 until A.columns; i <- 0 until A.columns) {
      var s = 0.0
      for (k <- 0 until A.rows) {
        s += A.getQuick(k, i) * Bt.getQuick(j, k)
      }
      expected(i)(j) = s * alpha
    }
    for (r <- 0 until A.columns; c <- 0 until A.columns) {
      assertEquals("("+r+", "+c+")", expected(r)(c), C.getQuick(r, c), TOL)
    }
  }

  def testZSum() {
    val sum = A.sumAllCells
    var expected = 0.0
    for (r <- 0 until A.rows; c <- 0 until A.columns) {
      expected += A.getQuick(r, c)
    }
    assertEquals(expected, sum, TOL)
  }
}
