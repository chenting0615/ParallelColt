package cern.colt.matrix.tdcomplex

import java.util.ArrayList
import junit.framework.TestCase
import cern.colt.function.tdcomplex.DComplexProcedure
import cern.colt.list.tint.IntArrayList
import cern.colt.matrix.tdouble.DoubleFactory3D
import cern.colt.matrix.tdouble.DoubleMatrix3D
import cern.jet.math.tdcomplex.DComplex
import cern.jet.math.tdcomplex.DComplexFunctions
import edu.emory.mathcs.utils.ConcurrencyUtils
//remove if not needed
import scala.collection.JavaConversions._

abstract class DComplexMatrix3DTest(arg0: String) extends TestCase(arg0) {

  /**
   * Matrix to test
   */
  protected var A: DComplexMatrix3D = _

  /**
   * Matrix of the same size as A
   */
  protected var B: DComplexMatrix3D = _

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
    ConcurrencyUtils.setThreadsBeginN_3D(1)
    for (s <- 0 until A.slices(); r <- 0 until A.rows(); c <- 0 until A.columns()) {
      A.setQuick(s, r, c, Array(Math.random(), Math.random()))
    }
    for (s <- 0 until B.slices(); r <- 0 until B.rows(); c <- 0 until B.columns()) {
      B.setQuick(s, r, c, Array(Math.random(), Math.random()))
    }
  }

  protected def tearDown() {
    A = B = null
  }

  protected def assertEquals(expected: Array[Double], actual: Array[Double], tol: Double) {
    for (i <- 0 until actual.length) {
      assertEquals(expected(i), actual(i), tol)
    }
  }

  def testAggregateComplexComplexComplexFunctionComplexComplexFunction() {
    val actual = A.aggregate(DComplexFunctions.plus, DComplexFunctions.sqrt)
    var expected = Array.ofDim[Double](2)
    for (s <- 0 until A.slices(); r <- 0 until A.rows(); c <- 0 until A.columns()) {
      expected = DComplex.plus(expected, DComplex.sqrt(A.getQuick(s, r, c)))
    }
    assertEquals(expected, actual, TOL)
  }

  def testAggregateComplexMatrix3DComplexComplexComplexFunctionComplexComplexComplexFunction() {
    val actual = A.aggregate(B, DComplexFunctions.plus, DComplexFunctions.mult)
    var expected = Array.ofDim[Double](2)
    for (s <- 0 until A.slices(); r <- 0 until A.rows(); c <- 0 until A.columns()) {
      expected = DComplex.plus(expected, DComplex.mult(A.getQuick(s, r, c), B.getQuick(s, r, c)))
    }
    assertEquals(expected, actual, TOL)
  }

  def testAssignComplexComplexFunction() {
    val Acopy = A.copy()
    A.assign(DComplexFunctions.acos)
    for (s <- 0 until A.slices(); r <- 0 until A.rows(); c <- 0 until A.columns()) {
      assertEquals(DComplex.acos(Acopy.getQuick(s, r, c)), A.getQuick(s, r, c), TOL)
    }
  }

  def testAssignComplexMatrix3D() {
    A.assign(B)
    for (s <- 0 until A.slices(); r <- 0 until A.rows(); c <- 0 until A.columns()) {
      assertEquals(B.getQuick(s, r, c), A.getQuick(s, r, c), TOL)
    }
  }

  def testAssignComplexMatrix3DComplexComplexComplexFunction() {
    val Acopy = A.copy()
    A.assign(B, DComplexFunctions.div)
    for (s <- 0 until A.slices(); r <- 0 until A.rows(); c <- 0 until A.columns()) {
      assertEquals(DComplex.div(Acopy.getQuick(s, r, c), B.getQuick(s, r, c)), A.getQuick(s, r, c), TOL)
    }
  }

  def testAssignComplexProcedureComplexComplexFunction() {
    val Acopy = A.copy()
    A.assign(new DComplexProcedure() {

      def apply(element: Array[Double]): Boolean = {
        if (DComplex.abs(element) > 3) {
          return true
        } else {
          return false
        }
      }
    }, DComplexFunctions.tan)
    for (s <- 0 until A.slices(); r <- 0 until A.rows(); c <- 0 until A.columns()) {
      if (DComplex.abs(Acopy.getQuick(s, r, c)) > 3) {
        assertEquals(DComplex.abs(Acopy.getQuick(s, r, c)), A.getQuick(s, r, c)(0), TOL)
        assertEquals(0, A.getQuick(s, r, c)(1), TOL)
      } else {
        assertEquals(Acopy.getQuick(s, r, c), A.getQuick(s, r, c), TOL)
      }
    }
  }

  def testAssignComplexProcedureDoubleArray() {
    val Acopy = A.copy()
    val value = Array(Math.random(), Math.random())
    A.assign(new DComplexProcedure() {

      def apply(element: Array[Double]): Boolean = {
        if (DComplex.abs(element) > 3) {
          return true
        } else {
          return false
        }
      }
    }, value)
    for (s <- 0 until A.slices(); r <- 0 until A.rows(); c <- 0 until A.columns()) {
      if (DComplex.abs(Acopy.getQuick(s, r, c)) > 3) {
        assertEquals(value, A.getQuick(s, r, c), TOL)
      } else {
        assertEquals(Acopy.getQuick(s, r, c), A.getQuick(s, r, c), TOL)
      }
    }
  }

  def testAssignComplexRealFunction() {
    val Acopy = A.copy()
    A.assign(DComplexFunctions.abs)
    for (s <- 0 until A.slices(); r <- 0 until A.rows(); c <- 0 until A.columns()) {
      assertEquals(DComplex.abs(A.getQuick(s, r, c)), A.getQuick(s, r, c)(0), TOL)
      assertEquals(0, A.getQuick(s, r, c)(1), TOL)
    }
  }

  def testAssignDoubleArray() {
    val expected = Array.ofDim[Double](2 * A.size.toInt)
    for (i <- 0 until 2 * A.size) {
      expected(i) = Math.random()
    }
    A.assign(expected)
    var idx = 0
    for (s <- 0 until A.slices(); r <- 0 until A.rows(); c <- 0 until A.columns()) {
      assertEquals(expected(idx), A.getQuick(s, r, c)(0), TOL)
      assertEquals(expected(idx + 1), A.getQuick(s, r, c)(1), TOL)
      idx += 2
    }
  }

  def testAssignDoubleArrayArrayArray() {
    val expected = Array.ofDim[Double](A.slices(), A.rows(), 2 * A.columns())
    for (s <- 0 until A.slices(); r <- 0 until A.rows(); c <- 0 until 2 * A.columns()) {
      expected(s)(r)(c) = Math.random()
    }
    A.assign(expected)
    for (s <- 0 until A.slices()) {
      assertTrue(A.rows() == expected(s).length)
      for (r <- 0 until A.rows()) {
        assertTrue(2 * A.columns() == expected(s)(r).length)
        for (c <- 0 until A.columns()) {
          assertEquals(expected(s)(r)(2 * c), A.getQuick(s, r, c)(0), TOL)
          assertEquals(expected(s)(r)(2 * c + 1), A.getQuick(s, r, c)(1), TOL)
        }
      }
    }
  }

  def testAssignDoubleDouble() {
    val value = Array(Math.random(), Math.random())
    A.assign(value(0), value(1))
    for (s <- 0 until A.slices(); r <- 0 until A.rows(); c <- 0 until A.columns()) {
      assertEquals(value, A.getQuick(s, r, c), TOL)
    }
  }

  def testAssignImaginary() {
    val Acopy = A.copy()
    val Im = DoubleFactory3D.dense.random(A.slices(), A.rows(), A.columns())
    A.assignImaginary(Im)
    for (s <- 0 until A.slices(); r <- 0 until A.rows(); c <- 0 until A.columns()) {
      assertEquals(Im.getQuick(s, r, c), A.getQuick(s, r, c)(1), TOL)
      assertEquals(Acopy.getQuick(s, r, c)(0), A.getQuick(s, r, c)(0), TOL)
    }
  }

  def testAssignReal() {
    val Acopy = A.copy()
    val Re = DoubleFactory3D.dense.random(A.slices(), A.rows(), A.columns())
    A.assignReal(Re)
    for (s <- 0 until A.slices(); r <- 0 until A.rows(); c <- 0 until A.columns()) {
      assertEquals(Re.getQuick(s, r, c), A.getQuick(s, r, c)(0), TOL)
      assertEquals(Acopy.getQuick(s, r, c)(1), A.getQuick(s, r, c)(1), TOL)
    }
  }

  def testCardinality() {
    val card = A.cardinality()
    assertEquals(A.size, card)
  }

  def testEqualsDoubleArray() {
    val value = Array(Math.random(), Math.random())
    A.assign(value(0), value(1))
    var eq = A == value
    assertTrue(eq)
    eq = A == Array(value(0) + 1, value(1) + 1)
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
    for (s <- 0 until A.slices(); r <- 0 until A.rows(); c <- 0 until A.columns()) {
      assertEquals(A.getQuick(s, r, c)(1), Im.getQuick(s, r, c), TOL)
    }
  }

  def testGetRealPart() {
    val Re = A.getRealPart
    for (s <- 0 until A.slices(); r <- 0 until A.rows(); c <- 0 until A.columns()) {
      assertEquals(A.getQuick(s, r, c)(0), Re.getQuick(s, r, c), TOL)
    }
  }

  def testGetNonZeros() {
    val sliceList = new IntArrayList()
    val rowList = new IntArrayList()
    val colList = new IntArrayList()
    val valueList = new ArrayList[Array[Double]]()
    A.getNonZeros(sliceList, rowList, colList, valueList)
    assertEquals(A.size, sliceList.size)
    assertEquals(A.size, rowList.size)
    assertEquals(A.size, colList.size)
    assertEquals(A.size, valueList.size)
    var idx = 0
    for (s <- 0 until A.slices(); r <- 0 until A.rows(); c <- 0 until A.columns()) {
      assertEquals(A.getQuick(sliceList.get(idx), rowList.get(idx), colList.get(idx)), valueList.get(idx), 
        TOL)
      idx += 1
    }
  }

  def testToArray() {
    val array = A.toArray()
    for (s <- 0 until A.slices(); r <- 0 until A.rows(); c <- 0 until A.columns()) {
      assertEquals(A.getQuick(s, r, c)(0), array(s)(r)(2 * c), TOL)
      assertEquals(A.getQuick(s, r, c)(1), array(s)(r)(2 * c + 1), TOL)
    }
  }

  def testVectorize() {
    val B = A.vectorize()
    var idx = 0
    for (s <- 0 until A.slices(); c <- 0 until A.columns(); r <- 0 until A.rows()) {
      assertEquals(A.getQuick(s, r, c), B.getQuick(idx += 1), TOL)
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

  def testViewSelectionComplexMatrix2DProcedure() {
    val value = Array(2, 3)
    A.setQuick(A.slices() / 2, A.rows() / 2, 0, value)
    val B = A.viewSelection(new DComplexMatrix2DProcedure() {

      def apply(element: DComplexMatrix2D): Boolean = {
        return DComplex.isEqual(element.getQuick(A.rows() / 2, 0), value, TOL)
      }
    })
    assertEquals(1, B.slices())
    assertEquals(A.rows(), B.rows())
    assertEquals(A.columns(), B.columns())
    assertEquals(A.getQuick(A.slices() / 2, A.rows() / 2, 0), B.getQuick(0, A.rows() / 2, 0), TOL)
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
    var expected = Array.ofDim[Double](2)
    for (s <- 0 until A.slices(); r <- 0 until A.rows(); c <- 0 until A.columns()) {
      expected = DComplex.plus(expected, A.getQuick(s, r, c))
    }
    assertEquals(expected, sum, TOL)
  }
}
