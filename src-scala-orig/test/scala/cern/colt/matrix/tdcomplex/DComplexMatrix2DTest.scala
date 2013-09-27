package cern.colt.matrix.tdcomplex

import java.util.ArrayList
import junit.framework.TestCase
import cern.colt.function.tdcomplex.DComplexProcedure
import cern.colt.function.tdcomplex.IntIntDComplexFunction
import cern.colt.list.tint.IntArrayList
import cern.colt.matrix.tdcomplex.impl.DenseDComplexMatrix1D
import cern.colt.matrix.tdouble.DoubleFactory2D
import cern.colt.matrix.tdouble.StrideMatrix2D
import cern.jet.math.tdcomplex.DComplex
import cern.jet.math.tdcomplex.DComplexFunctions
import edu.emory.mathcs.utils.ConcurrencyUtils
//remove if not needed
import scala.collection.JavaConversions._

abstract class DComplexMatrix2DTest(arg0: String) extends TestCase(arg0) {

  /**
   * Matrix to test
   */
  protected var A: DComplexMatrix2D = _

  /**
   * Matrix of the same size as A
   */
  protected var B: DComplexMatrix2D = _

  /**
   * Matrix of the size A.columns() x A.rows()
   */
  protected var Bt: DComplexMatrix2D = _

  protected var NROWS: Int = 13

  protected var NCOLUMNS: Int = 17

  protected var TOL: Double = 1e-10

  protected def setUp() {
    createMatrices()
    populateMatrices()
  }

  protected def createMatrices(): Unit

  protected def populateMatrices() {
    ConcurrencyUtils.setThreadsBeginN_2D(1)
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) {
      A.setQuick(r, c, Array(Math.random(), Math.random()))
    }
    for (r <- 0 until B.rows(); c <- 0 until B.columns()) {
      B.setQuick(r, c, Array(Math.random(), Math.random()))
    }
    for (r <- 0 until Bt.rows(); c <- 0 until Bt.columns()) {
      Bt.setQuick(r, c, Array(Math.random(), Math.random()))
    }
  }

  protected def tearDown() {
    A = B = Bt = null
  }

  def testAggregateComplexComplexComplexFunctionComplexComplexFunction() {
    val actual = A.aggregate(DComplexFunctions.plus, DComplexFunctions.square)
    var expected = Array.ofDim[Double](2)
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) {
      expected = DComplex.plus(expected, DComplex.square(A.getQuick(r, c)))
    }
    assertEquals(expected, actual, TOL)
  }

  def testAggregateComplexMatrix2DComplexComplexComplexFunctionComplexComplexComplexFunction() {
    val actual = A.aggregate(B, DComplexFunctions.plus, DComplexFunctions.mult)
    var expected = Array.ofDim[Double](2)
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) {
      expected = DComplex.plus(expected, DComplex.mult(A.getQuick(r, c), B.getQuick(r, c)))
    }
    assertEquals(expected, actual, TOL)
  }

  def testAssignComplexComplexFunction() {
    val Acopy = A.copy()
    A.assign(DComplexFunctions.acos)
    var tmp: Array[Double] = null
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) {
      tmp = DComplex.acos(Acopy.getQuick(r, c))
      assertEquals(tmp, A.getQuick(r, c), TOL)
    }
  }

  def testAssignComplexMatrix2D() {
    A.assign(B)
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) {
      assertEquals(B.getQuick(r, c), A.getQuick(r, c), TOL)
    }
  }

  def testAssignComplexMatrix2DComplexComplexComplexFunction() {
    val Acopy = A.copy()
    A.assign(B, DComplexFunctions.div)
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) {
      assertEquals(DComplex.div(Acopy.getQuick(r, c), B.getQuick(r, c)), A.getQuick(r, c), TOL)
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
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) {
      if (DComplex.abs(Acopy.getQuick(r, c)) > 3) {
        assertEquals(DComplex.tan(Acopy.getQuick(r, c)), A.getQuick(r, c), TOL)
      } else {
        assertEquals(Acopy.getQuick(r, c), A.getQuick(r, c), TOL)
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
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) {
      if (DComplex.abs(A.getQuick(r, c)) > 3) {
        assertEquals(value, A.getQuick(r, c), TOL)
      } else {
        assertEquals(Acopy.getQuick(r, c), A.getQuick(r, c), TOL)
      }
    }
  }

  def testAssignComplexRealFunction() {
    val Acopy = A.copy()
    A.assign(DComplexFunctions.abs)
    var tmp: Array[Double] = null
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) {
      tmp = A.getQuick(r, c)
      assertEquals(DComplex.abs(Acopy.getQuick(r, c)), tmp(0), TOL)
      assertEquals(0, tmp(1), TOL)
    }
  }

  def testAssignDoubleArray() {
    val expected = Array.ofDim[Double](2 * A.size.toInt)
    for (i <- 0 until 2 * A.size) {
      expected(i) = Math.random()
    }
    A.assign(expected)
    var idx = 0
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) {
      val elem = A.getQuick(r, c)
      assertEquals(expected(idx), elem(0), TOL)
      assertEquals(expected(idx + 1), elem(1), TOL)
      idx += 2
    }
  }

  def testAssignDoubleArrayArray() {
    val expected = Array.ofDim[Double](A.rows(), 2 * A.columns())
    for (r <- 0 until A.rows(); c <- 0 until 2 * A.columns()) {
      expected(r)(c) = Math.random()
    }
    A.assign(expected)
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) {
      val elem = A.getQuick(r, c)
      assertEquals(expected(r)(2 * c), elem(0), TOL)
      assertEquals(expected(r)(2 * c + 1), elem(1), TOL)
    }
  }

  def testAssignDoubleDouble() {
    val value = Array(Math.random(), Math.random())
    A.assign(value(0), value(1))
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) {
      val elem = A.getQuick(r, c)
      assertEquals(value, elem, TOL)
    }
  }

  def testAssignFloatArray() {
    val expected = Array.ofDim[Float](A.rows() * 2 * A.columns())
    for (i <- 0 until 2 * A.size) {
      expected(i) = Math.random().toFloat
    }
    A.assign(expected)
    var idx = 0
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) {
      val elem = A.getQuick(r, c)
      assertEquals(expected(idx), elem(0), TOL)
      assertEquals(expected(idx + 1), elem(1), TOL)
      idx += 2
    }
  }

  def testAssignImaginary() {
    val Im = DoubleFactory2D.dense.random(A.rows(), A.columns())
    val Acopy = A.copy()
    A.assignImaginary(Im)
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) {
      assertEquals(Acopy.getQuick(r, c)(0), A.getQuick(r, c)(0), TOL)
      assertEquals(Im.getQuick(r, c), A.getQuick(r, c)(1), TOL)
    }
  }

  def testAssignReal() {
    val Re = DoubleFactory2D.dense.random(A.rows(), A.columns())
    val Acopy = A.copy()
    A.assignReal(Re)
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) {
      assertEquals(Acopy.getQuick(r, c)(1), A.getQuick(r, c)(1), TOL)
      assertEquals(Re.getQuick(r, c), A.getQuick(r, c)(0), TOL)
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
    assertEquals(true, eq)
    eq = A == Array(value(0) + 1, value(1) + 1)
    assertEquals(false, eq)
  }

  def testEqualsObject() {
    var eq = A == A
    assertEquals(true, eq)
    eq = A == B
    assertEquals(false, eq)
  }

  def testForEachNonZero() {
    val Acopy = A.copy()
    val function = new IntIntDComplexFunction() {

      def apply(first: Int, second: Int, third: Array[Double]): Array[Double] = return DComplex.sqrt(third)
    }
    A.forEachNonZero(function)
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) {
      assertEquals(DComplex.sqrt(Acopy.getQuick(r, c)), A.getQuick(r, c), TOL)
    }
  }

  def testGetConjugateTranspose() {
    val Aconj = A.getConjugateTranspose
    assertEquals(A.rows(), Aconj.columns())
    assertEquals(A.columns(), Aconj.rows())
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) {
      assertEquals(A.getQuick(r, c)(0), Aconj.getQuick(c, r)(0), TOL)
      assertEquals(-A.getQuick(r, c)(1), Aconj.getQuick(c, r)(1), TOL)
    }
  }

  def testGetImaginaryPart() {
    val Im = A.getImaginaryPart
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) {
      assertEquals(A.getQuick(r, c)(1), Im.getQuick(r, c), TOL)
    }
  }

  def testGetNonZeros() {
    val rowList = new IntArrayList()
    val colList = new IntArrayList()
    val valueList = new ArrayList[Array[Double]]()
    A.getNonZeros(rowList, colList, valueList)
    assertEquals(A.size, rowList.size)
    assertEquals(A.size, colList.size)
    assertEquals(A.size, valueList.size)
    var idx = 0
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) {
      assertEquals(A.getQuick(rowList.get(idx), colList.get(idx)), valueList.get(idx), TOL)
      idx += 1
    }
  }

  def testGetRealPart() {
    val Re = A.getRealPart
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) {
      assertEquals(A.getQuick(r, c)(0), Re.getQuick(r, c), TOL)
    }
  }

  def testToArray() {
    val array = A.toArray()
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) {
      assertEquals(A.getQuick(r, c)(0), array(r)(2 * c), TOL)
      assertEquals(A.getQuick(r, c)(1), array(r)(2 * c + 1), TOL)
    }
  }

  def testVectorize() {
    val B = A.vectorize()
    var idx = 0
    for (c <- 0 until A.columns(); r <- 0 until A.rows()) {
      assertEquals(A.getQuick(r, c), B.getQuick(idx += 1), TOL)
    }
  }

  def testViewColumn() {
    val B = A.viewColumn(A.columns() / 2)
    assertEquals(A.rows(), B.size)
    for (r <- 0 until A.rows()) {
      assertEquals(A.getQuick(r, A.columns() / 2), B.getQuick(r), TOL)
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
    for (r <- 0 until A.rows() / 3; c <- 0 until A.columns() / 3) {
      assertEquals(A.getQuick(A.rows() / 2 + r, A.columns() / 2 + c), B.getQuick(r, c), TOL)
    }
  }

  def testViewRow() {
    val B = A.viewRow(A.rows() / 2)
    assertEquals(A.columns(), B.size)
    for (c <- 0 until A.columns()) {
      assertEquals(A.getQuick(A.rows() / 2, c), B.getQuick(c), TOL)
    }
  }

  def testViewRowFlip() {
    val B = A.viewRowFlip()
    assertEquals(A.size, B.size)
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) {
      assertEquals(A.getQuick(A.rows() - 1 - r, c), B.getQuick(r, c), TOL)
    }
  }

  def testViewSelectionComplexMatrix1DProcedure() {
    val value = Array(Math.random(), Math.random())
    A.setQuick(A.rows() / 3, 0, value)
    A.setQuick(A.rows() / 2, 0, value)
    val B = A.viewSelection(new DComplexMatrix1DProcedure() {

      def apply(element: DComplexMatrix1D): Boolean = {
        return DComplex.isEqual(element.getQuick(0), value, TOL)
      }
    })
    assertEquals(2, B.rows())
    assertEquals(A.columns(), B.columns())
    assertEquals(A.getQuick(A.rows() / 3, 0), B.getQuick(0, 0), TOL)
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

  def testViewStrides() {
    val rowStride = 3
    val colStride = 5
    val B = A.viewStrides(rowStride, colStride)
    for (r <- 0 until B.rows(); c <- 0 until B.columns()) {
      assertEquals(A.getQuick(r * rowStride, c * colStride), B.getQuick(r, c), TOL)
    }
  }

  def testZMultDComplexMatrix1DDComplexMatrix1DDComplexDComplexBoolean() {
    var y = new DenseDComplexMatrix1D(A.columns())
    for (i <- 0 until y.size) {
      y.setQuick(i, Array(Math.random(), Math.random()))
    }
    val alpha = Array(3, 2)
    val beta = Array(5, 4)
    var z: DComplexMatrix1D = null
    z = A.zMult(y, z, alpha, beta, false)
    var expected = Array.ofDim[Double](2 * A.rows())
    var tmp = Array.ofDim[Double](2)
    for (r <- 0 until A.rows()) {
      var s = Array.ofDim[Double](2)
      for (c <- 0 until A.columns()) {
        s = DComplex.plus(s, DComplex.mult(A.getQuick(r, c), y.getQuick(c)))
      }
      tmp(0) = expected(2 * r)
      tmp(1) = expected(2 * r + 1)
      tmp = DComplex.mult(beta, tmp)
      tmp = DComplex.plus(tmp, DComplex.mult(alpha, s))
      expected(2 * r) = tmp(0)
      expected(2 * r + 1) = tmp(1)
    }
    for (r <- 0 until A.rows()) {
      assertEquals(expected(2 * r), z.getQuick(r)(0), TOL)
      assertEquals(expected(2 * r + 1), z.getQuick(r)(1), TOL)
    }
    y = new DenseDComplexMatrix1D(A.rows())
    for (i <- 0 until y.size) {
      y.setQuick(i, Array(Math.random(), Math.random()))
    }
    z = null
    z = A.zMult(y, z, alpha, beta, true)
    expected = Array.ofDim[Double](2 * A.columns())
    for (r <- 0 until A.columns()) {
      var s = Array.ofDim[Double](2)
      for (c <- 0 until A.rows()) {
        s = DComplex.plus(s, DComplex.mult(DComplex.conj(A.getQuick(c, r)), y.getQuick(c)))
      }
      tmp(0) = expected(2 * r)
      tmp(1) = expected(2 * r + 1)
      tmp = DComplex.mult(beta, tmp)
      tmp = DComplex.plus(tmp, DComplex.mult(alpha, s))
      expected(2 * r) = tmp(0)
      expected(2 * r + 1) = tmp(1)
    }
    for (r <- 0 until A.columns()) {
      assertEquals(expected(2 * r), z.getQuick(r)(0), TOL)
      assertEquals(expected(2 * r + 1), z.getQuick(r)(1), TOL)
    }
  }

  def testZMultDoubleMatrix2DDoubleMatrix2DDoubleDoubleBooleanBoolean() {
    val alpha = Array(3, 2)
    val beta = Array(5, 4)
    var tmp = Array.ofDim[Double](2)
    var C: DComplexMatrix2D = null
    C = A.zMult(Bt, C, alpha, beta, false, false)
    var expected = Array.ofDim[Double](A.rows(), 2 * A.rows())
    for (j <- 0 until A.rows(); i <- 0 until A.rows()) {
      var s = Array.ofDim[Double](2)
      for (k <- 0 until A.columns()) {
        s = DComplex.plus(s, DComplex.mult(A.getQuick(i, k), Bt.getQuick(k, j)))
      }
      tmp(0) = expected(i)(2 * j)
      tmp(1) = expected(i)(2 * j + 1)
      tmp = DComplex.mult(tmp, beta)
      tmp = DComplex.plus(tmp, DComplex.mult(s, alpha))
      expected(i)(2 * j) = tmp(0)
      expected(i)(2 * j + 1) = tmp(1)
    }
    for (r <- 0 until A.rows(); c <- 0 until A.rows()) {
      assertEquals(expected(r)(2 * c), C.getQuick(r, c)(0), TOL)
      assertEquals(expected(r)(2 * c + 1), C.getQuick(r, c)(1), TOL)
    }
    C = null
    C = A.zMult(B, C, alpha, beta, true, false)
    expected = Array.ofDim[Double](A.columns(), 2 * A.columns())
    for (j <- 0 until A.columns(); i <- 0 until A.columns()) {
      var s = Array.ofDim[Double](2)
      for (k <- 0 until A.rows()) {
        s = DComplex.plus(s, DComplex.mult(DComplex.conj(A.getQuick(k, i)), B.getQuick(k, j)))
      }
      tmp(0) = expected(i)(2 * j)
      tmp(1) = expected(i)(2 * j + 1)
      tmp = DComplex.mult(tmp, beta)
      tmp = DComplex.plus(tmp, DComplex.mult(s, alpha))
      expected(i)(2 * j) = tmp(0)
      expected(i)(2 * j + 1) = tmp(1)
    }
    for (r <- 0 until A.columns(); c <- 0 until A.columns()) {
      assertEquals(expected(r)(2 * c), C.getQuick(r, c)(0), TOL)
      assertEquals(expected(r)(2 * c + 1), C.getQuick(r, c)(1), TOL)
    }
    C = null
    C = A.zMult(B, C, alpha, beta, false, true)
    expected = Array.ofDim[Double](A.rows(), 2 * A.rows())
    for (j <- 0 until A.rows(); i <- 0 until A.rows()) {
      var s = Array.ofDim[Double](2)
      for (k <- 0 until A.columns()) {
        s = DComplex.plus(s, DComplex.mult(A.getQuick(i, k), DComplex.conj(B.getQuick(j, k))))
      }
      tmp(0) = expected(i)(2 * j)
      tmp(1) = expected(i)(2 * j + 1)
      tmp = DComplex.mult(tmp, beta)
      tmp = DComplex.plus(tmp, DComplex.mult(s, alpha))
      expected(i)(2 * j) = tmp(0)
      expected(i)(2 * j + 1) = tmp(1)
    }
    for (r <- 0 until A.rows(); c <- 0 until A.rows()) {
      assertEquals(expected(r)(2 * c), C.getQuick(r, c)(0), TOL)
      assertEquals(expected(r)(2 * c + 1), C.getQuick(r, c)(1), TOL)
    }
    C = null
    C = A.zMult(Bt, C, alpha, beta, true, true)
    expected = Array.ofDim[Double](A.columns(), 2 * A.columns())
    for (j <- 0 until A.columns(); i <- 0 until A.columns()) {
      var s = Array.ofDim[Double](2)
      for (k <- 0 until A.rows()) {
        s = DComplex.plus(s, DComplex.mult(DComplex.conj(A.getQuick(k, i)), DComplex.conj(Bt.getQuick(j,
          k))))
      }
      tmp(0) = expected(i)(2 * j)
      tmp(1) = expected(i)(2 * j + 1)
      tmp = DComplex.mult(tmp, beta)
      tmp = DComplex.plus(tmp, DComplex.mult(s, alpha))
      expected(i)(2 * j) = tmp(0)
      expected(i)(2 * j + 1) = tmp(1)
    }
    for (r <- 0 until A.columns(); c <- 0 until A.columns()) {
      assertEquals(expected(r)(2 * c), C.getQuick(r, c)(0), TOL)
      assertEquals(expected(r)(2 * c + 1), C.getQuick(r, c)(1), TOL)
    }
  }

  def testZSum() {
    val actual = A.zSum()
    var expected = Array.ofDim[Double](2)
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) {
      expected = DComplex.plus(expected, A.getQuick(r, c))
    }
    assertEquals(expected, actual, TOL)
  }

  protected def assertEquals(expected: Array[Double], actual: Array[Double], tol: Double) {
    for (i <- 0 until actual.length) {
      assertEquals(expected(i), actual(i), tol)
    }
  }
}
