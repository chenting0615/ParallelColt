package cern.colt.matrix.tfcomplex.impl

import cern.colt.matrix.tfcomplex.FComplexMatrix2D
import cern.colt.matrix.tfcomplex.FComplexMatrix2DTest
//remove if not needed
import scala.collection.JavaConversions._

class DenseColumnFComplexMatrix2DTest(arg0: String) extends FComplexMatrix2DTest(arg0) {

  protected def createMatrices() {
    A = new DenseColumnFComplexMatrix2D(NROWS, NCOLUMNS)
    B = new DenseColumnFComplexMatrix2D(NROWS, NCOLUMNS)
    Bt = new DenseColumnFComplexMatrix2D(NCOLUMNS, NROWS)
  }

  def testAssignFloatArray() {
    val expected = Array.ofDim[Float](2 * A.size.toInt)
    for (i <- 0 until 2 * A.size) {
      expected(i) = Math.random().toFloat
    }
    A.assign(expected)
    var idx = 0
    for (c <- 0 until A.columns(); r <- 0 until A.rows()) {
      val elem = A.getQuick(r, c)
      assertEquals(expected(idx), elem(0), TOL)
      assertEquals(expected(idx + 1), elem(1), TOL)
      idx += 2
    }
  }

  def testAssignFloatArrayArray() {
    val expected = Array.ofDim[Float](A.columns(), 2 * A.rows())
    for (c <- 0 until A.columns(); r <- 0 until 2 * A.rows()) {
      expected(c)(r) = Math.random().toFloat
    }
    A.assign(expected)
    for (c <- 0 until A.columns(); r <- 0 until A.rows()) {
      val elem = A.getQuick(r, c)
      assertEquals(expected(c)(2 * r), elem(0), TOL)
      assertEquals(expected(c)(2 * r + 1), elem(1), TOL)
    }
  }

  def testFft2() {
    val Acopy = A.copy()
    A.asInstanceOf[DenseColumnFComplexMatrix2D].fft2()
    A.asInstanceOf[DenseColumnFComplexMatrix2D].ifft2(true)
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) {
      assertEquals(Acopy.getQuick(r, c), A.getQuick(r, c), TOL)
    }
  }

  def testFftColumns() {
    val Acopy = A.copy()
    A.asInstanceOf[DenseColumnFComplexMatrix2D].fftColumns()
    A.asInstanceOf[DenseColumnFComplexMatrix2D].ifftColumns(true)
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) {
      assertEquals(Acopy.getQuick(r, c), A.getQuick(r, c), TOL)
    }
  }

  def testFftRows() {
    val Acopy = A.copy()
    A.asInstanceOf[DenseColumnFComplexMatrix2D].fftRows()
    A.asInstanceOf[DenseColumnFComplexMatrix2D].ifftRows(true)
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) {
      assertEquals(Acopy.getQuick(r, c), A.getQuick(r, c), TOL)
    }
  }
}
