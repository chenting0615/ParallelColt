package cern.colt.matrix.tdcomplex.impl

import cern.colt.matrix.tdcomplex.DComplexMatrix2D
import cern.colt.matrix.tdcomplex.DComplexMatrix2DTest
//remove if not needed
import scala.collection.JavaConversions._

class DenseColumnDComplexMatrix2DTest(arg0: String) extends DComplexMatrix2DTest(arg0) {

  protected def createMatrices() {
    A = new DenseColumnDComplexMatrix2D(NROWS, NCOLUMNS)
    B = new DenseColumnDComplexMatrix2D(NROWS, NCOLUMNS)
    Bt = new DenseColumnDComplexMatrix2D(NCOLUMNS, NROWS)
  }

  def testAssignDoubleArray() {
    val expected = Array.ofDim[Double](2 * A.size.toInt)
    for (i <- 0 until 2 * A.size) {
      expected(i) = Math.random()
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

  def testAssignFloatArray() {
    val expected = Array.ofDim[Float](NROWS * 2 * NCOLUMNS)
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

  def testAssignDoubleArrayArray() {
    val expected = Array.ofDim[Double](NCOLUMNS, 2 * NROWS)
    for (c <- 0 until NCOLUMNS; r <- 0 until 2 * NROWS) {
      expected(c)(r) = Math.random()
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
    A.asInstanceOf[DenseColumnDComplexMatrix2D].fft2()
    A.asInstanceOf[DenseColumnDComplexMatrix2D].ifft2(true)
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) {
      assertEquals(Acopy.getQuick(r, c), A.getQuick(r, c), TOL)
    }
  }

  def testFftColumns() {
    val Acopy = A.copy()
    A.asInstanceOf[DenseColumnDComplexMatrix2D].fftColumns()
    A.asInstanceOf[DenseColumnDComplexMatrix2D].ifftColumns(true)
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) {
      assertEquals(Acopy.getQuick(r, c), A.getQuick(r, c), TOL)
    }
  }

  def testFftRows() {
    val Acopy = A.copy()
    A.asInstanceOf[DenseColumnDComplexMatrix2D].fftRows()
    A.asInstanceOf[DenseColumnDComplexMatrix2D].ifftRows(true)
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) {
      assertEquals(Acopy.getQuick(r, c), A.getQuick(r, c), TOL)
    }
  }
}
