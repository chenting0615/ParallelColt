package cern.colt.matrix.tdouble.impl

import cern.colt.matrix.MatrixTypes.DenseDoubleMatrix2D
import org.junit.Assert._

class DenseMatrix2DTest(arg0: String) extends DoubleMatrix2DTest(arg0) {

  protected def createMatrices() {
    A = new DenseDoubleMatrix2D(NROWS, NCOLUMNS)
    B = new DenseDoubleMatrix2D(NROWS, NCOLUMNS)
    Bt = new DenseDoubleMatrix2D(NCOLUMNS, NROWS)
  }

  def testAssignDoubleArray() {
    val expected = Array.ofDim[Double](A.size.toInt)
    for (i <- 0 until A.size.toInt) {
      expected(i) = Math.random()
    }
    A.assign(expected)
    var idx = 0
    for (r <- 0 until A.rows; c <- 0 until A.columns) {
      assertEquals(0, Math.abs(expected(idx) - A.getQuick(r, c)), TOL)
      idx += 1
    }
  }

/*
  def testAssignFloatArray() {
    val expected = Array.ofDim[Float](A.rows * A.columns)
    for (i <- 0 until A.rows * A.columns) {
      expected(i) = Math.random().toFloat
    }
    A.assignConstant(expected)
    var idx = 0
    for (r <- 0 until A.rows; c <- 0 until A.columns) {
      assertEquals(expected(idx += 1), A.getQuick(r, c), TOL)
    }
  }

  def testDct2() {
    val Acopy = A.copy()
    A.asInstanceOf[DenseMatrix2D].dct2(true)
    A.asInstanceOf[DenseMatrix2D].idct2(true)
    for (r <- 0 until A.rows; c <- 0 until A.columns) assertEquals(0, Math.abs(Acopy.getQuick(r,
      c) - A.getQuick(r, c)), TOL)
  }

  def testDctColumns() {
    val Acopy = A.copy()
    A.asInstanceOf[DenseMatrix2D].dctColumns(true)
    A.asInstanceOf[DenseMatrix2D].idctColumns(true)
    for (r <- 0 until A.rows; c <- 0 until A.columns) assertEquals(0, Math.abs(Acopy.getQuick(r,
      c) - A.getQuick(r, c)), TOL)
  }

  def testDctRows() {
    val Acopy = A.copy()
    A.asInstanceOf[DenseMatrix2D].dctRows(true)
    A.asInstanceOf[DenseMatrix2D].idctRows(true)
    for (r <- 0 until A.rows; c <- 0 until A.columns) assertEquals(0, Math.abs(Acopy.getQuick(r,
      c) - A.getQuick(r, c)), TOL)
  }

  def testDht2() {
    val Acopy = A.copy()
    A.asInstanceOf[DenseMatrix2D].dht2()
    A.asInstanceOf[DenseMatrix2D].idht2(true)
    for (r <- 0 until A.rows; c <- 0 until A.columns) assertEquals(0, Math.abs(Acopy.getQuick(r,
      c) - A.getQuick(r, c)), TOL)
  }

  def testDhtColumns() {
    val Acopy = A.copy()
    A.asInstanceOf[DenseMatrix2D].dhtColumns()
    A.asInstanceOf[DenseMatrix2D].idhtColumns(true)
    for (r <- 0 until A.rows; c <- 0 until A.columns) assertEquals(0, Math.abs(Acopy.getQuick(r,
      c) - A.getQuick(r, c)), TOL)
  }

  def testDhtRows() {
    val Acopy = A.copy()
    A.asInstanceOf[DenseMatrix2D].dhtRows()
    A.asInstanceOf[DenseMatrix2D].idhtRows(true)
    for (r <- 0 until A.rows; c <- 0 until A.columns) assertEquals(0, Math.abs(Acopy.getQuick(r,
      c) - A.getQuick(r, c)), TOL)
  }

  def testDst2() {
    val Acopy = A.copy()
    A.asInstanceOf[DenseMatrix2D].dst2(true)
    A.asInstanceOf[DenseMatrix2D].idst2(true)
    for (r <- 0 until A.rows; c <- 0 until A.columns) assertEquals(0, Math.abs(Acopy.getQuick(r,
      c) - A.getQuick(r, c)), TOL)
  }

  def testDstColumns() {
    val Acopy = A.copy()
    A.asInstanceOf[DenseMatrix2D].dstColumns(true)
    A.asInstanceOf[DenseMatrix2D].idstColumns(true)
    for (r <- 0 until A.rows; c <- 0 until A.columns) assertEquals(0, Math.abs(Acopy.getQuick(r,
      c) - A.getQuick(r, c)), TOL)
  }

  def testDstRows() {
    val Acopy = A.copy()
    A.asInstanceOf[DenseMatrix2D].dstRows(true)
    A.asInstanceOf[DenseMatrix2D].idstRows(true)
    for (r <- 0 until A.rows; c <- 0 until A.columns) assertEquals(0, Math.abs(Acopy.getQuick(r,
      c) - A.getQuick(r, c)), TOL)
  }

  def testFft2() {
    val nrows = 64
    val ncolumns = 128
    var A = new DenseMatrix2D(nrows, ncolumns)
    var Acopy = A.copy()
    A.asInstanceOf[DenseMatrix2D].fft2()
    A.asInstanceOf[DenseMatrix2D].ifft2(true)
    for (r <- 0 until nrows; c <- 0 until ncolumns) {
      assertEquals(Acopy.getQuick(r, c), A.getQuick(r, c), TOL)
    }
    A = A.viewTranspose()
    Acopy = A.copy()
    A.asInstanceOf[DenseMatrix2D].fft2()
    A.asInstanceOf[DenseMatrix2D].ifft2(true)
    for (r <- 0 until ncolumns; c <- 0 until nrows) {
      assertEquals(Acopy.getQuick(r, c), A.getQuick(r, c), TOL)
    }
  }

  def testGetFft2() {
    val Acopy = A.copy()
    val Ac = A.asInstanceOf[DenseMatrix2D].getFft2
    Ac.asInstanceOf[DenseDComplexMatrix2D].ifft2(true)
    for (r <- 0 until A.rows; c <- 0 until A.columns) {
      val elemAc = Ac.getQuick(r, c)
      assertEquals(Acopy.getQuick(r, c), elemAc(0), TOL)
      assertEquals(0, elemAc(1), TOL)
    }
  }

  def testGetIfft2() {
    val Acopy = A.copy()
    val Ac = A.asInstanceOf[DenseMatrix2D].getIfft2(true)
    Ac.asInstanceOf[DenseDComplexMatrix2D].fft2()
    for (r <- 0 until A.rows; c <- 0 until A.columns) {
      val elemAc = Ac.getQuick(r, c)
      assertEquals(Acopy.getQuick(r, c), elemAc(0), TOL)
      assertEquals(0, elemAc(1), TOL)
    }
  }

  def testGetFftColumns() {
    val Acopy = A.copy()
    val Ac = A.asInstanceOf[DenseMatrix2D].getFftColumns
    Ac.asInstanceOf[DenseDComplexMatrix2D].ifftColumns(true)
    for (r <- 0 until A.rows; c <- 0 until A.columns) {
      val elemAc = Ac.getQuick(r, c)
      assertEquals(Acopy.getQuick(r, c), elemAc(0), TOL)
      assertEquals(0, elemAc(1), TOL)
    }
  }

  def testGetIfftColumns() {
    val Acopy = A.copy()
    val Ac = A.asInstanceOf[DenseMatrix2D].getIfftColumns(true)
    Ac.asInstanceOf[DenseDComplexMatrix2D].fftColumns()
    for (r <- 0 until A.rows; c <- 0 until A.columns) {
      val elemAc = Ac.getQuick(r, c)
      assertEquals(Acopy.getQuick(r, c), elemAc(0), TOL)
      assertEquals(0, elemAc(1), TOL)
    }
  }

  def testGetFftRows() {
    val Acopy = A.copy()
    val Ac = A.asInstanceOf[DenseMatrix2D].getFftRows
    Ac.asInstanceOf[DenseDComplexMatrix2D].ifftRows(true)
    for (r <- 0 until A.rows; c <- 0 until A.columns) {
      val elemAc = Ac.getQuick(r, c)
      assertEquals(Acopy.getQuick(r, c), elemAc(0), TOL)
      assertEquals(0, elemAc(1), TOL)
    }
  }

  def testGetIfftRows() {
    val Acopy = A.copy()
    val Ac = A.asInstanceOf[DenseMatrix2D].getIfftRows(true)
    Ac.asInstanceOf[DenseDComplexMatrix2D].fftRows()
    for (r <- 0 until A.rows; c <- 0 until A.columns) {
      val elemAc = Ac.getQuick(r, c)
      assertEquals(Acopy.getQuick(r, c), elemAc(0), TOL)
      assertEquals(0, elemAc(1), TOL)
    }
  }
*/
}
