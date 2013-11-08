package cern.colt.matrix.tdouble.impl

import cern.colt.matrix.MatrixTypes._
import org.junit.Assert._

class DenseColumnDoubleMatrix2DTest(arg0: String) extends DoubleMatrix2DTest(arg0) {

  protected def createMatrices() {
    A = new DenseColumnDoubleMatrix2D(NROWS, NCOLUMNS)
    B = new DenseColumnDoubleMatrix2D(NROWS, NCOLUMNS)
    Bt = new DenseColumnDoubleMatrix2D(NCOLUMNS, NROWS)
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
    val expected = Array.ofDim[Float](NROWS * NCOLUMNS)
    for (i <- 0 until NROWS * NCOLUMNS) {
      expected(i) = Math.random().toFloat
    }
    A.assign(expected)
    var idx = 0
    for (r <- 0 until A.rows; c <- 0 until A.columns) {
      assertEquals(expected(idx), A.getQuick(r, c), TOL)
      idx += 1
    }
  }
*/

/*
  def testDct2() {
    val Acopy = A.copy()
    A.asInstanceOf[DenseColumnDoubleMatrix2D].dct2(true)
    A.asInstanceOf[DenseColumnDoubleMatrix2D].idct2(true)
    for (r <- 0 until A.rows; c <- 0 until A.columns) assertEquals(0, Math.abs(Acopy.getQuick(r,
      c) - A.getQuick(r, c)), TOL)
  }

  def testDctColumns() {
    val Acopy = A.copy()
    A.asInstanceOf[DenseColumnDoubleMatrix2D].dctColumns(true)
    A.asInstanceOf[DenseColumnDoubleMatrix2D].idctColumns(true)
    for (r <- 0 until A.rows; c <- 0 until A.columns) assertEquals(0, Math.abs(Acopy.getQuick(r,
      c) - A.getQuick(r, c)), TOL)
  }

  def testDctRows() {
    val Acopy = A.copy()
    A.asInstanceOf[DenseColumnDoubleMatrix2D].dctRows(true)
    A.asInstanceOf[DenseColumnDoubleMatrix2D].idctRows(true)
    for (r <- 0 until A.rows; c <- 0 until A.columns) assertEquals(0, Math.abs(Acopy.getQuick(r,
      c) - A.getQuick(r, c)), TOL)
  }

  def testDht2() {
    val Acopy = A.copy()
    A.asInstanceOf[DenseColumnDoubleMatrix2D].dht2()
    A.asInstanceOf[DenseColumnDoubleMatrix2D].idht2(true)
    for (r <- 0 until A.rows; c <- 0 until A.columns) assertEquals(0, Math.abs(Acopy.getQuick(r,
      c) - A.getQuick(r, c)), TOL)
  }

  def testDhtColumns() {
    val Acopy = A.copy()
    A.asInstanceOf[DenseColumnDoubleMatrix2D].dhtColumns()
    A.asInstanceOf[DenseColumnDoubleMatrix2D].idhtColumns(true)
    for (r <- 0 until A.rows; c <- 0 until A.columns) assertEquals(0, Math.abs(Acopy.getQuick(r,
      c) - A.getQuick(r, c)), TOL)
  }

  def testDhtRows() {
    val Acopy = A.copy()
    A.asInstanceOf[DenseColumnDoubleMatrix2D].dhtRows()
    A.asInstanceOf[DenseColumnDoubleMatrix2D].idhtRows(true)
    for (r <- 0 until A.rows; c <- 0 until A.columns) assertEquals(0, Math.abs(Acopy.getQuick(r,
      c) - A.getQuick(r, c)), TOL)
  }

  def testDst2() {
    val Acopy = A.copy()
    A.asInstanceOf[DenseColumnDoubleMatrix2D].dst2(true)
    A.asInstanceOf[DenseColumnDoubleMatrix2D].idst2(true)
    for (r <- 0 until A.rows; c <- 0 until A.columns) assertEquals(0, Math.abs(Acopy.getQuick(r,
      c) - A.getQuick(r, c)), TOL)
  }

  def testDstColumns() {
    val Acopy = A.copy()
    A.asInstanceOf[DenseColumnDoubleMatrix2D].dstColumns(true)
    A.asInstanceOf[DenseColumnDoubleMatrix2D].idstColumns(true)
    for (r <- 0 until A.rows; c <- 0 until A.columns) assertEquals(0, Math.abs(Acopy.getQuick(r,
      c) - A.getQuick(r, c)), TOL)
  }

  def testDstRows() {
    val Acopy = A.copy()
    A.asInstanceOf[DenseColumnDoubleMatrix2D].dstRows(true)
    A.asInstanceOf[DenseColumnDoubleMatrix2D].idstRows(true)
    for (r <- 0 until A.rows; c <- 0 until A.columns) assertEquals(0, Math.abs(Acopy.getQuick(r,
      c) - A.getQuick(r, c)), TOL)
  }

  def testFft2() {
    val nrows = 64
    val ncolumns = 128
    val A = new DenseColumnDoubleMatrix2D(nrows, ncolumns)
    val Acopy = A.copy()
    A.asInstanceOf[DenseColumnDoubleMatrix2D].fft2()
    A.asInstanceOf[DenseColumnDoubleMatrix2D].ifft2(true)
    for (r <- 0 until nrows; c <- 0 until ncolumns) {
      assertEquals(Acopy.getQuick(r, c), A.getQuick(r, c), TOL)
    }
  }

  def testGetFft2() {
    val Acopy = A.copy()
    val Ac = A.asInstanceOf[DenseColumnDoubleMatrix2D].getFft2
    Ac.asInstanceOf[DenseDComplexMatrix2D].ifft2(true)
    for (r <- 0 until A.rows; c <- 0 until A.columns) {
      val elemAc = Ac.getQuick(r, c)
      assertEquals(Acopy.getQuick(r, c), elemAc(0), TOL)
      assertEquals(0, elemAc(1), TOL)
    }
  }

  def testGetIfft2() {
    val Acopy = A.copy()
    val Ac = A.asInstanceOf[DenseColumnDoubleMatrix2D].getIfft2(true)
    Ac.asInstanceOf[DenseDComplexMatrix2D].fft2()
    for (r <- 0 until A.rows; c <- 0 until A.columns) {
      val elemAc = Ac.getQuick(r, c)
      assertEquals(Acopy.getQuick(r, c), elemAc(0), TOL)
      assertEquals(0, elemAc(1), TOL)
    }
  }

  def testGetFftColumns() {
    val Acopy = A.copy()
    val Ac = A.asInstanceOf[DenseColumnDoubleMatrix2D].getFftColumns
    Ac.asInstanceOf[DenseDComplexMatrix2D].ifftColumns(true)
    for (r <- 0 until A.rows; c <- 0 until A.columns) {
      val elemAc = Ac.getQuick(r, c)
      assertEquals(Acopy.getQuick(r, c), elemAc(0), TOL)
      assertEquals(0, elemAc(1), TOL)
    }
  }

  def testGetIfftColumns() {
    val Acopy = A.copy()
    val Ac = A.asInstanceOf[DenseColumnDoubleMatrix2D].getIfftColumns(true)
    Ac.asInstanceOf[DenseDComplexMatrix2D].fftColumns()
    for (r <- 0 until A.rows; c <- 0 until A.columns) {
      val elemAc = Ac.getQuick(r, c)
      assertEquals(Acopy.getQuick(r, c), elemAc(0), TOL)
      assertEquals(0, elemAc(1), TOL)
    }
  }

  def testGetFftRows() {
    val Acopy = A.copy()
    val Ac = A.asInstanceOf[DenseColumnDoubleMatrix2D].getFftRows
    Ac.asInstanceOf[DenseDComplexMatrix2D].ifftRows(true)
    for (r <- 0 until A.rows; c <- 0 until A.columns) {
      val elemAc = Ac.getQuick(r, c)
      assertEquals(Acopy.getQuick(r, c), elemAc(0), TOL)
      assertEquals(0, elemAc(1), TOL)
    }
  }

  def testGetIfftRows() {
    val Acopy = A.copy()
    val Ac = A.asInstanceOf[DenseColumnDoubleMatrix2D].getIfftRows(true)
    Ac.asInstanceOf[DenseDComplexMatrix2D].fftRows()
    for (r <- 0 until A.rows; c <- 0 until A.columns) {
      val elemAc = Ac.getQuick(r, c)
      assertEquals(Acopy.getQuick(r, c), elemAc(0), TOL)
      assertEquals(0, elemAc(1), TOL)
    }
  }
*/
}
