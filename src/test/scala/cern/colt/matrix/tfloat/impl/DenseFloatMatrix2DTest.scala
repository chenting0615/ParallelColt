package cern.colt.matrix.tfloat.impl

import cern.colt.matrix.tfloat.FloatMatrix2DTest
import cern.colt.matrix.MatrixTypes.DenseFloatMatrix2D
import cern.colt.matrix.MatrixTypes
import org.junit.Assert

class DenseFloatMatrix2DTest(arg0: String) extends FloatMatrix2DTest(arg0) {

  protected def createMatrices() {
    A = new DenseFloatMatrix2D(NROWS, NCOLUMNS)
    B = new MatrixTypes.DenseFloatMatrix2D(NROWS, NCOLUMNS)
    Bt = new MatrixTypes.DenseFloatMatrix2D(NCOLUMNS, NROWS)
  }

  def testAssignFloatArray() {
    val expected = Array.ofDim[Float](A.size.toInt)
    for (i <- 0 until A.size.toInt) {
      expected(i) = Math.random().toFloat
    }
    A.assign(expected)
    var idx = 0
    for (r <- 0 until A.rows; c <- 0 until A.columns) {
      Assert.assertEquals(0, Math.abs(expected(idx) - A.getQuick(r, c)), TOL)
      idx += 1
    }
  }

/*
  def testDct2() {
    val Acopy = A.copy()
    A.asInstanceOf[DenseFloatMatrix2D].dct2(true)
    A.asInstanceOf[DenseFloatMatrix2D].idct2(true)
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) assertEquals(0, Math.abs(Acopy.getQuick(r,
      c) - A.getQuick(r, c)), TOL)
  }

  def testDctColumns() {
    val Acopy = A.copy()
    A.asInstanceOf[DenseFloatMatrix2D].dctColumns(true)
    A.asInstanceOf[DenseFloatMatrix2D].idctColumns(true)
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) assertEquals(0, Math.abs(Acopy.getQuick(r,
      c) - A.getQuick(r, c)), TOL)
  }

  def testDctRows() {
    val Acopy = A.copy()
    A.asInstanceOf[DenseFloatMatrix2D].dctRows(true)
    A.asInstanceOf[DenseFloatMatrix2D].idctRows(true)
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) assertEquals(0, Math.abs(Acopy.getQuick(r,
      c) - A.getQuick(r, c)), TOL)
  }

  def testDht2() {
    val Acopy = A.copy()
    A.asInstanceOf[DenseFloatMatrix2D].dht2()
    A.asInstanceOf[DenseFloatMatrix2D].idht2(true)
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) assertEquals(0, Math.abs(Acopy.getQuick(r,
      c) - A.getQuick(r, c)), TOL)
  }

  def testDhtColumns() {
    val Acopy = A.copy()
    A.asInstanceOf[DenseFloatMatrix2D].dhtColumns()
    A.asInstanceOf[DenseFloatMatrix2D].idhtColumns(true)
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) assertEquals(0, Math.abs(Acopy.getQuick(r,
      c) - A.getQuick(r, c)), TOL)
  }

  def testDhtRows() {
    val Acopy = A.copy()
    A.asInstanceOf[DenseFloatMatrix2D].dhtRows()
    A.asInstanceOf[DenseFloatMatrix2D].idhtRows(true)
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) assertEquals(0, Math.abs(Acopy.getQuick(r,
      c) - A.getQuick(r, c)), TOL)
  }

  def testDst2() {
    val Acopy = A.copy()
    A.asInstanceOf[DenseFloatMatrix2D].dst2(true)
    A.asInstanceOf[DenseFloatMatrix2D].idst2(true)
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) assertEquals(0, Math.abs(Acopy.getQuick(r,
      c) - A.getQuick(r, c)), TOL)
  }

  def testDstColumns() {
    val Acopy = A.copy()
    A.asInstanceOf[DenseFloatMatrix2D].dstColumns(true)
    A.asInstanceOf[DenseFloatMatrix2D].idstColumns(true)
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) assertEquals(0, Math.abs(Acopy.getQuick(r,
      c) - A.getQuick(r, c)), TOL)
  }

  def testDstRows() {
    val Acopy = A.copy()
    A.asInstanceOf[DenseFloatMatrix2D].dstRows(true)
    A.asInstanceOf[DenseFloatMatrix2D].idstRows(true)
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) assertEquals(0, Math.abs(Acopy.getQuick(r,
      c) - A.getQuick(r, c)), TOL)
  }

  def testFft2() {
    val nrows = 64
    val ncolumns = 128
    var A = new DenseFloatMatrix2D(nrows, ncolumns)
    var Acopy = A.copy()
    A.asInstanceOf[DenseFloatMatrix2D].fft2()
    A.asInstanceOf[DenseFloatMatrix2D].ifft2(true)
    for (r <- 0 until nrows; c <- 0 until ncolumns) {
      assertEquals(Acopy.getQuick(r, c), A.getQuick(r, c), TOL)
    }
    A = A.viewDice()
    Acopy = A.copy()
    A.asInstanceOf[DenseFloatMatrix2D].fft2()
    A.asInstanceOf[DenseFloatMatrix2D].ifft2(true)
    for (r <- 0 until ncolumns; c <- 0 until nrows) {
      assertEquals(Acopy.getQuick(r, c), A.getQuick(r, c), TOL)
    }
  }

  def testGetFft2() {
    val Acopy = A.copy()
    val Ac = A.asInstanceOf[DenseFloatMatrix2D].getFft2
    Ac.asInstanceOf[DenseFComplexMatrix2D].ifft2(true)
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) {
      val elemAc = Ac.getQuick(r, c)
      assertEquals(Acopy.getQuick(r, c), elemAc(0), TOL)
      assertEquals(0, elemAc(1), TOL)
    }
  }

  def testGetIfft2() {
    val Acopy = A.copy()
    val Ac = A.asInstanceOf[DenseFloatMatrix2D].getIfft2(true)
    Ac.asInstanceOf[DenseFComplexMatrix2D].fft2()
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) {
      val elemAc = Ac.getQuick(r, c)
      assertEquals(Acopy.getQuick(r, c), elemAc(0), TOL)
      assertEquals(0, elemAc(1), TOL)
    }
  }

  def testGetFftColumns() {
    val Acopy = A.copy()
    val Ac = A.asInstanceOf[DenseFloatMatrix2D].getFftColumns
    Ac.asInstanceOf[DenseFComplexMatrix2D].ifftColumns(true)
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) {
      val elemAc = Ac.getQuick(r, c)
      assertEquals(Acopy.getQuick(r, c), elemAc(0), TOL)
      assertEquals(0, elemAc(1), TOL)
    }
  }

  def testGetIfftColumns() {
    val Acopy = A.copy()
    val Ac = A.asInstanceOf[DenseFloatMatrix2D].getIfftColumns(true)
    Ac.asInstanceOf[DenseFComplexMatrix2D].fftColumns()
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) {
      val elemAc = Ac.getQuick(r, c)
      assertEquals(Acopy.getQuick(r, c), elemAc(0), TOL)
      assertEquals(0, elemAc(1), TOL)
    }
  }

  def testGetFftRows() {
    val Acopy = A.copy()
    val Ac = A.asInstanceOf[DenseFloatMatrix2D].getFftRows
    Ac.asInstanceOf[DenseFComplexMatrix2D].ifftRows(true)
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) {
      val elemAc = Ac.getQuick(r, c)
      assertEquals(Acopy.getQuick(r, c), elemAc(0), TOL)
      assertEquals(0, elemAc(1), TOL)
    }
  }

  def testGetIfftRows() {
    val Acopy = A.copy()
    val Ac = A.asInstanceOf[DenseFloatMatrix2D].getIfftRows(true)
    Ac.asInstanceOf[DenseFComplexMatrix2D].fftRows()
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) {
      val elemAc = Ac.getQuick(r, c)
      assertEquals(Acopy.getQuick(r, c), elemAc(0), TOL)
      assertEquals(0, elemAc(1), TOL)
    }
  }
*/
}
