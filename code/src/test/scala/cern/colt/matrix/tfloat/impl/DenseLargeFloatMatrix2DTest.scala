package cern.colt.matrix.tfloat.impl

import cern.colt.matrix.tfcomplex.FComplexMatrix2D
import cern.colt.matrix.tfcomplex.impl.DenseLargeFComplexMatrix2D
import cern.colt.matrix.tfloat.FloatMatrix2D
import cern.colt.matrix.tfloat.FloatMatrix2DTest
//remove if not needed
import scala.collection.JavaConversions._

class DenseLargeFloatMatrix2DTest(arg0: String) extends FloatMatrix2DTest(arg0) {

  protected def createMatrices() {
    A = new DenseLargeFloatMatrix2D(NROWS, NCOLUMNS)
    B = new DenseLargeFloatMatrix2D(NROWS, NCOLUMNS)
    Bt = new DenseLargeFloatMatrix2D(NCOLUMNS, NROWS)
  }

  def testDct2() {
    val Acopy = A.copy()
    A.asInstanceOf[WrapperFloatMatrix2D].dct2(true)
    A.asInstanceOf[WrapperFloatMatrix2D].idct2(true)
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) assertEquals(0, Math.abs(Acopy.getQuick(r, 
      c) - A.getQuick(r, c)), TOL)
  }

  def testDctColumns() {
    val Acopy = A.copy()
    A.asInstanceOf[WrapperFloatMatrix2D].dctColumns(true)
    A.asInstanceOf[WrapperFloatMatrix2D].idctColumns(true)
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) assertEquals(0, Math.abs(Acopy.getQuick(r, 
      c) - A.getQuick(r, c)), TOL)
  }

  def testDctRows() {
    val Acopy = A.copy()
    A.asInstanceOf[WrapperFloatMatrix2D].dctRows(true)
    A.asInstanceOf[WrapperFloatMatrix2D].idctRows(true)
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) assertEquals(0, Math.abs(Acopy.getQuick(r, 
      c) - A.getQuick(r, c)), TOL)
  }

  def testDht2() {
    val Acopy = A.copy()
    A.asInstanceOf[WrapperFloatMatrix2D].dht2()
    A.asInstanceOf[WrapperFloatMatrix2D].idht2(true)
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) assertEquals(0, Math.abs(Acopy.getQuick(r, 
      c) - A.getQuick(r, c)), TOL)
  }

  def testDhtColumns() {
    val Acopy = A.copy()
    A.asInstanceOf[WrapperFloatMatrix2D].dhtColumns()
    A.asInstanceOf[WrapperFloatMatrix2D].idhtColumns(true)
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) assertEquals(0, Math.abs(Acopy.getQuick(r, 
      c) - A.getQuick(r, c)), TOL)
  }

  def testDhtRows() {
    val Acopy = A.copy()
    A.asInstanceOf[WrapperFloatMatrix2D].dhtRows()
    A.asInstanceOf[WrapperFloatMatrix2D].idhtRows(true)
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) assertEquals(0, Math.abs(Acopy.getQuick(r, 
      c) - A.getQuick(r, c)), TOL)
  }

  def testDst2() {
    val Acopy = A.copy()
    A.asInstanceOf[WrapperFloatMatrix2D].dst2(true)
    A.asInstanceOf[WrapperFloatMatrix2D].idst2(true)
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) assertEquals(0, Math.abs(Acopy.getQuick(r, 
      c) - A.getQuick(r, c)), TOL)
  }

  def testDstColumns() {
    val Acopy = A.copy()
    A.asInstanceOf[WrapperFloatMatrix2D].dstColumns(true)
    A.asInstanceOf[WrapperFloatMatrix2D].idstColumns(true)
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) assertEquals(0, Math.abs(Acopy.getQuick(r, 
      c) - A.getQuick(r, c)), TOL)
  }

  def testDstRows() {
    val Acopy = A.copy()
    A.asInstanceOf[WrapperFloatMatrix2D].dstRows(true)
    A.asInstanceOf[WrapperFloatMatrix2D].idstRows(true)
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) assertEquals(0, Math.abs(Acopy.getQuick(r, 
      c) - A.getQuick(r, c)), TOL)
  }

  def testFft2() {
    val nrows = 64
    val ncolumns = 128
    var A = new DenseLargeFloatMatrix2D(nrows, ncolumns)
    var Acopy = A.copy()
    A.asInstanceOf[DenseLargeFloatMatrix2D].fft2()
    A.asInstanceOf[DenseLargeFloatMatrix2D].ifft2(true)
    for (r <- 0 until nrows; c <- 0 until ncolumns) {
      assertEquals(Acopy.getQuick(r, c), A.getQuick(r, c), TOL)
    }
    A = A.viewDice()
    Acopy = A.copy()
    A.asInstanceOf[WrapperFloatMatrix2D].fft2()
    A.asInstanceOf[WrapperFloatMatrix2D].ifft2(true)
    for (r <- 0 until ncolumns; c <- 0 until nrows) {
      assertEquals(Acopy.getQuick(r, c), A.getQuick(r, c), TOL)
    }
  }

  def testGetFft2() {
    val Acopy = A.copy()
    val Ac = A.asInstanceOf[WrapperFloatMatrix2D].getFft2
    Ac.asInstanceOf[DenseLargeFComplexMatrix2D].ifft2(true)
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) {
      val elemAc = Ac.getQuick(r, c)
      assertEquals(Acopy.getQuick(r, c), elemAc(0), TOL)
      assertEquals(0, elemAc(1), TOL)
    }
  }

  def testGetIfft2() {
    val Acopy = A.copy()
    val Ac = A.asInstanceOf[WrapperFloatMatrix2D].getIfft2(true)
    Ac.asInstanceOf[DenseLargeFComplexMatrix2D].fft2()
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) {
      val elemAc = Ac.getQuick(r, c)
      assertEquals(Acopy.getQuick(r, c), elemAc(0), TOL)
      assertEquals(0, elemAc(1), TOL)
    }
  }

  def testGetFftColumns() {
    val Acopy = A.copy()
    val Ac = A.asInstanceOf[WrapperFloatMatrix2D].getFftColumns
    Ac.asInstanceOf[DenseLargeFComplexMatrix2D].ifftColumns(true)
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) {
      val elemAc = Ac.getQuick(r, c)
      assertEquals(Acopy.getQuick(r, c), elemAc(0), TOL)
      assertEquals(0, elemAc(1), TOL)
    }
  }

  def testGetIfftColumns() {
    val Acopy = A.copy()
    val Ac = A.asInstanceOf[WrapperFloatMatrix2D].getIfftColumns(true)
    Ac.asInstanceOf[DenseLargeFComplexMatrix2D].fftColumns()
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) {
      val elemAc = Ac.getQuick(r, c)
      assertEquals(Acopy.getQuick(r, c), elemAc(0), TOL)
      assertEquals(0, elemAc(1), TOL)
    }
  }

  def testGetFftRows() {
    val Acopy = A.copy()
    val Ac = A.asInstanceOf[WrapperFloatMatrix2D].getFftRows
    Ac.asInstanceOf[DenseLargeFComplexMatrix2D].ifftRows(true)
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) {
      val elemAc = Ac.getQuick(r, c)
      assertEquals(Acopy.getQuick(r, c), elemAc(0), TOL)
      assertEquals(0, elemAc(1), TOL)
    }
  }

  def testGetIfftRows() {
    val Acopy = A.copy()
    val Ac = A.asInstanceOf[WrapperFloatMatrix2D].getIfftRows(true)
    Ac.asInstanceOf[DenseLargeFComplexMatrix2D].fftRows()
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) {
      val elemAc = Ac.getQuick(r, c)
      assertEquals(Acopy.getQuick(r, c), elemAc(0), TOL)
      assertEquals(0, elemAc(1), TOL)
    }
  }
}
