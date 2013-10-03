package cern.colt.matrix.tdouble.impl

import cern.colt.matrix.MatrixTypes.DenseLargeDoubleMatrix2D

class DenseLargeMatrix2DTest(arg0: String) extends DoubleMatrix2DTest(arg0) {

  override protected def createMatrices() {
    A = new DenseLargeDoubleMatrix2D(NROWS, NCOLUMNS)
    B = new DenseLargeDoubleMatrix2D(NROWS, NCOLUMNS)
    Bt = new DenseLargeDoubleMatrix2D(NCOLUMNS, NROWS)
  }

/*
  def testDct2() {
    val Acopy = A.copy()
    A.asInstanceOf[WrapperMatrix2D].dct2(true)
    A.asInstanceOf[WrapperMatrix2D].idct2(true)
    for (r <- 0 until A.rows; c <- 0 until A.columns) assertEquals(0, Math.abs(Acopy.getQuick(r,
      c) - A.getQuick(r, c)), TOL)
  }

  def testDctColumns() {
    val Acopy = A.copy()
    A.asInstanceOf[WrapperMatrix2D].dctColumns(true)
    A.asInstanceOf[WrapperMatrix2D].idctColumns(true)
    for (r <- 0 until A.rows; c <- 0 until A.columns) assertEquals(0, Math.abs(Acopy.getQuick(r,
      c) - A.getQuick(r, c)), TOL)
  }

  def testDctRows() {
    val Acopy = A.copy()
    A.asInstanceOf[WrapperMatrix2D].dctRows(true)
    A.asInstanceOf[WrapperMatrix2D].idctRows(true)
    for (r <- 0 until A.rows; c <- 0 until A.columns) assertEquals(0, Math.abs(Acopy.getQuick(r,
      c) - A.getQuick(r, c)), TOL)
  }

  def testDht2() {
    val Acopy = A.copy()
    A.asInstanceOf[WrapperMatrix2D].dht2()
    A.asInstanceOf[WrapperMatrix2D].idht2(true)
    for (r <- 0 until A.rows; c <- 0 until A.columns) assertEquals(0, Math.abs(Acopy.getQuick(r,
      c) - A.getQuick(r, c)), TOL)
  }

  def testDhtColumns() {
    val Acopy = A.copy()
    A.asInstanceOf[WrapperMatrix2D].dhtColumns()
    A.asInstanceOf[WrapperMatrix2D].idhtColumns(true)
    for (r <- 0 until A.rows; c <- 0 until A.columns) assertEquals(0, Math.abs(Acopy.getQuick(r,
      c) - A.getQuick(r, c)), TOL)
  }

  def testDhtRows() {
    val Acopy = A.copy()
    A.asInstanceOf[WrapperMatrix2D].dhtRows()
    A.asInstanceOf[WrapperMatrix2D].idhtRows(true)
    for (r <- 0 until A.rows; c <- 0 until A.columns) assertEquals(0, Math.abs(Acopy.getQuick(r,
      c) - A.getQuick(r, c)), TOL)
  }

  def testDst2() {
    val Acopy = A.copy()
    A.asInstanceOf[WrapperMatrix2D].dst2(true)
    A.asInstanceOf[WrapperMatrix2D].idst2(true)
    for (r <- 0 until A.rows; c <- 0 until A.columns) assertEquals(0, Math.abs(Acopy.getQuick(r,
      c) - A.getQuick(r, c)), TOL)
  }

  def testDstColumns() {
    val Acopy = A.copy()
    A.asInstanceOf[WrapperMatrix2D].dstColumns(true)
    A.asInstanceOf[WrapperMatrix2D].idstColumns(true)
    for (r <- 0 until A.rows; c <- 0 until A.columns) assertEquals(0, Math.abs(Acopy.getQuick(r,
      c) - A.getQuick(r, c)), TOL)
  }

  def testDstRows() {
    val Acopy = A.copy()
    A.asInstanceOf[WrapperMatrix2D].dstRows(true)
    A.asInstanceOf[WrapperMatrix2D].idstRows(true)
    for (r <- 0 until A.rows; c <- 0 until A.columns) assertEquals(0, Math.abs(Acopy.getQuick(r,
      c) - A.getQuick(r, c)), TOL)
  }

  def testFft2() {
    val nrows = 64
    val ncolumns = 128
    var A = new DenseLargeMatrix2D(nrows, ncolumns)
    var Acopy = A.copy()
    A.asInstanceOf[DenseLargeMatrix2D].fft2()
    A.asInstanceOf[DenseLargeMatrix2D].ifft2(true)
    for (r <- 0 until nrows; c <- 0 until ncolumns) {
      assertEquals(Acopy.getQuick(r, c), A.getQuick(r, c), TOL)
    }
    A = A.viewTranspose()
    Acopy = A.copy()
    A.asInstanceOf[WrapperMatrix2D].fft2()
    A.asInstanceOf[WrapperMatrix2D].ifft2(true)
    for (r <- 0 until ncolumns; c <- 0 until nrows) {
      assertEquals(Acopy.getQuick(r, c), A.getQuick(r, c), TOL)
    }
  }

  def testGetFft2() {
    val Acopy = A.copy()
    val Ac = A.asInstanceOf[WrapperMatrix2D].getFft2
    Ac.asInstanceOf[DenseLargeDComplexMatrix2D].ifft2(true)
    for (r <- 0 until A.rows; c <- 0 until A.columns) {
      val elemAc = Ac.getQuick(r, c)
      assertEquals(Acopy.getQuick(r, c), elemAc(0), TOL)
      assertEquals(0, elemAc(1), TOL)
    }
  }

  def testGetIfft2() {
    val Acopy = A.copy()
    val Ac = A.asInstanceOf[WrapperMatrix2D].getIfft2(true)
    Ac.asInstanceOf[DenseLargeDComplexMatrix2D].fft2()
    for (r <- 0 until A.rows; c <- 0 until A.columns) {
      val elemAc = Ac.getQuick(r, c)
      assertEquals(Acopy.getQuick(r, c), elemAc(0), TOL)
      assertEquals(0, elemAc(1), TOL)
    }
  }

  def testGetFftColumns() {
    val Acopy = A.copy()
    val Ac = A.asInstanceOf[WrapperMatrix2D].getFftColumns
    Ac.asInstanceOf[DenseLargeDComplexMatrix2D].ifftColumns(true)
    for (r <- 0 until A.rows; c <- 0 until A.columns) {
      val elemAc = Ac.getQuick(r, c)
      assertEquals(Acopy.getQuick(r, c), elemAc(0), TOL)
      assertEquals(0, elemAc(1), TOL)
    }
  }

  def testGetIfftColumns() {
    val Acopy = A.copy()
    val Ac = A.asInstanceOf[WrapperMatrix2D].getIfftColumns(true)
    Ac.asInstanceOf[DenseLargeDComplexMatrix2D].fftColumns()
    for (r <- 0 until A.rows; c <- 0 until A.columns) {
      val elemAc = Ac.getQuick(r, c)
      assertEquals(Acopy.getQuick(r, c), elemAc(0), TOL)
      assertEquals(0, elemAc(1), TOL)
    }
  }

  def testGetFftRows() {
    val Acopy = A.copy()
    val Ac = A.asInstanceOf[WrapperMatrix2D].getFftRows
    Ac.asInstanceOf[DenseLargeDComplexMatrix2D].ifftRows(true)
    for (r <- 0 until A.rows; c <- 0 until A.columns) {
      val elemAc = Ac.getQuick(r, c)
      assertEquals(Acopy.getQuick(r, c), elemAc(0), TOL)
      assertEquals(0, elemAc(1), TOL)
    }
  }

  def testGetIfftRows() {
    val Acopy = A.copy()
    val Ac = A.asInstanceOf[WrapperMatrix2D].getIfftRows(true)
    Ac.asInstanceOf[DenseLargeDComplexMatrix2D].fftRows()
    for (r <- 0 until A.rows; c <- 0 until A.columns) {
      val elemAc = Ac.getQuick(r, c)
      assertEquals(Acopy.getQuick(r, c), elemAc(0), TOL)
      assertEquals(0, elemAc(1), TOL)
    }
  }
*/
}
