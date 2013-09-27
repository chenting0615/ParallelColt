package cern.colt.matrix.tdcomplex.impl

import cern.colt.matrix.tdcomplex.DComplexMatrix2D
import cern.colt.matrix.tdcomplex.DComplexMatrix2DTest
//remove if not needed
import scala.collection.JavaConversions._

class LargeDenseDComplexMatrix2DTest(arg0: String) extends DComplexMatrix2DTest(arg0) {

  protected def createMatrices() {
    A = new DenseLargeDComplexMatrix2D(NROWS, NCOLUMNS)
    B = new DenseLargeDComplexMatrix2D(NROWS, NCOLUMNS)
    Bt = new DenseLargeDComplexMatrix2D(NCOLUMNS, NROWS)
  }

  def testFft2() {
    val Acopy = A.copy()
    A.asInstanceOf[WrapperDComplexMatrix2D].fft2()
    A.asInstanceOf[WrapperDComplexMatrix2D].ifft2(true)
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) {
      assertEquals(Acopy.getQuick(r, c), A.getQuick(r, c), TOL)
    }
  }

  def testFftColumns() {
    val Acopy = A.copy()
    A.asInstanceOf[WrapperDComplexMatrix2D].fftColumns()
    A.asInstanceOf[WrapperDComplexMatrix2D].ifftColumns(true)
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) {
      assertEquals(Acopy.getQuick(r, c), A.getQuick(r, c), TOL)
    }
  }

  def testFftRows() {
    val Acopy = A.copy()
    A.asInstanceOf[WrapperDComplexMatrix2D].fftRows()
    A.asInstanceOf[WrapperDComplexMatrix2D].ifftRows(true)
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) {
      assertEquals(Acopy.getQuick(r, c), A.getQuick(r, c), TOL)
    }
  }
}
