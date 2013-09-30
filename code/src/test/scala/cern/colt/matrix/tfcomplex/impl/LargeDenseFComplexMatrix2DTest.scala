package cern.colt.matrix.tfcomplex.impl

import cern.colt.matrix.tfcomplex.FComplexMatrix2D
import cern.colt.matrix.tfcomplex.FComplexMatrix2DTest
//remove if not needed
import scala.collection.JavaConversions._

class LargeDenseFComplexMatrix2DTest(arg0: String) extends FComplexMatrix2DTest(arg0) {

  protected def createMatrices() {
    A = new DenseLargeFComplexMatrix2D(NROWS, NCOLUMNS)
    B = new DenseLargeFComplexMatrix2D(NROWS, NCOLUMNS)
    Bt = new DenseLargeFComplexMatrix2D(NCOLUMNS, NROWS)
  }

  def testFft2() {
    val Acopy = A.copy()
    A.asInstanceOf[WrapperFComplexMatrix2D].fft2()
    A.asInstanceOf[WrapperFComplexMatrix2D].ifft2(true)
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) {
      assertEquals(Acopy.getQuick(r, c), A.getQuick(r, c), TOL)
    }
  }

  def testFftColumns() {
    val Acopy = A.copy()
    A.asInstanceOf[WrapperFComplexMatrix2D].fftColumns()
    A.asInstanceOf[WrapperFComplexMatrix2D].ifftColumns(true)
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) {
      assertEquals(Acopy.getQuick(r, c), A.getQuick(r, c), TOL)
    }
  }

  def testFftRows() {
    val Acopy = A.copy()
    A.asInstanceOf[WrapperFComplexMatrix2D].fftRows()
    A.asInstanceOf[WrapperFComplexMatrix2D].ifftRows(true)
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) {
      assertEquals(Acopy.getQuick(r, c), A.getQuick(r, c), TOL)
    }
  }
}
