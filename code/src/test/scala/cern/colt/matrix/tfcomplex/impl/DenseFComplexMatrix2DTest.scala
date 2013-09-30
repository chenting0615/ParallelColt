package cern.colt.matrix.tfcomplex.impl

import cern.colt.matrix.tfcomplex.FComplexMatrix2D
import cern.colt.matrix.tfcomplex.FComplexMatrix2DTest
//remove if not needed
import scala.collection.JavaConversions._

class DenseFComplexMatrix2DTest(arg0: String) extends FComplexMatrix2DTest(arg0) {

  protected def createMatrices() {
    A = new DenseFComplexMatrix2D(NROWS, NCOLUMNS)
    B = new DenseFComplexMatrix2D(NROWS, NCOLUMNS)
    Bt = new DenseFComplexMatrix2D(NCOLUMNS, NROWS)
  }

  def testFft2() {
    val Acopy = A.copy()
    A.asInstanceOf[DenseFComplexMatrix2D].fft2()
    A.asInstanceOf[DenseFComplexMatrix2D].ifft2(true)
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) {
      assertEquals(Acopy.getQuick(r, c), A.getQuick(r, c), TOL)
    }
  }

  def testFftColumns() {
    val Acopy = A.copy()
    A.asInstanceOf[DenseFComplexMatrix2D].fftColumns()
    A.asInstanceOf[DenseFComplexMatrix2D].ifftColumns(true)
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) {
      assertEquals(Acopy.getQuick(r, c), A.getQuick(r, c), TOL)
    }
  }

  def testFftRows() {
    val Acopy = A.copy()
    A.asInstanceOf[DenseFComplexMatrix2D].fftRows()
    A.asInstanceOf[DenseFComplexMatrix2D].ifftRows(true)
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) {
      assertEquals(Acopy.getQuick(r, c), A.getQuick(r, c), TOL)
    }
  }
}
