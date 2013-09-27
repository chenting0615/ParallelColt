package cern.colt.matrix.tdcomplex.impl

import cern.colt.matrix.tdcomplex.DComplexMatrix2D
import cern.colt.matrix.tdcomplex.DComplexMatrix2DTest
//remove if not needed
import scala.collection.JavaConversions._

class DenseDComplexMatrix2DTest(arg0: String) extends DComplexMatrix2DTest(arg0) {

  protected def createMatrices() {
    A = new DenseDComplexMatrix2D(NROWS, NCOLUMNS)
    B = new DenseDComplexMatrix2D(NROWS, NCOLUMNS)
    Bt = new DenseDComplexMatrix2D(NCOLUMNS, NROWS)
  }

  def testFft2() {
    val Acopy = A.copy()
    A.asInstanceOf[DenseDComplexMatrix2D].fft2()
    A.asInstanceOf[DenseDComplexMatrix2D].ifft2(true)
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) {
      assertEquals(Acopy.getQuick(r, c), A.getQuick(r, c), TOL)
    }
  }

  def testFftColumns() {
    val Acopy = A.copy()
    A.asInstanceOf[DenseDComplexMatrix2D].fftColumns()
    A.asInstanceOf[DenseDComplexMatrix2D].ifftColumns(true)
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) {
      assertEquals(Acopy.getQuick(r, c), A.getQuick(r, c), TOL)
    }
  }

  def testFftRows() {
    val Acopy = A.copy()
    A.asInstanceOf[DenseDComplexMatrix2D].fftRows()
    A.asInstanceOf[DenseDComplexMatrix2D].ifftRows(true)
    for (r <- 0 until A.rows(); c <- 0 until A.columns()) {
      assertEquals(Acopy.getQuick(r, c), A.getQuick(r, c), TOL)
    }
  }
}
