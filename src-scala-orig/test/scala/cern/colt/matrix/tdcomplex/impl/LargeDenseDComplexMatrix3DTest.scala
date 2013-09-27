package cern.colt.matrix.tdcomplex.impl

import cern.colt.matrix.tdcomplex.DComplexMatrix3D
import cern.colt.matrix.tdcomplex.DComplexMatrix3DTest
//remove if not needed
import scala.collection.JavaConversions._

class LargeDenseDComplexMatrix3DTest(arg0: String) extends DComplexMatrix3DTest(arg0) {

  protected def createMatrices() {
    A = new DenseLargeDComplexMatrix3D(NSLICES, NROWS, NCOLUMNS)
    B = new DenseLargeDComplexMatrix3D(NSLICES, NROWS, NCOLUMNS)
  }

  def testFft3() {
    val Acopy = A.copy()
    A.asInstanceOf[WrapperDComplexMatrix3D].fft3()
    A.asInstanceOf[WrapperDComplexMatrix3D].ifft3(true)
    for (s <- 0 until A.slices(); r <- 0 until A.rows(); c <- 0 until A.columns()) {
      assertEquals(Acopy.getQuick(s, r, c), A.getQuick(s, r, c), TOL)
    }
  }

  def testFft2Slices() {
    val Acopy = A.copy()
    A.asInstanceOf[WrapperDComplexMatrix3D].fft2Slices()
    A.asInstanceOf[WrapperDComplexMatrix3D].ifft2Slices(true)
    for (s <- 0 until A.slices(); r <- 0 until A.rows(); c <- 0 until A.columns()) {
      assertEquals(Acopy.getQuick(s, r, c), A.getQuick(s, r, c), TOL)
    }
  }
}
