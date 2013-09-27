package cern.colt.matrix.tfcomplex.impl

import cern.colt.matrix.tfcomplex.FComplexMatrix3D
import cern.colt.matrix.tfcomplex.FComplexMatrix3DTest
//remove if not needed
import scala.collection.JavaConversions._

class DenseFComplexMatrix3DTest(arg0: String) extends FComplexMatrix3DTest(arg0) {

  protected def createMatrices() {
    A = new DenseFComplexMatrix3D(NSLICES, NROWS, NCOLUMNS)
    B = new DenseFComplexMatrix3D(NSLICES, NROWS, NCOLUMNS)
  }

  def testFft3() {
    val Acopy = A.copy()
    A.asInstanceOf[DenseFComplexMatrix3D].fft3()
    A.asInstanceOf[DenseFComplexMatrix3D].ifft3(true)
    for (s <- 0 until A.slices(); r <- 0 until A.rows(); c <- 0 until A.columns()) {
      assertEquals(Acopy.getQuick(s, r, c), A.getQuick(s, r, c), TOL)
    }
  }

  def testFft2Slices() {
    val Acopy = A.copy()
    A.asInstanceOf[DenseFComplexMatrix3D].fft2Slices()
    A.asInstanceOf[DenseFComplexMatrix3D].ifft2Slices(true)
    for (s <- 0 until A.slices(); r <- 0 until A.rows(); c <- 0 until A.columns()) {
      assertEquals(Acopy.getQuick(s, r, c), A.getQuick(s, r, c), TOL)
    }
  }
}
