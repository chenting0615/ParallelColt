package cern.colt.matrix.tfcomplex.impl

import cern.colt.matrix.tfcomplex.FComplexMatrix1D
import cern.colt.matrix.tfcomplex.FComplexMatrix1DTest
//remove if not needed
import scala.collection.JavaConversions._

class DenseFComplexMatrix1DTest(arg0: String) extends FComplexMatrix1DTest(arg0) {

  protected def createMatrices() {
    A = new DenseFComplexMatrix1D(SIZE)
    B = new DenseFComplexMatrix1D(SIZE)
  }

  def testFft() {
    val Acopy = A.copy()
    A.asInstanceOf[DenseFComplexMatrix1D].fft()
    A.asInstanceOf[DenseFComplexMatrix1D].ifft(true)
    for (i <- 0 until A.size.toInt) {
      assertEquals(Acopy.getQuick(i), A.getQuick(i), TOL)
    }
  }
}
