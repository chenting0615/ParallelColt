package cern.colt.matrix.tdcomplex.impl

import cern.colt.matrix.tdcomplex.DComplexMatrix1D
import cern.colt.matrix.tdcomplex.DComplexMatrix1DTest
//remove if not needed
import scala.collection.JavaConversions._

class DenseDComplexMatrix1DTest(arg0: String) extends DComplexMatrix1DTest(arg0) {

  protected def createMatrices() {
    A = new DenseDComplexMatrix1D(SIZE)
    B = new DenseDComplexMatrix1D(SIZE)
  }

  def testFft() {
    val Acopy = A.copy()
    A.asInstanceOf[DenseDComplexMatrix1D].fft()
    A.asInstanceOf[DenseDComplexMatrix1D].ifft(true)
    for (i <- 0 until A.size.toInt) {
      assertEquals(Acopy.getQuick(i), A.getQuick(i), TOL)
    }
  }
}
