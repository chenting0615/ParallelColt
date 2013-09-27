package cern.colt.matrix.tdcomplex.impl

//remove if not needed
import scala.collection.JavaConversions._

class DenseDComplexMatrix1DViewTest(arg0: String) extends DenseDComplexMatrix1DTest(arg0) {

  protected def createMatrices() {
    A = new DenseDComplexMatrix1D(SIZE).viewFlip()
    B = new DenseDComplexMatrix1D(SIZE).viewFlip()
  }
}
