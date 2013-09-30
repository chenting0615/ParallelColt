package cern.colt.matrix.tfcomplex.impl

//remove if not needed
import scala.collection.JavaConversions._

class DenseFComplexMatrix1DViewTest(arg0: String) extends DenseFComplexMatrix1DTest(arg0) {

  protected def createMatrices() {
    A = new DenseFComplexMatrix1D(SIZE).viewFlip()
    B = new DenseFComplexMatrix1D(SIZE).viewFlip()
  }
}
