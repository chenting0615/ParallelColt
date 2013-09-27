package cern.colt.matrix.tfcomplex.impl

//remove if not needed
import scala.collection.JavaConversions._

class SparseFComplexMatrix1DViewTest(arg0: String) extends SparseFComplexMatrix1DTest(arg0) {

  protected def createMatrices() {
    A = new SparseFComplexMatrix1D(SIZE).viewFlip()
    B = new SparseFComplexMatrix1D(SIZE).viewFlip()
  }
}
