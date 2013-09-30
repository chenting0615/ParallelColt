package cern.colt.matrix.tfloat.impl

//remove if not needed
import scala.collection.JavaConversions._

class SparseFloatMatrix1DViewTest(arg0: String) extends SparseFloatMatrix1DTest(arg0) {

  protected def createMatrices() {
    A = new SparseFloatMatrix1D(SIZE).viewFlip()
    B = new SparseFloatMatrix1D(SIZE).viewFlip()
  }
}
