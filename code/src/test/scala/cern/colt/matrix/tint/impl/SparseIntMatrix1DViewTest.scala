package cern.colt.matrix.tint.impl

//remove if not needed
import scala.collection.JavaConversions._

class SparseIntMatrix1DViewTest(arg0: String) extends SparseIntMatrix1DTest(arg0) {

  protected def createMatrices() {
    A = new SparseIntMatrix1D(SIZE).viewFlip()
    B = new SparseIntMatrix1D(SIZE).viewFlip()
  }
}
