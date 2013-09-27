package cern.colt.matrix.tint.impl

//remove if not needed
import scala.collection.JavaConversions._

class DenseIntMatrix1DViewTest(arg0: String) extends DenseIntMatrix1DTest(arg0) {

  protected def createMatrices() {
    A = new DenseIntMatrix1D(SIZE).viewFlip()
    B = new DenseIntMatrix1D(SIZE).viewFlip()
  }
}
