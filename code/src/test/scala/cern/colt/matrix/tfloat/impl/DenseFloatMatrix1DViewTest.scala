package cern.colt.matrix.tfloat.impl

//remove if not needed
import scala.collection.JavaConversions._

class DenseFloatMatrix1DViewTest(arg0: String) extends DenseFloatMatrix1DTest(arg0) {

  protected def createMatrices() {
    A = new DenseFloatMatrix1D(SIZE).viewFlip()
    B = new DenseFloatMatrix1D(SIZE).viewFlip()
  }
}
