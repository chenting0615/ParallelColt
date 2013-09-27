package cern.colt.matrix.tlong.impl

//remove if not needed
import scala.collection.JavaConversions._

class DenseLongMatrix1DViewTest(arg0: String) extends DenseLongMatrix1DTest(arg0) {

  protected def createMatrices() {
    A = new DenseLongMatrix1D(SIZE).viewFlip()
    B = new DenseLongMatrix1D(SIZE).viewFlip()
  }
}
