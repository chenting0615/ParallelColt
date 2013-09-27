package cern.colt.matrix.tlong.impl

//remove if not needed
import scala.collection.JavaConversions._

class SparseLongMatrix1DViewTest(arg0: String) extends SparseLongMatrix1DTest(arg0) {

  protected def createMatrices() {
    A = new SparseLongMatrix1D(SIZE).viewFlip()
    B = new SparseLongMatrix1D(SIZE).viewFlip()
  }
}
