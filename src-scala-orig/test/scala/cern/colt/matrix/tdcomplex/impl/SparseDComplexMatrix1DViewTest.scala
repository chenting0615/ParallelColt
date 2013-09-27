package cern.colt.matrix.tdcomplex.impl

//remove if not needed
import scala.collection.JavaConversions._

class SparseDComplexMatrix1DViewTest(arg0: String) extends SparseDComplexMatrix1DTest(arg0) {

  protected def createMatrices() {
    A = new SparseDComplexMatrix1D(SIZE).viewFlip()
    B = new SparseDComplexMatrix1D(SIZE).viewFlip()
  }
}
