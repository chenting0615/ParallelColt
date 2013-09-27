package cern.colt.matrix.tdouble.impl

//remove if not needed
import scala.collection.JavaConversions._

class SparseDoubleMatrix1DViewTest(arg0: String) extends SparseDoubleMatrix1DTest(arg0) {

  protected def createMatrices() {
    A = new SparseDoubleMatrix1D(SIZE).viewFlip()
    B = new SparseDoubleMatrix1D(SIZE).viewFlip()
  }
}
