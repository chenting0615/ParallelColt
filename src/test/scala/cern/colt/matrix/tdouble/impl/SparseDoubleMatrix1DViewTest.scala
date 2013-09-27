package cern.colt.matrix.tdouble.impl

//remove if not needed
import scala.collection.JavaConversions._

class SparseDoubleMatrix1DViewTest(arg0: String) extends SparseMatrix1DTest(arg0) {

  protected def createMatrices() {
    A = new SparseMatrix1D(SIZE).viewFlip()
    B = new SparseMatrix1D(SIZE).viewFlip()
  }
}
