package cern.colt.matrix.tdouble.impl

//remove if not needed
import scala.collection.JavaConversions._

class DenseDoubleMatrix1DViewTest(arg0: String) extends DenseMatrix1DTest(arg0) {

  protected def createMatrices() {
    A = new DenseMatrix1D(SIZE).viewFlip()
    B = new DenseMatrix1D(SIZE).viewFlip()
  }
}
