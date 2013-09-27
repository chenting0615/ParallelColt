package cern.colt.matrix.tdouble.impl

//remove if not needed
import scala.collection.JavaConversions._

class DenseDoubleMatrix3DViewTest(arg0: String) extends DenseDoubleMatrix3DTest(arg0) {

  protected def createMatrices() {
    A = new DenseDoubleMatrix3D(NCOLUMNS, NROWS, NSLICES).viewDice(2, 1, 0)
    B = new DenseDoubleMatrix3D(NCOLUMNS, NROWS, NSLICES).viewDice(2, 1, 0)
  }
}
