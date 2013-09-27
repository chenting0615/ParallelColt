package cern.colt.matrix.tint.impl

//remove if not needed
import scala.collection.JavaConversions._

class DenseIntMatrix3DViewTest(arg0: String) extends DenseIntMatrix3DTest(arg0) {

  protected def createMatrices() {
    A = new DenseIntMatrix3D(NCOLUMNS, NROWS, NSLICES).viewDice(2, 1, 0)
    B = new DenseIntMatrix3D(NCOLUMNS, NROWS, NSLICES).viewDice(2, 1, 0)
  }
}
