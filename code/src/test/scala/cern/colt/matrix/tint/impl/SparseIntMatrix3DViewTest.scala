package cern.colt.matrix.tint.impl

//remove if not needed
import scala.collection.JavaConversions._

class SparseIntMatrix3DViewTest(arg0: String) extends SparseIntMatrix3DTest(arg0) {

  protected def createMatrices() {
    A = new SparseIntMatrix3D(NCOLUMNS, NROWS, NSLICES).viewDice(2, 1, 0)
    B = new SparseIntMatrix3D(NCOLUMNS, NROWS, NSLICES).viewDice(2, 1, 0)
  }
}
