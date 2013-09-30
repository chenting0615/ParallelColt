package cern.colt.matrix.tfloat.impl

//remove if not needed
import scala.collection.JavaConversions._

class SparseFloatMatrix3DViewTest(arg0: String) extends SparseFloatMatrix3DTest(arg0) {

  protected def createMatrices() {
    A = new SparseFloatMatrix3D(NCOLUMNS, NROWS, NSLICES).viewDice(2, 1, 0)
    B = new SparseFloatMatrix3D(NCOLUMNS, NROWS, NSLICES).viewDice(2, 1, 0)
  }
}
