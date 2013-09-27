package cern.colt.matrix.tdouble.impl

//remove if not needed
import scala.collection.JavaConversions._

class SparseDoubleMatrix3DViewTest(arg0: String) extends SparseDoubleMatrix3DTest(arg0) {

  protected def createMatrices() {
    A = new SparseDoubleMatrix3D(NCOLUMNS, NROWS, NSLICES).viewDice(2, 1, 0)
    B = new SparseDoubleMatrix3D(NCOLUMNS, NROWS, NSLICES).viewDice(2, 1, 0)
  }
}
