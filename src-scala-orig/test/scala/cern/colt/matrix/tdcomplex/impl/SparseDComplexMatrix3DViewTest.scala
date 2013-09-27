package cern.colt.matrix.tdcomplex.impl

//remove if not needed
import scala.collection.JavaConversions._

class SparseDComplexMatrix3DViewTest(arg0: String) extends SparseDComplexMatrix3DTest(arg0) {

  protected def createMatrices() {
    A = new SparseDComplexMatrix3D(NCOLUMNS, NROWS, NSLICES)
      .viewDice(2, 1, 0)
    B = new SparseDComplexMatrix3D(NCOLUMNS, NROWS, NSLICES)
      .viewDice(2, 1, 0)
  }
}
