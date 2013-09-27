package cern.colt.matrix.tdcomplex.impl

//remove if not needed
import scala.collection.JavaConversions._

class DenseDComplexMatrix3DViewTest(arg0: String) extends DenseDComplexMatrix3DTest(arg0) {

  protected def createMatrices() {
    A = new DenseDComplexMatrix3D(NCOLUMNS, NROWS, NSLICES)
      .viewDice(2, 1, 0)
    B = new DenseDComplexMatrix3D(NCOLUMNS, NROWS, NSLICES)
      .viewDice(2, 1, 0)
  }
}
