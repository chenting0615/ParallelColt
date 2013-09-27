package cern.colt.matrix.tfcomplex.impl

//remove if not needed
import scala.collection.JavaConversions._

class DenseFComplexMatrix3DViewTest(arg0: String) extends DenseFComplexMatrix3DTest(arg0) {

  protected def createMatrices() {
    A = new DenseFComplexMatrix3D(NCOLUMNS, NROWS, NSLICES)
      .viewDice(2, 1, 0)
    B = new DenseFComplexMatrix3D(NCOLUMNS, NROWS, NSLICES)
      .viewDice(2, 1, 0)
  }
}
