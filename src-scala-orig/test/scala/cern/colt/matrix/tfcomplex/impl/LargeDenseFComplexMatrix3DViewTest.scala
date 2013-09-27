package cern.colt.matrix.tfcomplex.impl

//remove if not needed
import scala.collection.JavaConversions._

class LargeDenseFComplexMatrix3DViewTest(arg0: String) extends LargeDenseFComplexMatrix3DTest(arg0) {

  protected def createMatrices() {
    A = new DenseLargeFComplexMatrix3D(NCOLUMNS, NROWS, NSLICES)
      .viewDice(2, 1, 0)
    B = new DenseLargeFComplexMatrix3D(NCOLUMNS, NROWS, NSLICES)
      .viewDice(2, 1, 0)
  }
}
