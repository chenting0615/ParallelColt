package cern.colt.matrix.tdcomplex.impl

//remove if not needed
import scala.collection.JavaConversions._

class LargeDenseDComplexMatrix3DViewTest(arg0: String) extends LargeDenseDComplexMatrix3DTest(arg0) {

  protected def createMatrices() {
    A = new DenseLargeDComplexMatrix3D(NCOLUMNS, NROWS, NSLICES)
      .viewDice(2, 1, 0)
    B = new DenseLargeDComplexMatrix3D(NCOLUMNS, NROWS, NSLICES)
      .viewDice(2, 1, 0)
  }
}
