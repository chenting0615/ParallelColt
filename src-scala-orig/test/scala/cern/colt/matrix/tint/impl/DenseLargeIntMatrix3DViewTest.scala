package cern.colt.matrix.tint.impl

//remove if not needed
import scala.collection.JavaConversions._

class DenseLargeIntMatrix3DViewTest(arg0: String) extends DenseLargeIntMatrix3DTest(arg0) {

  protected def createMatrices() {
    A = new DenseLargeIntMatrix3D(NCOLUMNS, NROWS, NSLICES)
      .viewDice(2, 1, 0)
    B = new DenseLargeIntMatrix3D(NCOLUMNS, NROWS, NSLICES)
      .viewDice(2, 1, 0)
  }
}
