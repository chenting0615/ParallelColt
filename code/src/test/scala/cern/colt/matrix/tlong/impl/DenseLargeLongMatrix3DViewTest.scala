package cern.colt.matrix.tlong.impl

//remove if not needed
import scala.collection.JavaConversions._

class DenseLargeLongMatrix3DViewTest(arg0: String) extends DenseLargeLongMatrix3DTest(arg0) {

  protected def createMatrices() {
    A = new DenseLargeLongMatrix3D(NCOLUMNS, NROWS, NSLICES)
      .viewDice(2, 1, 0)
    B = new DenseLargeLongMatrix3D(NCOLUMNS, NROWS, NSLICES)
      .viewDice(2, 1, 0)
  }
}
