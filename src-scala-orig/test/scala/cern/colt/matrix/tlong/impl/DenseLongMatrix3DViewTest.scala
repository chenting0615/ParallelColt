package cern.colt.matrix.tlong.impl

//remove if not needed
import scala.collection.JavaConversions._

class DenseLongMatrix3DViewTest(arg0: String) extends DenseLongMatrix3DTest(arg0) {

  protected def createMatrices() {
    A = new DenseLongMatrix3D(NCOLUMNS, NROWS, NSLICES).viewDice(2, 1, 0)
    B = new DenseLongMatrix3D(NCOLUMNS, NROWS, NSLICES).viewDice(2, 1, 0)
  }
}
